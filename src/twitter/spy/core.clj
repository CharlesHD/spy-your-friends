(ns twitter.spy.core
  (:require [chu.alex.core :as alex]
            [chulper.core :as h]
            [clojure.core.cache :as cache]
            [clojure.core.matrix :as matrix]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [com.hypirion.clj-xchart :as c]
            [twitter.api.restful :as tw]
            [twitter.oauth :as auth]
            [twitter.request :refer [file-body-part status-body-part]]
            [twitter.spy.tfidf :as tfidf]
            [twitter.spy.word2vec :as w2v]))

(def id-tl-cache (atom (cache/ttl-cache-factory {} :ttl (* 1000 60 60 3))))

(defn mk-creds-from-file [f]
  (let [[app-key app-secret token token-secret]
        (str/split-lines (slurp f))]
    (auth/make-oauth-creds app-key
                           app-secret
                           token
                           token-secret)))

(defn get-tl
  [creds id times]
  (let [get-tw
        (fn [params]
          (:body (tw/statuses-user-timeline
                  :oauth-creds creds
                  :params (merge
                           {:user-id id
                            ;; :include-rts false
                            :count 200} params))))]
    (loop [tl (get-tw {})
           c (dec times)]
      (if (>= 0 c)
        tl
        (recur
         (concat tl
                 (get-tw {:max-id (dec (apply min (map :id tl)))}))
         (dec c))))))

(defn get-id-by-screen-name
 [creds screen-name]
 (-> (tw/users-lookup
      :oauth-creds creds
      :params {:screen-name screen-name})
     :body first :id))

(defn ratio
  [coll]
  (h/map-vals #(/ % (count coll)) (frequencies coll)))

(defn tweet-by
  [tl f]
  (->> tl f (frequencies) (into (sorted-map))))

(defn tweet-by-day
  [tl]
  (let [day-date #(as-> % d
                    (java.util.Date. d)
                    (java.util.Date. (.getYear d) (.getMonth d) (.getDate d)))]
  (tweet-by tl (partial map (comp day-date :created_at)))))

(defn tweet-by-month
  [tl]
  (let [month-date #(as-> % d
                    (java.util.Date. d)
                    (java.util.Date. (.getYear d) (.getMonth d) 0))]
    (tweet-by tl (partial map (comp month-date :created_at)))))

(defn tweet-by-hour
  [tl]
(let [get-hour #(-> %
                    (java.util.Date. )
                    (.getHours))]
  (merge
   (zipmap (range 24) (repeat 0))
   (tweet-by tl (partial map (comp get-hour :created_at))))))

(defn tweet-by-chart
  [construction options f tl]
  (let [extract (fn [coll]
                  (merge
                   (h/apply-vals {:x keys :y vals} coll)
                   {:style {:marker-type :none}}))
        normalize (fn [m]
                    (into (sorted-map)
                          (h/map-vals #(* 100 (/ % (count tl))) m)))
        opts {:width 800
              :height 600
              :legend {:position :inside-nw}
              :render-style :area}]
    (c/xy-chart
     (h/map-vals (comp extract normalize f #(% tl)) construction)
     (merge opts options))))

(defn- reply-to-other?
  [tweet]
  ((every-pred :in_reply_to_status_id #(not= (:in_reply_to_user_id %) (:id (:user %))))
   tweet))

(defn tweet-vs-rt-chart
  [options f tl]
  (tweet-by-chart
   {"Total" identity
    "Tweet" (partial remove :retweeted_status)
    ;; "Reply" (partial filter reply-to-other?)
    ;; "Personal" (partial remove reply-to-other?)
    "RT" (partial filter :retweeted_status)}
   (merge {:date-pattern "dd-MM-YYYY"} options)
   f
   tl))

(defn- parse-xml-string
  [s]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes s))))

(defn tweet-by-source-chart
  [options f tl]
  (tweet-by-chart (h/map-vals
                   constantly
                   (group-by (comp first :content parse-xml-string :source) tl))
                  options f tl))

(defn select-day
  [day tl]
  (filter #(= day (-> % :created_at (java.util.Date.) (.getDay))) tl))

(defn baos->body
  [baos]
  (json/read-json (str baos)))

(defn- add-to-cache
  ([c key delayed-val]
    (if (cache/has? c key)
      (cache/hit c key)
      (cache/miss c key @delayed-val)))
  ([key delayed-val]
   (swap! id-tl-cache add-to-cache key delayed-val)))

(defn add-tl
  [creds screen_name]
  (add-to-cache
   screen_name
   (delay (get-tl creds (get-id-by-screen-name creds screen_name) 16)))
  (println screen_name "added to the cache !"))

(defn send-chart
  [creds screen_name chart-fn msg]
  ;; first add the time-line to cache
  (add-tl creds screen_name)
  (let [tl (cache/lookup @id-tl-cache screen_name)
        chart-path (str "/tmp/" (gensym screen_name) ".png")]
    (c/spit (chart-fn tl) chart-path)
    (tw/statuses-update-with-media :oauth-creds creds
                                   :body [(file-body-part chart-path)
                                          (status-body-part (str "@" screen_name " " msg))])))


(defn- clean-mentions
  [tweet-text]
  (str/trim (str/replace tweet-text #"@\w+" "")))

(defn- doc-topics
  [doclist]
  (let [vec-fn (tfidf/tf-fn doclist)
        wvec (vec (tfidf/idf-vec doclist))
        avg #(matrix/div (reduce matrix/add %) (count %))
        vec-type (avg (map vec-fn doclist))]
    (->> vec-type
         (zipmap (range))
         (sort-by val >)
         (map #(get wvec (key %)))
         )))

(defn topics-vec
  [tl]
  (doc-topics (pmap (comp tfidf/tokenize clean-mentions :text) tl)))

(defn topics
  [tl]
  (->> tl
       topics-vec
       (map key)
       (remove #{"t.co" "https" "rt" "http" "lt" "fr"})
       (take 100)))

(declare w2v)

(defn request
  [tl reqs]
  (let [token (memoize (comp tfidf/tokenize clean-mentions :text))
        vec #(w2v/combine w2v %)
        sim (w2v/cosine-similarity w2v)
        d->vec (comp vec token)
        avg #(matrix/div (reduce matrix/add %) (count %))
        vec-type (fn [coll] (avg (map d->vec coll)))
        req-vec (fn [req] (vec (tfidf/tokenize req)))
        distrib (group-by
                 (comp #(java.util.Date. (.getYear %) (.getMonth %)  0 #_(* (int (/ (.getDate %) 7)) 7))
                       #(java.util.Date. %)
                       :created_at)
                 tl)]
    (reduce #(assoc %1 %2
                    (h/map-vals
                     (fn [dl] (sim (tfidf/tokenize %2) (tfidf/tokenize (str/join " " (map (comp clean-mentions :text) dl)))))
                     distrib))
            {} reqs)))


(defn request-chart
  [options reqs tl]
  (let [rq (request tl reqs)]
    (tweet-by-chart
     (h/map-vals constantly rq)
     options
     identity
     (map first (vals rq)))))

(defn build-alex
  [tl]
  (reset! alex/*gdr* {})
  (reset! alex/*alexandrins*
          (filter
           #(<= 10 (chu.alex.phonetique/compte-syllabe %) 13)
           (apply concat
                  (pmap
                   (comp chu.alex.extract/phrases clean-mentions :text)
                   (remove :retweeted_status tl)))))
  (alex/construire-gdr)
  (println "done"))

(defn tweet-sonnet
  [creds screen-name]
  (let [s (alex/sonnet)
        f (str "/tmp/" (gensym "sonnet") ".png")]
    (clojure.java.shell/sh "convert" (str "label:\"" s "\"") f)
    (tw/statuses-update-with-media :oauth-creds creds
                                   :body [(file-body-part f)
                                          (status-body-part (str "fait depuis la TL de : " screen-name))])))
