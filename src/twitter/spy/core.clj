(ns twitter.spy.core
  (:require [clojure.string :as str]
            [com.hypirion.clj-xchart :as c]
            [twitter.api.restful :as tw]
            [twitter.oauth :as auth]))

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
                            :include-rts false
                            :count 200} params))))]
    (loop [tl (get-tw {})
           c (dec times)]
      (if (>= 0 c)
        tl
        (recur
         (concat tl
                 (get-tw {:max-id (dec (apply min (map :id tl)))}))
         (dec c))))))

(defn tweet-by-day
  [tl]
  (->> tl
       (map :created_at)
       (map #(java.util.Date. %))
       (map #(java.util.Date. (+ 1900 (.getYear %)) (.getMonth %) (.getDate %)))
       (frequencies)
       (into (sorted-map))))

(defn tweet-by-day-chart
  [tbd]
  (c/xy-chart {"Tweet par jour" {:x (keys tbd) :y (vals tbd) :style {:marker-type :none}}}
              {:width 800
               :height 600
               :legend {:visible? false}
               :series-order (map str (keys tbd))
               :render-style :area
               :date-pattern "dd-MM-YY"}))

(defn map-keys
  "Apply f to every keys of a map.

  Exemple (map-vals name {:a 1 :b 2}) => {\"a\" 1 \"b\" 2}

  The case where f do not map uniquely keys is by replacement by default (assoc style) :
  (map-vals (constantly :c) {:a 1 :b 2}) => {:c 2}

  You can specify a combinator for saying how to deal with redundant keys :
  (map-vals (constantly :c) {:a 1 :b 2} conj) => {:c '(1 2)}
  "
  ([f m]
   (reduce-kv (fn [m' k v] (assoc m' (f k) v)) {} m))
  ([f m comb]
   (reduce-kv (fn [m' k v] (update m' (f k) comb v)) {} m)))
