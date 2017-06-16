(ns twitter.spy.tfidf
  "TFIDF and other text similarity metrics."
  (:require [clojure.string :as string]
            [taoensso.tufte :refer [defnp p]]
            [clojure.java.io :as io]
            [clojure.core.matrix :as m]))

(defnp tokenize-old [s]
  (if (empty? s)
    []
    (string/split
   (string/lower-case s)
   #"[^a-z0-9äöüáéíóúãâêîôûàèìòùçñ]+")))

(defnp tokenize [s]
  (let
      [f (io/file (str (gensym "tokenize")))
       res (do (spit f s)
               (->>
            (clojure.java.shell/sh
             "/opt/albert/bin/albScript"
             "-f"
             "/home/charleshd/scripts/amitext.js"
             (.getAbsolutePath f))
            :out
            (string/split-lines)
            (partition 2)
            (map vec)
            (map (fn [[a b]] {a (read-string b)}))
            (apply merge-with + (sorted-map))))]
    (clojure.java.shell/sh "rm" (.getAbsolutePath f))
    res
    ))

(defn safe-div
  [a b]
  (if (zero? b) 0 (/ a b)))

(defn word-vec
  [doc]
  (if (string? doc) ;; polymorphisme de pauvre
    (tokenize doc)
    (if (map? doc)
      doc
      (throw (ex-info "This is not word-vecable" {:arg doc})))))

(defn word-vec-norm
  [f wordvec]
  (reduce-kv #(assoc %1 %2 (f %3)) (sorted-map) wordvec))

(defn k-max-norm
  [k wordvec]
  (let [maxfreq (apply max (vals wordvec))
        f (fn [freq] (+ k (* (- 1 k) freq (/ 1 maxfreq))))]
    (word-vec-norm f wordvec)))

(defn log-norm
  [wordvec]
  (let [f (fn [freq] (+ 1 (Math/log freq)))]
    (word-vec-norm f wordvec)))

(defn binary-norm
  [wordvec]
  (let [f (fn [freq] (if (pos? freq) 1 0))]
    (if (map? wordvec)
      (word-vec-norm f wordvec)
      (map f wordvec))))

(defn tf
  [doc]
  (->> doc
       (word-vec)
       ;; (binary-norm)
       ))

(defn idf-vec
  [corpus]
  (let [tot (Math/log (count corpus))]
    (->>
     (apply concat (map (comp keys word-vec) corpus))
       frequencies
       (remove #(= (val %) 1))
       (into (sorted-map))
       (reduce-kv (fn [m k v] (assoc m k (- (Math/log v) tot))) {})
       (into (sorted-map)))))

(defn tfidf-fn
  [corpus]
  (let [idf (idf-vec corpus)]
     (fn [doc]
       (p :tfidf-fn
        (let [tfd (tf doc)]
          (m/array :double-array
           (for [[k v] idf] (* v (get tfd k 0)))
           #_(vals (reduce-kv (fn [m k v] (assoc m k (* v
                                                      (get tfd k 0)))) {} idf))))))))

(defn tf-fn
  [corpus]
  (let [idf (idf-vec corpus)]
    (fn [doc]
      (p :tf-fn
         (let [tfd (tf doc)]
           (m/array :double-array
            (for [k (keys idf)] (get tfd k 0))
            #_(vals (reduce-kv (fn [m k _] (assoc m k (get tfd k 0))) {} idf))))))))

(defn tfidf
  [corpus]
  (let [f (tfidf-fn corpus)]
    (reduce
     (fn [m doc]
       (conj m (f doc)))
     []
     corpus)))


(defn scalar
  [v1 v2]
  (m/mmul v1 v2))

(defn square-scalar
  [v]
  (scalar v v))

(defn norme2
  [v]
  (Math/sqrt (square-scalar v)))

(defn bitmask
  [v1 v2]
  (m/array (map #(if (zero? %2) 0 %1) v1 v2)))

(defn soergel-distance
  [v1 v2]
  (let [av1 (m/abs v1)
        av2 (m/abs v2)]
    (- 1
       (safe-div (reduce + (m/abs (m/sub av1 av2)))
                 (reduce + (map max av1 av2))))))

(defn cosine-similarity
  [v1 v2]
  (let [den (* (norme2 v1) (norme2 v2))]
    (safe-div (scalar v1 v2)
              (* den))))

(defn sorensen-dice-similarity
  [v1 v2]
  (safe-div (* 2 (scalar v1 v2))
     (+ (square-scalar v1)
        (square-scalar v2))))

(defn jaccard-similarity
  [v1 v2]
  (safe-div (scalar v1 v2)
     (+ (square-scalar v1)
        (square-scalar v2)
        (- (scalar v1 v2)))))

(defnp tfidf-cosine-similarity-matrix
  [corpus]
  (let [tfidf-vec (tfidf corpus)]
    (vec (for [v1 tfidf-vec]
           (vec (for [v2 tfidf-vec]
                  (cosine-similarity v1 v2)))))))

(defn tf-similarity-fn
  [similarity-fn corpus {:keys [memoize? masked? idf? binary?]
                           :or {:memoize? true
                                :masked? true
                                :idf? true
                                :binary? false}}]
  (let [f (if idf? tfidf-fn tf-fn)
        f (if memoize?
            (memoize (f corpus))
            (f corpus))
        f (if binary? (comp binary-norm f) f)]
    (fn [d1 d2] (p :tfidf-similarity-sim
                   (let [v1 (f d1) v2 (f d2)]
                     (similarity-fn (if masked? (bitmask v1 v2) v1) v2))))))

(def tfidf-cosine-similarity-fn (partial tf-similarity-fn cosine-similarity))
(def tfidf-soergel-similarity-fn (partial tf-similarity-fn soergel-distance))
(def tfidf-jaccard-similarity-fn (partial tf-similarity-fn jaccard-similarity))
(def tfidf-sorensen-dice-similarity-fn (partial tf-similarity-fn sorensen-dice-similarity))
