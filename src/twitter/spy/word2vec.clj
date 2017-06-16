(ns twitter.spy.word2vec
  (:import (org.deeplearning4j.models.word2vec
            Word2Vec
            Word2Vec$Builder)
           (org.deeplearning4j.text.sentenceiterator
            CollectionSentenceIterator)
           (org.deeplearning4j.text.tokenization.tokenizerfactory
            DefaultTokenizerFactory)
           (org.deeplearning4j.text.tokenization.tokenizer.preprocessor
            LowCasePreProcessor)
           (org.deeplearning4j.plot
            BarnesHutTsne
            BarnesHutTsne$Builder))
  (:require [nd4clj.matrix :as nd4]
            [clojure.core.matrix :as m])
  (:gen-class))

(m/set-current-implementation :nd4j)

(def layersize 50)

(defn make-word2vec
  [corpus]
  (let [it (CollectionSentenceIterator. corpus)
        tkn (DefaultTokenizerFactory.)]
    (.setTokenPreProcessor tkn (LowCasePreProcessor.))
    (-> (Word2Vec$Builder.)
        (.minWordFrequency 1)
        (.iterations 10)
        (.layerSize layersize)
        (.windowSize 5)
        (.iterate it)
        (.seed 42)
        (.tokenizerFactory tkn)
        (.build))))


(def tsne
  (-> (BarnesHutTsne$Builder.)
      (.setMaxIter 1000)
      (.stopLyingIteration 250)
      (.learningRate 500)
      (.useAdaGrad false)
      (.theta 0.5)
      (.setMomentum 0.5)
      (.normalize true)
      (.build)))

(defn combine
  [w2v wordmap]
  (let [getvec (fn [word] (or
                           (.getWordVector w2v word)
                           (m/zero-vector :nd4j layersize)))]
    (m/div
     (reduce-kv
      #(m/add %1
              (m/mul (getvec %2) %3))
      (m/zero-vector :nd4j layersize)
      wordmap)
     (reduce-kv
      #(if (.getWordVector w2v %2)
         (+ %1 %3)
         %1)
      0 wordmap))))

(defn prod
  [v1 v2]
  (-> (.mmul (.a v1)
             (.transpose (.a v2)))
      m/array
      m/to-vector
      first))

(defn cosine
  [v1 v2]
  (/ (prod v1 v2)
     (* (Math/sqrt (prod v1 v1))
        (Math/sqrt (prod v2 v2)))))

(defn cosine-similarity
  [w2v]
  (fn [wordmap1 wordmap2]
    (cosine (combine w2v wordmap1)
            (combine w2v wordmap2))))
