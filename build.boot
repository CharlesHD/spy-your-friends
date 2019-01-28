(set-env!
 :source-paths #{"src"}
 :resource-paths #{"res"}
 :target-path "tmp"
 :dependencies '[[org.clojure/clojure "1.9.0-alpha17"]
                 [twitter-api "1.8.0"]
                 [chulper "1.1.1"]
                 [alex "0.0.5"]
                 [com.taoensso/tufte "1.1.0"]
                 [com.hypirion/clj-xchart "0.2.0"]
                 [org.clojure/core.cache "0.6.5"]
                 [net.mikera/core.matrix "0.53.0" :classifier "tests"]
                 [net.mikera/vectorz-clj "0.45.0"]
                 [org.deeplearning4j/deeplearning4j-core "0.7.2"]
                 [org.deeplearning4j/deeplearning4j-nlp "0.7.2"]
                 [org.deeplearning4j/deeplearning4j-nn "0.7.2"]
                 [org.nd4j/nd4j-native "0.7.2"]
                 [org.clojars.ds923y/nd4clj "0.1.1-SNAPSHOT"]])

(task-options!
 pom {:project 'spy-your-friends
      :version "0.0.1"}
 jar {:manifest {"Foo" "bar"}})
