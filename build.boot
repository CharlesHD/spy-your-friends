(set-env!
 :source-paths #{"src"}
 :resource-paths #{"res"}
 :target-path "tmp"
 :dependencies '[[org.clojure/clojure "1.9.0-alpha17"]
                 [twitter-api "1.8.0"]
                 [com.taoensso/tufte "1.1.0"]
                 [com.hypirion/clj-xchart "0.2.0"]])

(task-options!
 pom {:project 'spy-your-friends
      :version "0.0.1"}
 jar {:manifest {"Foo" "bar"}})
