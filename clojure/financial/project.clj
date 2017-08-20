(defproject financial "0.1.0-SNAPSHOT"
  :description "TODO"
  :url "TODO"
  :license {:name "TODO: Choose a license"
            :url "http://choosealicense.com/"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.csv "0.1.2"]
                 [clj-time "0.10.0"]
                 [me.raynes/fs "1.4.6"]
                 [enclog "0.6.5"]
                 [org.encog/encog-core "3.3.0"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.10"]]
                   :source-paths ["dev"]}})
