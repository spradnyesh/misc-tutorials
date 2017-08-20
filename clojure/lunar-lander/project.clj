(defproject lunar-lander "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.encog/encog-core "3.3.0"]]
  :plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT" :exclusions [org.clojure/tools.nrepl]]
            [lein-environ "1.0.0"]
            [lein-autoexpect "1.4.0"]]
  :jvm-opts ["-Xmx4g" "-Xms1g"]
  :main lunar-lander.core)
