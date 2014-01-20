(defproject project-euler "0.1.0-SNAPSHOT"
  :description "Solve problems on project-euler website"
  :url "http://projecteuler.net/problems"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [expectations "1.4.52"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :plugins [[lein-autoexpect "1.0"]])
