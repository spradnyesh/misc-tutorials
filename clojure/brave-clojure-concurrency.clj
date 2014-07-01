(ns brave-clojure-concurrency)

(defmacro create-callback [f & args]
  `(let [p# (promise)]
     (future (do @p#
                 (apply ~f ~@args)))
     p#))

(defn caller []
  (println "inside caller, before registering callback")
  (let [p (create-callback #(do (println "inside callback")
                                (println %)))]
    (println "before sleeping")
    (Thread/sleep 1000)
    (println "before calling callback")
    (deliver p 1)
    (println "after calling callback")))
