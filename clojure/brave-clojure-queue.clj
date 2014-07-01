(ns brave-clojure-queue)

(defn append-to-file
  [filename s]
  (spit filename s :append true))

(defn format-quote [quote]
  (str "==== BEGIN QUOTE ===\n"
       quote
       "==== END QUOTE ===\n\n"))

(defn snag-quotes
  [n filename]
  (dotimes [_ n]
    (->> (slurp "http://www.iheartquotes.com/api/v1/random")
         format-quote
         (append-to-file filename)
         (future))))

(defn random-quote
  []
  (format-quote (slurp "http://www.iheartquotes.com/api/v1/random")))

(defmacro snag-quotes-queued
  [n filename]
  (let [quote-gensym (gensym)
        queue `(enqueue ~quote-gensym
                        (random-quote)
                        (append-to-file ~filename @~quote-gensym))]
    `(-> (future)
         ~@(take n (repeat queue)))))

(defmacro enqueue
  [q concurrent-promise-name & work]
  (let [concurrent (butlast work)
        serialized (last work)]
    `(let [~concurrent-promise-name (promise)]
       (future (deliver ~concurrent-promise-name (do ~@concurrent)))
       (deref ~q)
       ~serialized
       ~concurrent-promise-name)))

(defmacro wait
  "Sleep `timeout` seconds before evaluating body"
  [timeout & body]
  `(do (Thread/sleep ~timeout)
       ~@body))
