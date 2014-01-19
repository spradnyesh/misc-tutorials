(ns string-calculator.core)

(defn- string->numbers [string]
  (re-seq #"\d+|-\d+" string))

(defn sum-of-strings [string]
  (let [numbers (map read-string (string->numbers string))]
    (when-let [negative-numbers (not-empty (filter neg? numbers))]
      (throw (Exception. (str "negatives not allowed: " negative-numbers))))
    (reduce + (filter #(< % 1001) numbers))))
