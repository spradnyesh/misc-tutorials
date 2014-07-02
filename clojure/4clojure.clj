(ns user)

(defn ex43
  [l n]
  (map                               ; for every list
   #(map second %)                   ; get the 2nd val (not the index)
   (vals                             ; get lists from map
    (group-by #(rem (first %) n)     ; group-by into n keys
              (map-indexed #(list %1 %2) l))))) ; create indexed list
(defn ex43-2
  [l n]
  (apply map list (partition n l)))

(defn ex44
  [n l]
  (let [c (count l)
        n (rem n c)]
    (if (> n 0)
      (concat (drop n l) (take n l))
      (concat (drop (- c (- n)) l) (take (- c (- n)) l)))))
(defn ex44-2
  [n l]
  (let [n (mod n (count l))]
    (concat (drop n l) (take n l))))

(defn ex45
  [f]
  (fn [& args]
    (apply f (reverse args))))

(defn ex49
  [n l]
  (let [a (partition-all n l)]
    (concat (take 1 a)
            (list (apply concat (drop 1 a))))))
#(let [a (partition-all %1 %2)]
    (concat (take 1 a) (list (apply concat (doall (drop 1 a))))))

(defn ex50
  [l]
  (vals (group-by type l)))
#(vals (group-by type %))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ex53)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1st attempt fails because group-by wraps at 32
(defn ex56
  [l]
  (vec (keys (group-by identity l))))
;; 2nd attempt fails because set too wraps at 32 :(
(defn ex56-2
  [l]
  (vec (set l)))
(defn ex56-3
  [l]
  (loop [rslt [] a l]
    (if a
      (if (clojure.set/subset? (set (list (first a))) (set rslt))
        (recur rslt (next a))
        (recur (conj rslt (first a)) (next a)))
      rslt)))

(defn ex58
  [& fns]
  (fn [& args]
    (let [rslt (atom (apply (last fns) args))
          fns (reverse (drop-last fns))]
      (doall (for [f fns]
               (reset! rslt (f @rslt))))
      @rslt)))

(defn ex59 [& fns]
  (fn [& args]
    (loop [fns fns
           rslt []]
      (if fns
        (recur (next fns) (conj rslt (apply (first fns) args)))
        rslt))))
(defn ex59-2 [& f]
  (fn [& a]
    (map #(apply % a) f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ex60
  ([a b]
     (ex60 a (first b) (rest b)))
  ([a b c]
     (ex60 a [] b c))
  ([a b c d]
     (let [rslt (vector (apply a b))]
       (for [i c]
         (do (conj rslt (apply a (last rslt) i)))))))

(defn ex65)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ex66
  [a b]
  (let [c (max a b)
        d (min a b)]
    (loop [a c
           b d]
        (if (zero? b)
          a
          (recur b (rem a b))))))

(defn ex67
  [n]
  (letfn [(prime? [x]
            (not (some zero? (map (partial rem x) (range 2 x)))))]
    (take n (filter prime? (iterate inc 2)))))

(defn ex69
  [f & maps]
  (into {} (map (fn [[k v]] (vec [k (reduce f (map second v))]))
                (group-by first (apply concat (map vec maps))))))

(defn ex69-2 ; rewritten above (exactly same) w/ threading
  [f & maps]
  (into {} (map (fn [[k v]] [k (->> v
                                    (map second)
                                    (reduce f))])
                (->> maps
                     (map vec)
                     (apply concat)
                     (group-by first)))))

(defn ex70
  [s]
  (let [punctuations (into #{} ",.!")]
    (sort #(compare (clojure.string/upper-case %1)
                    (clojure.string/upper-case %2))
          (clojure.string/split (apply str (map #(if (clojure.set/subset? (str %) punctuations) "" %)
                                                s))
                                #" "))))

(defn ex134
  [k m]
  (let [v (get m k :n)]
    (if (or (= v :n)
            (not (nil? v)))
      false
      true)))

(defn ex156
  [default keys]
  (into {} (map #(vector % default) keys)))

(defn ex83
  [& bools]
  (boolean (and (not (every? true? bools))
                (some true? bools))))

(defn ex61
  [k v]
  (into {} (map #(vector %1 %2) k v)))

(defn ex166
  [operator operand1 operand2]
  (cond (and (not (operator operand1 operand2))
             (not (operator operand2 operand1))) :eq
        (operator operand1 operand2) :lt
        :else :gt))

(defn ex81
  [s1 s2]
  (loop [intersection []
         s2 s2]
    (if (empty? s2)
      (set intersection)
      (recur (if (clojure.set/subset? (set (vector (first s2))) s1)
               (conj intersection (first s2))
               intersection)
             (rest s2)))))
(defn ex81-2
  [s1 s2]
  (->> s2
       (map #(if (clojure.set/subset? (set (vector %)) s1) % nil))
       (remove nil?)
       (into #{})))

(defn ex62
  [f x]
  (cons x (lazy-seq (ex62 f (f x)))))

(defn ex107
  [n]
  (fn [x]
    (int (Math/pow x n))))

(defn ex99
  [a b]
  (loop [acc ()
         num (* a b)]
    (if (< num 10)
      (conj acc num)
      (recur (conj acc (rem num 10))
             (quot num 10)))))

(defn ex90
  [s1 s2]
  (set (for [x s1
             y s2]
         [x y])))
#(set (for [x %1 y %2] [x y]))

(defn ex63
  [f s]
  (loop [acc {}
         s s]
    (if (empty? s)
      acc
      (recur (let [frst (first s)
                   res (f frst)]
               (if-let [k (acc res)]
                 (merge acc {res (conj k frst)})
                 (merge acc {res (vector frst)})))
             (rest s)))))

(defn ex122
  [s]
  (loop [l (->> (clojure.string/split s #"")
                rest
                (map #(Integer/parseInt %))
                reverse)
         res []
         pow 0]
    (if (empty? l)
      (int (reduce + res))
      (recur (rest l)
             (conj res (* (Math/pow 2 pow) (first l)))
             (inc pow)))))

(defn ex88
  [s1 s2]
  (let [s (clojure.set/intersection s1 s2)]
    (clojure.set/union (clojure.set/difference s1 s)
                       (clojure.set/difference s2 s))))

(defn ex143
  [a b]
  (reduce + (map * a b)))

(defn ex135
  [& expr-stack]
  (loop [es expr-stack]
    (if (= 1 (count es))
      (first es)
      (recur (let [[a b c] (take 3 es)]
               (conj (drop 3 es) (b a c)))))))
(defn ex135-2
  [& expr-stack]
  (if (= 1 (count expr-stack))
      (first expr-stack)
      (recur (ex135-2 (let [[a b c] (take 3 expr-stack)]
                        (conj (drop 3 expr-stack) (b a c)))))))

(defn ex97
  [n]
  (if (= n 1)
    [1]
    (conj (vec (cons 1 (map (fn [[a b]] (+ a b)) (partition 2 1 (ex97 (dec n)))))) 1)))

(defn ex157
  [s]
  (loop [s s
         acc []
         i 0]
    (if (empty? s)
      acc
      (recur (rest s)
             (conj acc [(first s) i])
             (inc i)))))

(defn ex118
  [f s]
  (if (empty? s)
    nil
    (cons (f (first s))
          (lazy-seq (ex118 f (rest s))))))

(defn ex120
  [& nums]
  (letfn [(digits [num]
            (loop [acc ()
                   num num]
              (if (< num 10)
                (conj acc num)
                (recur (conj acc (rem num 10))
                       (quot num 10)))))
          (squared-sum [& digits]
            (reduce + (map #(Math/pow % 2) digits)))]
    (let [nums (first nums)
          ss (->> nums
                  (map digits)
                  (map #(apply squared-sum %)))]
      (count (filter true? (map (fn [[a b]]
                                  (< a b))
                                (zipmap nums ss)))))))

(defn ex95
  [tree]
  (if (not (coll? tree))
    (not (false? tree))
    (and (= 3 (count tree))
           (ex95 (nth tree 0))
           (ex95 (nth tree 1))
           (ex95 (nth tree 2)))))

(defn ex128
  [s]
  (let [suits {"S" :spade, "H" :heart, "D" :diamond, "C" :club}
        ranks {"T" 8, "J" 9, "Q" 10, "K" 11, "A" 12}
        [a b] (rest (clojure.string/split s #""))]
    (hash-map :suit (suits a)
              :rank (let [num (Integer/parseInt b 36)]
                      (if (< num 10)
                        (- num 2)
                        (ranks b))))))

(defn ex100
  [& nums]
  (letfn [(gcd [x y]
            (if (zero? y)
              x
              (gcd y (rem x y))))]
    (/ (reduce * nums) (reduce gcd nums))))
