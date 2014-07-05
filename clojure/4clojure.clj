(ns clojure4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 53
; 60 (failing due to laziness)
; 65
; 77
; 132 NPE
; 158
; 173
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn ex147
  [v]
  (cons v
        (lazy-seq (ex147 (concat (vector (first v))
                                 (map (partial apply +') (partition 2 1 v))
                                 (vector (last v)))))))

(defn ex96 ; incomplete
  [tree]
  (let [val (first tree)
        left (nth tree 1)
        right (nth tree 2)]
    (if (and (not (coll? left))
             (not (coll? right))
             (= left right))
      true
      (and (coll? left)
           (coll? right)
           (= (first left) (first right))
           ; ???
           ))))
(defn ex96-2
  [tree]
  (letfn [(inorder [tree]
            (if (coll? tree)
              (let [val (first tree)
                    left (nth tree 1)
                    right (nth tree 2)]
                (concat (inorder left)
                        (vector val)
                        (inorder right)))
              (vector tree)))]
    (= (inorder (nth tree 1))
       (reverse (inorder (nth tree 2))))))

(defn ex146
  [m]
  (reduce conj {}
          (apply concat (for [[k v] m]
                          (for [i v]
                            (vector (vector k (first i))
                                    (second i)))))))

(defn ex153
  [sets]
  (let [sets (seq sets)
        a (apply concat sets)
        b (seq (apply clojure.set/union sets))]
    (and (= (count a) (count b)))))

(defn ex74
  [s]
  (clojure.string/join ","
                       (filter (fn [x]
                                 (let [sqrt (Math/pow x 0.5)]
                                   (== sqrt (int sqrt))))
                               (map #(Integer/parseInt %) (clojure.string/split s #",")))))

(defn ex80 ; need largest divisors, not smallest
  [n]
  (loop [n n
                      a 2
                      acc []]
                 (if (= a n)
                   (concat acc [1 n])
                   (if (zero? (rem n a))
                     (recur (/ n a)
                            2
                            (conj acc a))
                     (recur n
                            (inc a)
                            acc)))))
(defn ex80-2 ; need to find divisors, not factors (see http://wiki.answers.com/Q/Why_is_496_a_perfect_number)
  [n]
  (loop [n n
         a (dec n)
         acc []]
    (if (= a 1)
      (concat acc [1 n])
      (if (zero? (rem n a))
        (recur (/ n a)
               (dec (/ n a))
               (conj acc a))
        (recur n
               (dec a)
               acc)))))
(defn ex80-3
  [n]
  (= n (reduce + (filter #(zero? (rem n %)) (range 1 n)))))

(defn ex77                              ; works for 1st test
  [words]
  (reduce (fn [a b]
            (if (clojure.set/intersection a b)
              (clojure.set/union a b)
              (concat a b)))
          (letfn [(anagram? [a b]
                    (= (sort a) (sort b)))]
            (for [i words
                  j words
                  :when (and (not= i j)
                             (anagram? i j))]
              (set [i j])))))
(defn ex77-2                            ; works for 2nd test
  [words]
  (distinct (letfn [(anagram? [a b]
                      (= (sort a) (sort b)))]
              (for [i words
                    j words
                    :when (and (not= i j)
                               (anagram? i j))]
                (set [i j])))))

(defn ex60
  ([a b]
     (ex60 a (first b) (rest b)))
  ([a b c]
     (ex60 a [] b c))
  ([a b c d]
     (let [rslt (vector (apply a b))]
       (for [i c]
         (do (conj rslt (apply a (last rslt) i)))))))
(defn ex60-2
  ([f s] (ex60-2 f (first s) (rest s)))
  ([f d s]
     (cons d
           (lazy-seq (ex60-2 f
                             (reduce f d (vector (first s)))
                             (rest s))))))

(defn ex102
  [string]
  (let [words (clojure.string/split string #"-")
        f (first words)
        r (rest words)]
    (apply str f (map (fn [s]
                        (str (clojure.string/capitalize (subs s 0 1)) (subs s 1)))
                      r))))

(defn ex75
  [n]
  (letfn [(gcd [x y]
            (if (zero? y)
              x
              (gcd y (rem x y))))
          (coprime? [x y]
            (= (gcd x y) 1))]
    (if (= n 1)
      1
      (count (filter true? (map (partial coprime? n) (range 1 (inc n))))))))

(defn ex86
  [n]
  (letfn [(digits [num]
            (loop [acc ()
                   num num]
              (if (< num 10)
                (conj acc num)
                (recur (conj acc (rem num 10))
                       (quot num 10)))))
          (squared-sum [& digits]
            (reduce + (apply map #(* % %) digits)))]
    (loop [n n
           acc #{}]
      (let [new-number (squared-sum (digits n))]
        (if (= 1 new-number)
          true
          (if (contains? acc new-number)
            false
            (recur new-number
                   (conj acc new-number))))))))

(defn ex78
  [f & args]
  (loop [val (apply f args)]
    (if (fn? val)
      (recur (val))
      val)))

(defn ex98
  [f s]
  (into #{} (map set (vals (group-by f s)))))

(defn ex115
  [n]
  (letfn [(digits [num]
            (map #(- (int %) 48) (str num)))]
    (let [d (digits n)
          c-by-2 (int (/ (count d) 2))]
      (= (reduce + (take c-by-2 d))
         (reduce + (take-last c-by-2 d))))))

(defn ex85 ; logic from http://stackoverflow.com/a/15498781 (1st approach)
  [s]
  (if (= 2 (count s))
    (set (conj (map (comp set vector) s) s #{}))
    (let [subsets (for [i s]
                    (clojure.set/difference s #{i}))]
      (conj (set (apply concat (map ex85 subsets))) s))))
(defn ex85-2
  ;; previous attempt timed out for last example
  ;; trying out logic from http://stackoverflow.com/a/15498781 ("another approach")
  [s]
  (set (conj
        (if (= 0 (count s))
          nil
          (let [res (ex85-2 (rest s))]
            (map #(if (coll? %)
                    (set %)
                    (set [%]))
                 (concat res
                         [(first s)]
                         (map #(if (coll? %)
                                 (conj % (first s))
                                 (conj [%] (first s)))
                              res)))))
        #{})))

(defn ex105
  [s]
  (loop [s s
         acc {}]
    (if (empty? s)
      acc
      (let [k (first s)
            v (take-while (complement keyword?) (rest s))
            r (drop (inc (count v)) s)]
        (recur r (conj acc [k v]))))))

(defn ex137
  [num base]
  (loop [acc ()
         num num]
    (if (< num base)
      (conj acc num)
      (recur (conj acc (rem num base))
             (quot num base)))))

(defn ex110
  [s]
  (let [res (flatten (map (fn [x]
                            [(count x) (first x)])
                          (partition-by identity s)))]
    (cons res
          (lazy-seq (ex110 res)))))

(defn ex144
  [num & functions]
  (cons num
        (lazy-seq (apply ex144
                         ((first functions) num)
                         (conj (vec (rest functions)) (first functions))))))


(defn ex108 ; wrong approach -> reduce won't (necessarily) work
  [& s]
  (letfn [(search [s1 s2]
            (let [s1 (if (coll? s1) s1 [s1])
                  a (first s1)
                  b (first s2)]
              (cond (= a b) [a]
                    (< a b) (search (drop-while #(> b %) s1) s2)
                    :else (search s2 s1))))]
    (if (= 1 (count s))
      (first (first s))
      (reduce search s))))
(defn ex108-2
  [& s]
  (letfn [(search [s1 s2]
            (if (coll? s1)
              (let [a (first s1)
                    b (first s2)]
                (cond (= a b) a
                      (< a b) (search (drop-while #(> b %) s1) s2)
                      :else (search s2 s1)))
              (if (= s1 (first (drop-while #(> s1 %) s2)))
                s1
                nil)))]
    (cond (= 1 (count s)) (first (first s))
          :else (let [low (apply search (take 2 s))]
                     (if (search low (last s))
                            low
                            (apply ex108-2 (map #(drop-while (partial >= low) %) s)))))))

(defn ex93
  [s]
  (if-not (coll? (first s))
    [s]
    (apply concat (map ex93 s))))

(defn ex114
  [n f l]
  (loop [n n
         l l
         acc []]
    (if (zero? n)
      (drop-last acc)
      (let [d (drop-while (complement f) l)]
        (recur (dec n)
               (rest d)
               (concat acc (take-while (complement f) l) [(first d)]))))))

(defn ex158
  [f]
  (fn [& args]
    (reduce apply f args)))

(defn ex132
  [f val coll]
  (when-not (empty? coll)
    (loop [acc [(first coll)]
           coll (rest coll)]
      (if (empty? coll)
        acc
        (if (f (last acc) (first coll))
          (recur (concat acc [val (first coll)])
                 (rest coll))
          (recur (concat acc [(first coll)])
                 (rest coll)))))))
(defn ex132-2 ; -1 fails for lazy sequences
  [f val coll]
  (when-not (empty? coll)
    (println coll)
    (if (keyword? (first coll))
      (cons (first coll)
              (lazy-seq (ex132-2 f val (rest coll))))
      (if (f (first coll) (second coll))
        (cons (first coll)
                (lazy-seq (ex132-2 f val (cons val (rest coll)))))
        (cons (first coll)
                (lazy-seq (ex132-2 f val (rest coll))))))))

(defn ex103
  [n s]
  (letfn [(powerset [s]
            (if (= 2 (count s))
              (set (conj (map (comp set vector) s) s #{}))
              (let [subsets (for [i s]
                              (clojure.set/difference s #{i}))]
                (conj (set (apply concat (map powerset subsets))) s))))]
    (set (filter #(= (count %) n) (powerset s)))))

(defn ex116
  [n]
  (let [not-balanced-primes [0 1 2 3]]
    (letfn [(prime? [x]
              (not (some zero? (map (partial rem x) (range 2 x)))))
            (next-prime [n]
              (first (take 1 (filter prime? (iterate inc (inc n))))))
            (prev-prime [n]
              (first (take 1 (filter prime? (iterate dec (dec n))))))]
      (and (= -1 (.indexOf not-balanced-primes n))
           (prime? n)
           (= n (/ (+ (next-prime n)
                      (prev-prime n))
                   2))))))

(defn ex121
  [f v]
  (eval (map #(if-let [a (% v)] a %) f)))
(defn ex121-2                          ; -1 does not return a function
  [f]
  (fn [v]
    (map #(if-let [a (% v)] a %) f)))
(defn ex121-3                 ; -2 does not work on nested expressions
  [f]
  (fn [v]
    (let [functions {'+ + '- - '* * '/ /}]
      (letfn [(replace-symbol-with-value [x]
                (if (coll? x)
                  (map replace-symbol-with-value x)
                  (if-let [a (v x)] a x)))
              (e-val [e & xpr]
                (if (number? e)
                  e
                  (if (some coll? xpr)
                    (apply (e functions)
                           (map #(apply e-val (if-not (coll? %)
                                                [%]
                                                %)) xpr))
                    (apply (e functions) xpr))))]
        (apply e-val (map replace-symbol-with-value f))))))

(defn ex148
  [n a b]
  (let [numbers (range n)
        div-a (filter #(zero? (rem % a)) numbers)
        div-b (filter #(zero? (rem % b)) numbers)
        union (distinct (concat div-a div-b))]
    (reduce + union)))
(defn ex148-2 ; -1 time-outs for large tests
  [n a b]
  (reduce + (filter #(or (zero? (rem % a))
                         (zero? (rem % b)))
                    (range n))))
