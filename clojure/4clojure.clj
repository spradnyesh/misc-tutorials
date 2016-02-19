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

;; ex46
(fn [f] (fn [a b] (f b a)))

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
#(loop [rslt [] a %]
   (if a
     (if (clojure.set/subset? (set (list (first a))) (set rslt))
       (recur rslt (next a))
       (recur (conj rslt (first a)) (next a)))
     rslt))

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

(defn ex60
  ([a b]
     (ex60 a (first b) (rest b)))
  ([a b c]
     (ex60 a [] b c))
  ([a b c d]
     (let [rslt (vector (apply a b))]
       (for [i c]
         (do (conj rslt (apply a (last rslt) i)))))))
