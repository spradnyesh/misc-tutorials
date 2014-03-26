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
