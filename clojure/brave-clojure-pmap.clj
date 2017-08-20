(def alphabet-length 26)

(def letters (mapv (comp str char (partial + 65))
                   (range alphabet-length)))

(defn random-string
  "returns a random string of specified length"
  [length]
  (apply str (take length (repeatedly #(rand-nth letters)))))

(defn random-string-list
  [list-length string-length]
  (doall (take list-length (repeatedly (partial random-string string-length)))))

(def orc-names (random-string-list 3000 7000))

(def orc-name-abbrevs (random-string-list 20000 300))

(defn ppmap
  "Partitioned pmap, for grouping map ops together to make parallel
overhead worthwhile"
  [grain-size f & colls]
  (apply concat
         (apply pmap
                (fn [& pgroups] (doall (apply map f pgroups)))
                (map (partial partition-all grain-size)
                     colls))))
