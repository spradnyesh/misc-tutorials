(ns project-euler.prob001)
;; http://projecteuler.net/problem=1

(defn multiples [list of]
  (filter #(= 0 (rem % of)) list))

(defn merge-without-duplicates [& lists]
  (seq (set (reduce concat lists))))

(defn sum-multiples-below [n & args]
  (let [numbers (range n)]
    (reduce + (apply merge-without-duplicates (map #(multiples numbers %) args)))))


(defn make-board
  [size]
  (apply vector (for [x (range size)
                      y (range size)]
                  {:pos-x x :pos-y y :value ""})))

(def board (atom {:current-pos [0 0]
                  :dir :right
                  :board []}))

(defn fill-board
  [size]
  (map #(swap! board assoc-in [:board % :value] %) (range (* size size))))

(defn init-board
  [size]
  (swap! board assoc-in [:board] (make-board size)))


(defn get-element
  [i j]
  (first (filter (fn [x]
                   (let [values (map val x)]
                     (and
                      (= i (first values))
                      (= j (second values)))))
                 (get-in @board [:board]))))

(defn get-position
  [value]
  (let [map1  (filter (fn [x]
                        (= value (:value x)))
                      (get-in @board [:board]))
        val-map (flatten (map vals map1))]
    (vector (first val-map) (second val-map))))


(defn change-dir-indexes
  [size]
  (flatten (cons size (reverse (vec  (zipmap (range 1 size) (range 1 size)))))))

(defn move-right
  [steps]
  (let [cur-pos (get-in @board [:current-pos])
        [x-pos y-pos] cur-pos
        ans (map (fn [y]
                   (:value (get-element x-pos y)))
                 (range y-pos (+ y-pos steps)))
        last-pos (get-position (last ans))
        [x y] last-pos]
    #_(println "!" cur-pos x-pos y-pos "!" last-pos x y "!")
    (swap! board assoc-in [:current-pos] [(inc x) y])
    ans))

(defn move-down
  [steps]
  (let [cur-pos (get-in @board [:current-pos])
        [x-pos y-pos] cur-pos
        ans (map (fn [x]
                   (:value (get-element x y-pos)))
                 (range x-pos (+ x-pos steps)))
        last-pos (get-position (last ans))
        [x y] last-pos]
    (swap! board assoc-in [:current-pos] [x (dec  y)])
    ans))


(defn move-left
  [steps]
  (let [cur-pos (get-in @board [:current-pos])
        [x-pos y-pos] cur-pos
        ans (map (fn [y]
                   (:value (get-element x-pos y)))
                 (reverse  (range (- (inc  y-pos) steps) (inc y-pos))))
        last-pos (get-position (last ans))
        [x y] last-pos]
    (swap! board assoc-in [:current-pos] [(dec x) y])
    ans))

(defn move-up
  [steps]
  (let [cur-pos (get-in @board [:current-pos])
        [x-pos y-pos] cur-pos
        ans (map (fn [x]
                   (:value (get-element x y-pos)))
                 (reverse (range (- (inc  x-pos) steps) (inc x-pos))))
        last-pos (get-position (last ans))
        [x y] last-pos]
    (swap! board assoc-in [:current-pos] [x (inc  y)])
    ans))

(defn change-direction
  [dir]
  (swap! board assoc-in [:dir] dir))

(defn create-fill-board
  [size]
  (init-board 8)
  (fill-board 8))


(defn execute-spiral
  [size]
  (mapcat (fn [x]
            (let [dir (get-in @board [:dir])
                  cur-pos (get-in @board [:current-pos])]
              (condp = dir
                :right (do (change-direction :down)
                           (move-right x))

                :down  (do (change-direction :left)
                           (move-down x))

                :left (do (change-direction :up)
                          (move-left x))

                :up (do (change-direction :right)
                        (move-up x)))))
          (change-dir-indexes size)))
