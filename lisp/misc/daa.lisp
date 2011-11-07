; (defvar lst '())
; (dotimes (i 1000) (setf lst (cons (random 1000) lst)))



;;;; helpers
(defun swap (a b)
  (list b a))


;;;; sorting

;;; bubble sort
;;  1.306 seconds of real time for 1000 elements
;;  13.516 seconds of real time for 2000 elements
(defun bubble-sort (lst)
  (let ((len (length lst)))
      (dotimes (i len)
        (loop for j from (1- len) downto (1+ i) do
             (let ((j-th (nth j lst))
                   (j-1-th (nth (1- j) lst)))
               (when (< j-th j-1-th)
                 (progn
                   (setf (nth j lst) j-1-th)
                   (setf (nth (1- j) lst) j-th))))))))

;;; selection sort
;;  0.978 seconds of real time
;;  9.895 seconds of real time
(defun select-sort (lst)
  (let ((len (length lst)))
      (dotimes (i (- len 2))
        (loop for j from (1+ i) to (1- len) do
             (let ((ith (nth i lst))
                   (jth (nth j lst)))
               (when (> ith jth)
                 (progn
                   (setf (nth i lst) jth)
                   (setf (nth j lst) ith))))))))

;;; insertion sort
;;   0.594 seconds of real time
;;  4.779 seconds of real time
(defun insert-sort (lst)
  (loop for i from 1 to (1- (length lst)) do
       (let ((tmp (nth i lst))
             (outer-j i))
         (do ((j (1- i) (1- j)))
             ((not (and (> j -1)
                        (> (nth j lst) tmp))))
           (setf (nth (1+ j) lst) (nth j lst))
           (setf outer-j j))
         (setf (nth outer-j lst) tmp))))

(defun quick-sort (lst))
(defun merge-sort (lst))
(defun binary-sort (lst))
(defun heap-sort (lst))


;;;; data structures
;;; stack

;;; queue

;;; tree
;; in-order
;; pre-order
;; post-order
;; search
;; avl
;; red-black

;;; graph
;; traversal
;; shortest path
; dijkstra
; bellman-ford
;; minimum spanning tree
; kruskals
; prims
