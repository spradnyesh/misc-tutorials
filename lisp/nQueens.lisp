;; define the array that holds the positions of the queens on the board
(defvar *Q-positions* (make-array 4))
;; initialize such that all the Qs are in the 1st column initially
(defun init-Q-positions ()
  (dotimes (i (length *Q-positions*))
    (setf (aref *Q-positions* i) (list i 0))))

(defun row (queen)
  (first queen))
(defun column (queen)
  (second queen))
(defun kth-queen (k)
  (aref *Q-positions* k))

(defun move-queen (k position)
  (setf (aref *Q-positions* k) (list k position)))
(defun move-queen-forward (k)
  (move-queen k (1+ (column (kth-queen k)))))
(defun move-queen-to-1st-column (k)
  (move-queen k 1))

(defun same-column? (i j)
  (when (= (column i)
           (column j))
    t))
(defun same-diagonal? (i j)
  (= (abs (- (column i) (column j)))
     (abs (- (row i) (row j)))))
;; is the k'th Q under attack from the previous ones?
;; k'th Q is under attack if:
;;     1. it can't be in same row, as i'th Q is in i'th row
;;     2. one of the previous Q is in the same column
;;     3. one of the previous Q is in the same diagonal
;; else: nil/false (Q isn't under attack)
(defun under-attack? (k)
  (dotimes (i k)
    (cond ((same-column? (kth-queen i) (kth-queen k)) (return t))
          ((same-diagonal? (kth-queen i)(kth-queen k)) (return t)))))
(defun build-board (n)
  (init-Q-positions)
  (do ((i 1)) ; start from the 2nd Q
      ((= i n) *Q-positions*)
    (if (under-attack? i)
         ; Q is under attack
        (if (< (column (kth-queen i))
               n)
            (move-queen-forward i) ; there is place to move forward
            (progn ; no place to move forward, retrace back 1 Q
              (move-queen-to-1st-column i)
              (setf i (1- i))))
        ; Q isn't under attack, move to the next Q
        (setf i (1+ i)))))
