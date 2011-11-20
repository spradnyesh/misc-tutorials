(defun row (queen)
  (first queen))
(defun column (queen)
  (second queen))

(defmacro move-queen (i)
  `(if (< (column (kth-queen ,i)) (1- n)) ; is there place to move forward?
       (move-queen-forward ,i)            ; yes
       (progn                             ; no, retrace back 1 Q
         (move-queen-to-1st-column ,i)
         (setf ,i (1- ,i))
         (retrace ,i n))))

(defun build-all-boards (n &aux (queens (make-array n)) (result nil))
  (defun init-Q-positions ()
    (dotimes (i (length queens))
      (setf (aref queens i) (list i 0))))
  (defun kth-queen (k)
    (aref queens k))

  (defun move-queen-to-position (k position)
    (setf (aref queens k) (list k position)))
  (defun move-queen-forward (k)
    (move-queen-to-position k (1+ (column (kth-queen k)))))
  (defun move-queen-to-1st-column (k)
    (move-queen-to-position k 0))


  ;; is the k'th Q under attack from the previous ones?
  ;; k'th Q is under attack if:
  ;;     1. it can't be in same row, as i'th Q is in i'th row
  ;;     2. one of the previous Q is in the same column
  ;;     3. one of the previous Q is in the same diagonal
  ;; else: nil/false (Q isn't under attack)
  (defun under-attack? (k)
    (defun same-column? (i j)
      (when (= (column i)
               (column j))
        t))
    (defun same-diagonal? (i j)
      (= (abs (- (column i) (column j)))
         (abs (- (row i) (row j)))))

    (dotimes (i k)
      (cond ((same-column? (kth-queen i) (kth-queen k)) (return t))
            ((same-diagonal? (kth-queen i)(kth-queen k)) (return t)))))
  (defun retrace (i n)
    (if (< (column (kth-queen i))
           (1- n))
        (move-queen-forward i)        ; there is place to move forward
        (progn            ; no place to move forward, retrace back 1 Q
          (move-queen-to-1st-column i)
          (setf i (1- i))
          (retrace i n))))

  (init-Q-positions)
  (do ((i 1))                           ; start from the 2nd Q
      ((>= (column (kth-queen 0)) (/ n 2)) result)
    (print (list i queens result))
    (if (under-attack? i)
        ; Q is under attack
        (move-queen i)
        ; Q isn't under attack, so if this is the last Q
        ;     then append current board-position to result and continue
        ;     else move to the next Q
        (if (= i (1- n))
            (progn
              (print "if")
              (setf result (append result (list queens)))
              (move-queen i))
            (progn
              (print "else")
              (setf i (1+ i)))))))


;;;; tests
(stefil:in-root-suite)
(stefil:defsuite* test-all)
(stefil:defsuite test-functions)
(stefil:defsuite test-problems)

(stefil:in-suite test-functions)

(stefil:deftest test-same-column-ok ()
  (stefil:is (eql t (same-column? '(0 0) '(1 0)))))
(stefil:deftest test-same-column-not-ok ()
  (stefil:is (not (eql t (same-column? '(0 0) '(1 1))))))
(stefil:deftest test-same-diagonal-ok ()
  (stefil:is (eql t (same-diagonal? '(0 0) '(1 1)))))
(stefil:deftest test-same-diagonal-not-ok ()
  (stefil:is (not (eql t (same-diagonal? '(0 0) '(1 2))))))

(stefil:in-suite test-problems)

(stefil:deftest test-build-board-ok ()
  (stefil:is (equalp #((0 1) (1 3) (2 0) (3 2)) (build-board 4))))
