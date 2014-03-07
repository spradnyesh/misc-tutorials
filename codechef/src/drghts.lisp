; http://www.codechef.com/problems/DRGHTS
(in-package :codechef)

(defun graph-to-forest (n passages)
  (let ((forest nil))
    (dotimes (i n)
      (push (list nil) forest))
    (dolist (p passages)
      (push (second p) (nth (1- (first p)) forest)))
    (dotimes (i n)
      (setf (nth i forest)
            (cons (1+ i) (list (remove-if #'null (nth i forest))))))
    (remove-if #'null forest :key #'second)))

(defun flatten (list)
  (let ((rslt nil))
    (dolist (l list)
      (if (atom l)
          (push l rslt)
          (dolist (f (flatten l))
            (push f rslt))))
    rslt))

(defun depth-first-helper (node forest)
  (let ((children (second (assoc node forest))))
    (when children
      (append children (loop for child in children
                          collecting (depth-first-helper child forest))))))

(defun depth-first (forest)
  (let ((expanded-forest nil))
    (dolist (tree forest)
      (let ((root (first tree)))
        (push (list root
                    (remove-duplicates (remove-if #'null (flatten (depth-first-helper root forest)))))
              expanded-forest)))
    expanded-forest))

(defun remove-closed-window-rooms (window-list forest)
  ;; remove complete node itself
  (dotimes (i (length window-list))
    (let ((w (nth i window-list)))
      (when (zerop w)
        (setf forest (remove-if #'(lambda (a)
                                    (= (1+ i) (first a)))
                                forest)))))
  ;; remove child from node
  (dolist (tree forest)
    (dotimes (i (length window-list))
      (let ((w (nth i window-list))
            (root (first tree)))
        (when (zerop w)
          (setf forest (remove-if #'(lambda (a)
                                      (= root a))
                                  forest
                                  :key #'first))
          (push (list root
                      (remove-if #'(lambda (a)
                                     (= (1+ i) a))
                                 (second tree)))
                forest)))))
  forest)

(defun make-symmetric (passages)
  (let ((rslt nil))
    (dolist (p passages)
      (push p rslt)
      (push (reverse p) rslt))
    rslt))

(defun furik (forest)
  (reduce #'+ forest :key #'(lambda (a) (length (second a)))))

(defun rubik (forest)
  (let ((rslt nil))
    (dolist (tree forest)
      (push (first tree) rslt)
      (dolist (node (second tree))
        (push node rslt)))
    (length (remove-duplicates rslt))))

(defun drghts (n m window-list passages)
  (declare (ignore m))
  (let ((forest  (depth-first (graph-to-forest n (make-symmetric passages)))
          (remove-closed-window-rooms window-list
                                      )))
    (list (furik forest) (rubik forest))))

(defun main ()
  (let ((n (read))
        (m (read))
        (window-list nil)
        (passages nil))
    (dotimes (i n)
      (push (read) window-list))
    (dotimes (i m)
      (push (list (read) (read)) passages))
    (apply #'format t "~a ~a" (drghts n m (reverse window-list) passages))))

#|(main)|#
