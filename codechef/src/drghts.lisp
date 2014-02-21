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

(defun add-to-set (n list)
  (if (not (member n list))
      (cons n (if (listp (first list))
                  (first list)
                  list))
      list))

(defun depth-first-helper (node list forest)
  (let ((children (second (assoc node forest))))
    (if children
        (loop for child in children
           collecting (add-to-set node (depth-first-helper child list forest)))
        (add-to-set node list))))

(defun depth-first (forest)
  (let ((expanded-forest nil))
    (dolist (tree forest)
      (let ((root (first tree)))
        (push (list root
                    (remove-if #'(lambda (a)
                                   (= a root))
                               (remove-duplicates (apply #'append
                                                         (depth-first-helper root
                                                                             nil
                                                                             forest)))))
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

(defun make-symmetric (forest)
  (let ((forest (sort forest #'> :key #'(lambda (a) (length (second a))))))
    (dolist (node forest)
      (dolist (child (second node))
        (let ((tmp (assoc child forest)))
          (setf forest (remove-if #'(lambda (a)
                                      (= child a))
                                  forest
                                  :key #'first))
          (push (list child (remove-if #'(lambda (a)
                                           (< a child))
                                       (remove-duplicates (append (second tmp)
                                                                  (remove-if #'(lambda (a)
                                                                                 (= child a))
                                                                             (second node))))))
                forest))))
    (remove-if #'null forest :key #'second)))

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
  (let ((forest (make-symmetric (remove-closed-window-rooms window-list
                                                            (depth-first (graph-to-forest n passages))))))
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
