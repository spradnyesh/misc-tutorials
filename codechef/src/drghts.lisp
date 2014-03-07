(in-package :codechef)

; http://www.codechef.com/problems/DRGHTS

(defun is-drght? (path window-list)
  (let ((src (first path))
        (dest (second path)))
    (when (and (not (zerop (* src (nth (1- src) window-list))))
               (not (zerop (* dest (nth (1- dest) window-list)))))
      t)))

(defun furik (list)
  (let ((rslt nil))
    (dolist (l list)
      (setf rslt (adjoin (first l) rslt))
      (setf rslt (adjoin (second l) rslt)))
    (length rslt)))

(defun rubik (list)
  (length list))

(defun drghts (n m window-list passages)
  (declare (ignore n m))
  (let ((rslt nil))
    (dolist (p passages)
      (when (is-drght? p window-list)
        (push p rslt)))
    (list (furik rslt) (rubik rslt))))

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
