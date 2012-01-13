;;;; http://rubyquiz.com/quiz1.html

(defun groupify (str)
  ;; split on " " and then concatenate before splitting into groups of 5
  (let* ((rslt (apply #'concatenate 'string
                       (split-sequence:SPLIT-SEQUENCE " " str :test 'string-equal)))
         (l (length rslt)))
    ;; append trailing "x" to make rslt length a multiple of 5
    (unless (zerop (mod l 5))
      (dotimes (i (mod l 5))
        (setf rslt (concatenate 'string rslt "x"))))
    ))
(defun char->num (char))
(defun num->char (num))
(defun solitaire (char))
(defun add-num (a b))
(defun sub-num (a b))
