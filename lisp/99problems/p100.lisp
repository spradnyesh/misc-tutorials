(defpackage :99problems
  (:use :common-lisp
        :it.bese.FiveAM
        :split-sequence))
(in-package :99problems)

(def-suite 3n+1)
(in-suite 3n+1)
(test test-3n+1 ()
      (is (= 7 (3n+1 10)))
      (is (= 26 (3n+1 100))))
(test test-3n+1-range ()
      (is (= 20 (3n+1-range 1 10)))
      (is (= 125 (3n+1-range 100 200)))
      (is (= 89 (3n+1-range 201 210)))
      (is (= 174 (3n+1-range 900 1000))))
(run! '3n+1)

(defun 3n+1-helper (n count)
  (cond ((= 1 n) (1+ count))
        ((= 1 (mod n 2)) (3n+1-helper (+ 1 (* 3 n)) (1+ count)))
        (t (3n+1-helper (/ n 2) (1+ count)))))
(defun 3n+1 (n)
  (3n+1-helper n 0))
(defun 3n+1-range (m n)
  (apply #'max
         (loop
            for i from m to n
            collect (3n+1 i))))
(defun 3n+1-from-file ()
  (with-open-file (fp "/home/pradyus/p100")
    (when fp
      (loop
         for line = (read-line fp nil)
         while line
         do (let* ((l (split-sequence " "
                                      line
                                      :test #'string-equal))
                   (m (parse-integer (first l)))
                   (n (parse-integer (second l))))
              (format t "~A ~A ~A~%" m n (3n+1-range m n)))))))
