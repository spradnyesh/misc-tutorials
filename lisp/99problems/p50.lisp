(defpackage :99problems
  (:use :common-lisp
        :it.bese.FiveAM))
(in-package :99problems)

(def-suite huffman)
(in-suite huffman)
(test test-huffman ()
      (is (equal '((a "0") (d "1") (b "10") (c "11") (e "110") (f "111"))
                 (huffman '((a 45) (b 13) (c 12) (d 16) (e 9) (f 5)))))
      (is (equal '((H "0") (D "1") (E "10") (A "11") (C "110") (B "111") (F "1110") (G "1111"))

                 (huffman '((a 3) (b 1) (c 2) (d 5) (e 5) (f 1) (g 1) (h 10))))))
(run! 'huffman)

(defun helper (lst str)
  (cond ((null lst) nil)
        ((= 1 (length lst))
         (list (first lst) (concatenate 'string str "0")))
        (t (append (list (list (first (first lst))
                               (concatenate 'string str "0"))
                         (list (first (second lst))
                               (concatenate 'string str "1")))
                   (helper (rest (rest lst))
                           (concatenate 'string str "1"))))))
(defun huffman (lst)
  (helper (sort lst #'(lambda (a b)
                        (when (< (second b) (second a)) t)))
          ""))
