(in-package :codechef-tests)

(def-suite :drghts :in :codechef)
(in-suite :drghts)

(test is-drght?-nil
  (is-false (codechef::is-drght? '(8 9) '(1 1 1 1 1 0 0 1 0 1))))
(test is-drght?-t
  (is-true (codechef::is-drght? '(1 10) '(1 1 1 1 1 0 0 1 0 1))))
(test furik-1
  (is (equal (codechef::furik '((5 4)
                                (2 3)))
             4)))
(test furik-2
  (is (equal (codechef::furik '((5 4)
                                (4 3)))
             3)))
(test rubik
  (is (equal (codechef::rubik '((5 4)
                                (2 3)))
             2)))
(test drghts-1
  (is (equal (codechef::drghts 6 5 '(1 1 1 1 1 0)
                               '((1 2) (1 6) (1 5) (2 4) (4 3)))
             '(5 4))))
(test drghts-2
  (is (equal (codechef::drghts 2 1 '(1 0) '((1 2)))
             '(0 0))))
(test drghts-3
  (is (equal (codechef::drghts 10 8 '(1 1 1 1 1 0 0 1 0 1)
                               '((8 9) (6 7) (9 5) (8 1) (6 5) (10 1) (6 9) (2 10)))
             '(4 3))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; data to test performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-cycles (passages)
  (let ((rslt nil))
    (dolist (p passages)
      (unless (or (= (first p) (second p))
                  (member (reverse p) rslt :test #'equal))
        (push p rslt)))
    rslt))
(defun generate-perf-data (n)
  (let* ((window-list (loop for i from 1 to n
                         collecting (random 2)))
         (passages (loop for i from 1 to (1- n)
                      collecting (list (1+ (random n)) (1+ (random n)))))
         (passages-without-cyles (remove-cycles passages)))
    (list (length passages-without-cyles) window-list passages-without-cyles)))

;; tests for above data functions
(test remove-cycles-1
  (is (equal (remove-cycles '((1 2) (2 3)))
             '((2 3) (1 2)))))
(test remove-cycles-2
  (is (equal (remove-cycles '((1 2) (2 3) (2 1)))
             '((2 3) (1 2)))))
