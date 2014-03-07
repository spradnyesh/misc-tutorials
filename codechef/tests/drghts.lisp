(in-package :codechef-tests)

; (5 (0 0 0 1 1 1) ((6 2) (1 4) (4 2) (4 5) (1 5)))

(def-suite :drghts :in :codechef)
(in-suite :drghts)

(test make-symmetric
  (is (equal (codechef::make-symmetric '((1 2) (1 6) (1 5) (2 4) (3 4)))
             '((4 3) (3 4) (4 2) (2 4) (5 1) (1 5) (6 1) (1 6) (2 1) (1 2)))))
(test graph-to-forest-1
  (is (equal (codechef::graph-to-forest 6 '((4 3) (3 4) (4 2) (2 4) (5 1) (1 5) (6 1) (1 6) (2 1) (1 2)))
             '((1 (5 6 2))
               (2 (4))
               (3 (4))))))
(test graph-to-forest-2
  (is (equal (codechef::graph-to-forest 6 '((1 5) (2 5) (3 6) (4 5) (5 6)))
             '((1 (5))
               (2 (5))
               (3 (6))
               (4 (5))
               (5 (6))))))
(test flatten
  (is (equal (codechef::flatten '(2 5 6 (4 (3 NIL)) NIL NIL))
             '(NIL NIL 4 NIL 3 6 5 2))))
(test depth-first-helper
  (is (equal (codechef::depth-first-helper 1 '((1 (2 5 6))
                                               (2 (4))
                                               (4 (3))))
             '(2 5 6 (4 (3 NIL)) NIL NIL))))
(test depth-first
  (is (equal (codechef::depth-first '((1 (2 5 6))
                                      (2 (4))
                                      (4 (3))))
             '((4 (3))
               (2 (3 4))
               (1 (4 3 6 5 2))))))
(test remove-closed-window-rooms
  (is (equal (codechef::remove-closed-window-rooms '(1 1 1 1 1 0) '((4 (3))
                                                                    (2 (3 4))
                                                                    (1 (4 3 6 5 2))))
             '((1 (4 3 5 2))
               (2 (3 4))
               (4 (3))))))
(test furik
  (is (equal (codechef::furik '((3 (5 4))
                                (4 (5))
                                (2 (4 3 5))
                                (1 (4 3 5 2))))
             10)))
(test rubik
  (is (equal (codechef::rubik '((3 (5 4))
                                (4 (5))
                                (2 (4 3 5))
                                (1 (4 3 5 2))))
             5)))
(test drghts-1
  (is (equal (codechef::drghts 6 5 '(1 1 1 1 1 0) '((1 2)
                                                    (1 6)
                                                    (1 5)
                                                    (2 4)
                                                    (4 3)))
             '(10 5))))
(test drghts-2
  (is (equal (codechef::drghts 2 1 '(1 0) '((1 2)))
             '(0 0))))
(test drghts-3
  (is (equal (codechef::drghts 10 8 '(1 1 1 1 1 0 0 1 0 1)
                               '((8 9) (6 7) (9 5) (8 1) (6 5) (10 1) (6 9) (2 10)))
             '(6 5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; data to test performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-cycles (passages)
  (let ((rslt nil))
    (dolist (p passages)
      (unless (member (reverse p) rslt :test #'equal)
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
