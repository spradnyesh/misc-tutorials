(in-package :codechef-tests)

(def-suite :drghts :in :codechef)
(in-suite :drghts)

(test graph-to-forest
  (is (equal (codechef::graph-to-forest 6 '((1 2)
                                            (1 6)
                                            (1 5)
                                            (2 4)
                                            (4 3)))
             '((1 (5 6 2))
               (2 (4))
               (4 (3))))))
(test add-to-set-nil
  (is (equal (codechef::add-to-set 1 '(1 2 3))
             '(1 2 3))))
(test add-to-set-t
  (is (equal (codechef::add-to-set 4 '(1 2 3))
             '(4 1 2 3))))
(test depth-first-helper
  (is (equal (codechef::depth-first-helper 1 nil '((1 (2 5 6))
                                                   (2 (4))
                                                   (4 (3))))
             '((1 2 4 3)
               (1 5)
               (1 6)))))
(test depth-first
  (is (equal (codechef::depth-first '((1 (2 5 6))
                                      (2 (4))
                                      (4 (3))))
             '((4 (3))
               (2 (4 3))
               (1 (2 4 3 5 6))))))
(test remove-closed-window-rooms-1
  (is (equal (codechef::remove-closed-window-rooms '(1 1 1 1 1 0) '((1 (2 3 4 5 6))
                                                                    (2 (3 4))
                                                                    (4 (3))
                                                                    (6 (1))))
             '((4 (3))
               (2 (3 4))
               (1 (2 3 4 5))))))
(test remove-closed-window-rooms-2
  (is (equal (codechef::remove-closed-window-rooms '(1 1 1 1 1 0) '((1 (2 3 4 5 6))
                                                                    (2 (3 4))
                                                                    (4 (3))))
             '((4 (3))
               (2 (3 4))
               (1 (2 3 4 5))))))
(test make-symmetric
  (is (equal (codechef::make-symmetric '((2 (3 4))
                                         (1 (2 3 4 5))
                                         (4 (3))))
             '((3 (5 4))
               (4 (5))
               (2 (3 4 5))
               (1 (2 3 4 5))))))
(test furik
  (is (equal (codechef::furik '((3 (5 4))
                                (4 (5))
                                (2 (3 4 5))
                                (1 (2 3 4 5))))
             10)))
(test rubik
  (is (equal (codechef::rubik '((1 (2 3 4 5))
                                (2 (3 4 5))
                                (3 (4 5))
                                (4 (5))))
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
  (let ((window-list (loop for i from 1 to n
                        collecting (random 2)))
        (passages (loop for i from 1 to (1- n)
                     collecting (list (1+ (random (1+ n))) (1+ (random (1+ n)))))))
    (list (1- n) window-list (remove-cycles passages))))

;; tests for above data functions
(test remove-cycles-1
  (is (equal (remove-cycles '((1 2) (2 3)))
             '((2 3) (1 2)))))
(test remove-cycles-2
  (is (equal (remove-cycles '((1 2) (2 3) (2 1)))
             '((2 3) (1 2)))))
