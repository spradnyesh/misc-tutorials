;;; Key
;; pv: Present Value
;; fv: Future Value
;; r: rate of interest (of deposit/loan)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun average (&rest numbers)
  (/ (apply #'+ numbers) (length numbers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple interest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun si-fv (pv r n)
  (* pv n r))

(defun si-pv (fv r n)
  (/ fv (* n r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound interest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ci-fv (pv r n)
  (* pv (expt (1+ r) n)))

(defun ci-pv (fv r n)
  (/ fv (expt (1+ r) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annuities
;; ***** remeber that annuity is paid at the _end_ of the year *****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a-fv (pmt r n)
  (let ((factor 0))
    (dotimes (i n)
      (setf factor (+ factor (expt (1+ r) i))))
    (* pmt factor)))

(defun pmt-for-afv (fv r n)
  (let ((factor 0))
    (dotimes (i n)
      (setf factor (+ factor (expt (1+ r) i))))
    (/ fv factor)))

(defun recurring-deposit-increase-table (pmt r n)
  (dotimes (i n)
    (format t "~a: ~a~%" i (a-fv pmt r (1+ i)))))

(defun a-pv (pmt r n)
  (let ((factor 0))
    (dotimes (i n)
      (setf factor (+ factor (/ 1 (expt (1+ r) (1+ i))))))
    (* pmt factor)))

(defun pmt-for-apv (pv r n)
  (let ((factor 0))
    (dotimes (i n)
      (setf factor (+ factor (/ 1 (expt (1+ r) (1+ i))))))
    (/ pv factor)))

;; k: #periods in a year (12 for monthly, 4 for quarterly, etc)
(defun effective-annual-rate (r k)
  (1- (expt (1+ (/ r k)) k)))

(defun loan-decrease-table (loan-amount r n)
  (let ((pmt (pmt-for-apv loan-amount r n))
        (loan-left loan-amount))
    (format t "***** pmt: ~a~%" pmt)
    (format t "begining of year: loan-left-1, loan-left-2, for-interest, for-principal~%")
    (dotimes (i n)
      (let* ((interest (* loan-left r))
             (principal (- pmt interest)))
        (format t
                "~a: ~a, ~a, ~a, ~a~%"
                (1+ i)
                loan-left
                (a-pv pmt r (- n i))
                interest
                principal)
        (setf loan-left (- loan-left principal))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perpetuities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun p-pv (pmt r &optional (g 0))
  (unless (= r g)
    (/ pmt (- r g))))

(defun pmt-for-ppv (pv r &optional (g 0))
  (unless (= r g)
    (* pv (- r g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Net Present Value (NPV) and Internal Rate of Return (IRR)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n-pv (c0 r &rest c1-n)
  (let ((len (length c1-n))
        (rslt 0))
    (dotimes (i len)
      (setf rslt (+ rslt (/ (nth i c1-n) (expt (1+ r) (1+ i))))))
    (- rslt c0)))

;; http://en.wikipedia.org/wiki/Internal_rate_of_return#Numerical_solution
(defun irr (c0 &rest c1-n)
  (let* ((n (length c1-n))
         (a (apply #'+ c1-n))
         (p (/ (log (/ a (abs c0)))
               (log (/ a (apply #'n-pv c0 1 c1-n))))))
    (do* ((steps 0 (1+ steps))
          (r.n-1 (1- (expt (/ a (abs c0))
                           (/ 2 (1+ n))))
                 r.n)
          (r.n (1- (expt (1+ r.n-1) p))
               r.n+1)
          (npv.n-1 (apply #'n-pv c0 r.n-1 c1-n)
                   npv.n)
          (npv.n (apply #'n-pv c0 r.n c1-n)
                 npv.n+1)
          (r.n+1 (- r.n (* npv.n (/ (- r.n r.n-1) (- npv.n npv.n-1))))
                 (- r.n (* npv.n (/ (- r.n r.n-1) (- npv.n npv.n-1)))))
          (npv.n+1 (apply #'n-pv c0 r.n+1 c1-n)
                   (apply #'n-pv c0 r.n+1 c1-n)))
         ((or (= steps 50)
              (zerop npv.n+1)) (realpart r.n+1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lecture problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun p3.10 ()
  (pmt-for-afv (ci-pv (a-pv 100000 .08 20) .08 10) .08 20))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assignment problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
