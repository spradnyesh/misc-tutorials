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

;; annuity w/ growth (If the first cash flow is C, the next one will be C(1+g), and so on, where g is the growth rate in cash flow)
(defun a-pv-growth (pmt r n g)
  (* pmt (/ 1 (- r g)) (- 1 (/ (expt (1+ g) n) (expt (1+ r) n)))))

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
(defun n-pv (r c0 &rest c1-n)
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
          (npv.n-1 (apply #'n-pv r.n-1 c0 c1-n)
                   npv.n)
          (npv.n (apply #'n-pv r.n c0 c1-n)
                 npv.n+1)
          (r.n+1 (- r.n (* npv.n (/ (- r.n r.n-1) (- npv.n npv.n-1))))
                 (- r.n (* npv.n (/ (- r.n r.n-1) (- npv.n npv.n-1)))))
          (npv.n+1 (apply #'n-pv r.n+1 c0 c1-n)
                   (apply #'n-pv r.n+1 c0 c1-n)))
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
(defun a.3.2.7 ()
  (round (- (round (ci-pv (a-pv-growth 45000 .06 38 .03) .06 4))
            (round (+ (round (ci-pv (+ 8000 (ci-fv 12000 .06 1)) .06 1))
                      (round (ci-pv (+ 8500 (ci-fv 12000 .06 2)) .06 1))
                      (round (ci-pv (+ 9000 (ci-fv 12000 .06 3)) .06 1))
                      (round (ci-pv (+ 9500 (ci-fv 12000 .06 4)) .06 1))))
            (round (a-pv-growth 25000 .06 41 .03)))))
(defun a.4.1.2 ()
  (let* ((capex 1200000)
         (n 25)
         (r .05)
         (amc (a-pv 100000 r n))
         (yr15-overhaul (ci-pv 700000 r 15))
         (labour (a-pv-growth 180000 r n .02))
         (revenue (a-pv-growth 150000 r n .04))
         (save (a-pv 200000 r n))
         (salvage (ci-pv 300000 r n)))
    (+ (- capex) (- amc) (- yr15-overhaul) (- labour) revenue save salvage)))

(defun a.4.1.7 ()
  (let* ((A-pv.revenue (a-pv-growth (ci-fv (* 70 45 50000) .06 1) .14 10 .06 ))
         (investment 2000000)
         (B-pv.revenue (a-pv-growth (* 70 39.6 90000) .14 10 .06)))
    (- B-pv.revenue A-pv.revenue investment)))

(defun a.4.1.8 ()
  (let* ((g .25)
         (r .14)
         (n 4)
         ;; all values are PVs
         (capex 12)
         (revenue (a-pv-growth 15 r n g))
         (cost (a-pv-growth 9 r n g))
         (depreciation (a-pv (/ capex n) r n))
         (value (ci-pv 5 r n))
         (tax (* .35 (- revenue cost depreciation))))
    (* 1000000 (+ (- capex)
                  revenue
                  (- cost)
                  (- tax)
                  value))))

(defun a.4.1.9 ()
  (let* ((r .05)
         (n 10)
         ;; all values are PVs
         (capex 200)
         (revenue (a-pv-growth 33 r n .049999))
         (cost (a-pv-growth 12 r n .04))
         (admin (a-pv 1 r n))
         (depreciation (a-pv (/ capex n) r n))
         (value (ci-pv 100 r n))
         (tax (* .35 (- revenue cost admin depreciation))))
    (* 1000000 (+ (- capex)
                  revenue
                  (- cost)
                  (- admin)
                  (- tax)
                  value))))
