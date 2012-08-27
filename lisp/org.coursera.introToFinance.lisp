;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; week 4 (npv, irr, decision making)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
-- to find the cost of 2 machines m1 & m2 w/ different capex, maintainance fees and lives:
    1. find npv
    2. find pmt
    3. compare

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; week 5 (bonds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
-- zero-coupon bonds *always* trade/sell at discount (ie pv < fv) coz it's ci-pv (there ain't any coupons/pmt)
-- at maturity
  -- zero-coupon: i get only fv
  -- coupon: i get both fv and last pmt => fv + pmt
-- long term bonds are more sensitive (since r2 can vary a lot), and hence get a higher r1 (as compared to a short term bond)
-- coupon-rate => r1, yield-to-maturity => r2
-- price (pv) is *inversely proportional* to market rate (r2). this is because when r2 > r1, i can sell bond and invest money elsewhere, so bond is not really that valuable (and hence (selling/buying) price is low); and vice-versa
  -- when pv > fv (coz r1 > r2) => trading at premium
  -- when pv < fv (coz r1 < r2) => trading at discount
  -- when pv = fv (coz r1 = r2) => trading at par (or at face-value)
-- govt bonds are also called 'no-default' or 'risk-free' bonds since we expect the govt to pay the face-value at end of maturity (and coupons every 6 months) _w/o failure_
-- the uncertainty concerning bond values/prices due to interest rates fluctuations is known as the 'interest rate risk' of bonds
  -- 2 types of 'interest rate risk'
    -- 'price risk'
      -- for a 10 yrs (20 periods) bond, i know both P0 (pv, based on r1 and fv defined in contract/bond) and P20 (fv, already defined in contract/bond), but it's not possible to find P10 (price after 5 yrs), coz r2 is fluctuating
      -- even a zero-coupon bond has price risk at all points before maturity
      -- P10 = PV (pmt = 30, n = 10, fv = 1000, r = ?) => P10 decreases as r increases
    -- 'coupon reinvestment risk'
      -- since the ytm is changing continuously, i don't know at what rate will my coupon can be re-invested. this risk is called the coupon reinvestment risk
      -- this risk is absent in 'zero coupon bond', simply b'coz there ain't any coupon to reinvestment
      -- P10 = PV (pmt = 30, n = 10, fv = 1000, r = ?) => decreases as r increases
               + fv10 (pmt = 30, n = 10, r = ?) => the re-invested 10 coupons from the last 5 yrs => increases as r increases
    -- the only instrument that does not have any risk is a 'zero coupon bond' that is _held until maturity_
      -- risk (price risk) is there only if it is sold earlier than maturity
-- corporate bonds and default risk
  -- like govt bonds, they are subject to 'interest rate risk'
  -- almost always pay coupons
  -- unlike govt bonds, they are subject to 'default risk'
    -- rating agencies rates corporate bonds against defaulting (not paying the coupon and/or fv)
  -- most bonds are contracts/loans issued by a bank
  -- for 2 identical bonds (same maturities, coupon rates, fv) b/n a govt bond and a corporate bond, the corporate bond will have lesser price (pv), coz it has a higher risk (default risk). why should i pay higher for a bond that may default (has a higher risk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; week 6 (stocks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
-- when n -> infinite, the value of P_n -> 0, so stock-pv = (sum_i:0->inf (/ div_i (expt (1+ r) i)))
-- assume perpetuities (not annuities) in calculations, coz most ideas are (assumed to be) long term
-- ICPS: Invested Capital Per Share
-- ROI (Return On Investment = IRR) > 0
-- EPS (Earnings Per Share) = ICPS * ROI
-- EPS (Earnings Per Share) is the same as CFPS (Cash Flow Per Share)
  -- EPS = Dividend + RE (Retained Earnings)
    -- Div/EPS => (1 - b), RE/EPS => b
    -- b => re-invested in the company for growth
    -- g = b * ROI => engine of growth
-- theoritically: PV of a share = ((EPS / r) + PVGO), where PVGO: PV of (future) growth opportunities
  -- p0 = PV (div)
    -- no-growth:-
      -- p0 = div/r = EPS/r (here, div = eps, since there is no 'b' which is being re-invested in growth)
    -- growth (can only happen when b > 0):-
      -- PVGO => difference of 'firm w/ growth' and 'firm w/o growth'
      -- when b = 0 => g = 0 => p0 = div/(r - g) = div/r (same as p0 w/o growth (see above))
-- important funda about growth
  -- by having b > 0, ie having growth, what is really growing is the Cash Flow, or EPS; but
  -- if r > IRR (or ROI), then the value of the share (p0) is really decreasing
    -- see examples p.6.6 (w/o growth) => 50 and p.6.7 (w/ roi=.10, b=.7, r=.12) => 36, below
    -- it would've made more sense to invest the b in another investment giving back r ;)
    -- this issue is caused coz management sees the incomplete world (IRR only) and not that IRR < r :(
  -- otoh, if r < IRR (or ROI), then the value of the share (p0) is really increasing
    -- see examples p.6.6 (w/o growth) => 50 and p.6.8 (w/ roi=.14, b=.5, r=.12) => 84, below
  -- note that g (= b * roi) is the same in both 6.7 and 6.8
    -- *** what the management should look at is the comparison b/n IRR and r, and not fixate on g
-- share market
  -- market-cap = price-per-share * #shares
  -- value-of-company = market-cap - debt
  -- EPS (accounting) != EPS (finance)
"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formulae
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pv: Present Value
;; fv: Future Value (or Face Value in case of bonds)
;; r: rate of interest (of deposit/loan)
;; n: #periods

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

(defun ci-r (pv fv n)
  (1- (expt (/ fv pv) (/ 1 n))))

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
  (if (> r g)
      (* pmt (/ 1 (- r g)) (- 1 (/ (expt (1+ g) n) (expt (1+ r) n))))
      (let ((rslt 0)
            (c pmt))
        (dotimes (i 100)
          (setf rslt (+ rslt (/ c (/ 1 (expt (1+ r) (1+ i))))))
          (setf c (* c (1+ g))))
        rslt)))

;; effective-annual-rate
;; k: #periods in a year (12 for monthly, 4 for quarterly, etc)
(defun ear (r k)
  (1- (expt (1+ (/ r k)) k)))

;; advertised-annual-rate
(defun r (ear k)
  (* k (1- (expt (1+ ear) (/ 1 k)))))

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
  (growth-stock-pv pmt r g 100000))

;; TODO
(defun pmt-for-ppv (pv r &optional (g 0))
  (declare (ignore pv r g)))

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
;; bonds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yeild-to-maturity (r) of a zero-coupon bond
;; this is the same as the 'r' of a compound-interest investment
(defun ytm (pv fv n)
  (ci-r pv fv n))

;; r1: r for coupon (decided at time when contract was made by govt, cannot change over tenure) => coupon-rate
;; r2: r for market rate (decided by market, may change over tenure) => yield-to-maturity
(defun coupon-pv (fv n r1 &optional (r2 r1))
  ;; adjust n and r for 6 months (in US) PMT, instead of 1 yr
  (let* ((n (* n 2))
         (r1 (/ r1 2))
         (r2 (/ r2 2))
         (pmt (* fv r1)))
    (+ (a-pv pmt r2 n)
       (ci-pv fv r2 n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun stock-pv (fv r &rest div)
  (let ((n (length div))
        (rslt 0))
    (dotimes (i n)
      (setf rslt (+ rslt (/ (nth i div)
                            (expt (1+ r) (1+ i))))))
    (+ rslt (/ fv (expt (1+ r) n)))))

;; 'dividend-stock' are stocks where dividends are surely paid and of same value every time
(defun dividend-stock-perpetuity-pv (div r)
  (/ div r))

(defun dividend-stock-annuity-pv (div r n)
  (a-pv div r n))

;; div: 1st div ((i+1)_th = (* i_th (1+ g)))
(defun growth-stock-pv (div r g &optional (n 100))
  (if (> r g)
      (/ div (- r g))
      (let ((divs nil)
            (c div))
        (dotimes (i n)
          (push c divs)
          (setf c (* c (1+ g))))
        (apply #'stock-pv 0 r divs))))

(defun growth-stock-pv-2 (cps roi r b)
  (growth-stock-pv (* (- 1 b) cps roi) r (* b roi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; excel functions that i cannot replicate here :(
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun excel-rate (n div pv fv)
  (declare (ignore n div pv fv))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lecture problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun p3.10 ()
  (pmt-for-afv (ci-pv (a-pv 100000 .08 20) .08 10) .08 20))

(defun p.6.6 ()
  (let ((roi .1)
        (cps 60)
        (r .12)                         ; market-capitalization-rate
        (b 0))
    (growth-stock-pv-2 cps roi r b)))

(defun p.6.7 ()
  (let ((roi .1)
        (cps 60)
        (r .12)
        (b .7))
    (values (* b roi)
            (growth-stock-pv-2 cps roi r b))))

(defun p.6.8 ()
  (let* ((roi .14)
         (cps 60)
         (r .12)
         (b .5)
         (div (* b (* .1 cps)))    ; picked up the same div from p.6.6
         (pv.6.6 (p.6.6)))         ; => 50
    (values (* b roi)
            (/ (-
                (growth-stock-pv div r (* b roi)) ; => 60
                pv.6.6)
               pv.6.6))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assignment 5-a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a.5.a.1 ()
  (ci-pv 10000 .015 10))
(defun a.5.a.2 ()
  t)
(defun a.5.a.3 ()
  nil)
(defun a.5.a.4 ()
  (* 2 100 (ytm 950 1000 4)))
(defun a.5.a.5 ()
  nil)
#|(defun a.5.a.6 ()
  (let* ((n (* 2 6))
         (r (/ .05 2))
         (fv 100000)
         (pv 89793)
         (div (* fv r))
         (forwarded-fv 0)) ; take all div's fwd in time
    (dotimes (i n)
      (setf forwarded-fv (+ forwarded-fv (ci-fv div r (- n i 1)))))
    (setf forwarded-fv (+ forwarded-fv fv))
    (* 2 (ci-r pv forwarded-fv n))))|#
(defun a.5.a.6 ()
  (let* ((n (* 2 6))
         (r1 (/ .05 2))
         (fv 100000)
         (pv 89793)
         (div (* fv r1)))
    (* 2 100 (+ (excel-rate n div (- pv) fv)
                3.56)))) ; answer obtained from excel
(defun a.5.a.7 ()
  nil)
(defun a.5.a.8 ()
  (let* ((fv 1000)
         (r-1 (/ .08 2))
         (r-2 (/ .05 2))
         (n-1 (* 2 9))
         (n-2 (* 2 6))
         (value-1 0)
         (value-2 0))
    (setf value-1 (ci-pv fv r-1 n-1))
    (format t "~f, " value-1) ; value of 1000$ in the begining (9 yrs before maturity) at 8% pa
    (setf value-2 (ci-pv fv r-2 n-2))
    (format t "~f, " value-2) ; value of 1000$ today (6 yrs before maturity) at 5% pa
    (setf value-2 (ci-pv value-2 r-1 (- n-1 n-2)))
    (format t "~f~% " value-2) ; value of todays value in the begining at 8% pa (that i would've earned till now)
    (- value-1 value-2))) ; since value-1 < value-2, it's a profit and needs to be shown in the +ve
(defun a.5.a.9 ()
  (let* ((pv 1500000)
         (fv-zero 3500000)
         (fv-coupon 2000000)
         (div (/ 125000 2))
         (n (* 10 2))
         (ytm-zero 0)
         (ytm-coupon 0))
    (setf ytm-zero (* 2 100 (ytm pv fv-zero n)))
    (format t "~f, " ytm-zero)
    (setf ytm-coupon (+ (excel-rate n div pv fv-coupon)
                        (* 2 5.16)))
    (format t "~f, " ytm-coupon)
    ))
(defun a.5.a.10 ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assignment 5-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a.5.b.1 ())
(defun a.5.b.2 ())
(defun a.5.b.3 ())
(defun a.5.b.4 ())
(defun a.5.b.5 ())
(defun a.5.b.6 ())
(defun a.5.b.7 ())
(defun a.5.b.8 ())
(defun a.5.b.9 ())
(defun a.5.b.10 ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assignment 6-a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a.6.a.1 ())
(defun a.6.a.2 ())
(defun a.6.a.3 ())
(defun a.6.a.4 ())
(defun a.6.a.5 ())
(defun a.6.a.6 ())
(defun a.6.a.7 ())
(defun a.6.a.8 ())
(defun a.6.a.9 ())
(defun a.6.a.10 ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assignment 6-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun a.6.b.1 ())
(defun a.6.b.2 ())
(defun a.6.b.3 ())
(defun a.6.b.4 ())
(defun a.6.b.5 ())
(defun a.6.b.6 ())
(defun a.6.b.7 ())
(defun a.6.b.8 ())
(defun a.6.b.9 ())
(defun a.6.b.10 ())

(defun a.7.a.1 ())
(defun a.7.a.2 ())
(defun a.7.a.3 ())
(defun a.7.a.4 ())
(defun a.7.a.5 ())
(defun a.7.a.6 ())
(defun a.7.a.7 ())
(defun a.7.a.8 ())
(defun a.7.a.9 ())
(defun a.7.a.10 ())
(defun a.7.b.1 ())
(defun a.7.b.2 ())
(defun a.7.b.3 ())
(defun a.7.b.4 ())
(defun a.7.b.5 ())
(defun a.7.b.6 ())
(defun a.7.b.7 ())
(defun a.7.b.8 ())
(defun a.7.b.9 ())
(defun a.7.b.10 ())
