; 1st step: loop through hard-coded list
(dolist (ele (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9)))
    (format t "~a " ele))
(format t "~%")

; 2nd step: loop through list passed as param
(defun getEven (nosList)
    (dolist (ele (remove-if-not #'evenp nosList)) 
        (format t "~a " ele)) 
    (format t "~%"))
(getEven (list 1 2 3 4 5 6 7 8 9))

; 3rd step: use own fn for telling if ele is even
(defun evenOrNot (x)
    (= 0 (mod x 2)))
; (format t "~a~%" (evenOrNot 2))
; (format t "~a~%" (evenOrNot 3))
(defun getEvenOrNot (nosList)
    (dolist (ele (remove-if-not #'evenOrNot nosList)) 
        (format t "~a " ele))
    (format t "~%"))
(getEvenOrNot (list 1 2 3 4 5 6 7 8 9))

; 4th step: use inline lambda fn for evenOrNot above
(defun getEvenLambda (nosList)
    (dolist 
        (ele 
            (remove-if-not 
                #'(lambda (x) 
                    (= 0 (mod x 2))) 
                nosList)) 
        (format t "~a " ele))
    (format t "~%"))
(getEvenLambda (list 1 2 3 4 5 6 7 8 9))
