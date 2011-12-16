;; this is an extremely good and easy way to create an encapsulation/object
(defun obj ()
  (let ((i 0))
    (defun get-next-i ()
      (setf  i (1+ i)))
    (defun set-i (new-i)
      (setf i new-i))
    (defun reset-i ()
      (setf i 0))))

;; ??? how do i create multiple copies of the above object ???

;; http://psg.com/~dlamkins/sl/chapter15.html
(defun cls ()
  (let ((i 0))
    #'(lambda (operation &rest arguments)
        (ecase operation
          (get-next-i
           (setf i (1+ i)))
          (set-i
           (let ((new-i (first arguments)))
             (setf i new-i)))
          (reset-i
           (setf i 0))))))

;; refactored the above a bit
(defun cls-1 ()
  (let ((i 0))
    #'(lambda (operation &rest arguments)
        (defun get-next-i ()
          (setf  i (1+ i)))
        (defun set-i (new-i)
          (setf i new-i))
        (defun reset-i ()
          (setf i 0))
        (ecase operation
          (get-next-i
           (get-next-i))
          (set-i
           (let ((new-i (first arguments)))
             (set-i new-i)))
          (reset-i
           (reset-i))))))

;; support class/static variables too
(let ((s 0))
  (defun cls-2 ()
    (let ((i 0))
      #'(lambda (operation &rest arguments)
          (defun get-next-i ()
            (setf  i (1+ i)))
          (defun get-next-s ()
            (setf  s (1+ s)))
          (defun set-s (new-s)
            (setf s new-s))
          (defun reset-s ()
            (setf s 0))
          (defun set-i (new-i)
            (setf i new-i))
          (defun reset-i ()
            (setf i 0))
          (ecase operation
            (get-next-i
             (get-next-i))
            (set-i
             (let ((new-i (first arguments)))
               (set-i new-i)))
            (reset-i
             (reset-i)))))))
