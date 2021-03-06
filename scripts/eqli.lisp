;; $ eqli > ~/installed

(defclass pkg ()
  ((size :initarg :size :accessor pkg-size)
   (unit :initarg :unit :accessor pkg-unit)
   (group-name :initarg :group-name :accessor pkg-group-name)))
(defun do-the-do ()
  (let ((in (open "/home/pradyus/installed"))
		(rslt nil))
	(when in
	  (loop for line = (read-line in nil)
		 while line do
		   #|(format t "~a~%" line)|#
		   (let* ((split (split-sequence:split-sequence " " line :test #'string-equal))
				  (3rd (nth 3 split))
				  (sizeUnit (subseq 3rd 1 (- (length 3rd) 1)))
				  (len (length sizeUnit))
				  (pkg (make-instance 'pkg :size (parse-integer (subseq sizeUnit 0 (- len 2))
																:junk-allowed t)
									:unit (subseq sizeUnit (- len 2))
									:group-name (nth 6 split))))
			 (when (string-equal "MB" (pkg-unit pkg))
			   (push pkg rslt))))
	  (close in))
	#|(print rslt)|#
	(dolist (pkg (sort rslt #'> :key #'pkg-size))
	  (format t "~AMB: ~A~%" (pkg-size pkg) (pkg-group-name pkg)))))
(do-the-do)
