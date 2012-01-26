(loop for n = (parse-integer (read-line) :junk-allowed t)
              while (/= n 42)
              do (format t "~D~%" n))
