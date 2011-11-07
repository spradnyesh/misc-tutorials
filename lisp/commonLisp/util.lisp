(defpackage util)
(in-package util)

;(shadow 'lisp:elementp)
(deftype element ()
  '(satifies elementp))
(defun elementp (obj)
  (or
   (string= (symbol-name
             (or
              (and (consp (type-of obj)) (first (type-of obj)))
              (type-of obj)))
             "SIMPLE-BASE-STRING")
   (characterp obj)
   (numberp obj)
   (packagep (find-package obj))))
(export 'elementp)
(export 'element)
