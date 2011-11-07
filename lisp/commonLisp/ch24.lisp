(defpackage :ch24
  (:use :cl))
(in-package :ch24)

;;;;changing a const is an error condtion. changing a param is allowed
;;;;and works too, but param goes to original value on reload of
;;;;file/package. changing a var is definitely allowed and works too,
;;;;but var does *NOT* go to original value on reload of file/package
(defconstant +constant+ 0)
(defparameter *param* 1)
(defvar *var* 2)
