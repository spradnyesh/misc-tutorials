(defpackage "match")

(defun variablep (smbl)
  (let* (
        (smbl-name (symbol-name smbl))
        (first-char (char smbl-name 0)))
    (char= first-char #\A)))

(write (variablep 'abc))
(write (variablep 'def))
