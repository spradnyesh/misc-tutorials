(defvar tools)
(setf tools (list 'hammer 'screwdriver))
(cons 'pliers tools)
tools
(setf tools (cons 'pliers tools))
tools
(append '(saw wrench) tools)
tools
(setf tools (append '(saw wrench) tools))
tools
