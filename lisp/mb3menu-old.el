; mb3menu.el --- popup menu for mb3
; 
; This function loads pops a menu for mb3, these functions are
; convienence functions based upon the major mode



(defvar mb3-menu
  '("MB3 menu"
    ("-- Applications --"
     ("Jump to..."	     imenu)
     ("Man Page"             manual-entry)
     ("Find symbol" cscope-find-c-symbol)
     ("Find definition" cscope-find-global-definition)
     ("Find functions called" cscope-find-functions-called)
     ("Find functions calling" cscope-find-functions-calling)
     ("Find file" cscope-find-file)
     ("Find files including" cscope-find-files-including)
     ("Find text" cscope-find-text-string)
     ("Find egrep pattern" cscope-find-egrep-pattern)
     ("--" nil)  ; separator
     ("Describe function..." describe-function)
     ("Describe variable..." describe-variable)
)))

(defun x-mb3-menu (event)
  "MB3 popup convienence menu."
  (interactive "e")
    (setq name (car (x-popup-menu event mb3-menu)))
    (if name
	(call-interactively name)))
