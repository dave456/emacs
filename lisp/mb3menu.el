; mb3menu.el --- popup menu for mb3
; 
; This function loads pops a menu for mb3, these functions are
; convienence functions based upon the major mode

(defvar mb3-menu
  '("MB3 menu"
    ("-- Context --"
     ("Jump to..."		imenu)
     ("Find symbol" 		cscope-find-this-symbol)
     ("Find text"		cscope-find-this-text-string)
     ("Find definition"		cscope-find-global-definition)
     ("Find functions calling this"	cscope-find-functions-calling-this-function)
     ("Find functions called by"	cscope-find-called-functions)
     ("Find file"		cscope-find-this-file)
     ("Find files including"	cscope-find-files-including-file)
     ("Find egrep pattern"	cscope-find-egrep-pattern)
     ("--" nil)  ; separator
     ("Bookmark"		bmkp-bookmark-set-confirm-overwrite)
     ("--" nil)  ; separator
     ("Man Page"		manual-entry)
     ("Describe function..."	describe-function)
     ("Describe variable..."	describe-variable)
)))

(defun x-mb3-menu (event)
  "MB3 popup convienence menu."
  (interactive "e")
    (setq name (car (x-popup-menu event mb3-menu)))
    (if name
	(call-interactively name)))
