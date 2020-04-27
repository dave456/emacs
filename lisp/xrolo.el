; xfile.el --- popup menu for rolodex
; 
; This function loads a file or directory from the current directory
; using the popup menus.

(require 'wrolo "~/lisp/wrolo.elc")

(defvar rolo-menu
  '("Rolo menu"
    ("-- Rolodex --"
     ("Add Entry"	rolo-add)
     ("Edit Entry"      rolo-edit)
     ("Delete Entry"    rolo-kill)
     ("Sort Entries"    rolo-sort)
     ("Display Matches" rolo-display-matches)
     ("Regexp Search"   rolo-grep)
     ("String Search"   rolo-fgrep)
     ("Yank"            rolo-yank))))

(defun x-rolo-menu (event)
  "Popup menu for using rolodex."
  (interactive "e")
    (setq name (car (x-popup-menu event rolo-menu)))
    (if name
	(call-interactively name)))
