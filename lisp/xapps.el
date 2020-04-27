; xapps.el --- popup menu for emacs apps
; 
; This function pops up an applications menu


(defvar xapps-menu
  '("Apps menu"
    ("-- Applications --"
     ("Send Mail"	mh-smail)
     ("Read Mail"       mh-rmail)
     ("Insert Letter"   mh-insert-letter)
     ("Pack Folder"     mh-pack-folder)
)))

(defun x-apps-menu (event)
  "Popup menu for using mail."
  (interactive "e")
    (setq name (car (x-popup-menu event xapps-menu)))
    (if name
	(call-interactively name)))
