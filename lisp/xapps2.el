; xapps2.el --- popup menu for emacs apps
; 
; This function loads a file or directory from the current directory
; using the popup menus. Works with Xemacs



(defvar xapps-menu
  '("Apps menu"
     ["Send Mail"	mh-smail t]
     ["Read Mail"       mh-rmail t]
     "----"
     ["Insert Letter"   mh-insert-letter t]
     ["Pack Folder"     mh-pack-folder t]
))


(defun x-apps-menu (event)
  "Popup menu for using mail."
  (interactive "e")
    (setq name (car (popup-menu xapps-menu event)))
    (if name
	(call-interactively name)))
