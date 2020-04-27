; xbuf.el --- popup menu for emacs apps
; 
; This function pops up a buffer list

(defvar buf-menu
  '("Buffers"
    :filter buffers-menu-filter
))

(defun x-buf-menu (event)
  "Popup menu for buffer list"
  (interactive "e")
    (setq name (car (popup-menu buf-menu event)))
    (if name
	(call-interactively name)))
