
(defun x-get-filename (directory event)
  "Find a file via menus in X.  The selected file is returned.
   DIRECTORY is the default directory to begin the search in, and EVENT
   is the argument passed in by the X mouse software"
  (let (menu files result key file limit (continue t))
    (while continue
      (setq limit (/ (frame-height) 2)
	    menu (list (cons (format "SELECT %s" directory)
			     (cons 'ACCEPT directory)))
	    files (cons nil (directory-files directory)))
      (while (setq files (cdr files))
	(setq file (car files)
	      key 'ACCEPT)
	(if (file-directory-p (concat directory file))
	    (setq key 'EXPAND
		  file (concat file "/")))
	(if (string-equal file "//") (setq file "/"))
	(setq menu (cons (cons file (cons key file)) menu)))
      (if (> (length menu) limit)
	  (let ((pane nil) (panes nil) (count 0))
	    (setq limit (/ (length menu) (1+ (/ (length menu) limit))))
	    (while menu
	      (setq pane (cons (car menu) pane))
	      (setq menu (cdr menu)
		    count (1+ count))
	      (and (> count limit)
		   (setq panes (cons (cons directory pane) panes))
		   (setq pane nil
			 count 0)))
	    (if pane (setq panes (cons (cons directory pane) panes)))
	    (setq menu panes))
	(setq menu (cons (cons directory (nreverse menu)) nil)))
      (setq result (x-popup-menu event (cons "find file menu" menu)))
      (setq file (expand-file-name (concat directory (cdr result))))
      (cond ((null result) 		(setq continue nil))
	    ((eq (car result) 'ACCEPT)  (setq continue nil result file))
	    (t 				(setq continue t directory file))))
    result))

(defun x-select-file (event)
  "Select and edit a file using menus under X."
  (interactive "e")
  (let ((name (x-get-filename default-directory event)))
    (if name (find-file name))))
