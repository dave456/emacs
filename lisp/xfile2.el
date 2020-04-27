
(defun x-get-filename (directory event)
  "Find a file via menus in X.  The selected file is returned.
   DIRECTORY is the default directory to begin the search in, and EVENT
   is the argument passed in by the X mouse software"
  (let (menu files result key file limit (continue t))
    (while continue
      (setq limit (/ (frame-height) 2)
	    files (cons nil (directory-files directory)))
      (while (setq files (cdr files))
	(setq file (car files))
	(if (file-directory-p (concat directory file))
	    (setq file (concat file "/")))
	(if (string-equal file "//") (setq file "/"))
	(setq menu (cons (vector file '(list file) t) menu)))
      (setq menu (cons default-directory (nreverse menu)))
      (setq result (popup-menu menu event))
      (setq file (expand-file-name (concat directory (cdr result))))
      (cond ((null result) 		(setq continue nil))
	    (t 				(setq continue t directory file))))
    (setq result file)
    result))

(defun x-select-file (event)
  "Select and edit a file using menus under X."
  (interactive "e")
  (let ((name (x-get-filename default-directory event)))
    (if name (find-file name))))
