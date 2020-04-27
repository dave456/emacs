; xfile.el --- popup menu for loading files/directories
; 
; This function loads a file or directory from the current directory
; using the popup menus.

(defun x-find-file (event)
  "Popup menu for opening file in current directory."
  (interactive "e")
  (let ((menu (make-file-menu default-directory)))
    (message "Creating menu...")
    (setq name (car (x-popup-menu event menu)))
    (if name
	(find-file (concat default-directory name)))))

(defun make-file-menu (directory)
  (let
      ((files (directory-files directory nil nil))
       (split-files)
       (one-list)
       (pane)
       (menu '("Files")))
    (setq split-files (split-big-list files 10))
    (while (not (null split-files))
      (setq one-list (car split-files))
      (setq pane (list (make-pane-name one-list)))
      (while (not (null one-list))
	(setq pane (append pane (list (list (car one-list) (car one-list)))))
	(setq one-list (cdr one-list)))
      (setq split-files (cdr split-files))
      (setq menu (append menu (list pane))))
    menu))

(defun make-pane-name (l)
  (concat (car l) "..."))

(defun split-big-list (l n)
  (cond
   ((null l) l)
   (t (let
       ((i 0)
	(s nil))
       (while (< i n)
	 (setq i (+ 1 i))
	 (setq s (append s (list (car l))))
	 (setq l (cdr l))
	 (if (null l) (setq i (+ n 1))))
       (append (list s) (split-big-list l n))))))
