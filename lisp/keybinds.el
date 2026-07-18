;;
;; Some convienent functions
;;

;; Tweaked open file routine
(defun find-file-writable (arg)
  "Find a file and make the buffer writable"
  (interactive "p")
  (call-interactively 'find-file)
  (if buffer-read-only (call-interactively 'toggle-read-only)))

;; Toggle for insert/overstrike mode
(defun toggle-insert-mode ()
  "Toggles between insert and overstrike mode"
  (interactive)
  (setq overwrite-mode (not overwrite-mode)))

;; clean up backup files - don't have this bound anywhere...
(defun cleanup-backup-files ()
  "Clean up backup files not accessed in a week"
  (message "Deleting old backup files...")
  (let ((week (* 60 60 24 7))
	(current (float-time (current-time))))
    (dolist (file (directory-files temporary-file-directory t))
      (when (and (backup-file-name-p file)
		 (> (- current (float-time (fifth (file-attributes file))))
		    week))
	(message "%s" file)
	(delete-file file)))))

;;
;; This handles cases where windows and non-windows users have been
;; editing the same file and we have mixed line endings. Add it to
;; language and text hooks where appropriate. I should probably
;; rename this, because it doesn't actually remove the eols, it
;; just hides them.
;;
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; just a function to do ansi highlighting on a buffer, this is
;; annoying as it does actually modify the buffer. Probably should
;; figure out a better way to do this...
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;;
;; Define a custom minor mode and keymap for overriding keybinds
;; in other modes.
;;

;; The keymap for our minor mode
(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

;; The minor mode that holds our custom keybindings
(define-minor-mode my-mode
  "Maintains keybindings I really want"
  :init-value t
  ;;:lighter " my-mode"
  :keymap my-mode-map)

;;
;; Place keybindings in this mode map that you want to override
;; EVERYTHING. Oftentimes modes will redefine keys. Keys added
;; here will always take precedence. This is for us old guys who
;; don't want their keymaps to change... ;)
;;
;; If you use the global yas mode thingy, then yas will potentially
;; override, if you stick to listing minors then its order dependent.
;;
(define-key my-mode-map [mouse-3] #'mouse-save-then-kill)

;;
;; The keymaps in emulation-mode-map-alists take precedence over
;; minor-mode-map-alist
;;
(define-globalized-minor-mode global-my-mode my-mode my-mode)
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;; Advertise our new mode
(provide 'my-mode)

;;
;; Key mappings - these can be overridden by modes
;;
(define-key esc-map "!" 'shell-command)
(define-key esc-map "b" 'buffer-menu)
(define-key esc-map "c" 'compile)
(define-key esc-map "f" 'find-file-writable)
(define-key esc-map "g" 'goto-line)
(define-key esc-map "i" 'insert-file)
(define-key esc-map "l" 'bookmark-bmenu-list)
(define-key esc-map "m" 'manual-entry)
(define-key esc-map "q" 'query-replace)
(define-key esc-map "r" 'query-replace)
(define-key esc-map "R" 'replace-string)
(define-key esc-map "s" 'ispell-buffer)
(define-key esc-map "S" 'split-window-vertically)
(define-key esc-map "u" 'undo)
(define-key esc-map "w" 'save-buffer)
(define-key esc-map "W" 'write-file)
(define-key esc-map "" 'keyboard-quit)

(global-set-key [find] 'isearch-forward)
(global-set-key [insert] 'toggle-insert-mode)
(global-set-key [remove] 'kill-region)
(global-set-key [select] 'set-mark-command)
(global-set-key [prev] 'scroll-down)
(global-set-key [next] 'scroll-up)
(global-set-key [help] 'apropos)
(global-set-key [menu] 'eval-print-last-sexp)

(global-set-key [f5] 'font-lock-mode)
(global-set-key [f6] 'split-window-vertically)
(global-set-key [f7] 'split-window-horizontally)
(global-set-key [f8] 'delete-window)
(global-set-key [f9] 'other-window)
(global-set-key [f10] 'switch-to-buffer)
(global-set-key [C-.] 'call-last-kbd-macro)
(global-set-key [M-q] 'server-edit)

;;
;; Pseudo key bindings (custom variables used by modes, etc.)
;;
(setq smerge-command-prefix "\C-cv")

;;
;; set some mouse keybindings
;;
(cond (window-system
  (global-set-key [S-mouse-1] 'copy-region-as-kill)
  (global-set-key [S-mouse-3] 'exec-macro-at-mouse)
  (global-set-key [C-S-down-mouse-1] 'mouse-buffer-menu)
  (global-set-key [C-S-down-mouse-2] 'x-select-file)
  (global-set-key [C-S-down-mouse-3] 'x-mb3-menu)
  (global-set-key [C-M-down-mouse-3] 'mouse-set-font)
  ))
