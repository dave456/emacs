;;
;; Some convienent functions for keybinds
;;

;;
;; Tweaked open file routine. This just toggles read-only mode on the buffer
;; NOTUSED
;;
(defun find-file-writable (arg)
  "Find a file and make the buffer writable"
  (interactive "p")
  (call-interactively 'find-file)
  (when buffer-read-only
    (call-interactively 'toggle-read-only)))

;; Toggle for insert/overstrike mode
(defun toggle-insert-mode ()
  "Toggles between insert and overstrike mode"
  (interactive)
  (setq overwrite-mode (not overwrite-mode)))

;;
;; When opening read-only files invoke sudo to allow editing of
;; system files, etc. There is also some hackery to make dired via
;; find-file work correctly and not open via sudo.
;; Finally, new files should not be opened with sudo by default.
;;
;; At one point I was messing with file-exists-p and
;; file-directory-p. I leave this comment here for my future self
;; in case I ever need to remember those functions again.
;;
(defun find-file-sudo (arg)
  "open file, if read-only open via sudo"
  (interactive "p")
  (call-interactively 'find-file)
  (when (and buffer-read-only buffer-file-name)
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

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
  ;;:lighter " my"
  :keymap my-mode-map)

;;
;; Place keybindings in this mode map that you want to override
;; EVERYTHING. Oftentimes modes will redefine keys. Keys added
;; here will always take precedence. Basically, here be keybindings
;; that I am totally addicted to.
;;
(define-key my-mode-map [mouse-3] #'mouse-save-then-kill)

;;
;; The keymaps in emulation-mode-map-alists take precedence over
;; minor-mode-map-alist
;;
(define-globalized-minor-mode global-my-mode my-mode my-mode)
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;;
;; A cute ui hook in the minor mode menu to turn off our minor mode
;; in the minibuffer. For debugging keybinds, i.e. my overrides
;; broke something...
;;
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (my-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)

;; Advertise our mode
(provide 'my-mode)

;;
;; Key mappings - these can be overridden by modes
;;
(define-key esc-map "!" 'shell-command)
(define-key esc-map "b" 'buffer-menu)
(define-key esc-map "c" 'compile)
(define-key esc-map "f" 'find-file-sudo)
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
