;
; Misc stuff
;
(set-background-color "gray91")
(set-foreground-color "black")
(set-face-background 'region "gray80")
;(set-face-foreground 'modeline "black")
;(set-face-background 'modeline "white")
(set-face-foreground 'highlight "white")
(set-face-background 'highlight "DarkGreen")
(set-face-foreground 'secondary-selection "white")
(set-face-background 'secondary-selection "DarkBlue")
; (set-face-font 'italic "-adobe-courier-medium-o-normal--14-100-100-100-m-90-iso8859-1")
; (set-face-font 'bold-italic "-adobe-courier-bold-o-normal--14-100-100-100-m-90-iso8859-1")
; (make-face 'ForestGreen-italic)
; (set-face-font 'ForestGreen-italic "-adobe-courier-medium-o-normal--14-100-100-100-m-90-iso8859-1")
; (set-face-foreground 'ForestGreen-italic "ForestGreen")
(set-mouse-color "red")
(set-cursor-color "red")


; various indentation settings
(setq c-indent-level 4)
(setq c-continued-statement-offset 4)
(setq c-argdecl-indent 4)
(setq ksh-indent 4)

; turn off truncation on windows smaller than frame
(setq truncate-partial-width-windows nil)

; disable region locking
(put 'narrow-to-region 'disabled nil)


;; Add various file suffixes to mode lists

; gradle mode
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

; makefile mode
(assoc "\\.mak" auto-mode-alist)
(setq auto-mode-alist (cons '("\\.mak$" . makefile-mode) auto-mode-alist))

; force all .h files to c++ mode
(assoc "\\.h$" auto-mode-alist)
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))

; typescript mode
(setq auto-mode-alist (cons '("\\.ts$" . typescript-mode) auto-mode-alist))


;(assoc "\\.c[px][px]$" auto-mode-alist)
;(setq auto-mode-alist (cons '("\\.c[px][px]$" . c++-mode) auto-mode-alist))
;(assoc "\\.h$" auto-mode-alist)
;(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))
;(assoc "\\.uil$" auto-mode-alist)
;(setq auto-mode-alist (cons '("\\.uil$" . c-mode) auto-mode-alist))
;(assoc "[Mm]akefile" auto-mode-alist)
;(setq auto-mode-alist (cons '("[Mm]akefile" . makefile-mode) auto-mode-alist))


; turn on 8-bit clean functionality
;(standard-display-european t)

; set mail preferences
(setq mhl-formfile "mhl.format")
(setq mh-summary-height 8)
(setq mh-lpr-command-format "lpr -Phertz -p -J '%s'")

; Add missing support functions for utf conversion
(defun utf-16-le-pre-write-conversion (start end) nil)
(defun utf-16-be-pre-write-conversion (start end) nil)

; my tweaked open file routine
(defun find-file-writable (arg)
  "Find a file and make the buffer writable"
  (interactive "p")
  (call-interactively 'find-file)
  (if buffer-read-only (call-interactively 'toggle-read-only)))

(defun toggle-insert-mode ()
  "Toggles between insert and overstrike mode"
  (interactive)
  (setq overwrite-mode (not overwrite-mode)))

(defun exec-macro-at-mouse (event)
  "Moves the cursor to mouse and executes last keyboard macro there"
  (interactive "e")
  (mouse-set-point event)
  (cond ((not last-kbd-macro) (start-kbd-macro nil))
	(t (call-last-kbd-macro))))

(defun set-color-prefs ()
  "Sets colors that I like"
  (interactive)
  (set-background-color "gray91")
  (set-foreground-color "black")
  (set-face-background 'region "white")
  ;(set-face-foreground 'modeline "black")
  ;(set-face-background 'modeline "white")
  (set-face-foreground 'highlight "white")
  (set-face-background 'highlight "DarkGreen")
  (set-face-foreground 'secondary-selection "white")
  (set-face-background 'secondary-selection "DarkBlue")
  (set-face-foreground 'ForestGreen-italic "ForestGreen")
  (set-mouse-color "red")
  (set-cursor-color "red"))

;; my customizations for all of c-mode and related modes
(defun my-c-mode-common-hook ()
  (setq c-basic-offset 4))

(defun c-indent-4 ()
  "Sets C indent level to 4"
  (interactive)
  (setq c-indent-level 4)
  (setq c-continued-statement-offset 4)
  (setq c-argdecl-indent 4))

(defun c-indent-8 ()
  "Sets C indent level to 8"
  (interactive)
  (setq c-indent-level 8)
  (setq c-continued-statement-offset 8)
  (setq c-argdecl-indent 8))

(defun goto-matching-paren ()
  "Move cursor to the matching parenthesis."
  (interactive)
  (cond ((looking-at "[[({]") (forward-sexp 1) (backward-char 1))
	((looking-at "[])}]") (forward-char 1) (backward-sexp 1))
        (t (ding) (message "Unbalanced parenthesis"))))


;
; Key mappings
;
(define-key esc-map "!" 'shell-command)
(define-key esc-map "b" 'buffer-menu)
;(define-key esc-map "B" 'bookmark-list)
(define-key esc-map "c" 'compile)
(define-key esc-map "f" 'find-file-writable)
(define-key esc-map "g" 'goto-line)
(define-key esc-map "i" 'insert-file)
(define-key esc-map "K" 'delete-window)
(define-key esc-map "l" 'load-file)
(define-key esc-map "m" 'manual-entry)
;(define-key esc-map "M" 'bookmark-set)
(define-key esc-map "q" 'query-replace)
(define-key esc-map "r" 'query-replace)
(define-key esc-map "R" 'replace-string)
(define-key esc-map "s" 'ispell-buffer)
(define-key esc-map "S" 'split-window-vertically)
(define-key esc-map "u" 'advertised-undo)
(define-key esc-map "w" 'save-buffer)
(define-key esc-map "W" 'write-file)
(define-key esc-map "" 'what-line)
(define-key esc-map "" 'keyboard-quit)
(define-key ctl-x-map "" 'cd)
(define-key ctl-x-map "" 'buffer-menu)
(define-key ctl-x-map "" 'find-file-writable)
(define-key ctl-x-map "" 'advertised-undo)
(define-key ctl-x-map "" 'save-buffers-kill-emacs)

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

;;
;; EMACS V19 and NON XEMACS code
;;
    ;
    ; Non X Stuff
    ;
    (cond ((null window-system)
       ; Sleaze some keys - will make keymaps work on serial consoles, etc.
       (setq keypad-map (make-keymap))
	    (define-key esc-map "[" keypad-map)
	    (define-key keypad-map "1~" 'isearch-forward)
	    (define-key keypad-map "2~" 'toggle-insert-mode)
	    (define-key keypad-map "3~" 'kill-region)
	    (define-key keypad-map "4~" 'set-mark-command)
	    (define-key keypad-map "5~" 'scroll-down)
	    (define-key keypad-map "6~" 'scroll-up)
	    (define-key keypad-map "17~" 'split-window-vertically)
	    (define-key keypad-map "18~" 'split-window-horizontally)
	    (define-key keypad-map "19~" 'delete-window)
	    (define-key keypad-map "20~" 'other-window)
	    (define-key keypad-map "21~" 'switch-to-buffer)
	    (define-key keypad-map "28~" 'apropos)
	    (define-key keypad-map "29~" 'eval-print-last-sexp)

       ;
       ; Another grody hack when not running X, to
       ; set up arrow keys
       ;
       (setq arrow-map (make-keymap))
            (define-key esc-map "O" arrow-map)
	    (define-key arrow-map "A" 'previous-line)
	    (define-key arrow-map "D" 'backward-char)
	    (define-key arrow-map "B" 'next-line)
	    (define-key arrow-map "C" 'forward-char)
    ))

    ;
    ; X Stuff
    ;
    (cond (window-system
       ; set some keyboad X stuff
       (global-set-key [S-mouse-1] 'copy-region-as-kill)
       (global-set-key [S-mouse-3] 'exec-macro-at-mouse)
       (global-set-key [C-down-mouse-1] 'x-apps-menu)
       (global-set-key [C-S-down-mouse-1] 'mouse-buffer-menu)
       (global-set-key [C-S-down-mouse-2] 'x-select-file)
       (global-set-key [C-S-down-mouse-3] 'x-mb3-menu)
       (global-set-key [C-M-down-mouse-3] 'mouse-set-font)

       ; enable highlighting modes
       (setq hilit-mode-enable-list  '(not text-mode)
	     hilit-background-mode   'light
	     hilit-inhibit-hooks     nil
	     hilit-inhibit-rebinding nil
	     hilit-quietly	     t)
    ))

(put 'downcase-region 'disabled nil)
