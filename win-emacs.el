
;; Load my keybinds
(load-file "~/lisp/keybinds.el")
(load-file "~/lisp/yaml-mode.elc")

(add-to-list 'exec-path "C:/Program Files/Git/usr/bin/")

;;
;; Custom stuff I wrote or swiped - load on demand
;;
(autoload 'x-select-file "~/lisp/xfile.elc")
(autoload 'x-mb3-menu "~/lisp/mb3menu.elc")
;;(autoload 'yaml-mode "~/lisp/yaml-mode.elc")

;;
;; Add the melpa repos (the newest stuff)
;; With both dev and stable it shows dupes in package list, deal with it...
;;
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "https:://melpa.org/") 'APPEND)
;(add-to-list 'package-archives '("melpa-stable" . "http:://melpa.org/") 'APPEND)
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") 'APPEND)

;; tell tramp to use ssh pipes instead of scp where possible, its more
;; responsive, but uses more bandwidth...
;(setq tramp-default-method "ssh")

;; to suppress annoying magit messages (why is this needed anymore??)
(setq magit-last-seen-setup-instructions "1.4.0")

;; suppress obnoxious popup warnings - they still show up in warnings buffer
(setq warning-minimum-level :emergency)

;;
;; This handles cases where windows and non-windows users have been
;; editing the same file and we have mixed line endings. Add it to
;; language and text hooks where appropriate...
;;
;; To be clear, this hides the characters, but does not actually
;; remove them. If you cut and paste into other applications you
;; can still pick up these characters without realizing it.
;;
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


;;
;; my custom language mode hooks
;;

;; lots of stuff uses c-basic-offset now, so putting it in a language hook is probably a mistake
(setq c-basic-offset 4)

;; hooks for all C syntax based languages (c, c++, java, etc.)
(add-hook 'c-mode-common-hook 
	  (lambda ()
	    (remove-dos-eol)))
	    ;;(cscope-minor-mode +1)))

;; C language hooks (this is JUST C) - flycheck using C standard 99
(add-hook 'c-mode-hook
	  (lambda ()
	    (flycheck-mode +1)
	    (setq flycheck-clang-language-standard "gnu99")))

;; c++ language hooks
(add-hook 'c++-mode-hook
	  (lambda ()
	    (flycheck-mode +1)
	    (setq flycheck-clang-language-standard "gnu99")))
;           (setq flycheck-gcc-language-standard "c++11") - this one can get super annoying...

;; jedi does some cool stuff with python, use it by default
;(add-hook 'python-mode-hook
;	  (lambda ()
;	    (jedi:setup)
;	    (setq jedi:complete-on-dot t)))

;; java mode hooks
(add-hook 'java-mode-hook
	  (lambda ()
	    ;(ajc-java-complete-mode +1) ; jedi like auto complete (annoying - causes slow inital load)
	    (setq indent-tabs-mode nil))) ; eclipse just can't deal with emacs tabs...

;; typescript mode hook with flycheck enabled, currently set to check syntax actively
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)))

;; ocaml modes with flycheck on save
(add-hook 'tuareg-mode-hook 
	  (lambda ()
	    (merlin-mode +1)
	    (flycheck-mode +1)))

;; handle windows dos crap in plain text files
(add-hook 'text-mode-hook
	  (lambda ()
	    (remove-dos-eol)))

(add-hook 'logview-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil '(("Ignoring" . font-lock-keyword-face)))))
	      


;;
;; Add various file suffixes to mode lists
;;
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode)) ; gnu makefile mode??
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode)) ; force all .h files to c++ mode
(setq auto-mode-alist (cons '("\\.ts$" . typescript-mode) auto-mode-alist))
;; Before there was add-to-list, real elisp programmers did it this way...
;; These are all standard associations now.
;(assoc "\\.c[px][px]$" auto-mode-alist)
;(setq auto-mode-alist (cons '("\\.c[px][px]$" . c++-mode) auto-mode-alist))
;(assoc "\\.uil$" auto-mode-alist)
;(setq auto-mode-alist (cons '("\\.uil$" . c-mode) auto-mode-alist))


;;
;; Regexp mods for supporting custom build output (error parsing)
;; These allow clicking on the build output in an emacs buffer
;; and automatically opening a buffer at the point of the error.
;; These are my custom regex's. There are probably way better
;; ones on the interwebs...
;;
(require 'compile)

;; add compilation errors for java gradle/maven
(add-to-list 'compilation-error-regexp-alist 'gradle)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(gradle
   "\\:compileJava\\(\\/.+?\\):\\([0-9]+\\)"
   1 2))

;; add compilation errors for typescript via gulp
(add-to-list 'compilation-error-regexp-alist 'gulp)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(gulp
   "\\(^[_[:alnum:]-/]+.ts\\)(\\([0-9]+\\),\\([0-9]+\\))"
   1 2 3))

;;
;; Enable ansi escape sequences for compilation mode buffer.
;; It defaults to dumb terminal, and too many programs
;; simply ignore the TERM setting...
;;
;; Beware, actually running emacs in a dumb terminal and letting
;; this go means you might get wacky characters in said dumb terminal.
;; When is the last time you used a dumb terminal though??
;;
(require 'ansi-color)
(add-hook 'compilation-filter-hook
	  (lambda ()
	    (toggle-read-only)
	    (ansi-color-apply-on-region compilation-filter-start (point))
	    (toggle-read-only)))

;;
;; Seriously? Was this really broken? I'm waving my curmudgeon cane at you emacs!
;; Enables space for completion (AS IT SHOULD BE).... (emacs22 and beyond)
;;
(if (boundp 'minibuffer-local-filename-completion-map)
    (progn
      (define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
      (define-key minibuffer-local-must-match-filename-map " " 'minibuffer-complete-word)))

;;
;; Backup file settings...
;;
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t )

;;
;; Toggle on and off useful (or not so useful) functionality.
;; custom-set-variables has a habit of picking some of these up,
;; and resaving them as custom so be careful.
;;
(which-func-mode)     			; turns on
(setq inhibit-splash-screen t)		; splash screens were like from the 80s right? Totally. -barf-
(setq frame-title-format "%b")		; file name in titlebar
(set-mouse-color "red")			; love my big red cursor....
(set-cursor-color "red")
(set-default-coding-systems 'unix)      ; windows tweak
(desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "/home/dave_lindner/.emacs.d/bookmarks")
 '(custom-enabled-themes (quote (deeper-blue)))
 '(magit-git-executable "/usr/local/bin/git")
 '(package-selected-packages (quote (json-mode dash magit)))
 '(ring-bell-function (quote ignore))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(split-width-threshold 500)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-remote-path
   (quote
    (tramp-default-remote-path "/usr/local/bin" "/usr/local/sbin" "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "outline" :family "Consolas"))))
 '(bmkp-no-local ((t (:foreground "red"))))
 '(ediff-current-diff-A ((t (:background "navy" :foreground "white"))))
 '(ediff-even-diff-A ((t (:background "dark slate gray"))))
 '(font-lock-builtin-face ((t (:foreground "SkyBlue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "forestgreen" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "ForestGreen" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "forest green"))))
 '(font-lock-function-name-face ((t (:foreground "dodger blue" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "SkyBlue"))))
 '(font-lock-string-face ((t (:foreground "light slate grey"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))))
 '(font-lock-variable-name-face ((t (:foreground "plum"))))
 '(logview-name ((t (:inherit font-lock-string-face :foreground "blue"))))
 '(logview-thread ((t (:inherit font-lock-variable-name-face :foreground "gray40"))))
 '(magit-branch-local ((t (:foreground "DodgerBlue4"))))
 '(magit-branch-remote ((t (:foreground "dark green"))))
 '(magit-diff-added ((t (:background "#cceecc" :foreground "black"))))
 '(magit-diff-added-highlight ((t (:background "#cceecc" :foreground "black"))))
 '(magit-diff-removed ((t (:background "#eecccc" :foreground "black"))))
 '(magit-diff-removed-highlight ((t (:background "#eecccc" :foreground "black"))))
 '(magit-section-heading ((t (:foreground "medium blue" :weight bold))))
 '(magit-section-highlight ((t (:background "white"))))
 '(magit-tag ((t (:foreground "dark violet")))))
