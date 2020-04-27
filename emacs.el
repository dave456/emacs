;;
;; Load my keybinds and other misc binding functions.
;;
(load-file "~/lisp/keybinds.el")

;;
;; Custom stuff I wrote or swiped 
;; load on demand
;;
(autoload 'x-select-file "~/lisp/xfile.elc")
(autoload 'x-mb3-menu "~/lisp/mb3menu.elc")
(autoload 'groovy-mode "~/lisp/groovy-mode.elc")

;;
;; I go back and forth on emacsclient, if its commented out, it probably
;; pissed me off recently. I think I read somewhere I wasn't supposed
;; to do it this way anymore.
;;
;;(server-start)

;;
;; Add the melpa repos (the newest stuff)
;; With both dev and stable it shows dupes in package list, deal with it...
;;
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") 'APPEND)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") 'APPEND)

;; yas is pretty sweet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/src/yasnippet-snippets")
(yas-reload-all)
;; yas is cool, but not quite that cool, pick modes to use it in (see language hooks)
;;(yas-global-mode t) 

;; tell tramp to use ssh pipes instead of scp where possible
(setq tramp-default-method "ssh")

;; to suppress annoying magit messages (why is this needed anymore??)
(setq magit-last-seen-setup-instructions "1.4.0")


;;
;; custom language mode hooks
;;

;;
;; This handles cases where windows and non-windows users have been
;; editing the same file and we have mixed line endings. Add it to
;; language and text hooks where appropriate...
;;
(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; lots of stuff uses c-basic-offset now, so putting it in a language hook is probably a mistake
(setq c-basic-offset 4)

;; hooks for all C syntax based languages (c, c++, java, etc.)
(add-hook 'c-mode-common-hook 
	  (lambda ()
	    (hide-dos-eol)
	    (cscope-minor-mode +1)))

;; C language hooks (this is JUST C) - flycheck using C standard 99
(add-hook 'c-mode-hook
	  (lambda ()
	    (flycheck-mode +1)
	    (yas-minor-mode +1)
	    (setq flycheck-clang-language-standard "gnu99")))

;; c++ language hooks - flycheck using C++11 by default: this can get annoying...
;; currently just checking c++ using C99
(add-hook 'c++-mode-hook
	  (lambda ()
	    (flycheck-mode +1)
	    (yas-minor-mode +1)
;	    (setq flycheck-gcc-language-standard "c++11")))
	    (setq flycheck-clang-language-standard "gnu99")))

;; jedi does some cool stuff with python, use it by default
(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    (setq jedi:complete-on-dot t)))

;; java mode hooks
(add-hook 'java-mode-hook
	  (lambda ()
	    (yas-minor-mode +1)
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

;; handle mixed windows dos eol crap in plain text files, can be added to other
;; modes too if needed...
(add-hook 'text-mode-hook
	  (lambda ()
	    (hide-dos-eol)))

;;
;; Add various file suffixes to mode lists
;;
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode)) ; gnu makefile mode??
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode)) ; force all .h files to c++ mode
(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))
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
;;
;; Not sure why I had to write some of these regex matches for
;; error output. Maybe user error on my part??
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
;; this go means you might get wacky characters in said dumb
;; terminal.
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
      (define-key minibuffer-local-must-match-filename-map " " 'minibuffer-complete-word)
      )
  )

;;
;; Backup file settings...
;;
;; While littering the filesystem with ~ files everywhere is a great way
;; to show the world that emacs is the best editor ever, its a tad passive
;; aggressive...
;;
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 1
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(menu-bar-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(split-width-threshold 500)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray91" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(bmkp-no-local ((t (:foreground "red"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "Forestgreen" :slant italic))))
 '(font-lock-doc-face ((((class color) (min-colors 88) (background light)) (:foreground "Forestgreen" :slant italic))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1" :weight bold))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background light)) (:foreground "dark slate blue"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "gray40"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue"))))
 '(magit-branch-local ((t (:foreground "DodgerBlue4"))))
 '(magit-branch-remote ((t (:foreground "dark green"))))
 '(magit-diff-added ((t (:background "#cceecc" :foreground "black"))))
 '(magit-diff-added-highlight ((t (:background "#cceecc" :foreground "black"))))
 '(magit-diff-removed ((t (:background "#eecccc" :foreground "black"))))
 '(magit-diff-removed-highlight ((t (:background "#eecccc" :foreground "black"))))
 '(magit-section-heading ((t (:foreground "medium blue" :weight bold))))
 '(magit-section-highlight ((t (:background "white"))))
 '(magit-tag ((t (:foreground "dark violet")))))
