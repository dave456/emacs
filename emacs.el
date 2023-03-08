;;
;; Load my keybinds and other misc functions.
;;
;; Truth be told, this makes it look like I really don't have that many
;; customizations in my emacs file...  Its vanilla emacs. Really.
;;
(load-file "~/lisp/keybinds.el")

;;
;; Custom stuff I wrote or swiped 
;; load on demand
;;
(autoload 'x-select-file "~/lisp/xfile.elc")
(autoload 'x-mb3-menu "~/lisp/mb3menu.elc")
(autoload 'groovy-mode "~/lisp/groovy-mode.elc")
(autoload 'ajc-java-complete-mode "~/lisp/ajc-java-complete-config.elc")

;;
;; I go back and forth on emacsclient, if its commented out, it probably
;; pissed me off recently.
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

;; tell tramp to use ssh pipes instead of scp where possible
(setq tramp-default-method "ssh")

;; to suppress annoying magit messages (why is this needed anymore??)
(setq magit-last-seen-setup-instructions "1.4.0")

;; suppress obnoxious popup warnings - they still show up in warnings buffer
(setq warning-minimum-level :emergency)

;; yas is pretty sweet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/src/yasnippet-snippets")
(yas-reload-all)
;; yas is cool, but not quite that cool, pick modes to use in (see language hooks)
;;(yas-global-mode t) 

;;
;; my custom language mode hooks - these might be getting out of hand...
;;

;; lots of stuff uses c-basic-offset now, so putting it in a language hook is probably a mistake
(setq c-basic-offset 4)

;; hooks for all C syntax based languages (c, c++, java, etc.)
(add-hook 'c-mode-common-hook 
	  (lambda ()
	    (remove-dos-eol)
	    (yas-minor-mode +1)
	    (which-function-mode +1)
	    (cscope-minor-mode +1)))

;; C language hooks (this is JUST C) - flycheck using C standard 99
(add-hook 'c-mode-hook
	  (lambda ()
	    (flycheck-mode +1)
	    (setq flycheck-clang-language-standard "gnu99")))

;; c++ language hooks - flycheck using C++11 by default: c++11 can get annoying...
;; currently just checking c++ using C99
(add-hook 'c++-mode-hook
	  (lambda ()
	    (flycheck-mode +1)
;	    (setq flycheck-gcc-language-standard "c++11")))
	    (setq flycheck-clang-language-standard "gnu99")))

;; jedi does some cool stuff with python, use it by default?
;; yas can do battle with jedi, so you probably don't want to use them together
(add-hook 'python-mode-hook
	  (lambda ()
	    ;(jedi:setup)
	    (setq jedi:complete-on-dot t)))

;; java mode hooks
(add-hook 'java-mode-hook
	  (lambda ()
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

;; shell script mode - which function gets really confused in shell scripts
(add-hook 'sh-mode-hook
	  (lambda ()
	    (yas-minor-mode +1)
	    (which-function-mode -1)
	    (remove-dos-eol)))

;; handle mixed windows dos eol crap in plain text files, can be added to other
;; modes too if needed...
;; TODO: rename remove-dos-eol() to something like hide,
;; so people who use my emacs file don't have kittens that the files are being
;; modified.
(add-hook 'text-mode-hook
	  (lambda ()
	    (remove-dos-eol)))

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
;;
;; Why would you not want this??
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
;; this go means you WILL get wacky characters in said dumb
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
      kept-old-versions 2
      version-control t)

;;
;; Toggle on and off useful (or not so useful) functionality.
;; custom-set-variables has a habit of picking some of these up,
;; and resaving them as custom so be careful.
;;
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
 '(custom-enabled-themes (quote (deeper-blue)))
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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(bmkp-no-local ((t (:foreground "red"))))
 '(font-lock-builtin-face ((t (:foreground "SkyBlue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "forestgreen" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "ForestGreen" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "forestgreen"))))
 '(font-lock-function-name-face ((t (:foreground "dodger blue" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "SkyBlue"))))
 '(font-lock-string-face ((t (:foreground "light slate grey"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue"))))
 '(logview-name ((t (:inherit font-lock-string-face :foreground "blue"))))
 '(logview-thread ((t (:inherit font-lock-variable-name-face :foreground "gray40"))))
 '(magit-branch-local ((t (:foreground "dodger blue"))))
 '(magit-branch-remote ((t (:foreground "forestgreen"))))
 '(magit-diff-added ((t (:background "#002b00" :foreground "white"))))
 '(magit-diff-added-highlight ((t (:background "#002b00" :foreground "white"))))
 '(magit-diff-removed ((t (:background "#2b0000" :foreground "white"))))
 '(magit-diff-removed-highlight ((t (:background "#2b0000" :foreground "white"))))
 '(magit-section-heading ((t (:foreground "RoyalBlue1" :weight bold))))
 '(magit-section-highlight ((t (:background "gray14"))))
 '(magit-tag ((t (:foreground "MediumOrchid1")))))
