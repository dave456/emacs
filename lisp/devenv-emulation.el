
;; This is devenv-emulation, an Emacs Lisp package for GNU Emacs.
;; Copyright (C) 2007 Thomas Becker.

;; devenv-emulation is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;; Everyone is granted permission to copy, modify and redistribute
;; devenv-emulation, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; The HTML documentation of devenv-emulation can be found at
;;
;; http://thbecker.net/free_software_utilities/emacs_lisp/devenv_emulation/devenv_emulation.html
;;
;; Send bug reports, questions, and comments to: DevenvEmulation@thbecker.net

; Minimal settings for devenv-emulation to compile and debug
(defvar devenv-emulation-active-project-name "helloworld" "*Default project name for devenv-emulation")
(defvar devenv-emulation-active-compilation-directory "~/HelloWorldDir" "*Default compilation directory for devenv-emulation")

; Optional settings to specify a compile command and an executable for devenv-emulation
(defvar devenv-emulation-active-compile-command nil "*Default compile commmand for devenv-emulation (optional)")
(defvar devenv-emulation-active-executable nil "*Default executable name for devenv-emulation (optional)")

; Target settings for devenv-emulation
(defvar devenv-emulation-object-file-extension ".o" "*Default object file extension for devenv-emulation")
(defvar devenv-emulation-object-file-prefix "" "*Default prefix for making a target name from an object file name")
(defvar devenv-emulation-clean-all-target "clean" "*Default target to clean the active project for devenv-emulation")
(defvar devenv-emulation-make-all-target "all" "*Default target to make the active project for devenv-emulation")

; Internally used variables
(defvar devenv-emulation-last-user-target nil "Last target interactively selected by the user")
(defvar devenv-emulation-user-target-history-list nil "Variable to hold the user target history list")
(defvar devenv-emulation-user-target-list nil "User-specified list of targets (for completion of targets)")

(defun devenv-emulation-set-key-bindings (keymap)
  "Sets the key bindings for devenv-emulation in the indicated map."
  (interactive)
  (define-key keymap [f4] 'next-error)
  (define-key keymap [S-f4] 'previous-error)

  (define-key keymap [f5] 'devenv-emulation-start-debugging)
  (define-key keymap [S-f5] 'devenv-emulation-stop-debugging)
  (define-key keymap [C-f5] '(lambda () (interactive) (shell-command (devenv-emulation-active-executable-path))))

  (define-key keymap [f7] 'devenv-emulation-make-active-project)
  (define-key keymap [S-f7] 'devenv-emulation-clean-active-project)
  (define-key keymap [C-f7] 'devenv-emulation-compile-buffer-file-to-object-file)

  (define-key keymap [f9] 'gud-break)
  (define-key keymap [S-f9] 'gud-remove)
  (define-key keymap [C-f9] 'gud-tbreak)
  (define-key keymap [f10] 'gud-next)
  (define-key keymap [S-f10] 'gud-cont)
  (define-key keymap [f11] 'gud-step)
  (define-key keymap [S-f11] 'gud-finish)
  (define-key keymap [f12] 'gud-down)
  (define-key keymap [S-f12] 'gud-up)
  (define-key keymap [?\C-c ?c] 'devenv-emulation-switch-to-compilation-buffer)
  (define-key keymap [?\C-c ?d] 'devenv-emulation-switch-to-gud-buffer)
  (define-key keymap [?\C-c ?m] 'devenv-emulation-make-target)
)

(defun devenv-emulation-add-target-names-for-completion (&rest target-names)
  "Adds target names to the list of target names used for completion
when devenv-emulation-make-target prompts for a target name."
  (let ((current-target (car target-names)))
    (while current-target
      (if (not (devenv-emulation-memqual (cons current-target nil) devenv-emulation-user-target-list))
          (setq devenv-emulation-user-target-list
                (cons (cons current-target nil) devenv-emulation-user-target-list)))
      (setq target-names (cdr target-names))
      (setq current-target (car target-names)))))

(defun devenv-emulation-clean-active-project ()
  "Cleans the active project. Prompts for saving modified buffers."
  (interactive)
  (devenv-emulation-my-compile-helper devenv-emulation-clean-all-target))

(defun devenv-emulation-make-active-project ()
  "Makes the active project. Prompts for saving modified buffers."
  (interactive)
  (devenv-emulation-my-compile-helper devenv-emulation-make-all-target))

(defun devenv-emulation-compile-buffer-file-to-object-file ()
  "Compiles the file visited by the current buffer.
Saves the current buffer automatically. Prompts for saving other buffers
if modified."
  (interactive)
  (save-buffer)
  (devenv-emulation-my-compile-helper
   (devenv-emulation-target-name-for-current-buffer)))

(defun devenv-emulation-target-name-for-current-buffer ()
  "Makes the target name for the source file visited in the current buffer."
    (if (fboundp 'devenv-emulation-file-name-2-target)
        (devenv-emulation-file-name-2-target (buffer-file-name))
      (concat devenv-emulation-object-file-prefix
              (file-name-sans-extension (buffer-name))
              devenv-emulation-object-file-extension)))

(defun devenv-emulation-make-target ()
  "Prompts for a target, then makes that target.
 Prompts for saving other buffers if modified."
  (interactive)
  (let (user-target)
    (unwind-protect
        (progn
          (substitute-key-definition 'exit-minibuffer
                                     'devenv-emulation-exit-target-read
                                     minibuffer-local-completion-map)
          (setq user-target (completing-read
                             (format "Make tartget%s: "
                                     (if devenv-emulation-last-user-target
                                         (format " (default %s)" devenv-emulation-last-user-target)
                                       ""))
                             devenv-emulation-user-target-list
                             nil ; predicate to limit table matches
                             nil ; require match
                             nil ; initial input
                             'devenv-emulation-user-target-history-list ; history list to use
                             devenv-emulation-last-user-target ; default value
                             nil ; inherit input method
                             )))
      (substitute-key-definition 'devenv-emulation-exit-target-read
                                 'exit-minibuffer
                                 minibuffer-local-map))
    (setq devenv-emulation-last-user-target user-target)
    (devenv-emulation-my-compile-helper user-target)))
        
(defun devenv-emulation-my-compile-helper (target)
  "Makes the indicated target. Prompts for saving modified buffers."
  (let ((gud-buffer-window (devenv-emulation-gud-buffer-window)))
    (if (and gud-buffer-window (null (devenv-emulation-compilation-buffer-window)))
        (progn (select-window gud-buffer-window) (switch-to-buffer "*compilation*"))))
  (let ((old-default-directory default-directory)
        (compile-command devenv-emulation-active-compile-command))
    (if (not compile-command)
        (let ((make-script-name
               (concat
                (devenv-emulation-directory-name-with-trailing-backslash devenv-emulation-active-compilation-directory)
                "make_"
                devenv-emulation-active-project-name)))
          (if (file-exists-p make-script-name)
              (setq compile-command make-script-name)
            (setq compile-command (concat "make -f " devenv-emulation-active-project-name ".mak")))))
    (setq compile-command (concat compile-command " " target))
    (cd devenv-emulation-active-compilation-directory)
    (let ((compilation-buffer-window (devenv-emulation-compilation-buffer-window))
          (compilation-buffer-window-height))
      (if (devenv-emulation-compilation-buffer-window)
          (setq compilation-buffer-window-height (window-height compilation-buffer-window)))
      (unwind-protect
          (compile compile-command)
        (cd old-default-directory))
      (pop-to-buffer "*compilation*")
      (goto-char (point-max))
      (if (and compilation-buffer-window-height compilation-buffer-window)
          (set-window-text-height compilation-buffer-window (1- compilation-buffer-window-height))))))

(defun devenv-emulation-start-debugging ()
  "Starts a debugging session for the active executable.
If there is already an active debugging session, runs the active exectutable under
the debugger."
  (interactive)
  (condition-case nil
      (progn (gud-call "run") (devenv-emulation-switch-to-gud-buffer-helper nil))
    (error (progn
             (devenv-emulation-switch-to-gud-buffer-helper t)
             (gdb (concat "gdb " (devenv-emulation-active-executable-path)))))))
                             
(defun devenv-emulation-stop-debugging ()
  "Calls \"kill\" on gdb."
  (interactive)
  (devenv-emulation-switch-to-gud-buffer-helper nil)
  (condition-case nil
      (gud-call "kill")
    (error (message "No debugging session found"))))

(defun devenv-emulation-switch-to-gud-buffer ()
  "Switches to the gud buffer, if any."
  (interactive)
  (devenv-emulation-switch-to-gud-buffer-helper nil))

(defun devenv-emulation-switch-to-gud-buffer-helper (force)
  "Switches to the gud buffer, if any. With non-nil argument, forces a gud
buffer into existence via display-buffer."
  (let* ((gud-buffer-name (devenv-emulation-gud-buffer-name))
         (gud-buffer (get-buffer gud-buffer-name)))
    (if (and (not gud-buffer) (not force))
        (error "No debugging session found"))
    (let ((gud-buffer-window (if gud-buffer (get-buffer-window gud-buffer) nil)))
      (if gud-buffer-window
          (select-window gud-buffer-window)
        (let ((compilation-buffer-window (devenv-emulation-compilation-buffer-window)))
          (if compilation-buffer-window
              (progn (select-window compilation-buffer-window) (switch-to-buffer gud-buffer-name))
            (switch-to-buffer-other-window gud-buffer-name)
            (set-window-text-height (get-buffer-window (current-buffer)) (1- compilation-window-height))))))))

(defun devenv-emulation-switch-to-compilation-buffer ()
  "Switches to the compilation buffer, if any."
  (interactive)
  (let* ((compilation-buffer-name "*compilation*")
         (compilation-buffer (get-buffer compilation-buffer-name)))
    (if (not compilation-buffer)
        (error "No compilation buffer found"))
    (let ((compilation-buffer-window (if compilation-buffer (get-buffer-window compilation-buffer) nil)))
      (if compilation-buffer-window
          (select-window compilation-buffer-window)
        (let ((gud-buffer-window (devenv-emulation-gud-buffer-window)))
          (if gud-buffer-window
              (progn (select-window gud-buffer-window) (switch-to-buffer compilation-buffer-name))
            (switch-to-buffer-other-window compilation-buffer-name)
            (set-window-text-height (get-buffer-window (current-buffer)) (1- compilation-window-height))))))))

(defun devenv-emulation-active-executable-path ()
  (if devenv-emulation-active-executable
      devenv-emulation-active-executable
    (concat
       (devenv-emulation-directory-name-with-trailing-backslash devenv-emulation-active-compilation-directory)
       devenv-emulation-active-project-name)))

(defun devenv-emulation-compilation-buffer-window ()
  (let ((compilation-buffer (get-buffer "*compilation*")))
    (if compilation-buffer
        (get-buffer-window compilation-buffer)
      nil)))

(defun devenv-emulation-gud-buffer-name ()
  (concat "*gud-" (file-name-nondirectory (devenv-emulation-active-executable-path)) "*"))

(defun devenv-emulation-gud-buffer ()
  (get-buffer (devenv-emulation-gud-buffer-name)))

(defun devenv-emulation-gud-buffer-window ()
  (let ((gud-buffer (devenv-emulation-gud-buffer)))
    (if gud-buffer
        (get-buffer-window gud-buffer)
      nil)))

(defun devenv-emulation-directory-name-with-trailing-backslash (directory-name)
  (let ((length-of-directory-name (length directory-name)))
    (if (string= "/" (substring
                      directory-name
                      (1- length-of-directory-name)
                      length-of-directory-name))
        directory-name
      (concat directory-name "/"))))

(defun devenv-emulation-exit-target-read ()
  "Substitutes minibuffer-complete-and-exit when reading a target.
Used by devenv-emulation-make-target."
  (interactive)
  (if (or (not (= (minibuffer-prompt-end) (point-max))) devenv-emulation-last-user-target)
      (exit-minibuffer)
    (ding)
    (goto-char (minibuffer-prompt-end))
    (insert "[No default]")
    (sit-for 2)
    (delete-region (minibuffer-prompt-end) (point-max))))

(defun devenv-emulation-memqual (member-arg list-arg)
  "Acts like memq, but tests equality using equal instead of eq"
  (let ((tmplist  list-arg))
    (catch 'found
      (while tmplist
        (and (equal (car tmplist) member-arg)
             (throw 'found tmplist))
        (setq tmplist (cdr tmplist))))))

;; --EOF--