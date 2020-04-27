(defvar cscope-mode-hook nil)

(defconst cscope-font-lock-keywords-1
  (list
   '("\\<\\(A\\(CTIVITY\\|PPLICATION\\)\\|DATA\\|END_\\(A\\(CTIVITY\\|PPLICATION\\)\\|DATA\\|MODEL\\|PARTICIPANT\\|T\\(OOL_LIST\\|RANSITION\\)\\|WORKFLOW\\)\\|MODEL\\|PARTICIPANT\\|T\\(OOL_LIST\\|RANSITION\\)\\|WORKFLOW\\)\\>" . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for WPDL mode")
(defvar cscope-font-lock-keywords wpdl-font-lock-keywords-1)

(defun cscope-mode ()
  "Major mode for cscope output buffers"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(cscope-font-lock-keywords))(setq major-mode 'wpdl-mode)
  (setq mode-name "cscope")
  (run-hooks 'cscope-mode-hook))

(provide `cscope-mode)

