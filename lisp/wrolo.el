;;!emacs
;;
;; FILE:         wrolo.el V2 (Renamed from rolo.el in earlier versions to avoid
;;                            load path conflicts with the rolo.el written by
;;                            another author.)
;; SUMMARY:      Hierarchical, multi-file, easy to use rolodex system
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:     7-Jun-89 at 22:08:29
;; LAST-MOD:      6-Jan-92 at 16:37:13 by Bob Weiner
;;
;; Copyright (C) 1989, 1991, 1992 Bob Weiner and Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not yet part of GNU Emacs.
;; This file is part of Hyperbole but may be used separately.
;;
;; DESCRIPTION:  
;;
;;  The 'put whatever you feel like into it' rolodex.
;;
;;  FEATURES:
;;
;;   1.  Multiple rolodex files with free text lookup.  No structured
;;       fields are used.
;;
;;   2.  Hierarchical rolodex entries as in:
;;        *    Company
;;        **     Manager
;;        ***      Underlings
;;
;;       Searching for Manager turns up all Underlings.  Searching for
;;       Company retrieves all listed employees.
;;
;;       This hierarchical system has proved very effective for retrieving
;;       computer system administration problem reports by vendor name,
;;       problem number or by subject area without having to resort to a
;;       database system and also for extraction of relevant text sections from
;;       reports.
;;
;;   3.  String and regular expression searching capabilities.  Matches are
;;       found anywhere within entries, so entries may be of any format you
;;       like without the bother of fixed field restrictions.
;;       Ability to restrict number of matches or to report number of matches
;;       without displaying entries.
;;
;;   4.  Smart addition, editing and sorting of entries by hierarchy level.
;;
;;   5.  Support for Hyperbole buttons within rolodex entries.
;;       (See Hyperbole "README" and distribution available via
;;        anonymous ftp from wilma.cs.brown.edu (IP# = 128.148.31.66)
;;        in the "pub/hyperbole" directory.)
;;
;;   See "wrolo-logic.el" for logical search functions (and, or, not, xor).
;;   See "wrolo-menu.el" for menu handling functions.
;;
;;
;;  SETUP:
;;
;;   The variable 'rolo-file-list' is a list of files to search for
;;   matching rolodex entries.  To add personal files to rolo-file-list,
;;   when you find these functions are useful for any sort of list lookup,
;;   add the following to your ~/.emacs file (substituting where you see
;;   <fileN>):
;;
;;      (setq rolo-file-list (append rolo-file-list '("<file1>" "<file2>")))
;;
;;   We recommend that entries in 'rolo-file-list' have ".otl" suffixes
;;   so that they do not conflict with file names that other rolodex
;;   programs might use and so that they are edited in 'outline-mode' by
;;   default.  If you want the latter behavior, uncomment and add something
;;   like the following to one of your GNU Emacs initialization files:
;;
;;     ;; Add to the list of suffixes that causes automatic mode invocation
;;     (setq auto-mode-alist
;;        (append '(("\\.otl$" . outline-mode)) auto-mode-alist))
;;
;;   The buffers containing the rolodex files are not killed after a search
;;   on the assumption that another search is likely to follow within this
;;   Emacs session.  You may wish to change this behavior with the following
;;   setting:
;;
;;     (setq rolo-kill-buffers-after-use t)
;;
;;   After an entry is killed, the modified rolodex file is automatically
;;   saved.  If you would rather always save files yourself, use this
;;   setting:
;;
;;     (setq rolo-save-buffers-after-use nil)
;;
;;   When adding an entry from within a buffer containing a mail
;;   message, the rolodex add function will extract the sender's name
;;   and e-mail address and prompt you with the name as a default.  If
;;   you accept it, it will enter the name and the email address using
;;   the format given by the 'rolo-email-format' variable.  See its
;;   documentation if you want to change its value.
;;
;;
;;   If you use Hyperbole V2.3 or greater (unreleased as of this writing),
;;   then no other rolodex setup is necessary, simple select the "Rolo/"
;;   menu item from the top level Hyperbole menu.
;;
;;   Otherwise, add the following to your "~/.emacs" file:
;;
;;     (autoload 'rolo-menu "rolo-menu" "Load wrolo system." t)
;;     (global-set-key "\C-x4r" 'rolo-menu)
;;
;;   And then simply invoke the rolodex menu with {C-x 4 r} after Emacs
;;   has read those lines in your init file.
;;
;;
;;  SUMMARY OF USE:
;;
;;   The rolo menu provides access to the following commands:
;;
;;     Menu Item       Function              Description
;;     ====================================================================
;;     Add             rolo-add              Adds a rolodex entry
;;     Display-again   rolo-display-matches  Displays last matches again
;;     Edit            rolo-edit             Edits an existing rolodex entry
;;     Kill            rolo-kill             Removes an entry from the rolodex
;;     Order           rolo-sort             Sorts all levels in rolodex
;;     Regexp-find     rolo-grep             Finds all entries containing
;;                                             a regular expression
;;     String-find     rolo-fgrep            Finds all entries containing
;;                                             a string
;;     Yank            rolo-yank             Inserts first matching rolodex
;;                                             entry at point
;;
;;   For any of these commands that prompt you for a name, you may use the form
;;   parent/child to locate a child entry below a parent entry, e.g.
;;   from the example near the top, we could give Company/Manager/Underlings.
;;
;;   Here is a snippet from our group rolodex file.  The ';'s should be
;;   removed of course and the '*'s should begin at the start of the
;;   line.  If a rolodex file begins with two separator lines whose
;;   first three characters are "===", then these lines and any text
;;   between them are prepended to the output buffer whenever any
;;   entries are retrieved from that file.
;;
;;=============================================================================
;;			      GROUP ROLODEX
;; <Last Name>, <First Name>  <Co/Categ>   W<Work #>   H<Home #>  P<Pager #>
;;				           F<Fax #>    M<Modem #> C<Cellular #>
;;					   R<Other-radio #>
;;        <Address>	   <Miscellaneous Info, Key Words>
;;=============================================================================
;;*   EX594, Digital-Systems-Research
;;**  Weiner, Bob	      Motorola     W2087                  P7-7489
;;	  FL19, L-1035
;;
;;
;;  FOR PROGRAMMERS:
;;
;;   Entries in rolodex files are separated by patterns matching
;;   'rolo-entry-regexp'.  Each entry may have any number of sub-entries
;;   which represent the next level down in the entry hierarchy.
;;   Sub-entries' separator patterns are always longer than their parents'.
;;   For example, if an entry began with '*' then its sub-entries would begin
;;   with '**' and so on.  Blank lines in rolodex files will not end up where
;;   you want them if you use the rolo-sort commands; therefore, blank lines
;;   are not recommended.  If you change the value of
;;   'rolo-entry-regexp', you will have to modify 'rolo-sort'.
;;
;;   The following additional functions are provided:
;;
;;     'rolo-sort-level' sorts a specific level of entries in a rolodex file;
;;     'rolo-map-level' runs a user specified function on a specific level of
;;       entries in a rolodex file;
;;     'rolo-fgrep-file', same as 'rolo-fgrep' but operates on a single file;
;;     'rolo-grep-file', same as 'rolo-grep' but operates on a single file;
;;     'rolo-display-matches', display last set of rolodex matches, if any;
;;     'rolo-toggle-narrow-to-entry' toggles between display of current entry
;;       and display of all matching entries.
;;
;;
;;  MOD HISTORY:
;;
;;   12/17/89
;;     Added internal 'rolo-shrink-window' function for use in
;;     compressing/uncompressing the rolo view window to/from a size just
;;     large enough for the selected entry.  This is useful when a search
;;     turns up more entries than desired.
;;
;;   02/21/90
;;     Modified 'rolo-grep-file' and 'rolo-map-level' so they only set buffers
;;     read-only the first time they are read in.  This way, if someone edits a
;;     rolodex file and then does a rolo-fgrep or other function, the buffer
;;     will not be back in read-only mode.
;;
;;   04/18/91
;;     Modified 'rolo-grep-file' to expand any hidden entries in rolo file
;;     before doing a search.
;;
;;   12/24/91
;;     Added Hyperbole button support.
;;
;;   12/30/91
;;     Added convenient support for entry add, edit, kill and yank.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar rolo-email-format "%s\t\t<%s>"
  "Format string to use when adding an entry with e-mail addr from a mail msg.
It must contain a %s indicating where to put the entry name and a second
%s indicating where to put the e-mail address.")

(defvar rolo-file-list '("~/.rolodex.otl")
  "*List of files containing rolodex entries.
The first file should be a user-specific rolodex file, typically in the home
directory.  The second file is often a shared, group-specific rolodex file.

A rolo-file consists of:
   (1) an optional header beginning with and ending with a line which matches
       rolo-hdr-regexp;
   (2) one or more rolodex entries beginning with rolo-entry-regexp, which
       may be nested.")

(defvar rolo-kill-buffers-after-use nil
  "*Non-nil means kill rolodex file buffers after searching them for entries.
Only unmodified buffers are killed.")

(defvar rolo-save-buffers-after-use t
  "*Non-nil means save rolodex file after an entry is killed.")

;;; ************************************************************************
;;; Commands
;;; ************************************************************************

(defun rolo-add (name &optional file)
  "Adds a new entry in personal rolodex for NAME.
Last name first is best, e.g. \"Smith, John\".
With prefix argument, prompts for optional FILE to add entry within.
NAME may be of the form: parent/child to insert child below a parent
entry which begins with the parent string."
  (interactive
   (progn
     (or (fboundp 'mail-fetch-field) (require 'mail-utils))
     (let* ((lst (rolo-name-and-email))
	    (name (car lst))
	    (email (car (cdr lst)))
	    (entry (read-string "Name to add to rolo: "
				(or name email))))
       (list (if (and email name
		      (string-match (concat "\\`" (regexp-quote entry)) name))
		 (format rolo-email-format entry email) entry)
	     current-prefix-arg))))
  (if (or (not (stringp name)) (string= name ""))
      (error "(rolo-add): Invalid name: '%s'" name))
  (if (and (interactive-p) file)
      (setq file (completing-read "File to add to: "
				  (mapcar 'list rolo-file-list))))
  (if (null file) (setq file (car rolo-file-list)))
  (cond ((and file (or (not (stringp file)) (string= file "")))
	 (error "(rolo-add): Invalid file: '%s'" file))
	((and (file-exists-p file) (not (file-readable-p file)))
	 (error "(rolo-add): File not readable: '%s'" file))
	((not (file-writable-p file))
	 (error "(rolo-add): File not writable: '%s'" file)))
  (set-buffer (or (get-file-buffer file) (find-file-noselect file)))
  (if (interactive-p) (message "Locating insertion point for '%s'..." name))
  (let ((parent "") (level "") end)
    (widen) (goto-char 1)
    (while (setq end (string-match "/" name))
      (setq parent (substring name 0 end)
	    name (substring name (min (1+ end) (length name))))
      (if (re-search-forward
	   (concat "\\(" rolo-entry-regexp "\\)[ \t]*" 
		   (regexp-quote parent)) nil t)
	  (setq level (buffer-substring (match-beginning 1)
					(match-end 1)))
	(error "(rolo-add): '%s' part of name not found in \"%s\"."
	       parent file)))
    (narrow-to-region (point)
		      (progn (rolo-to-entry-end t level) (point)))
    (goto-char (point-min))
    (let* ((len (length name))
	   (name-level (concat level "*"))
	   (level-len (length name-level))
	   (entry "")
	   (entry-spc "")
	   (entry-level)
	   (match)
	   (again t))
      (while (and again
		  (re-search-forward
		   (concat "\\(" rolo-entry-regexp "\\)\\([ \t]*\\)")
		   nil 'end))
	(setq entry-level (buffer-substring (match-beginning 1)
					    (match-end 1)))
	(if (/= (length entry-level) level-len)
	    (rolo-to-entry-end t entry-level)
	  (setq entry (buffer-substring (point) (+ (point) len))
		entry-spc (buffer-substring (match-beginning 2)
					    (match-end 2)))
	  (cond ((string< entry name)
		 (rolo-to-entry-end t entry-level))
		((string< name entry)
		 (setq again nil) (beginning-of-line))
		(t ;; found existing entry matching name
		 (setq again nil match t)))))
      (setq buffer-read-only nil)
      (if match
	  nil
	(insert (or entry-level (concat level "*"))
		(if (string= entry-spc "") "   " entry-spc)
		name "\n")
	(backward-char 1))
      (widen)
      (pop-to-buffer (current-buffer))
      ;; Fixes non-display update bug when buf is on screen before
      ;; interactive command invocation. 
      (goto-char (point))
      (if (interactive-p)
	  (message "Edit entry at point.")
	))))

(defun rolo-display-matches ()
  "Re-display buffer of previously found rolodex matches."
  (interactive)
  (let ((obuf (if (boundp 'obuf) obuf (current-buffer)))
	(display-buf (if (boundp 'display-buf)
			 display-buf (get-buffer rolo-display-buffer))))
    (pop-to-buffer display-buf)
    (setq buffer-read-only nil)
    (if (fboundp 'ep:but-create) (ep:but-create))
    (rolo-shrink-window)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (let ((buf (get-buffer-window obuf)))
	(if buf (select-window buf) (switch-to-buffer buf)))))

(defun rolo-edit (&optional name file)
  "Edits a rolodex entry given by optional NAME within 'rolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
With no NAME arg, simply displays FILE or first entry in 'rolo-file-list' in an
editable mode.  NAME may be of the form: parent/child to edit child below a
parent entry which begins with the parent string."
  (interactive "sName to edit in rolo: \nP")
  (if (or (and name (not (stringp name))) (string= name ""))
      (error "(rolo-edit): Invalid name: '%s'" name))
  (if (and (interactive-p) current-prefix-arg)
      (setq file (completing-read "Entry's File: "
				  (mapcar 'list rolo-file-list))))
  (let ((file-list (if file (list file) rolo-file-list)))
    (or file (setq file (car file-list)))
    (if (null name)
	(progn (if (not (file-writable-p file))
		  (error "(rolo-edit): File not writable: '%s'" file))
	       (find-file-other-window file) (setq buffer-read-only nil))
      (if (rolo-to name file-list)
	  (progn
	    (setq file buffer-file-name)
	    (if (file-writable-p file)
		(setq buffer-read-only nil)
	      (message
	       "(rolo-edit): Entry found but file not writable: '%s'" file)
	      (beep))
	    (pop-to-buffer (current-buffer)))
	(message "(rolo-edit): '%s' not found." name)
	(beep)
	(pop-to-buffer (or (get-file-buffer (car file-list))
			   (find-file-noselect (car file-list))))
	(setq buffer-read-only nil))
      (widen)
      ))
  ;; Fixes non-display update bug when buf is on screen before
  ;; interactive command invocation.
  (goto-char (point))
  )

(defun rolo-fgrep (string &optional max-matches rolo-file count-only)
  "Display rolodex entries matching STRING.
To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
ROLO-FILE or rolo-file-list.  Default is to find all matching entries.  Each
entry is displayed with all of its sub-entries.  Optional COUNT-ONLY non-nil
means don't display matching entries.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
rolo-file-list."
  (interactive "sRolodex string to match: \nP")
  (let ((total-matches
	  (rolo-grep (regexp-quote string) max-matches rolo-file count-only)))
    (if (interactive-p)
	(message (concat (if (= total-matches 0) "No" total-matches)
			 " matching entr"
			 (if (= total-matches 1) "y" "ies")
			 " found in rolodex.")))
    total-matches))

(defun rolo-grep (regexp &optional max-matches rolo-bufs count-only)
  "Display rolodex entries matching REGEXP.
To a maximum of prefix arg MAX-MATCHES, in buffer(s) from optional ROLO-BUFS or
rolo-file-list.  Default is to find all matching entries.  Each entry is
displayed with all of its sub-entries.  Optional COUNT-ONLY non-nil means don't
display matching entries.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
rolo-file-list."
  (interactive "sRolodex regular expression to match: \nP")
  (let ((rolo-file-list
	 (cond ((null rolo-bufs) rolo-file-list)
	       ((listp rolo-bufs) rolo-bufs)
	       ((list rolo-bufs))))
	(obuf (current-buffer))
	(display-buf (if count-only
			 nil
		       (set-buffer (get-buffer-create rolo-display-buffer))))
	(total-matches 0)
	(inserting (or (eq max-matches t)
		       (and (integerp max-matches) (< max-matches 0))))
	)
    (if count-only nil
      (setq buffer-read-only nil) 
      (or inserting (erase-buffer)))
    (mapcar '(lambda (file)
	       (let ((num-matched
		      (rolo-grep-file file regexp max-matches count-only)))
		 (setq total-matches (+ total-matches num-matched))
		 (if (integerp max-matches)
		     (setq max-matches
			   (if (>= max-matches 0)
			       (- max-matches num-matched)
			     (+ max-matches num-matched))))))
	    rolo-file-list)
    (if (or count-only inserting (= total-matches 0))
	nil
      (rolo-display-matches))
    (if (interactive-p)
	(message (concat (if (= total-matches 0) "No" total-matches)
			 " matching entr"
			 (if (= total-matches 1) "y" "ies")
			 " found in rolodex.")))
    total-matches))

(defun rolo-kill (name &optional file)
  "Kills a rolodex entry given by NAME within 'rolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
NAME may be of the form: parent/child to kill child below a parent entry
which begins with the parent string.
Returns t if entry is killed, nil otherwise."
  (interactive "sName to kill in rolo: \nP")
  (if (or (not (stringp name)) (string= name ""))
      (error "(rolo-kill): Invalid name: '%s'" name))
  (if (and (interactive-p) current-prefix-arg)
      (setq file (completing-read "Entry's File: "
				  (mapcar 'list rolo-file-list))))
  (let ((file-list (if file (list file) rolo-file-list))
	(killed))
    (or file (setq file (car file-list)))
    (if (rolo-to name file-list)
	(progn
	  (setq file buffer-file-name)
	  (if (file-writable-p file)
	      (let ((kill-op '(progn
				(kill-region start (rolo-to-entry-end t level))
				(setq killed t)
				(rolo-save-buffer)
				(rolo-kill-buffer)))
		    start end level)
		(setq buffer-read-only nil)
		(re-search-backward rolo-entry-regexp nil t)
		(setq end (match-end 0))
		(beginning-of-line)
		(setq start (point)
		      level (buffer-substring start end))
		(goto-char end)
		(skip-chars-forward " \t")
		(if (interactive-p)
		    (let ((entry-line (buffer-substring
				       (point)
				       (min (+ (point) 60)
					    (progn (end-of-line) (point))))))
		      (if (y-or-n-p (format "Kill `%s...' " entry-line))
			  (progn
			    (eval kill-op)
			    (message "Killed"))
			(message "Aborted")))
		  (eval kill-op)))
	    (message
	     "(rolo-kill): Entry found but file not writable: '%s'" file)
	    (beep)))
      (message "(rolo-kill): '%s' not found." name)
      (beep))
    killed))

(defun rolo-sort (&optional rolo-file)
  "Sorts up to 14 levels of entries in ROLO-FILE (default is personal rolo).
Assumes entries are delimited by one or more '*'characters.
Returns list of number of groupings at each entry level." 
  (interactive
   (list (let ((default "")
	       (file))
	 (setq file
	       (completing-read
		(format "Sort rolo file (default %s): "
			(file-name-nondirectory
			 (setq default
			       (if (and buffer-file-name
					(memq
					 t (mapcar
					    '(lambda (file)
					       (equal buffer-file-name
						      (expand-file-name file)))
					    rolo-file-list)))
				   buffer-file-name
				 (car rolo-file-list)))))
		(mapcar 'list rolo-file-list)))
	 (if (string= file "") default file))))
  (if (or (not rolo-file) (equal rolo-file ""))
      (setq rolo-file (car rolo-file-list)))
  (if (not (and (stringp rolo-file) (file-readable-p rolo-file)))
      (error "(rolo-sort): Invalid or unreadable file: %s" rolo-file))
  (let ((level-regexp (regexp-quote "**************"))
	(entries-per-level-list)
	(n))
    (while (not (equal level-regexp ""))
      (setq n (rolo-sort-level rolo-file level-regexp))
      (if (or (/= n 0) entries-per-level-list)
	  (setq entries-per-level-list
		(append (list n) entries-per-level-list)))
      (setq level-regexp (substring level-regexp 0 (- (length level-regexp) 2))))
    entries-per-level-list))

(defun rolo-sort-level (rolo-file level-regexp &optional max-groupings)
  "Sorts groupings of entries in ROLO-FILE at hierarchy level LEVEL-REGEXP.
To a maximum of optional MAX-GROUPINGS.  Nil value of MAX-GROUPINGS means all
groupings at the given level.  LEVEL-REGEXP should simply match the text of
any rolodex entry of the given level, not the beginning of a line (^); an
example, might be (regexp-quote \"**\") to match level two.  Returns number
of groupings sorted."
  (interactive "sRolodex file to sort: \nRegexp for level's entries: \nP")
  (rolo-map-level
    '(lambda (start end) (sort-lines nil start end))
    rolo-file
    level-regexp
    max-groupings))

(defun rolo-toggle-narrow-to-entry ()
  "Toggle between display of current entry and display of all matched entries.
Useful when bound to a mouse key."
  (interactive)
  (if (rolo-narrowed-p)
      (widen)
    (if (or (looking-at rolo-entry-regexp)
	    (re-search-backward rolo-entry-regexp nil t))
	(progn (forward-char)
	       (narrow-to-region (1- (point)) (rolo-display-to-entry-end)))))
  (rolo-shrink-window)
  (goto-char (point-min)))

(defun rolo-yank (name &optional regexp-p)
  "Inserts at point the first rolodex entry matching NAME.
With optional prefix arg, REGEXP-P, treats NAME as a regular expression instead
of a string."
  (interactive "sName to insert record for: \nP")
  (let ((rolo-display-buffer (current-buffer)))
    (if regexp-p
	(rolo-grep name -1)
      (rolo-grep (regexp-quote name) -1))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun rolo-fgrep-file (rolo-buf string &optional max-matches count-only)
  "Retrieve entries in ROLO-BUF matching STRING to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Returns number of matching entries found."
  (rolo-grep-file rolo-buf (regexp-quote string) max-matches count-only))

(defun rolo-grep-file (rolo-buf regexp &optional max-matches count-only)
  "Retrieve entries in ROLO-BUF matching REGEXP to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Returns number of matching entries found."
  (let ((new-buf-p) (actual-buf))
    (if (and (or (null max-matches) (eq max-matches t) (integerp max-matches))
	     (or (setq actual-buf (rolo-buffer-exists-p rolo-buf))
		 (if (file-exists-p rolo-buf)
		     (setq actual-buf (find-file-noselect rolo-buf t)
			   new-buf-p t))))
	(let ((hdr-pos) (num-found 0) (curr-entry-level)
	      (incl-hdr t))
	  (if max-matches
	      (cond ((eq max-matches t)
		     (setq incl-hdr nil max-matches nil))
		    ((< max-matches 0)
		     (setq incl-hdr nil
			   max-matches (- max-matches)))))
	  (set-buffer actual-buf)
	  (if new-buf-p (setq buffer-read-only t))
	  (save-restriction
	    (widen)
	    (goto-char 1)
	    ;; Ensure no entries in outline mode are hidden.
	    ;; Uses 'show-all' function from outline.el.
	    (and (search-forward "\C-m" nil t)
		 (show-all))
	    (if (re-search-forward rolo-hdr-regexp nil t 2)
		(progn (forward-line)
		       (setq hdr-pos (cons (point-min) (point)))))
	    (re-search-forward rolo-entry-regexp nil t)
	    (while (and (or (null max-matches) (< num-found max-matches))
			(re-search-forward regexp nil t))
	      (re-search-backward rolo-entry-regexp nil t)
	      (let ((start (point))
		    (next-entry-exists))
		(re-search-forward rolo-entry-regexp nil t)
		(rolo-to-entry-end
		 t (setq curr-entry-level (buffer-substring start (point))))
		(or count-only
		    (if (and (= num-found 0) incl-hdr)
			(let* ((src (or (buffer-file-name actual-buf)
					actual-buf))
			       (src-line
				(format
				 (concat (if (boundp 'hbut:source-prefix)
					     hbut:source-prefix
					   "@loc> ")
					 "%s")
				 (prin1-to-string src))))
			  (set-buffer rolo-display-buffer)
			  (goto-char (point-max))
			  (if hdr-pos
			      (progn
				(insert-buffer-substring
				 actual-buf (car hdr-pos) (cdr hdr-pos))
				(insert src-line "\n\n"))
			    (insert (format rolo-hdr-format src-line)))
			  (set-buffer actual-buf))))
		(setq num-found (1+ num-found))
		(or count-only
		    (append-to-buffer rolo-display-buffer start (point))))))
	  (rolo-kill-buffer actual-buf)
	  num-found)
      0)))

(defun rolo-map-level (func rolo-buf level-regexp &optional max-groupings)
  "Perform FUNC on each grouping of ROLO-BUF entries at hierarchy level LEVEL-REGEXP
to a maximum of optional argument MAX-GROUPINGS.  Nil value of MAX-GROUPINGS
means all groupings at the given level.  FUNC should take two arguments, the
start and the end of the region that it should manipulate.  LEVEL-REGEXP
should simply match the text of any rolodex entry of the given level, not the
beginning of a line (^); an example, might be (regexp-quote \"**\") to match
level two.  Returns number of groupings matched."
  (let ((new-buf-p) (actual-buf))
    (if (and (or (null max-groupings) (< 0 max-groupings))
	     (or (setq actual-buf (rolo-buffer-exists-p rolo-buf))
		 (if (file-exists-p rolo-buf)
		     (setq actual-buf (find-file-noselect rolo-buf t)
			   new-buf-p t))))
	(let ((num-found 0)
	      (exact-level-regexp (concat "^\\(" level-regexp "\\)[ \t\n]"))
	      (outline-regexp rolo-entry-regexp)
	      (level-len))
	  ;; Load 'outline' library since its functions are used here.
	  (if (not (boundp 'outline-mode-map))
	      (load-library "outline"))
	  (set-buffer actual-buf)
	  (if new-buf-p (setq buffer-read-only t))
	  (goto-char (point-min))
	  ;; Pass buffer header if it exists
	  (if (re-search-forward rolo-hdr-regexp nil t 2)
	      (forward-line))
	  (while (and (or (null max-groupings) (< num-found max-groupings))
		      (re-search-forward exact-level-regexp nil t))
	    (setq num-found (1+ num-found))
	    (let* ((opoint (prog1 (point) (beginning-of-line)))
		   (grouping-start (point))
		   (start grouping-start)
		   (level-len (or level-len (- (1- opoint) start)))
		   (next-level-len)
		   (next-entry-exists)
		   (grouping-end)
		   (no-subtree))
	      (while (and (progn
			    (if (setq next-entry-exists
				      (re-search-forward rolo-entry-regexp nil t 2))
				(setq next-level-len (- (point)
							(progn (beginning-of-line)
							       (point)))
				      grouping-end (< next-level-len level-len)
				      no-subtree (<= next-level-len level-len))
			      (setq grouping-end t no-subtree t)
			      (goto-char (point-max)))
			    (let ((end (point)))
			      (goto-char start)
			      (hide-subtree) ; And hide multiple lines of entry
			      ;; Move to start of next entry at equal or higher level
			      (setq start
				    (if no-subtree
					end
				      (if (re-search-forward rolo-entry-regexp
							     nil t)
					  (progn (beginning-of-line) (point))
					(point-max))))
			      ;; Remember last expression in 'progn' must always
			      ;; return non-nil
			      (goto-char start)))
			  (not grouping-end)))
	      (let ((end (point)))
		(goto-char grouping-start)
		(funcall func grouping-start end)
		(goto-char end))))
	  (show-all)
	  (rolo-kill-buffer actual-buf)
	  num-found)
      0)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun rolo-buffer-exists-p (rolo-buf)
  "Returns buffer given by ROLO-BUF or nil.
ROLO-BUF may be a file-name, buffer-name, or buffer."
  (car (memq (get-buffer (or (and (stringp rolo-buf)
				  (get-file-buffer rolo-buf))
			     rolo-buf))
	     (buffer-list))))

(defun rolo-display-to-entry-end ()
  "Go to end of current entry, ignoring sub-entries."
  (if (re-search-forward (concat rolo-hdr-regexp "\\|"
				 rolo-entry-regexp) nil t)
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

	  
(defun rolo-format-name (name-str first last)
  "Reverse order of NAME-STR field given my regexp match field FIRST and LAST."
  (if (match-beginning last)
      (concat (substring name-str (match-beginning last) (match-end last))
	      ", "
	      (substring name-str (match-beginning first) (match-end first)))))

(defun rolo-kill-buffer (&optional rolo-buf)
  "Kills optional ROLO-BUF if unchanged and 'rolo-kill-buffers-after-use' is t.
Default is current buffer."
  (or rolo-buf (setq rolo-buf (current-buffer)))
  (and rolo-kill-buffers-after-use (not (buffer-modified-p rolo-buf))
       (kill-buffer rolo-buf)))

(defun rolo-name-and-email ()
  "If point is in a mail message, returns list of (name email-addr) of sender.
Name is returned as 'last, first-and-middle'."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
	  (narrow-to-region (point-min) (point)))
      (let ((email) (name) (from))
	(setq email (mail-fetch-field "reply-to")
	      from  (mail-fetch-field "from"))
	(if from
	    (cond
	     ;; Match: email, email (name), email "name"
	     ((string-match
	       (concat "^\\([^\"<>() \t\n]+\\)"
		       "\\([ \t]*[(\"][ \t]*\\([^\"()]+\\)[ \t]+"
		       "\\([^\" \t()]+\\)[ \t]*[)\"]\\)?[ \t]*$")
	       from)
	      (setq name (rolo-format-name from 3 4))
	      (or email (setq email (substring from (match-beginning 1)
					       (match-end 1)))))
	     ;; Match: <email>, name <email>, "name" <email>
	     ((string-match
	       (concat "^\\(\"?\\([^\"<>()\n]+\\)[ \t]+"
		       "\\([^\" \t()<>]+\\)\"?[ \t]+\\)?"
		       "<\\([^\"<>() \t\n]+\\)>[ \t]*$")
	       from)
	      (setq name (rolo-format-name from 2 3))
	      (or email (setq email (substring from (match-beginning 4)
					       (match-end 4)))))))
	(if (or name email)
	    (list name email))))))

(defun rolo-name-at ()
  "If point is at start of an entry in 'rolo-display-buffer', returns entry.
Otherwise, returns nil."
  (if (string= (buffer-name) rolo-display-buffer)
      (save-excursion
	(if (or (looking-at rolo-entry-regexp)
		(progn (end-of-line)
		       (re-search-backward rolo-entry-regexp nil t)))
	    (progn (goto-char (match-end 0))
		   (skip-chars-forward " \t")
		   (if (or (looking-at "[^ \t\n\^M]+ ?, ?[^ \t\n\^M]+")
			   (looking-at "\\( ?[^ \t\n\^M]+\\)+"))
		       (buffer-substring (match-beginning 0)
					 (match-end 0))))))))

(defun rolo-narrowed-p ()
  (or (/= (point-min) 1) (/= (1+ (buffer-size)) (point-max))))

(defun rolo-save-buffer (&optional rolo-buf)
  "Saves optional ROLO-BUF if changed and 'rolo-save-buffers-after-use' is t.
Default is current buffer.  Used, for example, after a rolo entry is killed."
  (or rolo-buf (setq rolo-buf (current-buffer)))
  (and rolo-save-buffers-after-use (buffer-modified-p rolo-buf)
       (set-buffer rolo-buf) (save-buffer)))

(defun rolo-shrink-window ()
  (let* ((lines (count-lines (point-min) (point-max)))
	 (height (window-height))
	 (window-min-height 2)
	 (desired-shrinkage (1- (min (- height lines)))))
    (and (>= lines 0)
	 (/= desired-shrinkage 0)
	 (> (frame-height) (1+ height))
	 (shrink-window 
	   (if (< desired-shrinkage 0)
	       (max desired-shrinkage (- height (/ (frame-height) 2)))
  (min desired-shrinkage (- height window-min-height)))))))

(defun rolo-to (name &optional file-list)
  "Moves point to entry for NAME within optional FILE-LIST.
'rolo-file-list' is used as default when FILE-LIST is nil.
Leaves point immediately after match for NAME within entry.
Switches internal current buffer but does not alter the screen.
Returns non-nil iff matching entry is found."
  (or file-list (setq file-list rolo-file-list))
  (let ((found) file)
    (while (and (not found) file-list)
      (setq file (car file-list)
	    file-list (cdr file-list))
      (cond ((and file (or (not (stringp file)) (string= file "")))
	     (error "(rolo-to): Invalid file: '%s'" file))
	    ((and (file-exists-p file) (not (file-readable-p file)))
	     (error "(rolo-to): File not readable: '%s'" file)))
      (set-buffer (or (get-file-buffer file) (find-file-noselect file)))
      (let ((case-fold-search t) (real-name name) (parent "") (level) end)
	(widen) (goto-char 1)
	(while (setq end (string-match "/" name))
	  (setq level nil
		parent (substring name 0 end)
		name (substring name (min (1+ end) (length name))))
	  (cond ((progn
		   (while (and (not level) (search-forward parent nil t))
		     (save-excursion
		       (beginning-of-line)
		       (if (looking-at
			    (concat "\\(" rolo-entry-regexp "\\)[ \t]*" 
				    (regexp-quote parent)))
			   (setq level (buffer-substring (match-beginning 1)
							 (match-end 1))))))
		   level))
		((equal name real-name)) ;; Try next file.
		(t ;; Found parent but not child
		 (setq buffer-read-only nil)
		 (pop-to-buffer (current-buffer))
		 (error "(rolo-to): '%s' part of name not found in \"%s\"."
			parent file)))
	  (if level
	      (narrow-to-region (point)
				(save-excursion
				  (rolo-to-entry-end t level) (point)))))
	(goto-char (point-min))
	(let* ((len (length name))
	       (name-level (concat level "*"))
	       (level-len (length name-level)))
	  (while (and (search-forward name nil t)
		      (save-excursion
			(beginning-of-line)
			(setq found
			      (looking-at
			       (concat "\\(" rolo-entry-regexp "\\)[ \t]*"
				       (regexp-quote name)))))
		      level
		      (/= level-len
			  (length (buffer-substring (match-beginning 1)
						    (match-end 1)))))
	    (setq found nil))
	  ))
      (or found (rolo-kill-buffer)) ;; conditionally kill
      )
    (widen)
    found))

(defun rolo-to-entry-end (&optional include-sub-entries curr-entry-level)
"Goes to end of whole entry if optional INCLUDE-SUB-ENTRIES is non-nil.
CURR-ENTRY-LEVEL is a string whose length is the same as the last found entry
header.  If INCLUDE-SUB-ENTRIES is nil, CURR-ENTRY-LEVEL is not needed.
Returns current point."
  (while (and (setq next-entry-exists
		    (re-search-forward rolo-entry-regexp nil t))
	      include-sub-entries
	      (> (- (point) (save-excursion
			      (beginning-of-line)
			      (point)))
		 (length curr-entry-level))))
  (if next-entry-exists
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar rolo-display-buffer "*Rolodex*"
  "Buffer used to display set of last matching rolodex entries.")

(defvar rolo-entry-regexp "^\*+"
  "Regular expression to match the beginning of a rolodex entry.
This pattern must match the beginning of the line.  Entries may be nested
through the use of increasingly longer beginning patterns.")

(defconst rolo-hdr-format
  (concat
   "======================================================================\n"
   "%s\n"
   "======================================================================\n")
  "Header to insert preceding a file's first rolodex entry match when
file has none of its own.  Used with one argument, the file name."
)

(defconst rolo-hdr-regexp "^==="
  "Regular expression to match the first and last lines of rolodex file headers.
This header is inserted into rolo-display-buffer before any entries from the
file are added.")

(provide 'wrolo)
