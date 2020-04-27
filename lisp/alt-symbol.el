;;; alt-symbol.el --- Easy access to alternate 8bit symbols (ISO 8859-Latin1)

;; Copyright (C) 1993 Markus Ast (ast@intes-stuttgart.de)
;;                  & Michael Gschwind (mike@vlsivie.tuwien.ac.at)

;; Version : 1.08 09-Dec-1993
;; Keywords: alt, iso, latin, 8bit

;; This file works with GNU Emacs19 or higher, but is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; alt-symbol|Markus Ast|ast@intes-stuttgart.de|
;; Easy access to alternate 8bit symbols (ISO 8859-Latin1) |
;; 09-Dec-1993|$Revision 1.08$|~/misc/alt-symbol.el.Z|

;; Description:
;;--------------
;; alt-symbol.el contains elisp code to display and easily enter 8bit chars
;; according to the ISO-8859-1 standard (known as ISO-Latin1).
;; Therefore alt-symbol.el defines special 8bit-keymaps as an alternative to
;; the standard Emacs '8859-1-map' and provides an easy acces to those keymaps.
;;
;; Instead of using the standard Emacs [\C-x 8] prefix binding to ISO-Latin1
;; (e.g. typing long key sequence [\C-x 8 " a] to get an "a umlaut character)
;; one may enter/edit 8bit-characters by one of two methods:
;;
;; 1) For keyboards having a separate (non-Meta) ALT-key the following
;;    GLOBALLY valid keybindings are defined:
;;
;;    NOTE: THIS WAY OF ENTERING ACCENTS IS INCOMPATIBLE WITH EMACS 19.23 
;;          OR HIGHER. USE ISO-TRANSL+ FOR THIS INPUT METHOD INSTEAD.
;;          (iso-transl+ is available via anonymous ftp from 
;;          ftp.vlsivie.tuwien.ac.at in /pub/8bit.)
;;
;;    Type [Alt-" a] to get an "a umlaut character (or [Alt-" O] etc.),
;;    type [Alt-^ a] to get an ^a circumflex character (or [Alt-^ e] etc.),
;;    type [Alt-' a] to get an 'a accented character (or [Alt-' e] etc.),
;;    type [Alt-` a] to get an `a accented character (or [Alt-` e] etc.),
;;    type [Alt-~ n] to get an ~n accented character (or [Alt-~ o] etc.),
;;    type [Alt-, c] to get an ,c cedilla character ...
;;
;;    Also the following shorthand bindings are defined:
;;        [Alt-a] for "a umlaut,    [Alt-A] for "A umlaut,
;;        [Alt-e] for 'e accent,    [Alt-E] for 'E accent,
;;        [Alt-n] for ~n enye,      [Alt-N] for ~N enye,
;;        [Alt-o] for "o umlaut,    [Alt-O] for "O umlaut,
;;        [Alt-s] for german ssharp,[Alt-S] for german ssharp,
;;        [Alt-u] for "u umlaut,    [Alt-U] for "U umlaut,
;;        [Alt-y] for "y umlaut,    [Alt-Y] for Yen sign,
;;        [Alt-$] for cent-sign,    [Alt-m] for micro-sign,
;;        [Alt-c] for copyright,    [Alt-r] for registered,
;;        [Alt-<] for << character, [Alt->] for >> character,
;;        [Alt-!] and [Alt-?] for up-down-inverted ! respectively ? char
;;
;;    Additionally [ALT-8] is a short hand for [C-x 8] as defined in iso-insert
;;
;; 2) For ANY keyboards alt-symbol.el provides several LOCAL minor-mode toggles
;;    supplying ELECTRIC ACCENTS (this means that when you type an accent
;;    sign, it will put this accent over the next character, subject to the
;;    availability of this combination in the ISO-Latin1 set).
;;    To turn an accent character into an electric accent, invoke one, some
;;    or all of the following alt-...-mode toggle commands:
;;
;;    minor-mode name:  PREFIX: main 8bit-preferences:
;;   'alt-umlaut-mode'     "    for umlauts ("a) and german sharp-s (\3)
;;   'alt-aigu-mode'       '    for acute accented chars (like french 'e)
;;   'alt-circumflex-mode' ^    for circumflex accented chars (french ^e)
;;   'alt-grave-mode'      `    for grave accented chars (like french 'e)
;;   'alt-tilde-mode'      ~    for spanish/portugese characters (like ~n)
;;   'alt-slash-mode'      /    for scandinavian characters (slashed O etc.)
;;   'alt-ring-mode'       *    for scandinavian characters (ring-headed A)
;;                              'alt-ring-mode' equals 'alt-slash-mode'
;;                              (choose which PREFIX character You prefer).
;;   'alt-cedilla-mode'    ,    for cedilla character extra, this minor-mode
;;                              is obsolete (since cedilla is a default-binding
;;                              in all minor-modes listed above).
;;
;;   Invoking one of the above toggle commands will add/delete a typical 8bit
;;   character to/from the mode-line (e.g. "a for 'alt-umlaut-mode') indicating
;;   that this minor-mode is on or off, respectively.
;;   If - for instance - the 'alt-umlaut-mode' is turned on an ["a] key will
;;   insert an "a umlaut in the current buffer. Double [""] invokes just one
;;   ["], i.e. the command locally bound to ["] (usually 'self-insert-command,
;;   but may be buffer's local-map binding, if any, e.g. tex-insert-quote).
;;   This mimic is completely buffer-local (i.e. does NOT affect other buffers,
;;   wether they share the same major-mode keymaps or not!)
;;   Toggling back will rebind " to the command it was (locally or globally)
;;   bound before switching 'alt-umlaut-mode' on (usually self-insert-command).
;;
;;   Evaluation of iso-electric-mode causes electric mode to be
;;   turned on/off or toggled (depending on the parameter specified:
;;   positive arg -> on, negative -> off, none -> toggle) for the
;;   following accent characters: ' ` ^ " ~

;; Customization:
;;----------------
;; If you want ISO electric mode to be turned on automatically in, say,
;; text-mode, you can add the following code to your .emacs initialization
;; file:
;;      (setq text-mode-hook
;;          '(lambda () (auto-fill-mode 1)))
;;
;;
;; Put the following in your .emacs file (replace [f11...] by any key
;; sequence you like best, e.g. [?\A-^], [gold ?^] or the like):
;;
;;    (global-set-key [f11 ?"] 'alt-umlaut-mode)
;;    (global-set-key [f11 ?`] 'alt-aigu-mode)
;;    (global-set-key [f11 ?^] 'alt-circumflex-mode)
;;    (global-set-key [f11 ?'] 'alt-grave-mode)
;;    (global-set-key [f11 ?~] 'alt-tilde-mode)
;;    (global-set-key [f11 ?/] 'alt-slash-mode)
;;    (global-set-key [f11 ?*] 'alt-ring-mode)
;;    (global-set-key [f11 ?,] 'alt-cedilla-mode)
;;    (load "alt-symbol")
;;
;; Or - if you prefer autoloading - replace '(load "alt-symbol")' by:
;;
;;    (autoload 'alt-umlaut-mode     "alt-symbol" "8bit-toggle" t)
;;    (autoload 'alt-aigu-mode       "alt-symbol" "8bit-toggle" t)
;;    (autoload 'alt-circumflex-mode "alt-symbol" "8bit-toggle" t)
;;    (autoload 'alt-grave-mode      "alt-symbol" "8bit-toggle" t)
;;    (autoload 'alt-tilde-mode      "alt-symbol" "8bit-toggle" t)
;;    (autoload 'alt-slash-mode      "alt-symbol" "8bit-toggle" t)
;;    (autoload 'alt-ring-mode       "alt-symbol" "8bit-toggle" t)
;;    (autoload 'alt-cedilla-mode    "alt-symbol" "8bit-toggle" t)
;;
;; P.S.: The Authors recommend key bindings like:
;;      (global-set-key  [?\A-'] 'alt-grave-mode) ; If You have ALT key
;;      (define-key GOLD-map "'" 'alt-grave-mode) ; If You use an edt-emulation
;;
;; Variables:
;;------------
;; 'alt-mode-global' nil "Setting this variable to non-nil BEFORE loading
;;                        'alt-symbol' inhibits local definitions, i.e. makes
;;                        'alt-symbol' behave as a global minor-mode !

;; History:
;;----------
;; hilit19.el,v
;; Revision 1.08  09-Dec-1993 Markus & Michael
;; Emacs-19.21 or newer loads 'iso-transl.el instead of 'iso-insert.el' and
;; all 'insert-...-...' commands from former 'iso-insert.el' are replaced by
;; corresponding [code] bindings according to 'iso-transl.el' !
;; alt-symbol.el can now be loaded via (require 'alt-symbol) also !
;;
;; Revision 1.07  06-Nov-1993 Markus & Michael
;; Updated documentation.
;;
;; Revision 1.06  03-Nov-1993 Markus & Michael
;; Corrected [ALT-E/e] to insert-E/e-acute (instead of insert-E/e-aigu).
;;
;; Revision 1.05  2-Nov-1993 Markus & Michael
;; Add iso-electric-mode which makes ' ` ^ ~ " electric with
;; just one function call.
;;
;; Revision 1.04  11-Oct-1993  Markus & Michael
;; Correction of some document type-errors.
;; Erased confusing/clockwise Alt-key-bindings.
;; Added some characters to the alt-map, altered cent sign to
;; \A-$ to be compatible with the alt-...-mode key bindings.
;;
;; Revision 1.03  11-Oct-1993  Markus & Michael
;; Extended ALT's global-map bindings with clockwise key-bindings for
;; umlaut, aigu, circumflex, grave (e.g. to a,b,c,d for "a,'a,^a,`a)
;;
;; Revision 1.02  11-Oct-1993  Markus & Michael
;; New command 'alt-local-command' bound to every alt-...-map's prefix
;; accent (the accent to start that map) now uses that accent character's
;; previous local binding (if any, just 'self-insert-command' otherwise).
;;
;; Revision 1.01  09-Oct-1993  Markus & Michael
;; Added 'alt-mode-global' to force alt-symbol to be a global minor-mode.
;; 'insert-y-acute' in 'alt-umlaut-map' changed to 'insert-y-umlaut'.
;; Corrected some minor type-errors in doc-strings and comment.
;;
;; Revision 1.00  08-Oct-1993  Markus & Michael
;; This file is an updated unification of 'international.el' (written by
;; Michael Gschwind) and 'AltSym.el' (written by Markus Ast) and thus
;; replaces all previous versions of 'international.el' and 'AltSym.el' !

;; Problems:
;;-----------
;; If the ALT bindings do not work, check out the PROBLEMS file which comes
;; with emacs - it tells you how to bind the ALT key.
;; If you have only ALT keys, but no META key, emacs will use the ALT keys
;; as META keys - there is no way to avoid this - you can, however define a
;; Meta_L key using the xmodmap keysym command; if you add this META key to
;; the modifier table using the xmodmap add command, the ALT keys will
;; be available for these bindings
;;
;; Some X11 fonts do not contain the upper 128 ISO characters in their
;; character set. You can list the fonts available on your system
;; with the command 'xlsfonts' and preview them with 'xfd'. After you
;; have found a suitable font, add a line like this to your ~/.Xdefaults
;; resource file:
;;
;; emacs*Font: -adobe-courier-medium-r-normal--18-180-75-75-m-110-iso8859-1
;;

;; Bug:
;;------
;; * 8bit-chars don't work within I-Search - same as original \C-x 8 mimic -,
;;   i.e. will exit I-Search and insert the last-command-char in current-buffer
;;   (but it works within RE-Search or query-replace if You - for instance -
;;    specify PREFIX-" to toggle that mode on in RE-Search's minibuffer before
;;    inserting an umlaut character).
;; * Missing bindings for: insert-general-currency-sign, insert-no-break-space
;;   insert-ordinal-indicator-feminine/masculine and insert-not-sign


;;; Code:
;;;=======
(defvar alt-mode-global nil
"Setting this variable to non-nil BEFORE loading 'alt-symbol' inhibits local
definitions, i.e. makes 'alt-symbol' behave as a global minor-mode !")

;; Requires ISO-Latin-1 definitions from standard emacs
(require 'disp-table)
(require 'iso-syntax)
(if (string-lessp emacs-version "19.21")
    (require 'iso-insert)
    (require 'iso-transl))

;; Force full 8bit-display
(standard-display-european t)

;; Definition of additional keymaps for alt-... bindings:
;;--------------------------------------------------------
(defun alt-defkey (key-map)
  "Defines some alternate default bindings for KEY-MAP."
  (define-key key-map " "  [168])
  (define-key key-map "C" [199])
  (define-key key-map "D" [208])
  (define-key key-map "E" [198])
  (define-key key-map "L" [163])
  (define-key key-map "M" [181])
  (define-key key-map "P" [182])
  (define-key key-map "R" [174])
  (define-key key-map "S" [223])           ; (only small \3 is valid)
  (define-key key-map "T" [222])
  (define-key key-map "X" [168])
  (define-key key-map "Y" [165])
  (define-key key-map "c" [231])
  (define-key key-map "d" [240])
  (define-key key-map "e" [230])
  (define-key key-map "l" [163])
  (define-key key-map "m" [181])
  (define-key key-map "p" [182])
  (define-key key-map "r" [174])
  (define-key key-map "s" [223])
  (define-key key-map "t" [254])
  (define-key key-map "x" [168])
  (define-key key-map "y" [165])
  (define-key key-map "|" [166])
  (define-key key-map "$" [162])
  (define-key key-map "&" [167])
  (define-key key-map "<" [171])
  (define-key key-map ">" [187])
  (define-key key-map "!" [161])
  (define-key key-map "?" [191])
  (define-key key-map "." [183])
  (define-key key-map "=" [175])
  (define-key key-map "-" [173])
  (define-key key-map "+" [177])
  (define-key key-map "*" [215])
  (define-key key-map "/" [247])
  (define-key key-map "1" [185])
  (define-key key-map "2" [178])
  (define-key key-map "3" [179])
  (define-key key-map "4" [188])
  (define-key key-map "5" [189])
  (define-key key-map "6" [190])
  (define-key key-map "7" [190])
  (define-key key-map "8" [190])
  (define-key key-map "0" [176])
)
(defun alt-local-command ()
 "Call last typed character's local command (if any) or 'self-insert-command'."
 (interactive)
 (call-interactively (or (local-key-binding (char-to-string last-command-char))
                         'self-insert-command)))

;;NEW alt-umlaut-map:
(setq alt-umlaut-map (make-keymap))
(alt-defkey alt-umlaut-map)     ; Bind DEFAULTs first
(define-key alt-umlaut-map "\"" 'alt-local-command)
(define-key alt-umlaut-map "A"  [196])
(define-key alt-umlaut-map "E"  [203])
(define-key alt-umlaut-map "I"  [207])
(define-key alt-umlaut-map "O"  [214])
(define-key alt-umlaut-map "U"  [220])
(define-key alt-umlaut-map "Y"  [255]); (only small "y is valid)
(define-key alt-umlaut-map "a"  [228])
(define-key alt-umlaut-map "e"  [235])
(define-key alt-umlaut-map "i"  [239])
(define-key alt-umlaut-map "o"  [246])
(define-key alt-umlaut-map "u"  [252])
(define-key alt-umlaut-map "y"  [255])
(define-key alt-umlaut-map "3"  [223])

;;NEW alt-aigu-map:
(setq alt-aigu-map (make-keymap))
(alt-defkey alt-aigu-map)    ; Bind DEFAULTs first
(define-key alt-aigu-map "'" 'alt-local-command)
(define-key alt-aigu-map " " [180])
(define-key alt-aigu-map "A" [193])
(define-key alt-aigu-map "E" [201])
(define-key alt-aigu-map "I" [205])
(define-key alt-aigu-map "O" [211])
(define-key alt-aigu-map "U" [218])
(define-key alt-aigu-map "Y" [221])
(define-key alt-aigu-map "a" [225])
(define-key alt-aigu-map "e" [233])
(define-key alt-aigu-map "i" [237])
(define-key alt-aigu-map "o" [243])
(define-key alt-aigu-map "u" [250])
(define-key alt-aigu-map "y" [253])

;;NEW alt-cedilla-map:
(setq alt-cedilla-map (make-sparse-keymap))
(alt-defkey alt-cedilla-map)    ; Bind DEFAULTs first
(define-key alt-cedilla-map "," 'alt-local-command)
(define-key alt-cedilla-map " " [184])

;;NEW alt-circumflex-map:
(setq alt-circumflex-map (make-keymap))
(alt-defkey alt-circumflex-map)    ; Bind DEFAULTs first
(define-key alt-circumflex-map "^" 'alt-local-command)
(define-key alt-circumflex-map "A" [194])
(define-key alt-circumflex-map "E" [202])
(define-key alt-circumflex-map "I" [206])
(define-key alt-circumflex-map "O" [212])
(define-key alt-circumflex-map "U" [219])
(define-key alt-circumflex-map "a" [226])
(define-key alt-circumflex-map "e" [234])
(define-key alt-circumflex-map "i" [238])
(define-key alt-circumflex-map "o" [244])
(define-key alt-circumflex-map "u" [251])

;;NEW alt-grave-map:
(setq alt-grave-map (make-sparse-keymap))
(alt-defkey alt-grave-map)    ; Bind DEFAULTs first
(define-key alt-grave-map "`" 'alt-local-command)
(define-key alt-grave-map "A" [192])
(define-key alt-grave-map "E" [200])
(define-key alt-grave-map "I" [204])
(define-key alt-grave-map "O" [210])
(define-key alt-grave-map "U" [217])
(define-key alt-grave-map "a" [224])
(define-key alt-grave-map "e" [232])
(define-key alt-grave-map "i" [236])
(define-key alt-grave-map "o" [242])
(define-key alt-grave-map "u" [249])

;;NEW alt-tilde-map:
(setq alt-tilde-map (make-sparse-keymap))
(alt-defkey alt-tilde-map)    ; Bind DEFAULTs first
(define-key alt-tilde-map "~" 'alt-local-command)
(define-key alt-tilde-map " " [172])
(define-key alt-tilde-map "A" [195])
(define-key alt-tilde-map "N" [209])
(define-key alt-tilde-map "O" [213])
(define-key alt-tilde-map "a" [227])
(define-key alt-tilde-map "n" [241])
(define-key alt-tilde-map "o" [245])

;;NEW alt-ring-map
(setq alt-ring-map (make-keymap))
(alt-defkey alt-ring-map)    ; Bind DEFAULTs first
(define-key alt-ring-map "*" 'alt-local-command)
(define-key alt-ring-map "/" 'alt-local-command)
(define-key alt-ring-map "a" [229])
(define-key alt-ring-map "A" [197])
(define-key alt-ring-map "c" [169])
(define-key alt-ring-map "C" [169])
(define-key alt-ring-map "e" [230])
(define-key alt-ring-map "E" [198])
(define-key alt-ring-map "o" [248])
(define-key alt-ring-map "O" [216])


;; Bind 8bit-symbols to ALT-Keysequence (\A-... faster than \C-x 8 ...)
;;---------------------------------------------------------------------
(define-key global-map [ ?\A-A ] [196])
(define-key global-map [ ?\A-E ] [201])
(define-key global-map [ ?\A-N ] [209])
(define-key global-map [ ?\A-O ] [214])
(define-key global-map [ ?\A-S ] [223])
(define-key global-map [ ?\A-U ] [220])
(define-key global-map [ ?\A-Y ] [165])
(define-key global-map [ ?\A-a ] [228])
(define-key global-map [ ?\A-c ] [169])
(define-key global-map [ ?\A-e ] [233])
(define-key global-map [ ?\A-i ] [239])
(define-key global-map [ ?\A-m ] [181])
(define-key global-map [ ?\A-n ] [241])
(define-key global-map [ ?\A-o ] [246])
(define-key global-map [ ?\A-r ] [174])
(define-key global-map [ ?\A-s ] [223])
(define-key global-map [ ?\A-u ] [252])
(define-key global-map [ ?\A-y ] [255])
(define-key global-map [ ?\A-| ] [166])
(define-key global-map [ ?\A-$ ] [162])
(define-key global-map [ ?\A-& ] [167])
(define-key global-map [ ?\A-< ] [171])
(define-key global-map [ ?\A-> ] [187])
(define-key global-map [ ?\A-! ] [161])
(define-key global-map [ ?\A-? ] [191])
(define-key global-map [ ?\A-. ] [183])
(define-key global-map [ ?\A-= ] [175])
(define-key global-map [ ?\A-- ] [173])
(define-key global-map [ ?\A-+ ] [177])
(define-key global-map [ ?\A-* ] [215])
(define-key global-map [ ?\A-/ ] [247])
(define-key global-map [ ?\A-1 ] [185])
(define-key global-map [ ?\A-2 ] [178])
(define-key global-map [ ?\A-3 ] [179])
(define-key global-map [ ?\A-4 ] [188])
(define-key global-map [ ?\A-5 ] [189])
(define-key global-map [ ?\A-6 ] [190])
(define-key global-map [ ?\A-7 ] [190])
(define-key global-map [ ?\A-0 ] [176])

;; keys for making the various accents in french, spanish and portuguese
(define-key global-map [ ?\A-" ] alt-umlaut-map)
(define-key global-map [ ?\A-' ] alt-aigu-map)
(define-key global-map [ ?\A-^ ] alt-circumflex-map)
(define-key global-map [ ?\A-` ] alt-grave-map)
(define-key global-map [ ?\A-~ ] alt-tilde-map)
(define-key global-map [ ?\A-/ ] alt-ring-map)
(define-key global-map [ ?\A-* ] alt-ring-map)
(define-key global-map [ ?\A-, ] alt-cedilla-map)
(define-key isearch-mode-map [?\A-8] nil)
(if (string-lessp emacs-version "19.21")
    (define-key global-map [ ?\A-8 ] 8859-1-map) ;; faster than C-x 8
    (define-key key-translation-map [ ?\A-8 ]
               (lookup-key key-translation-map "\C-x8")))

;; Define minor-mode toggles for " ' ^ ` ~ * and , -Prefixes
;;-----------------------------------------------------------
(define-key global-map "\C-x88"   (make-sparse-keymap))
(define-key global-map "\C-x88\"" 'alt-umlaut-mode)     ; C-x 8 8 "
(define-key global-map "\C-x88'"  'alt-aigu-mode)       ; C-x 8 8 '
(define-key global-map "\C-x88^"  'alt-circumflex-mode) ; C-x 8 8 ^
(define-key global-map "\C-x88`"  'alt-grave-mode)      ; C-x 8 8 `
(define-key global-map "\C-x88~"  'alt-tilde-mode)      ; C-x 8 8 ~
(define-key global-map "\C-x88*"  'alt-ring-mode)       ; C-x 8 8 *
(define-key global-map "\C-x88/"  'alt-slash-mode)      ; C-x 8 8 / (=alt-ring-mode)
(define-key global-map "\C-x88,"  'alt-cedilla-mode)    ; C-x 8 8 ,

;; Provide alt-symbol's mode-line string (behaviour is buffer-local)
(defvar alt-symbol-string nil
       "Holds status respectively mode-line string for alt-symbol-modes.")
(or alt-mode-global (make-variable-buffer-local 'alt-symbol-string))
(or (assq 'alt-symbol-string minor-mode-alist) (setq minor-mode-alist
   (cons '(alt-symbol-string alt-symbol-string) minor-mode-alist)))

(defun  alt-symbol-string () "Utility function for alt-...-mode toggles."
  (setq alt-symbol-string (concat " "
                          (if alt-ring-mode       (char-to-string 229))
                          (if alt-umlaut-mode     (char-to-string 228))
                          (if alt-cedilla-mode    (char-to-string 231))
                          (if alt-aigu-mode       (char-to-string 233))
                          (if alt-circumflex-mode (char-to-string 234))
                          (if alt-grave-mode      (char-to-string 232))
                          (if alt-tilde-mode      (char-to-string 241))
                          (if alt-slash-mode      (char-to-string 248))))
  (or (string< " " alt-symbol-string) (setq alt-symbol-string nil))
  (force-mode-line-update))


;;;###autoload
(defun alt-umlaut-mode (arg)
  "Toggles alt-umlaut-mode. With arg, turn that mode on iff arg is positive.
In alt-umlaut-mode \" is a prefix key for alternate symbols, e.g. \"a for an
a-diaeresis character (code \\344), a double key \"\" just invokes one \"."
  (interactive "P")
  (setq alt-umlaut-mode (if (null arg) (not alt-umlaut-mode)
                                       (> (prefix-numeric-value arg) 0)))
  (alt-symbol-string))

(defvar alt-umlaut-mode nil "Buffer-local minor-mode variable.")
(or alt-mode-global (make-variable-buffer-local 'alt-umlaut-mode))
(setq       alt-map-umlaut (make-sparse-keymap))
(define-key alt-map-umlaut "\"" alt-umlaut-map)
(or (assq 'alt-umlaut-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'alt-umlaut-mode alt-map-umlaut)
                                      minor-mode-map-alist)))

;;;###autoload
(defun alt-aigu-mode (arg)
  "Toggles alt-aigu-mode. With arg, turn that mode on iff arg is positive.
In alt-aigu-mode ' is a prefix key for alternate symbols, e.g. 'a for an
acute-accented \"a\" character (code \\341), a double key '' just invokes one '."
  (interactive "P")
  (setq alt-aigu-mode (if (null arg) (not alt-aigu-mode)
                                     (> (prefix-numeric-value arg) 0)))
  (alt-symbol-string))

(defvar alt-aigu-mode nil "Buffer-local minor-mode variable.")
(or alt-mode-global (make-variable-buffer-local 'alt-aigu-mode))
(setq       alt-map-aigu (make-sparse-keymap))
(define-key alt-map-aigu "'" alt-aigu-map)
(or (assq 'alt-aigu-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'alt-aigu-mode alt-map-aigu)
                                      minor-mode-map-alist)))

;;;###autoload
(defun alt-circumflex-mode (arg)
  "Toggles alt-circumflex-mode. With arg, turn that mode on iff arg is positive.
In alt-circumflex-mode ^ is a prefix key for alternate symbols, e.g. ^a for a
circumflexed \"a\" character (code \\342), a double key ^^ just invokes one ^."
  (interactive "P")
  (setq alt-circumflex-mode (if (null arg) (not alt-circumflex-mode)
                                           (> (prefix-numeric-value arg) 0)))
  (alt-symbol-string))

(defvar alt-circumflex-mode nil "Buffer-local minor-mode variable.")
(or alt-mode-global (make-variable-buffer-local 'alt-circumflex-mode))
(setq       alt-map-circumflex (make-sparse-keymap))
(define-key alt-map-circumflex "^" alt-circumflex-map)
(or (assq 'alt-circumflex-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'alt-circumflex-mode alt-map-circumflex)
                                      minor-mode-map-alist)))

;;;###autoload
(defun alt-grave-mode (arg)
  "Toggles alt-grave-mode. With arg, turn that mode on iff arg is positive.
In alt-grave-mode ` is a prefix key for alternate symbols, e.g. `a for a
grave-accented \"a\" character (code \\342), a double key `` just invokes one `."
  (interactive "P")
  (setq alt-grave-mode (if (null arg) (not alt-grave-mode)
                                      (> (prefix-numeric-value arg) 0)))
  (alt-symbol-string))

(defvar alt-grave-mode nil "Buffer-local minor-mode variable.")
(or alt-mode-global (make-variable-buffer-local 'alt-grave-mode))
(setq       alt-map-grave (make-sparse-keymap))
(define-key alt-map-grave "`" alt-grave-map)
(or (assq 'alt-grave-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'alt-grave-mode alt-map-grave)
                                      minor-mode-map-alist)))

;;;###autoload
(defun alt-tilde-mode (arg)
  "Toggles alt-tilde-mode. With arg, turn that mode on iff arg is positive.
In alt-tilde-mode ~ is a prefix key for alternate symbols, e.g. ~a for a
tilde-headed \"a\" character (code \\343), a double key ~~ just invokes one ~."
  (interactive "P")
  (setq alt-tilde-mode (if (null arg) (not alt-tilde-mode)
                                      (> (prefix-numeric-value arg) 0)))
  (alt-symbol-string))

(defvar alt-tilde-mode nil "Buffer-local minor-mode variable.")
(or alt-mode-global (make-variable-buffer-local 'alt-tilde-mode))
(setq       alt-map-tilde (make-sparse-keymap))
(define-key alt-map-tilde "~" alt-tilde-map)
(or (assq 'alt-tilde-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'alt-tilde-mode alt-map-tilde)
                                      minor-mode-map-alist)))

;;;###autoload
(defun alt-slash-mode (arg)
  "Toggles alt-slash-mode. With arg, turn that mode on iff arg is positive.
In alt-slash-mode / is a prefix key for alternate symbols, e.g. /o for a
slashed \"o\" character (code \\370), a double key // just invokes one /.
Remark: 'alt-slash-mode' and 'alt-ring-mode' are synonyms."
  (interactive "P")
  (setq alt-slash-mode (if (null arg) (not alt-slash-mode)
                                      (> (prefix-numeric-value arg) 0)))
  (alt-symbol-string))

(defvar alt-slash-mode nil "Buffer-local minor-mode variable.")
(or alt-mode-global (make-variable-buffer-local 'alt-slash-mode))
(setq       alt-map-slash (make-sparse-keymap))
(define-key alt-map-slash "/" alt-ring-map)
(or (assq 'alt-slash-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'alt-slash-mode alt-map-slash)
                                      minor-mode-map-alist)))

;;;###autoload
(defun alt-ring-mode (arg)
  "Toggles alt-ring-mode. With arg, turn that mode on iff arg is positive.
In alt-ring-mode * is a prefix key for alternate symbols, e.g. *a for a
ring-headed \"a\" character (code \\345), a double key ** inserts just one *.
Remark: 'alt-slash-mode' and 'alt-ring-mode' are synonyms."
  (interactive "P")
  (setq alt-ring-mode (if (null arg) (not alt-ring-mode)
                                      (> (prefix-numeric-value arg) 0)))
  (alt-symbol-string))

(defvar alt-ring-mode nil "Buffer-local minor-mode variable.")
(or alt-mode-global (make-variable-buffer-local 'alt-ring-mode))
(setq       alt-map-ring (make-sparse-keymap))
(define-key alt-map-ring "*" alt-ring-map)
(or (assq 'alt-ring-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'alt-ring-mode alt-map-ring)
                                      minor-mode-map-alist)))

;;;###autoload
(defun alt-cedilla-mode (arg)
  "Toggles alt-cedilla-mode. With arg, turn that mode on iff arg is positive.
In alt-cedilla-mode , is a prefix key for alternate symbols, e.g. ,c for a
cedilla-ed \"c\" character (code \\347), a double key ,, inserts just one ,."
  (interactive "P")
  (setq alt-cedilla-mode (if (null arg) (not alt-cedilla-mode)
                                      (> (prefix-numeric-value arg) 0)))
  (alt-symbol-string))

(defvar alt-cedilla-mode nil "Buffer-local minor-mode variable.")
(or alt-mode-global (make-variable-buffer-local 'alt-cedilla-mode))
(setq       alt-map-cedilla (make-sparse-keymap))
(define-key alt-map-cedilla "," alt-cedilla-map)
(or (assq 'alt-cedilla-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'alt-cedilla-mode alt-map-cedilla)
                                      minor-mode-map-alist)))
(defun iso-electric-mode (arg)
  "Toggles electric mode for characters ' ` \" ^ ~.
With arg, turn that mode on iff arg is positive."
  (interactive "P")
  (setq iso-electric-mode (if (null arg) (not iso-electric-mode)
                                      (> (prefix-numeric-value arg) 0)))
  (setq alt-umlaut-mode iso-electric-mode)
  (setq alt-aigu-mode iso-electric-mode)
  (setq alt-grave-mode iso-electric-mode)
  (setq alt-tilde-mode iso-electric-mode)
  (setq alt-circumflex-mode iso-electric-mode)
  (alt-symbol-string))

(defvar iso-electric-mode nil "Buffer-local minor-mode variable")
(or alt-mode-global (make-variable-buffer-local 'iso-electric-mode))

(provide 'alt-symbol)

;;; alt-symbol.el ends here
