;; ASCII -> Umlaut Konvertierung für Emacs
;; © 1994 Michael Gschwind
;; © 1992 Martin Schulz 
;# This is a rather complete ASCII to Umlaut conversion script.
;# I have converted a large dictionary with it.
;# No, there si no equivalent ss -> sS script -
;# I have tried it and found it didn't work well.
;#
;# Usage:  sed -f umlaut.sed  input > output
;#
;# Enjoy.
;#

(require 'iso-cvt)

;# first convert everything:
(defvar umlaut-trans-tab '(
			   ("ae" "ä")
			   ("oe" "ö")
			   ("ue" "ü")
			   ("Ae" "Ä")
			   ("Oe" "Ö")
			   ("Ue" "Ü")

;# then fix the errors:
			   ("qü" "que")
			   ("Qü" "Que")
			   ("äü" "äue")

			   ("\\([aeoAEO]\\)ü\\([^b]\\)" "\\1ue\\2")
;# exc:    unge-u"bt
;#         Extra-u"bertragung
;#      ...-u"b...
;# but:    Treuebruch
			   ("\\([Tt]\\)reübr" "\\1reuebr")
			   ("\\([bB]\\)auebene" "\\1auebene") ;??? mike

;# amgiguous: tue:  tun  > Wichtigtuer, grosstuerisch
;#                  Tu"r > Tu"ren
			   ("türei" "tuerei")
			   ("türi" "tueri")
			   ("tüs" "tues")

			   ("\\([wW]\\)ichtigtür" "\\1ichtigtuer")
			   ("\\([gG]\\)rosstür" "\\1rosstuer")
			   ("Nichtstür" "Nichtstuer")
			   ("\\([gG]\\)eheimnistür" "\\1eheimnistuer")
			   ("\\([hH]\\)eimlichtür" "\\1eimlichtuer")
;# really amgiguous are Geheimtuer <-> Geheimtu"r
			   ("\\([^sS]\\)tünd" "\\1tuend")

;# amgiguous: zue:  anzuerkennen <-> erzu"rnen
			   ("zür\\([^n]\\)" "zuer\\1")
			   ("zübnen" "zuebnen")
			   ("züngen" "zuengen")
			   ("züisen" "zueisen")

;# diverse Ausnahmen:
			   ("söben" "soeben")
			   ("\\([aA]\\)venü" "\\1venue")
			   ("\\([bB]\\)ellevü" "\\1ellevue")
			   ("Chaiselongü" "Chaiselongue")
			   ("\\([dD]\\)ütt" "\\1uett")
			   ("Obö" "Oboe")
			   ("Oktäd" "Oktaed")
			   ("Teträd" "Tetraed")
			   ("\\([sS]\\)tatü" "\\1tatue")
			   ("äro" "aero")
			   ("Äro" "Aero")
			   ("\\([gG]\\)ürill" "\\1uerill")
			   ("Individün" "Individuen")
			   ("\\([kK]\\)öxist" "\\1oexist")
			   ("\\([kK]\\)öffiz" "\\1oeffiz")
			   ("\([kK]\)öduka" "\\1oeduka")
			   ("\\([pP]\\)öt$" "\\1oet")
			   ("\\([pP]\\)ösie$" "\\1oesie")
			   ("\\([pP]\\)öt\\([^tc]\\)" "\\1oet\\2")
			   ("\\([rR]\\)evü" "\\1evue")
			   ("Vakün" "Vakuen")

;#[iI]ntelektuell, [iI]ndividuell, sexuell, aktuell, [Dd]uell,
;#punktuell, rituell, virtuell, visuell, manuell
			   ("\\([txsdD]\\)üll" "\\1uell")
			   ("\\([mM]\\)anüll" "\\1anuell")
;# aber die ...tu"llen:
			   ("\\([gG]\\)ummituelle" "\1ummitülle")
			   ("\\([kK]\\)abeltuelle" "\\1abeltülle")
			   ("\\([pP]\\)lastiktuelle" "\\1lastiktülle")

;# diverse Komposita:
;# there are certainly more...
;s/\([bB]\)rutto"/\1ruttoe/
;s/\([bB]\)u"ro"/\1u"roe/
;s/\([cC]\)holera"/\1holerae/
;s/\([dD]\)omino"/\1ominoe/
;s/\([eE]\)cho"/\1choe/
;s/\([eE]\)lektro"\([^n]\)/\1lektoe\2/
;s/\([eE]\)uropa"x/\1uropaex/
;s/\([eE]\)xtra"/\1xtrae/
;s/\([fF]\)oto"/\1otoe/
;s/\([gG]\)ala"/\1alae/
;s/\([hH]\)omo"\([^o]\)/\1omoe\2/
;s/\([kK]\)onto"/\1ontoe/
;s/\([mM]\)ikro"/\1ikroe/
;s/\([nN]\)etto"/\1ettoe/
;s/\([oO]\)pto"/\1ptoe/
;s/\([pP]\)harma"/\1harmae/
;s/\([pP]\)hoto"/\1hotoe/
;s/\([pP]\)iezo"/\1iezoe/
;s/\([pP]\)seudo"/\1seudoe/
;s/\([rR]\)adio"/\1adioe/
;
;# Namen:
			   ("Michäl" "Michael")
			   ("\\([mM]\\)anül" "\\1anuel")
			   ("Rafäl" "Rafael")
			   ("Raphäl" "Raphael")
			   ("Samül" "Samuel")

;# ausla"ndisches und geographisches:
;s/\([bB]\)u"no/\1ueno/
;s/Ca"n/Caen/
;s/Ca"sar/Caesar/
;s/\([cC]\)itro"n/\1itroen/
;s/\([iI]\)sra"l/\1srael/
;s/\([iI]\)tzeho"/\1tzehoe/
;s/Langu"doc/Languedoc/
;s/Monro"/Monroe/
;s/Pa"lla/Paella/
;s/Su"z/Suez/
;s/Venezu"l/Venezuel/

;# Endungen:
;s/\([aeot]\)u"$/\1ue/

;# special:
;s/Atue/Atu"/
;s/AtU[Ee]/AtU"/
			   ))

