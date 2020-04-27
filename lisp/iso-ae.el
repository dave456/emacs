;; ASCII -> Umlaut Konvertierung f�r Emacs
;; � 1994 Michael Gschwind
;; � 1992 Martin Schulz 
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
			   ("ae" "�")
			   ("oe" "�")
			   ("ue" "�")
			   ("Ae" "�")
			   ("Oe" "�")
			   ("Ue" "�")

;# then fix the errors:
			   ("q�" "que")
			   ("Q�" "Que")
			   ("��" "�ue")

			   ("\\([aeoAEO]\\)�\\([^b]\\)" "\\1ue\\2")
;# exc:    unge-u"bt
;#         Extra-u"bertragung
;#      ...-u"b...
;# but:    Treuebruch
			   ("\\([Tt]\\)re�br" "\\1reuebr")
			   ("\\([bB]\\)auebene" "\\1auebene") ;??? mike

;# amgiguous: tue:  tun  > Wichtigtuer, grosstuerisch
;#                  Tu"r > Tu"ren
			   ("t�rei" "tuerei")
			   ("t�ri" "tueri")
			   ("t�s" "tues")

			   ("\\([wW]\\)ichtigt�r" "\\1ichtigtuer")
			   ("\\([gG]\\)rosst�r" "\\1rosstuer")
			   ("Nichtst�r" "Nichtstuer")
			   ("\\([gG]\\)eheimnist�r" "\\1eheimnistuer")
			   ("\\([hH]\\)eimlicht�r" "\\1eimlichtuer")
;# really amgiguous are Geheimtuer <-> Geheimtu"r
			   ("\\([^sS]\\)t�nd" "\\1tuend")

;# amgiguous: zue:  anzuerkennen <-> erzu"rnen
			   ("z�r\\([^n]\\)" "zuer\\1")
			   ("z�bnen" "zuebnen")
			   ("z�ngen" "zuengen")
			   ("z�isen" "zueisen")

;# diverse Ausnahmen:
			   ("s�ben" "soeben")
			   ("\\([aA]\\)ven�" "\\1venue")
			   ("\\([bB]\\)ellev�" "\\1ellevue")
			   ("Chaiselong�" "Chaiselongue")
			   ("\\([dD]\\)�tt" "\\1uett")
			   ("Ob�" "Oboe")
			   ("Okt�d" "Oktaed")
			   ("Tetr�d" "Tetraed")
			   ("\\([sS]\\)tat�" "\\1tatue")
			   ("�ro" "aero")
			   ("�ro" "Aero")
			   ("\\([gG]\\)�rill" "\\1uerill")
			   ("Individ�n" "Individuen")
			   ("\\([kK]\\)�xist" "\\1oexist")
			   ("\\([kK]\\)�ffiz" "\\1oeffiz")
			   ("\([kK]\)�duka" "\\1oeduka")
			   ("\\([pP]\\)�t$" "\\1oet")
			   ("\\([pP]\\)�sie$" "\\1oesie")
			   ("\\([pP]\\)�t\\([^tc]\\)" "\\1oet\\2")
			   ("\\([rR]\\)ev�" "\\1evue")
			   ("Vak�n" "Vakuen")

;#[iI]ntelektuell, [iI]ndividuell, sexuell, aktuell, [Dd]uell,
;#punktuell, rituell, virtuell, visuell, manuell
			   ("\\([txsdD]\\)�ll" "\\1uell")
			   ("\\([mM]\\)an�ll" "\\1anuell")
;# aber die ...tu"llen:
			   ("\\([gG]\\)ummituelle" "\1ummit�lle")
			   ("\\([kK]\\)abeltuelle" "\\1abelt�lle")
			   ("\\([pP]\\)lastiktuelle" "\\1lastikt�lle")

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
			   ("Mich�l" "Michael")
			   ("\\([mM]\\)an�l" "\\1anuel")
			   ("Raf�l" "Rafael")
			   ("Raph�l" "Raphael")
			   ("Sam�l" "Samuel")

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

