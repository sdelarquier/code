c File: lnblnk.f
c Last Modification: 14-JAN-1997
c Author: Dieter Andre'
c
c Replacement for the French builtin routine lnblnk
c gives the position of the last nonblank character in string
c actually gives the position of the first blank character

	INTEGER*2 FUNCTION lnblnk( strin)
	CHARACTER strin*(*)
	
	INTEGER*2 ili
	
	lnblnk= 0
	DO 10 ili= 1, LEN( strin)
	  IF (strin( ili:ili) .NE. ' ') lnblnk= ili
10	CONTINUE	
	RETURN
	END
	
