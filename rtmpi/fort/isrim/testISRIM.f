C f77 -o testISRIM testISRIM.f isrim.f basis.f
C
      PROGRAM TESTISRIM
C     test isrim
C
C     .. Local Scalars ..
      DOUBLE PRECISION OUTPUT(10),Y,DAYNO,LT,Z,LA,F107,AP
	dayno=123d0
	lt=12d0
	z=300d0
	f107=135d0
	ap=15d0

1	continue
	write(*,*) 'kinst (station code),  ipar (parameter code)'
	read(*,*)kinst,ip
	if(kinst.eq.-99) stop
	la=55d0
                  CALL ISRIM
     *            (KINST,DAYNO,LT,Z,LA,F107,AP,IP,OUTPUT)
                  Y = OUTPUT(1)
                  IF (IP.EQ.1) Y = DLOG10(DABS(OUTPUT(1)))
	write(*,*)KINST,DAYNO,LT,Z,LA,F107,AP,IP,output(1),output(4)
	go to 1
      STOP
      END
