C
      PROGRAM TBLRRP
C     tblrrp writes a table of local/regional radar model  
C     parameter values.
C
C     Usage - tblrrp kinst dayno1 dayno2 daynod lt1 lt2 ltd ...
C                    z1 z2 zd f107 ap ip tabfile
C        kinst     - STATION/INSTRUMENT CODE
C             31 -> MILLSTONE HILL STEERABLE (regional model)
C             32 -> MILLSTONE HILL ZENITH (local model)
C         !!  10 -> JICAMARCA - NOT AVAILABLE!!
C             20 -> ARECIBO
C             25 -> MU RADAR
C             40 -> ST. SANTIN
C             72 -> EISCAT TROMSO
C             80 -> SONDRESTROM (local)
C             95 -> EISCAT SVALBARD
C              0 -> AMERICA REGIONAL MODEL (GEODETC 15-70 DEGREES)
C
C        dayno1   - start day number (january 1 = 1)
C        dayno2   - end day number
C        daynod   - day number increment
C        lt1      - start local time (hours)
C        lt2      - end local time
C        ltd      - local time increment
C        z1       - start altitude (km)
C        z2       - end altitude
C        zd       - altitude increment
C        la1      - start geodetic latitude (degree)
C        la2      - end geodetic latitude (degree)
C        lad      - geodetic latitude increment (degree) =0 LOCAL MODEL
C        f107     - 10.7 cm solar flux
C        ap       - 3-hour ap index
C        ip       - parameter code
c                      520 - log10 electron density
c                      550 - ion temperature (k)
c                      560 - electron temperature (k)
c                      1270 - parallel drifts
C        tabfile  - output file name
C
C     e.g.: tblrrp 32 1 1 1 12 12 12 ...
C                 100 1000 100 135 15 550 tbmhrp.out
C
C     .. Local Scalars ..
      DOUBLE PRECISION AP,DAYNO,DAYNO1,DAYNO2,DAYNOD,F107,LA,LA1,LA2,
     *                 LAD,LT,LT1,LT2,LTD,Y,Z,Z1,Z2,ZD
      INTEGER I,IP,IPAR,J,K,M,NDAYNO,NLA,NLT,NZ,STATUS
      CHARACTER*24 FM
      CHARACTER*128 ARG,TABFILE
C     ..
C     .. External Functions ..
      INTEGER IARGC
      EXTERNAL IARGC
C     ..
C     .....GET COMMAND LINE ARGUMENTS.....
C     .. External Subroutines ..
      EXTERNAL GETARG,ISRIM
C     ..
C
C     .. Intrinsic Functions ..
      INTRINSIC DABS,DLOG10,LEN
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION OUTPUT(10)
C     ..
      IF (IARGC().NE.17.AND.IARGC().NE.14) THEN
         WRITE (6,*) 'GENERAL USAGE --- '
         WRITE (6,FMT=
     *'(''tb3D4D kinst dayno1 dayno2 daynod lt1 lt2 ''
     *    ''ltd z1 z2 zd la1 la2 lad f107 ap ip tabfile'')')
         WRITE (6,*) 'lOCAL MODEL ONLY --- '
         WRITE (6,FMT=
     *'(''tb3D4D kinst dayno1 dayno2 daynod lt1 lt2 ''
     *    ''ltd z1 z2 zd f107 ap ip tabfile'')')
         STOP
      END IF
C
      CALL GETARG(1,ARG)
      READ (UNIT=ARG,FMT=*) KINST
      CALL GETARG(2,ARG)
      READ (UNIT=ARG,FMT=*) DAYNO1
      CALL GETARG(3,ARG)
      READ (UNIT=ARG,FMT=*) DAYNO2
      CALL GETARG(4,ARG)
      READ (UNIT=ARG,FMT=*) DAYNOD
      CALL GETARG(5,ARG)
      READ (UNIT=ARG,FMT=*) LT1
      CALL GETARG(6,ARG)
      READ (UNIT=ARG,FMT=*) LT2
      CALL GETARG(7,ARG)
      READ (UNIT=ARG,FMT=*) LTD
      CALL GETARG(8,ARG)
      READ (UNIT=ARG,FMT=*) Z1
      CALL GETARG(9,ARG)
      READ (UNIT=ARG,FMT=*) Z2
      CALL GETARG(10,ARG)
      READ (UNIT=ARG,FMT=*) ZD
      I = 0
      LA1 = 42
      LA2 = 42
      LAD = 1
      IF (IARGC().EQ.17) THEN
      CALL GETARG(11,ARG)
      READ (UNIT=ARG,FMT=*) LA1
      CALL GETARG(12,ARG)
      READ (UNIT=ARG,FMT=*) LA2
      CALL GETARG(13,ARG)
      READ (UNIT=ARG,FMT=*) LAD
      I = 3
      ENDIF
      CALL GETARG(11+I,ARG)
      READ (UNIT=ARG,FMT=*) F107
      CALL GETARG(12+I,ARG)
      READ (UNIT=ARG,FMT=*) AP
      CALL GETARG(13+I,ARG)
      READ (UNIT=ARG,FMT=*) IP
      CALL GETARG(14+I,TABFILE)
C
      DO 10 I = 1,LEN(TABFILE)
         IF (TABFILE(I:I).NE.' ') THEN
            LT = I
         END IF
   10 CONTINUE
      OPEN (UNIT=16,FILE=TABFILE(1:LT),IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         WRITE (6,FMT='(''Error opening output file'')')
         STOP
      END IF

      NDAYNO = (DAYNO2-DAYNO1)/DAYNOD + 1
      NLT = (LT2-LT1)/LTD + 1
      NZ = (Z2-Z1)/ZD + 1
      IF (LAD.EQ.0D0) THEN
         NLA = 1
      ELSE
         NLA = (LA2-LA1)/LAD + 1
      END IF
C
      DAYNO = DAYNO1
      DO 50 K = 1,NDAYNO
         LT = LT1
         DO 40 J = 1,NLT
            Z = Z1
            DO 30 I = 1,NZ
               LA = LA1
               DO 20 M = 1,NLA
                  OUTPUT(4) = -1.d0
c                  OUTPUT(5) = 1.d0
                  OUTPUT(5) = 2.d0
                  IF (IP.EQ.520) IPAR = 1
                  IF (IP.EQ.560) IPAR = 2
                  IF (IP.EQ.550) IPAR = 3
                  IF (IP.EQ.1270) IPAR = 4
                  IF (IP.EQ.-520) IPAR = 5
                  IF (IP.EQ.-560) IPAR = 6
                  IF (IP.EQ.-550) IPAR = 7
                  IF (IP.EQ.-1270) IPAR =8

                  CALL ISRIM
     *            (KINST,DAYNO,LT,Z,LA,F107,AP,IPAR,OUTPUT)
c                  IF (OUTPUT(4).LT.0) WRITE(*,*) OUTPUT(4)
                  Y = OUTPUT(1)
                  IF (IP.EQ.520) Y = DLOG10(DABS(OUTPUT(1)))
                  IF (IP.EQ.520 .AND. OUTPUT(1).LT.
     *                0d0) OUTPUT(1) = -OUTPUT(1)
                  IF (IP.EQ.520) THEN
                     FM = '(2F7.2,4F8.1,F10.4)'
                  ELSE IF (IP.EQ.550 .OR. IP.EQ.560) THEN
                     FM = '(2F7.2,4F8.1,F8.1)'
                  ELSE
                     FM = '(2F7.2,4F8.1,F10.4)'
                  END IF
                   WRITE (16,FMT=FM) DAYNO,LT,Z,LA,F107,AP,Y
                  LA = LA + LAD
   20          CONTINUE
               Z = Z + ZD
   30       CONTINUE
            LT = LT + LTD
   40    CONTINUE
         DAYNO = DAYNO + DAYNOD
   50 CONTINUE
      STOP
      END
