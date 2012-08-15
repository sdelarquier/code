      SUBROUTINE aacgm_init( filename, ascii_flag)
      IMPLICIT NONE
C      logical*1 filename(80), ascii_flag	! f90; 20000909; DA
      logical*1 ascii_flag
      character filename(80)
      INTEGER I_NUM_TERMS, I_NUM_AXES, I_NUM_LEVEL, I_NUM_FLAG
      PARAMETER (I_NUM_TERMS = 121)
      PARAMETER (I_NUM_AXES  =   3)
      PARAMETER (I_NUM_LEVEL =   3)
      PARAMETER (I_NUM_FLAG  =   2)
C
C     This program is designed to initialize the common
C     block used by the AACGM coordinate conversion
C     program, SFC$$convert_geo_coord.f.
C
C     The program, SFC$$convert_geo_coord.f has a set of
C     DATA statements which initialize the coordinate
C     conversion to the current epoch using the AACGM
C     coordinate system.  If the AACGM coordinates for
C     an earlier epoch are desired, or some other
C     coordinate system is desired, the common block
C     can be reinitialized using this subroutine.
C
C     The subroutine reads the data for the common block
C     from a named file.  The data file can be either
C     and ASCII file, or a binary data file.  
C
C     The coefficients are ordered according to the
C     standard ordering of data in FORTRAN.
C
C     CALLING SEQUENCE:
C        call AACGM_INIT( filename, ascii_flag)
C          where filename is the name of the file containing
C          the coefficients, and is a logical*1 array of characters
C          where ascii_flag is a logical*1 flag.  It is set
C          to .TRUE. if the file contains ASCII data and .FALSE.
C          if the file contains binary data.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     $Log: aacgm_init.f,v $
c Revision 1.2  1996/03/11  21:43:13  baker
c *** empty log message ***
c
c Revision 1.2  1996/03/11  21:43:13  baker
c *** empty log message ***
c
c Revision 1.1  1996/03/11  19:21:34  baker
c Initial revision
c
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      integer fnum/71/,i,t,a,l,f
      double precision D_COEF(I_NUM_TERMS, I_NUM_AXES, I_NUM_LEVEL,
     $     I_NUM_FLAG)
      character*80 fname

      common /SFC$$COEFFS_COM/D_COEF

      do i=1,80
         fname(i:i) = filename(i)
         enddo
      if (.NOT. ascii_flag) then
         open(unit=fnum,form='unformatted', type='old',name=fname)
         read(fnum)((((d_coef(t,a,l,f),t=1,I_NUM_TERMS),a=1,I_NUM_AXES),
     $        l=1,I_NUM_LEVEL),f=1,I_NUM_FLAG)
      else
         open(unit=fnum,form='formatted',type='old',name=fname)
         read(fnum,*)((((d_coef(t,a,l,f),t=1,I_NUM_TERMS),
     $        a=1,I_NUM_AXES),l=1,I_NUM_LEVEL),f=1,I_NUM_FLAG)
         endif
      close(fnum)
      return
      end
