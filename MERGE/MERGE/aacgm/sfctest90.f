      program sfcconv
c
c     $Revision: 1.1 $
c
c  sfcconv  - program to test the Geographic to/from Geomagnetic Coordinate
c             subprogram to be run at AFSFC
c
c
c  Comments: This program and the SFC subroutine have been developed on a
c            DEC VAX 7000 using VMS FORTRAN and trying to maintain the
c            SFC style of coding.  The calling sequence is identical to the
c            previous version of the subprogram.
c
c     This version has been ported to the unix environment.  The original
c     program used a loop and ran through a set of altitudes from
c     0 km to 2000 km.  It opened the file SFCOUT.DAT for each height
c     and made use of the fact the VMS keeps multiple versions of
c     files to allow you to have a separate file for each altitude.
c
c     For the UNIX implementation the automatic loop through the
c     altitudes has been replaced by a request for the user to
c     input an arbitrary altitude to be used.
c
c     $Log:	sfctest.f,v $
c Revision 1.1  94/10/14  10:59:07  10:59:07  baker (Kile Baker S1G)
c Initial revision
c 
c

C  BEGIN PROGRAM

C  RUN FOR ALTITUDES FROM 0 KM TO 2000 KM

c      DO 200 JH = 0, 20

c      r_height_in = JH * 100.0

      type *,'enter the height to be used'
      accept *,r_height_in
C
C
      OPEN(1,FILE='SFCOUT.DAT',FORM='FORMATTED',
     X     STATUS ='UNKNOWN')

C  RUN FOR ALL GEOGRAPHICS LATITUDES (STEPPING BY 2 DEGREES) AND LONGITUDES
C  (STEPPING BY TEN DEGREES)

      do 99 LAT = 88, -88, -2
        GLAT = FLOAT(LAT)

        do 96 LON = 0, 350, 10
          GLON = FLOAT(LON)

C Convert from Geograhic (Geocentric) to Geomagnetic

      call SFC$$CONVERT_GEO_COORD(GLAT,
     $                                  GLON,
     $                                  R_HEIGHT_IN,
     $                                  CGLAT,
     $                                  CGLON,
     $                                  1,
     $                                  I_ERROR)
      if (I_ERROR .ne. 0) type *,'err: glat, glong, i_err',GLAT, GLON, I_ERROR

C Convert back to Geographics as a sanity check

      call SFC$$CONVERT_GEO_COORD(CGLAT,
     $                                  CGLON,
     $                                  R_HEIGHT_IN,
     $                                  GMLAT_,
     $                                  GMLON_,
     $                                  2,
     $                                  I_ERROR)
      if (I_ERROR .ne. 0) type *,'err: mlat, mlong, i_err',CGLAT, CGLON, I_ERROR

C
      write(1,1000) R_HEIGHT_IN, GLAT, GLON, CGLAT, CGLON,
     X GMLAT_, GMLON_
  96  continue
C
  99  continue

      CLOSE(1)
200   CONTINUE
1000  format(3f8.2,4F13.5)
      stop
      end

