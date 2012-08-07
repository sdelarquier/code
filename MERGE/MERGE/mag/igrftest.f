c SD_BEGIN>
      implicit real*8 (a-h,o-z)
      integer*2 ifrst
      real*8 x,y,z
      ifrst=1
      date=1993.0
      flat=65.
      flon=300.
      elev=600.
      call igrfcall(ifrst,date,flat,flon,elev,x,y,z)
      print*,x,y,z
      stop
      end
      
C<SD_END 
