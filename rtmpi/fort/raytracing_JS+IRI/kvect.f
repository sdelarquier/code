      program nitial


      integer kk,numpos,numvec,raynum,dummy
      double precision radpos(500000),thtpos(500000)
      double precision phipos(500000),grppth(500000)
      double precision radvec(500000),thtvec(500000)
      double precision phivec(500000)
      double precision earthr, pi
      double precision FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP
      double precision ELBEG,ELEND,ELSTEP
      character raystr*6

      earthr = 6370.0
      PI = 3.1415926536
      raynum = 0

      open(343,FILE='rays.dat',status='unknown',form='unformatted')
      open(434,FILE='kvect.dat',status='unknown',form='unformatted')
      read(343) FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP,ELBEG,ELEND,ELSTEP
      read(434) FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP,ELBEG,ELEND,ELSTEP
c     write(57,'(9e14.7)')FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP,ELBEG,ELEND,ELSTEP
      do while(.true.)
         read(343,end=100)NUMPOS
         read(434,end=100)numvec
         raynum = raynum + 1

         if(raynum.gt.1)then
            print *,'];'
         endif

         write(unit=raystr,fmt='(I6.6)') raynum
         print *,"ray" // raystr, " = ["

         read(343)(RADPOS(KK), KK=1,NUMPOS),(THTPOS(KK), KK=1,NUMPOS),
     &    (PHIPOS(KK), KK=1,NUMPOS),(grppth(KK), KK=1,NUMPOS)

         read(434)(radvec(kk), kk=1,numvec),(thtvec(kk), kk=1,numvec),
     &    (phivec(kk), kk=1,numvec)

         if (numpos.ne.numvec)then
            print *,'!!! numpos != numvec', numpos, numvec, ' !!!'
            return
         endif

         do kk=1,numpos
            print '(F8.2, F8.2, F8.2, F8.3, F8.3, F8.3)',
     &       radpos(kk)-earthr,90.-(180.*thtpos(kk)/pi),
     &       180.*phipos(kk)/pi,
     &       radvec(kk),thtvec(kk),phivec(kk)
         end do
      end do

 100  if(raynum.gt.0)then
         print *,'];'
c         call plotrays(raynum)
      endif

      close(343)
      close(434)
      end

      subroutine plotrays(raynum)
      integer raynum,idx
      character raystr*6

      idx = 0
c      print *,'hold on;'

      do while(idx.lt.raynum)
        idx = idx+1
        write(unit=raystr, fmt='(I6.6)') idx
        print *,"plot(ray"//raystr, "(:,2), ray"//raystr, "(:,1),'k');"
        print *,'hold on'
      end do
      end
