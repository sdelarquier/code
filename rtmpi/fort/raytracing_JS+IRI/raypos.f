      program nitial

      integer kk,numpos,raynum,dummy
      double precision radpos(500000),thtpos(500000)
      double precision phipos(500000),grppth(500000)
      double precision earthr
      double precision FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP
      double precision ELBEG,ELEND,ELSTEP
      character raystr*6

      earthr = 6370.0
      raynum = 0

      open(343,FILE='rays.dat',status='unknown',form='unformatted')
      read(343) FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP,ELBEG,ELEND,ELSTEP
c     write(57,'(9e14.7)')FBEG,FEND,FTEP,AZBEG,AZEND,AZSTEP,ELBEG,ELEND,ELSTEP
      do while(.true.)
         read(343,end=100)NUMPOS
         raynum = raynum + 1

         if(raynum.gt.1)then
            print *,'];'
         endif

         write(unit=raystr,fmt='(I6.6)') raynum
         print *,"ray" // raystr, " = ["

         read(343)(RADPOS(KK), KK=1,NUMPOS),(THTPOS(KK), KK=1,NUMPOS),
     &    (PHIPOS(KK), KK=1,NUMPOS),(grppth(KK), KK=1,NUMPOS)

         do kk=1,numpos
            print *,radpos(kk)-earthr,' ',thtpos(kk),' ',phipos(kk)
         end do
      end do

 100  if(raynum.gt.0)then
         print *,'];'
      endif

      close(343)
      end

