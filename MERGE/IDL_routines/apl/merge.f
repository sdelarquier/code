	program MERGE
c	*************

c**********************************************************************
c Program to merge radial velocities from a pair of radars.
c All vectors (k1, k2, V, and B) are expressed in the horizontal
c local cartesian frame with x>0 southward, y>0 eastward, and
c z>0 upward.
c For the format of the output file (merge.dat), see the
c "write(unit_dat)".
c
c Features handled by Version 2.1
c --------------------------------
c - The azimuth correction for the cone angle effect is done.
c - The Earth is supposed to be a geoide, except for the azimuth
c   correction due to the cone angle effect.
c - Error calculation, Quality indices for vector velocities and maps 
c   are included.
c - Divergence free analysis, for correction and extension of the
c   maps, are included.
c --------------------------------
c
c This program was written in CETP (France). It results from the
c discussions conducted within the "Merging Working group"
c (M. Pinnock, J.-C. Cerisier, C. Hanuise, M. Ruohoniemi, C. Senior).
c It is described in the postcript document 'MERGE: A Fortran Program'.
c
c For any inquiry, please contact C. Senior or J.-C. Cerisier at:
c   Catherine.Senior@cetp.ipsl.fr
c   Jean-Claude.Cerisier@cetp.ipsl.fr
c
c	$Log:	merge.f,v $
c Revision 2.4  95/01/11  13:08:17  13:08:17  senior (Catherine SENIOR)
c - The output file is now opened in MERGE_INFO, with a name of the form YYMMDDX.mrg
c (stnd or not), where X is the symbol for the pair.
c - The program now starts properly if the start time given is less than the start
c times of the .fit files
c 
c Revision 2.3  94/11/23  16:29:10  16:29:10  senior (Catherine SENIOR)
c Allows for Standard Options. 
c Results from standard runs are kept in merge_stnd.dat
c Results from non standard runs are kept in merge.dat
c 
c Revision 2.2  94/09/07  17:56:55  17:56:55  senior (Catherine SENIOR)
c Got rid of '$' characters in the Revision number recorded in 'merge.dat'
c IDL does not read easily these '$' characters.
c 
c Revision 2.1  94/09/06  15:10:11  15:10:11  senior (Catherine SENIOR)
c First version to be released into the Leicester Software Depository
c 
c**********************************************************************

	include 'info.inc'
	parameter (ng = 75, npts = 25)
	integer*2 unit_dat, unit_prt, unit_los                        !JMR
	integer*2 sdate(2,6),edate(2,6)
	integer*2 mhz(2),khz(2),lagfr(2),smsep(2),intt(2),nrang(2)
	integer*2 old_lag(2),old_sep(2)
	integer*2 n(2,0:15,0:15), rng(2,npts,0:15,0:15)
	integer*2 gsc(2,0:15,ng),n_avg(2,0:15,ng)
	integer*2 pnt_qlty(0:15,0:15),ngood,lgood                     !JMR
	integer*4 t,st,et,time_change,CNV_MDHMS_SEC
	integer*4 QUALITY_MAP,map_qlty
	integer*4 length, TRIM
	real*4    pos1(5), pos2(5), pos(2,5)
	real*4	  v_height(2,0:15,ng),range(2,ng)
	real*4	  az(2,-1:16,-1:16), abb(0:15,0:15),alt(-1:16,-1:16)
	real*4    latg(-1:16,-1:16), long(-1:16,-1:16)
	real*4    latm(0:15,0:15), lonm(0:15,0:15)
	real*4    bx(0:15,0:15), by(0:15,0:15), bz(0:15,0:15)
	real*4    kx(2,0:15,0:15), ky(2,0:15,0:15), kz(2,0:15,0:15)
	real*4	  vlos(2,0:15,0:15)
	real*4    p(2,0:15,ng),v(2,0:15,ng)
	real*4	  verr_v(2,0:15,ng), verr_w(2,0:15,ng)
	real*4	  width(2,0:15,ng),werr(2,0:15,ng)
	real*4    el(2,0:15,ng),el_low(2,0:15,ng),el_high(2,0:15,ng)
	real*4    vx(0:15,0:15), vy(0:15,0:15), vz(0:15,0:15)
	real*4    vx_erv(0:15,0:15),vy_erv(0:15,0:15),vz_erv(0:15,0:15)
	real*4    vx_erw(0:15,0:15),vy_erw(0:15,0:15),vz_erw(0:15,0:15)
	logical*1 bad(2,0:15,ng),fgood(0:15,0:15),fvlos(2,0:15,0:15)
	logical*1 flag_data,flag_change
	character*20 rcs_rev
	common/radars/pos
	common/hght/v_height
	common/gate/range
	common/date/sdate,edate
	common/param/mhz,khz,lagfr,smsep,intt,nrang
	common/old_parm/old_lag,old_sep
	common/vradial/vlos,fvlos
	common/table/latg,long,alt,latm,lonm,az,abb,n,rng,
     &			bx,by,bz,kx,ky,kz
	common/data/p,v,verr_v,verr_w,width,werr,el,el_low,el_high,
     &			bad,gsc,n_avg
	data unit_dat/3/, unit_prt/4/, unit_los/9/                    !JMR
c_______________________________


	open(unit_prt,file='merge.prt',status='unknown',
     &			form='formatted')


c	open(unit_los,file='merge.los',status='unknown',              !JMR
c     &			form='unformatted')                             !JMR
c

	rcs_rev = '$Revision: 2.4 $'
	length = TRIM(rcs_rev)
	rcs_rev(1:1) = ' '
	rcs_rev(length:length) = ' '
	print 88, rcs_rev
	write(unit_prt,88) rcs_rev
 88	format(//,' MERGE: ',a20,//)

	call MERGE_INFO

c	print*,' after call to MERGE_INFO tperiod,tstep = ',tperiod,tstep

c	print*,' rcs_rev = ',rcs_rev
c	print*,(infi(i),i=1,27)
c	print*,(infr(i),i=1,8)

	version = 3.0                           !JMR testing junk

						!writes the general
	write(unit_dat) version,infi,infr	!header on output file
c	write(unit_dat) rcs_rev,infi,infr	!header on output file
c	---------------

c... Makes the first read of data to initialize parameters ...

	map = 0
	st = CNV_MDHMS_SEC(syr,smo,sday,shr,smin,ssec)
	et = CNV_MDHMS_SEC(eyr,emo,eday,ehr,emin,esec)

	call RADAR_POS(st1,syr,st,pos1)
	call RADAR_POS(st2,syr,st,pos2)
	do j=1,5
	  pos(1,j) = pos1(j)
	  pos(2,j) = pos2(j)
	enddo

	t = st
   2	call READ_INIT(t,ierror)
	if(ierror.ne.0) then
	   t = t + tperiod                                          !CSR
	   t = t + tstep                                            !JMR
	   go to 2
	endif


	write(unit_prt,303) st1,lagfr(1),smsep(1),intt(1),nrang(1)
	write(unit_prt,304) st2,lagfr(2),smsep(2),intt(2),nrang(2)
	print 303,          st1,lagfr(1),smsep(1),intt(1),nrang(1)
	print 304,          st2,lagfr(2),smsep(2),intt(2),nrang(2)
 303	format(/,' Radar1 ident = ',i2,',  lagfr,smsep,intt,nrang ',4i6)
 304	format(  ' Radar2 ident = ',i2,',  lagfr,smsep,intt,nrang ',4i6)

c... Makes (or read) the lookup table if elev. angle
c... information has not to be used (ialt=1 or 2) ...

	if(ialt.ne.3) then
c	  call LOOK_TABLE(ier)
c	  if(ier.eq.1) then
	    call VIRTUAL
	    call MAKE_TABLE
c	  endif
	endif

c************************************
c... Starts Data Processing ...
c************************************

  1	map = map + 1
	write(unit_prt,200) map
	print 200,          map
 200	format(/,' Map ',i3)

	call READ_SCAN(t,flag_data,flag_change,time_change)

	if(flag_data) then

	  if(ialt.eq.3) then			!compute lookup table
	    call VIRTUAL			!parameters for each
	    call MAKE_TABLE
	  endif					!angle is used

	  call MERGE_VEC(ngood,fgood,vx,vy,vz,vx_erv,vy_erv,vz_erv,
     &			 vx_erw,vy_erw,vz_erw,pnt_qlty)

	  if(idiv.eq.1) then
	    call HOR_PROJ(+1,vx,vy,vz,bx,by,bz)
	    call DIV_FREE(vx,vy,latg,long,az,fgood,niter)
	    if(iext.eq.1) call DIV_EXTEND(vx,vy,latg,long,az,fgood,ngood)
	    call HOR_PROJ(-1,vx,vy,vz,bx,by,bz)
	  endif

					!quality index for the map
	  map_qlty = QUALITY_MAP(ngood,fgood,pnt_qlty)

	endif

c... Writes header for the scan on output file ...


	print*,' about to do my write-out..'

	lgood = 16*16                                                    !JMR..
	do ib = 0,15                                                    
	  do jb = 0,15
	    do k = 1,2                                             
	      if (fvlos(k,ib,jb).eq. .false.) then 
		vlos(k,ib,jb) = 9999
	      endif
	    end do
	  end do
	end do

       	write(unit_los) sdate,edate,lagfr,smsep,map_qlty,lgood,niter,height_r
       	print*,         sdate,edate,lagfr,smsep,map_qlty,lgood,niter,height_r
       	if (lgood.ne.0) then
	  do ib = 0,15
	    do jb = 0,15
	      write(unit_los) ib,jb,latg(ib,jb),long(ib,jb),
     &                              latm(ib,jb),lonm(ib,jb),
     &	                            vlos(1,ib,jb),vlos(2,ib,jb),
     &                              bx(ib,jb),by(ib,jb),bz(ib,jb)
	    end do
	  end do
	endif
                                                                        !..JMR
	print*,' done my writeout!'


	write(unit_dat) sdate,edate,mhz,khz,lagfr,smsep,map_qlty,ngood,
     &			niter,height_r
c	---------------

	write(unit_prt,7040) map_qlty
	write(unit_prt,7050) ngood
	write(unit_prt,7060) niter

 7040	format('  Map quality:		',i8)
 7050	format('  Number of good points:',i8)
 7060	format('  Number of iterations:	',i8)

c... Writes data to output file ...

	if(ngood.ne.0) then
	  do ib=0,15
	    do jb=0,15
	      if(fgood(ib,jb)) then
	        write(unit_dat) ib,jb,
c		---------------
     &			 latg(ib,jb),long(ib,jb),
     &			 latm(ib,jb),lonm(ib,jb),
     &			 bx(ib,jb),by(ib,jb),bz(ib,jb),
     &			 vlos(1,ib,jb),vlos(2,ib,jb),
     &			 vx(ib,jb),vy(ib,jb),vz(ib,jb),
     &			 vx_erv(ib,jb),vy_erv(ib,jb),vz_erv(ib,jb),
     &			 vx_erw(ib,jb),vy_erw(ib,jb),vz_erw(ib,jb),
     &			 pnt_qlty(ib,jb)

c		write(unit_prt,8000) ib,jb
c		write(unit_prt,8010) latg(ib,jb), long(ib,jb)
c		write(unit_prt,8020) latm(ib,jb), lonm(ib,jb)
c		write(unit_prt,8030) vlos(1,ib,jb), vlos(2,ib,jb)
c		write(unit_prt,8040) bx(ib,jb), by(ib,jb), bz(ib,jb)
c		write(unit_prt,8050) vx(ib,jb), vy(ib,jb), vz(ib,jb)
c		write(unit_prt,8060) vx_erv(ib,jb),vy_erv(ib,jb),
c     &				     vz_erv(ib,jb)
c		write(unit_prt,8070) vx_erw(ib,jb),vy_erw(ib,jb),
c     &				     vz_erw(ib,jb),pnt_qlty(ib,jb)

 8000		format('========== Indices i, j:',2i13,' ==========')
 8010		format('GG Latitude and Longitude:',2f14.4)
 8020		format('MG Latitude and Longitude:',2f14.4)
 8030		format('vlos12 :		  ',2f14.3)
 8040		format('Bxyz :			  ',3f14.7)
 8050		format('Vx Vy Vz :		  ',3f14.3)
 8060		format('EVxyz :			  ',3f14.5)
 8070		format('EWxyz and qlty :	  ',3f14.5,i8)
	      endif
	    enddo
	  enddo
	endif

				!start time of next scan
   	if(.not.flag_change) then

c         t = t + tperiod                                       !CSR 

	  t = t + tstep                                         !JMR smooth

	else
	  t = time_change
	  go to 2
	endif

   	if(t.lt.et) go to 1	!process next scan

	stop
	end
