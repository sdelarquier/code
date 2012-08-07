	subroutine MAKE_TABLE
c	*********************
c
c*********************************************************************
c Makes the Lookup table. The parameters of the table are kept in
c the commons 'gate' and 'table'. They include:
c
c	- range:  list of slant ranges
c	- latg, long: geocentric coordinates of the centers of the
c		      diamonds
c	- alt: 	real altitude of the center of the diamonds
c	- latm, lonm: magnetic coordinates of the centers of the
c		      diamonds
c	- az : The azimuths of radar1 and radar2 from the centers of
c		      the diamonds
c	- abb: Angle between the two radar beams at the centers of the
c		      diamonds
c	- bx, by, bz: The magnetic field components at the centers of
c		      the diamonds
c	- kx, ky, kz: The unit k vectors representing the directions of
c		      the radars at the centers of the diamonds
c	- nb:  The number of ranges which come into each diamond, for the
c	       two radars
c	- rng: The list   of ranges which come into each diamonds
c 
c Last written: Oct. 93 (CS)
c
c NOTE:	* latg,long, alt, and az are dimensionned from -1 to 16
c	  to create virtual beams in case the divergence-free analysis
c	  is to be done.
c	* alt is calculated from the formula
c		alt = 100 + (virtual_height-100) * 0.75
c
c	$Log:	make_table.f,v $
c Revision 1.3  94/11/04  14:32:29  14:32:29  senior (Catherine SENIOR)
c Call to TRIGO routines has changed to take into account the
c singularities at geographic pole (in the south). Also, the
c projection of the map to the reference altitude height_r
c has changed
c 
c Revision 1.2  94/10/20  12:09:24  12:09:24  senior (Catherine SENIOR)
c Brings the geographic and geomagnetic longitude of the grid point
c to the ]-180.,180.] interval (end of the routine)
c 
c Revision 1.1  94/09/01  16:13:50  16:13:50  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.3 $
c*********************************************************************

	include   'info.inc'
	parameter (ng = 75, npts = 25)
	integer*2 sdate(2,6),edate(2,6)
	integer*2 mhz(2),khz(2),lagfr(2),smsep(2),intt(2),nrang(2)
	integer*2 order,mgflag,err,range_gate
	integer*2 unit_table
	integer*2 n(2,0:15,0:15),rng(2,npts,0:15,0:15)
	integer*2 ifrst
	real*4    pos(2,5),gdlat(2),gdlon(2)
	real*4	  out_lat,out_lon,out_r
	real*4	  latr(2),lonr(2),azstp(2),boraz(2),rrad(2),heit(2)
	real*4    xr(2),yr(2),zr(2),dd(2)
	real*4    kkx,kky,kkz
	real*4    latp,lonp,rr
	real*4    v_height(2,0:15,ng),range(2,ng)
	real*4	  az0_center(2),az0_edge(2),azc_center(2),azc_edge(2)
	real*4    az_center(2),az_edge(2)
	real*4    bx(0:15,0:15),by(0:15,0:15),bz(0:15,0:15)
	real*4    kx(2,0:15,0:15),ky(2,0:15,0:15),kz(2,0:15,0:15)
	real*4    d(2,0:15,0:16),frho(0:15,0:15),abb(0:15,0:15)
	real*4	  az(2,-1:16,-1:16),alt(-1:16,-1:16)
	real*4    latg(-1:16,-1:16),long(-1:16,-1:16)
	real*4    latm(0:15,0:15),lonm(0:15,0:15)
	common/radars/pos
	common/gate/range
	common/hght/v_height
	common/date/sdate,edate
	common/param/mhz,khz,lagfr,smsep,intt,nrang
	common/table/latg,long,alt,latm,lonm,az,abb,n,rng,
     &		     bx,by,bz,kx,ky,kz
	data unit_table/16/
	data rad_to_deg/57.296/
	SAVE
c__________________________

	print *,' '
	print *,(' Makes the Look-up table')

c... Range gates ...

	do k=1,2
	  do ir=1,ng
	    range_gate = ir
C slant_range uses now frang and rsep; 20021003 DA
	    range(k,ir) =  SLANT_RANGE(lagfr(k)* 0.15,smsep(k)* 0.15,pos(k,5),
     &					0.,range_gate)
	  enddo
	enddo

c... Radar positions ...

	do k=1,2
	  gdlat(k) = pos(k,1)
	  gdlon(k) = pos(k,2)
	  call GEODTGC(+1,gdlat(k),gdlon(k),rrad(k),latr(k),
     &			lonr(k),del)
	  boraz(k) = pos(k,3)
	  azstp(k) = pos(k,4)
	enddo
	method = 1
	if(latr(1).lt.0.) method = 2

c... Initializations

	do ib=0,15
	  do jb=0,15
	    latm(ib,jb) = 0.
	    lonm(ib,jb) = 0.
	    abb(ib,jb) = 0.
	    bx(ib,jb) = 0.
	    by(ib,jb) = 0.
	    bz(ib,jb) = 0.
	    do k=1,2
	      n(k,ib,jb) = 0
	      kx(k,ib,jb) = 0.
	      ky(k,ib,jb) = 0.
	      kz(k,ib,jb) = 0.
	      do l=1,npts
	        rng(k,l,ib,jb) = 0
              enddo
	    enddo
          enddo
        enddo

	do ib=-1,16
	  do jb=-1,16
	    latg(ib,jb) = 0.
	    long(ib,jb) = 0.
	    alt(ib,jb) = 0.
	    do k=1,2
	      az(k,ib,jb) = 0.
	    enddo
	  enddo
	enddo

c... Radar geometry ...

	col1 = 90.- latr(1)
	col2 = 90.- latr(2)
	dlon = lonr(2) - lonr(1)
	call TRIGO1(1,dbr,col2,col1,dlon,ang1,ang2)

c... Computes the ground distances d(k,ib,jb),in degrees, between the
c... radars and the intersection of the beam axis with the edges of
c... the diamonds. Indices are:
c... k, for the radar; ib, for the beam center; jb, for the beam edge.

	do ib=0,15
	  do k=1,2
	    azim = boraz(k)+(float(ib)-7.5)*azstp(k)
	    call GEOCNVRT(gdlat(k),gdlon(k),azim,0.,azi,dum)
	    az0_center(k) = azi
	  enddo
	  do jb=0,16
	    iter = 0
	    do k=1,2
	      azim = boraz(k)+(float(jb)-8.0)*azstp(k)
	      call GEOCNVRT(gdlat(k),gdlon(k),azim,0.,azi,dum)
	      az0_edge(k) = azi
	    enddo
   1	    do k=1,2
	      if(iter.eq.0) then
		az_center(k) = az0_center(k)
		az_edge(k) = az0_edge(k)
	      else
		az_center(k) = azc_center(k)
		az_edge(k) = azc_edge(k)
	      endif
	      az_center(k) = XMOD(az_center(k))
	      az_edge(k) = XMOD(az_edge(k))
	    enddo
	    ang3 = TEST1(ang1 - az_center(1))			!d1
	    ang4 = TEST1(ang2 + az_edge(2))
	    call TRIGO2(1,dbr,dd(2),d(1,ib,jb),dum,ang3,ang4)
	    ang3 = TEST1(ang1 - az_edge(1))			!d2
	    ang4 = TEST1(ang2 + az_center(2))
	    call TRIGO2(1,dbr,d(2,ib,jb),dd(1),dum,ang3,ang4)
c!!!!!!!!!!!!!!!!! only one iteration for the cone angle correction
	    if(iter.eq.0) then
	      do k=1,2
		call CORR_AZIM(+1,k,ib,rrad(k),d(k,ib,jb),
     &				az_center(k),heit(k))
		call CORR_AZIM(-1,k,jb,rrad(k),dd(k),az_edge(k),
     &				heit(k))
		azc_center(k) = az_center(k)
		azc_edge(k) = az_edge(k)
	      enddo
	      iter = iter + 1
	      go to 1
	    endif
	  enddo
	enddo

c... Computes geocentric latg, long, and Earth radius at the center ...
c... M of the cell (NG is the geographic north pole)
c... Latg, long, and az are extended to virtual beams -1 and 16 in
c... case the divergence-free analysis is to be done ...

        do ib=-1,16
	  do jb =-1,16
	    do k=1,2
	      if(k.eq.1) im = ib
	      if(k.eq.2) im = jb
	      azim = boraz(k)+(float(im)-7.5)*azstp(k)
	      call GEOCNVRT(gdlat(k),gdlon(k),azim,0.,azi,dum)
	      az0_center(k) = azi
	    enddo
	    iter = 0
   2        do k=1,2
	      if(iter.eq.0) then
	        az_center(k) = az0_center(k)
	      else
		az_center(k) = azc_center(k)
	      endif
	      az_center(k) = XMOD(az_center(k))
	    enddo
	    ang3 = TEST1(ang1 - az_center(1))    
	    ang4 = TEST1(ang2 + az_center(2))
						!solves R1-R2-M
	    call TRIGO2(1,dbr,dd(2),dd(1),angle,ang3,ang4)
	    if(iter.eq.0) then
	      do k=1,2
		if(k.eq.1) im = ib
		if(k.eq.2) im = jb
		call CORR_AZIM(+1,k,im,rrad(k),dd(k),az_center(k),
     &				heit(k))
	        azc_center(k) = az_center(k)
	      enddo
	      alt(ib,jb) = (heit(1)+heit(2))/2.
	      alt(ib,jb) = 100. + (alt(ib,jb)-100.) * 0.75
	      iter = iter + 1
	      go to 2
	    endif
	    if(angle.gt.90.) angle = 180.- angle
	    r1 = TEST1(az_center(1))                          !solves NG-R1-M
	    call TRIGO1(method,colatg,col1,dd(1),r1,gm,gn)
	    latg(ib,jb) = 90.- colatg
	    gn = TEST2(az_center(1),gn)
	    long(ib,jb) = XMOD(lonr(1) + gn)
	    gm = sign(gm,-az_center(1))
	    az(1,ib,jb) = XMOD(gm)
	    r2 = TEST1(az_center(2))                          !solves NG-R2-M
	    call TRIGO1(method,dum1,col2,dd(2),r2,gm,dum2)
	    gm = sign(gm,-az_center(2))
	    az(2,ib,jb) = XMOD(gm)
	    if(ib.ge.0.and.ib.le.15.and.jb.ge.0.and.jb.le.15) then
     	      abb(ib,jb) = angle
	      call GEODTGC(-1,dum1,dum2,frho(ib,jb),latg(ib,jb),
     &			   long(ib,jb),del)
	    endif
	  enddo
        enddo

c... Determines the number and list of ranges falling into the diamonds

        do ib=0,15					!for radar1
	  do jb=0,15
	    m = 0
	    do ir=1,ng
	      rn = range(1,ir)
	      heit(1) = v_height(1,ib,ir)
c!!!!!! The Earth radius at ranges falling into the cell (ib,jb) is
c!!!!!! approximatted by its value at the center of the cell
	      rho = frho(ib,jb) + heit(1)
	      if(rn.le.(rho-rrad(1))) go to 300
	      w = (rrad(1)**2+rho**2-rn**2)/(2.*rrad(1)*rho)
	      r = acosd(w)
	      if((r.ge.d(1,ib,jb)).and.(r.lt.d(1,ib,jb+1)))then
	        m = m + 1
	  	n(1,ib,jb) = n(1,ib,jb) + 1
	        rng(1,m,ib,jb) = ir
	      endif
  300         continue
            enddo
	  enddo 
        enddo

        do ib=0,15					!for radar2
          do jb=0,15
	    m = 0
	    do ir=1,ng
	      rn = range(2,ir)
	      heit(2) = v_height(2,jb,ir)
	      rho = frho(ib,jb) + heit(2)
	      if(rn.le.(rho-rrad(2))) go to 400
	      w = (rrad(2)**2+rho**2-rn**2)/(2.*rrad(2)*rho)
	      r = acosd(w)
	      if((r.ge.d(2,jb,ib+1)).and.(r.lt.d(2,jb,ib)))then
	  	m = m + 1
		n(2,ib,jb) = n(2,ib,jb) + 1
		rng(2,m,ib,jb) = ir
              endif
  400	      continue
	    enddo
          enddo
        enddo

	do k=1,2
	  call GSPTCAR(+1,rrad(k),latr(k),lonr(k),xr(k),yr(k),zr(k))
	enddo
	order = 4
	mgflag = 1

c... Computes other parameters ...

	tm = sdate(1,1)
	ifrst = 1
	call MAGCMP(tm,rho,latg(0,0),long(0,0),bbx,bby,bbz,b,ifrst)
	ifrst = 0

	do ib=0,15
	  do jb=0,15

c... Computes the k and b vectors ...
	    rho = frho(ib,jb) + alt(ib,jb)
            call GSPTCAR(+1,rho,latg(ib,jb),long(ib,jb),xm,ym,zm)
            call MAGCMP(tm,rho,latg(ib,jb),long(ib,jb),
     &      		bbx,bby,bbz,b,ifrst)
	    bx(ib,jb) = bbx ! * 1.e-5  removed conversio to Gauss DA
	    by(ib,jb) = bby ! * 1.e-5  mag values now in nT
	    bz(ib,jb) = bbz ! * 1.e-5
	    call NORM_VEC(bbx,bby,bbz)
	    do k=1,2
	      xx = xr(k) - xm
	      yy = yr(k) - ym
	      zz = zr(k) - zm
	      call NORM_VEC(xx,yy,zz)
              call GLBTHOR(+1,latg(ib,jb),long(ib,jb),xx,yy,zz,
     &  		    kkx,kky,kkz)
	      call NORM_VEC(kkx,kky,kkz)
	      kx(k,ib,jb) = kkx
	      ky(k,ib,jb) = kky
	      kz(k,ib,jb) = -(kkx*bbx + kky*bby)/bbz
	      call NORM_VEC(kx(k,ib,jb),ky(k,ib,jb),kz(k,ib,jb))
	    enddo

c... Projects the center of the diamond to the standard altitude
c... height_r along the magnetic field (B and K are not recalculated)

	    rr = height_r+frho(ib,jb)
	    call PROJECT_B(rr,latp,lonp,rho,latg(ib,jb),long(ib,jb),
     &			   bbx,bby,bbz)
	    latg(ib,jb) = latp
	    long(ib,jb) = lonp

c... Computes geomagnetic position ...
	    call CNV$COORD(latg(ib,jb),long(ib,jb),height_r,order,
     &			out_lat,out_lon,out_r,mgflag,err)
	    latm(ib,jb) = out_lat
	    lonm(ib,jb) = out_lon
	    long(ib,jb) = XMOD(long(ib,jb))
	    lonm(ib,jb) = XMOD(lonm(ib,jb))

	  enddo
        enddo

c... Records lookup table if it has to be used for several scans
c... (cases when ialt=1 or 2)
C Record the table in any case DA

	write(unit_table) st,lag,smp,ialt,height

c	if(ialt.ne.3) then
	  write(unit_table) range
	  write(unit_table) latg,long,alt,latm,lonm,az,abb,
     &			    n,rng,bx,by,bz,kx,ky,kz
c	endif

	return
	end

