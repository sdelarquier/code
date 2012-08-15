	subroutine LOOK_TABLE(ier)
c	**************************

c****************************************************************
c This routine searches for the lookup table.
c
c	ier = 0:	The correct lookup table exists
c	ier = 1:	the correct lookup table does not exists
c
c If ier = 1, the routine writes on unit_table the header of
c the lookup table, and the routine MAKE_TABLE, called by the
c main program computes the other parameters of the lookup table.
c
c If ier = 0, the routine reads the lookup table parameters.
c
c Last written: Nov. 93 (CS)
c
c	$Log:	look_table.f,v $
c Revision 1.1  94/09/01  16:13:49  16:13:49  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c****************************************************************

	include	  'info.inc'
	parameter (ng = 75, npts = 25)
	integer*2 st(2),st_table(2)
	integer*2 lag(2),lag_table(2)
	integer*2 smp(2),smp_table(2)
	integer*2 ialt_table,unit_table
	integer*2 mhz(2),khz(2),intt(2),nrang(2)
	integer*2 n(2,0:15,0:15),rng(2,npts,0:15,0:15)
	real*4	  height_table
	real*4	  range(2,ng)
	real*4	  latg(-1:16,-1:16),long(-1:16,-1:16)
	real*4	  latm(0:15,0:15),lonm(0:15,0:15)
	real*4	  az(2,-1:16,-1:16),abb(0:15,0:15),alt(-1:16,-1:16)
	real*4	  bx(0:15,0:15),by(0:15,0:15),bz(0:15,0:15)
	real*4	  kx(2,0:15,0:15),ky(2,0:15,0:15),kz(2,0:15,0:15)
	logical*1 bad
	common/param/mhz,khz,lag,smp,intt,nrang
	common/gate/range
	common/table/latg,long,alt,latm,lonm,az,abb,n,rng,
     &			bx,by,bz,kx,ky,kz
	data unit_table/16/
c_____________

	st(1) = st1
	st(2) = st2
	ier = 0

	open(unit_table,file='table.dat',status='old',
     &		form ='unformatted',err=6,iostat=ios)

   6	if(ios.ne.0) then
	  open(unit_table,file='table.dat',status='new',
     &		form='unformatted')
	  write(unit_table) st,lag,smp,ialt,height
	  ier = 1
	else
	  bad = .false.
	  read(unit_table) st_table,lag_table,smp_table,
     &				ialt_table,height_table
	  do k=1,2
	    if(st(k).ne.st_table(k)) bad = .true.
	    if(lag(k).ne.lag_table(k)) bad = .true.
	    if(smp(k).ne.smp_table(k)) bad = .true.
	  enddo
	  if(ialt.ne.ialt_table) bad = .true.
	  if((ialt.eq.1).and.(height.ne.height_table)) bad = .true.
	  if(bad) then
	    rewind unit_table
	    write(unit_table) st,lag,smp,ialt,height
	    ier = 1
	  else
	    print *,' The correct lookup table exists: reads it'
	    read(unit_table) range
	    read(unit_table) latg,long,alt,latm,lonm,az,abb,n,rng,
     &			     bx,by,bz,kx,ky,kz
	  endif
	endif

	return
	end
