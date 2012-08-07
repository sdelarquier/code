c*****************************************************************************c
c    This subroutine may be used to filter the velocity map of each scan.
c
c Logic: Each beam-gate cell and its 3x3 template is examined in turn. The
c	 velocity values and scatter types are recorded into arrays.  If it
c        has valid data, the velocity of the center cell is counted twice.
c        The number (itemnum)  of valid velocity entries over the template is
c        determined, and 10 is the maximum count (3X3 + 1).  The number 
c        (ntemnum) of cells on the adjacent beams at the same range with valid
c        data are also counted (1 or 2).
c
c        (For beam-edge and range-edge cells, the templates are 2X3 and 3X2,
c        respectively.)
c
c        The velocity value at the center cell will
c        be rewritten with the median value of the velocity over the sampling 
c        template and the scatter type will be reassigned accordingly if the
c        number of valid counts (itemnum) >6 (>5, beam-edge) and the number of
c        adjacent beams (ntemnum) >0.  A cell that is initially off will be
c        turned on and assigned the median velocity value and its associated
c        type if the number of valid counts >7 (>5).
c
c        In all other cases, the cell will be turned off.
c
c >>     Median filtering is very effective at eliminating spurious data while
c        preserving structure.  The choices for filter criteria were supplied
c        by JMR (Dec 19/94).  The check on the number of adjacent beams 
c        was found to handle the problem of the occassional bad beam quite
c        well.
c
c ** Note that the beam indices within the subroutines are shifted by 1,
c ** i.e. (0:15) -> (1:16). 
c
c $Log:	median_filter.f,v $
c Revision 1.5  95/01/16  10:07:59  10:07:59  miker (Michael Ruohoniemi S1G)
c The non-standard def'n of 'new' arrays was changed with the add'n of a PARAMETER command
c 
c Revision 1.4  95/01/09  16:29:18  16:29:18  miker (Michael Ruohoniemi S1G)
c added the Log in the comments, for future use.
c 
c
c
*****************************************************************************

	subroutine median_filter(kbad,vel,gscat,ifilt,dim_1,dim_2)
	implicit   none
	integer*2  ifilt,dim_1,dim_2
	real*4     vel(dim_1,dim_2)
	logical*1  kbad(dim_1,dim_2),gscat(dim_1,dim_2)
c
c	call the median filter routine
c
	print*,' Warning! This is the non-standard median_filter routine!'
	print*,' The bad-beam check has been turned off!'
	print*,' ifilt = ',ifilt

	call median_sub(kbad,vel,gscat,ifilt,dim_1,dim_2)

	return
	end


c******************************************************************************
c	subroutine median filters the velocity map using median (Tukey)
c	filter. The input parameter is the velocity map which will
c	contain the filtered velocity upon returning to the calling 
c	routine.
c
	subroutine median_sub(kbad,vel,gscat,ifilt,dim_1,dim_2)
	implicit   none
	integer*2  ifilt,dim_1,dim_2
	logical*1  kbad(dim_1,dim_2),gscat(dim_1,dim_2)
	real*4     vel(dim_1,dim_2)
	integer*2  beam,range,bbeam,rrange
	logical*1  kbad_pix,gscat_pix
	real*4     vel_pix
	parameter(bbeam=16, rrange=75)
	logical*1  newkbad(bbeam,rrange),newgscat(bbeam,rrange)
	real*4     newvel(bbeam,rrange)

	do beam = 1,dim_1
	   do range = 1,dim_2

	      call tukey_sub(beam,range,kbad,vel,gscat,
     #                            kbad_pix,vel_pix,gscat_pix,ifilt,dim_1,dim_2)

	      newkbad(beam,range)  =  kbad_pix
	      newvel(beam,range)   =  vel_pix
	      newgscat(beam,range) =  gscat_pix

	   end do
	end do

	do beam = 1,dim_1                               !refill the work arrays
	  do range = 1,dim_2

	    kbad(beam,range)  =  newkbad(beam,range)
	    vel(beam,range)   =  newvel(beam,range)
	    gscat(beam,range) =  newgscat(beam,range)

	   end do
	end do

	return
	end

c******************************************************************************
c	Function tukey returns median (tukey) filtered value of the velocity
c	of the range and beam as specified by the input parameters.
c	The function is called tukey(beam,range,vel) where beam and range are
c       beam and range of the radar and vel is the velocity map array.
c
c
	subroutine tukey_sub(beam,range,kbad,vel,gscat,
     #                            newkbad,newvel,newgscat,ifilt,dim_1,dim_2)

	implicit   none
	integer*2  beam,range
	integer*2  ifilt,dim_1,dim_2
	logical*1  kbad(dim_1,dim_2),gscat(dim_1,dim_2)
	real*4     vel(dim_1,dim_2)
	integer*2  itemnum,ntemnum
	real*4     item(10),item_g(10),item_b(10)
C	integer*2  rang,bm,index,med	! f90; 20000909; DA
	integer*4  rang,bm,index,med
	logical*1  newkbad,newgscat
	real*4     newvel
	integer*2  template(0:2)/-1,0,1/
	integer*2  i
	integer*2  npmin_ib_py,npmin_eb_py,npmin_ib_pn,npmin_eb_pn
        integer*2  npmin_nb,ifilt_bak
	integer*2  npmin_py(3),npmin_pn(3),npmin_nr(3)
	data       npmin_py/4,5,5/                     !min # of counts pix on
	data       npmin_pn/5,5,6/                     !min # of counts pix off
	data       npmin_nr/0,0,1/                     !min # of neighbours
	data       ifilt_bak/0/

	npmin_ib_py =  npmin_py(ifilt)
	npmin_eb_py =  NINT(.67*npmin_ib_py)

	npmin_ib_pn =  npmin_pn(ifilt)
	npmin_eb_pn =  NINT(.67*npmin_ib_pn)

	npmin_nb    =  npmin_nr(ifilt)

	if (ifilt.ne.ifilt_bak) then
	  print*,' in tukey, ifilt = ',ifilt
	  print*,' npmin_ib_py, npmin_ib_pn = ',npmin_ib_py,npmin_ib_pn
	  print*,' npmin_eb_py, npmin_eb_pn = ',npmin_eb_py,npmin_eb_pn
          print*,' npmin_nb = ',npmin_nb
          ifilt_bak = ifilt
        endif

	index    =  0
	itemnum  =  0
	ntemnum  =  0

	do index = 0,8
	  rang   = range + template(mod(index,3))
	  bm     = beam  + template(index/3)


	  if ((rang.ge.1).and.(rang.le.dim_2).and.(bm.ge.1).and.
     #                                                     (bm.le.dim_1)) then 

	    if (.not.kbad(bm,rang)) then
	      itemnum           =  itemnum + 1    !number of pixels that are on
	      item(itemnum)     =  vel(bm,rang)
	      i= gscat(bm,rang) 
	      item_g(itemnum)   =  i	!int( gscat(bm,rang))
	      item_b(itemnum)   =  vel(bm,rang)

	      if (bm.eq.beam .and. rang.eq.range) then
	        itemnum           =  itemnum + 1    !count centre pixel twice
	        item(itemnum)     =  vel(bm,rang) 
		i= gscat(bm,rang) 
		item_g(itemnum)   =  i	!int( gscat(bm,rang))
	        item_b(itemnum)   =  vel(bm,rang)
	      endif

	      if (bm.ne.beam .and. rang.eq.range) then
	        ntemnum           =  ntemnum + 1    !count neighbouring beams
	      endif

	    end if
	  end if
	end do


c here we identify conditions for which med-filtering will NOT be done; the
c pixel (if on) will be turned off

	if (beam.gt.1 .and. beam.lt.16) then                       !inner beams

	  if ( .not.kbad(beam,range) ) then                        !pixel is on

	    if (itemnum.lt.npmin_ib_py .or. ntemnum.lt.npmin_nb) then
	      newvel   =  10000.
              newkbad  = .true.  	   !not enough points for median filter
	      newgscat = .true.
	      return
	    endif

	  else                                                    !pixel is off
	    if (itemnum.lt.npmin_ib_pn .or. ntemnum.lt.npmin_nb) then
	      newvel   =  10000.               
              newkbad  = .true.            !not enough to turn on
	      newgscat = .true.
	      return
            endif

	  end if

	else                                                       !outer beams

	  if ( .not.kbad(beam,range) ) then                        !pixel is on

	   if (itemnum.lt.npmin_eb_py .or. ntemnum.lt.npmin_nb) then   
	      newvel   =  10000.                                     
              newkbad  = .true.  	   !not enough points for median filter
	      newgscat = .true.
	      return
	    endif
	  else                                                    !pixel is off

	    if (itemnum.lt.npmin_eb_pn .or. ntemnum.lt.npmin_nb) then
	      newvel   =  10000.               
              newkbad  = .true.                 !not enough to turn on
	      newgscat = .true.
	      return
	    endif

	  end if

	endif
	
	med     =  itemnum/2 + 1                    !calculate the median index
	
	call quicksort(item,itemnum)                !now sort the item

   	newvel  =  item(med)
	newkbad = .false.

	do i = 1,itemnum                            !reassign scatter type
	  if (newvel.eq.item_b(i)) then             !to that of the median 
	    newgscat = int( item_g(i))                    !value
	  endif
	enddo

	return
	end









