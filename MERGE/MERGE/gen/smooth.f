c
c	the following subroutines are used by velmap2 to smooth
c	the velocity map of each scan
c
c
c	smooth smooths the velocity map. it firsts reduces the
c	impulse noise using median filter and then smooths the
c	map using neighbohrs averaging technique.
c	the input/output parameter is the velocity map array which
c	is to be smoothed upon returning to the calling routine
c 	
c	Last written:  Mar 6/91
c  
c       $Log:	smooth.f,v $
c Revision 1.2  94/09/06  09:33:03  09:33:03  baker (Kile Baker S1G)
c Corrected bug in "Tukey".  The beam number should be tested against "dim_1"
c not "dim_2".
c 
c Revision 1.1  94/08/04  10:13:59  10:13:59  baker (Kile Baker S1G)
c Initial revision
c 
c
	subroutine smooth(kbad,vel,dim_1,dim_2)
	implicit none
	integer*2 dim_1
	integer*2 dim_2
	real*4 vel(dim_1,dim_2)
	logical*1 kbad(dim_1,dim_2)
c
c	call the neighbor averaging routine
c
	call average(vel,kbad,dim_1,dim_2)
c
c	call the median filter routine
c
	call median(vel,kbad,dim_1,dim_2)

	return
	end


c
c	subroutine median filters the velocity map using median( Tukey)
c	filter. the input parameter is the velocity map which will
c	contain the filtered velocity upon returning to the calling 
c	routine
c
	subroutine median(vel,kbad,dim_1,dim_2)
	implicit none
	integer*2 dim_1
	integer*2 dim_2
	real*4 vel(dim_1,dim_2)
	integer*2 beam,range
c	real*4 tukey
	logical*1 kbad(dim_1,dim_2)
	real*4 newvel(100,100)
	logical*1 newkbad(100,100)

	do beam = 1,dim_1
	   do range = 1,dim_2
	      call tukey(beam,range,vel,
     #        kbad,newvel,newkbad,dim_1,dim_2)
	   end do
	end do

	do beam = 1,dim_1
	   do range = 1,dim_2
	      vel(beam,range) = newvel(beam,range)
	      kbad(beam,range) = newkbad(beam,range)
	   end do
	end do

	return
	end

c
c	function tukey returns median (tukey) filtered value of the velocity
c	of the range and beam as specified by the input parameters.
c	the function is called tukey(beam,range,vel) where
c	beam and range are beam and range of the radar and
c	vel is the velocity map array
c
	subroutine tukey(beam,range,vel,
     #  kbad,newvel,newkbad,dim_1,dim_2)
	implicit none
	integer*2 dim_1,dim_2
	integer*2 beam,range
	real*4 vel(dim_1,dim_2)
	integer*2 itemnum
	real*4 item(9)                 !template item
	real*4 tempitem(9),itemerr(9)
	real*4 itempwr(9)
C	integer*2 rang,bm,index,med	! f90; 20000909; DA
	integer*4 rang,bm,index,med
	logical*1 kbad(dim_1,dim_2)
	logical*1 newkbad(100,100)
	real*4 newvel(100,100)
	integer*2 template(0:2)/-1,0,1/
	logical*1 pixeloff            ! true if there is no need to turnon pixel
C	integer*2 index2,rang2,bm2,centerbm,centerrang, centerindex	! f90; 20000909; DA
	integer*4 index2,rang2,bm2,centerbm,centerrang, centerindex
	reaL*4 upperbound,lowerbound
	real*4 velmin/5000./           !limit on velocity error

	itemnum =0
	index = 0
	pixeloff = .true.
c
c	check if there is a need to turn on pixel
c
	do while((pixeloff).and. (index .le.8))
	   rang = range + template(mod(index,3))
	   bm = beam + template(index/3)
	   if((rang.ge.1).and.(rang.le.dim_2).and.(bm.ge.1).and.
     #     (bm.le.dim_1)) then 
	      if(.not.kbad(bm,rang)) then
	         centerindex = abs(index -8)
	         if(centerindex.eq.index)pixeloff = .false.  !center already on
	         centerrang = rang + template(mod(centerindex,3))
	         centerbm = bm + template(centerindex/3)
	         index2 = 0
	         do while((pixeloff).and. (index2.le.8))
	            rang2 = centerrang + template(mod(index2,3))
	            bm2 = centerbm + template(index2/3)
	            if((rang2.ge.1).and.(rang2.le.dim_2).and.
     #              (bm2.ge.1).and.(bm2.le.dim_1)) then 
	               if(.not.kbad(bm2,rang2))then
	                  pixeloff=.false.
	               end if	 
	            end if
	            index2 = index2 + 1
	         end do
	      end if
	   end if
	   index = index  +1
	end do     	   
	index = 0
c
c	do tukey filter iff there is a need to turn on the center pixel or
c	element
c
	do while((.not.pixeloff).and.(index .le.8))
	   rang = range + template(mod(index,3))
	   bm = beam + template(index/3)
	   if((rang.ge.1).and.(rang.le.dim_2).and.(bm.ge.1).and.
     #     (bm.le.dim_1)) then 
	      if(.not.kbad(bm,rang)) then
	         itemnum = itemnum + 1        !number of pixels that are on
	         item(itemnum) = vel(bm,rang)    !initialize the template item
	         tempitem(itemnum) = vel(bm,rang)
	      end if
	   end if
	   index = index + 1
	end do

	if(itemnum.le.3) then       !all the points are bad
	   newvel(beam,range) = vel(beam,range)
           newkbad(beam,range) = kbad(beam,range)
	   return
	end if

	med = itemnum/2 + 1      !calculate the median index
	
	call quicksort(item,itemnum)  !now sort the item

	index = 1
	do while(item(med).ne.tempitem(index)) !search the error index
	   index = index + 1
	end do

c	upperbound = vel(beam,range) + 2.*velerr(beam,range)
c	lowerbound = vel(beam,range) - 2.*velerr(beam,range)
c	if((item(med).gt.lowerbound).and.
c     #     (item(med).lt.upperbound))then
c	   newvel(beam,range) = vel(beam,range)
c	   newvelerr(beam,range) = velerr(beam,range)
c	   newpwr(beam,range) = pwr(beam,range)
c	   return
c	end if
	   
	newvel(beam,range)= item(med)
	newkbad(beam,range) = .false.
	return
	end

c
c	subroutine average performs the neighbor averaging for each beam
c	and range pair of the image
c
	subroutine average(vel,kbad,dim_1,dim_2)
	implicit none
	integer*2 dim_1,dim_2
	integer*2 beam,range
	real*4 vel(dim_1,dim_2)
C	integer*2 rang,bm,index,med	! f90; 20000909; DA
	integer*4 rang,bm,index,med
	logical*1 kbad(dim_1,dim_2)
	integer*2 template(0:2)/-1,0,1/
	real*4 weight
	real*4 sumweight,sumwvel,sumwpwr,sumerr
	logical*1 pixeloff
	integer*2 num
	real*4 averaged,upperbound,lowerbound
	real*4 velmin /5000./

	do beam = 1,dim_1
	   do range = 1,dim_2
	      sumweight = 0.
	      sumwpwr = 0.
	      num = 0
	      sumwvel = 0.
	      sumerr = 0.
	      index = 0
	      pixeloff = .true.
c
c	      perform averaging only when the center pixel is on
c
	      if(.not.kbad(beam,range)) then
	         do while(index.le.8)
	            rang = range + template(mod(index,3))
	            bm = beam + template(index/3)
	            if((rang.ge.1).and.(rang.le.dim_2).and.(bm.ge.1).and.
     #              (bm.le.dim_1)) then 
	            if(.not.kbad(bm,rang))then
c		          if((beam.ne.bm).or.(range.ne.rang)) then
		          if(beam.ne.bm) then
	                     pixeloff =.false.
	                  end if
	                  num = num + 1
	                  sumwvel = sumwvel + vel(bm,rang)
	               end if
	            end if
	            index = index + 1
	         end do
c
c	         if there is no data at the neighboring beam then turn the
c	         pixel off
c
	      end if
	         if((beam.gt.1).and.(beam.lt.dim_1)) then
	            if(pixeloff) then
	               kbad(beam,range) = .true.
	               num = 0
   	            end if
	         end if

	      if(num.gt.7) then    !some  points are good
	            vel(beam,range) = sumwvel/num
	            kbad(beam,range) = .false.
  	      end if
	   end do
	end do
	return
	end
