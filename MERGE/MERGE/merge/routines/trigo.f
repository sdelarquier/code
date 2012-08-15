c	$Log:	trigo.f,v $
c Revision 1.2  94/11/04  10:19:15  10:19:15  senior (Catherine SENIOR)
c Added a second method to handle singulatitirities at the southern
c geographic pole
c 
c Revision 1.1  94/09/01  16:14:05  16:14:05  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.2 $

	subroutine TRIGO1(method,pa,pb,pc,ga,gb,gc)
c	*******************************************
c
c**************************************************************
c Spherical Trigonometry: Solves the spherical triangle
c given by one angle and two arcs.
c	inputs  : pb,pc,ga (b,c,A)
c	outputs : pa,gb,gc (a,B,C)
c All angles are in degrees
c Input angles must be between [0.,180.]
c All output angles are between 0. and 180. degrees
c
c A second method has been added for singularities at the pole
c
c First written: May 93 (CS)
c**************************************************************

	if(method.eq.1) then
	   if(ga.eq.0.) then
	     ang = pb-pc
	     if((ang.gt.-180.).and.(ang.le.0.)) then
	       gb = 0.
	       gc = 180.
	       pa = -ang
	     else if((ang.gt.0.).and.(ang.le.180.)) then
	       gb = 180.
	       gc = 0.
	       pa = ang
	     endif
	     return
	   endif
	   if(ga.eq.180.) then
	     ang = XMOD(pb+pc)
	     if((ang.gt.0.).and.(ang.le.180.)) then
	       gb = 0.
	       gc = 0.
	       pa = ang
	     else if((ang.gt.-180.).and.(ang.le.0.)) then
	       gb = 180.
	       gc = 180.
	       pa = -ang
	     endif
	     return
	   endif
	   cb=cosd((pb-pc)/2.)
	   cc=cosd((pb+pc)/2.)
	   ca=1./tand(ga/2.)
	   atb=atan2d(cb*ca,cc)
	   sb=sind((pb-pc)/2.)
	   sc=sind((pb+pc)/2.)
	   atc=atan2d(sb*ca,sc)
	   gb=atb+atc
	   gc=atb-atc
  	   sgb=sind((gb-gc)/2.)
	   csa=cosd(ga/2.)
	   sga=sb*csa/sgb
	   pa=2.*asind(sga)
	   return
	else if (method.eq.2) then
	   cpa = cosd(pb)*cosd(pc) + sind(pb)*sind(pc)*cosd(ga)
	   pa = acosd(cpa)
	   cgb = (cosd(pb)*sind(pc)-sind(pb)*cosd(pc)*cosd(ga))/sind(pa)
	   gb = acosd(cgb)
	   cgc = (cosd(pc)*sind(pb)-sind(pc)*cosd(pb)*cosd(ga))/sind(pa)
	   gc = acosd(cgc)
	   return
	endif

	end

c_________________________________________________________________

	subroutine TRIGO2(method,pa,pb,pc,ga,gb,gc)
c	*******************************************

c**************************************************************
c Spherical Trigonometry: Solves the spherical triangle
c given by two angles and one arc.
c	inputs  : pa,gb,gc (a,B,C)
c	outputs : pb,pc,ga (b,c,A)
c All angles are in degrees
c Input angles must be between [0.,180.]
c All output angles are between 0. and 180. degrees
c
c A second method has been added for singularities at the pole
c
c Last written: May 93 (CS)
c**************************************************************

	if(method.eq.1) then
	   if(pa.eq.0.) then
	     ang = XMOD(gb+gc)
	     if((ang.gt.-180.).and.(ang.le.0.)) then
	       ga = 180.+ ang
	       pb = 180.
	       pc = 180.
	     else if((ang.gt.0.).and.(ang.le.180.)) then
	       ga = 180.- ang
	       pb = 0.
	       pc = 0.
	     endif
	     return
	   endif
	   if(pa.eq.180.) then
	     ang = gb-gc
	     if((ang.gt.-180.).and.(ang.le.0.)) then
	       ga = 180.+ ang
	       pb = 0.
	       pc = 180.
	     else if((ang.gt.0.).and.(ang.le.180.)) then
	       ga = 180.- ang
	       pb = 180.
	       pc = 0.
	     endif
	     return
	   endif
	   cb = cosd((gb-gc)/2.)
	   cc = cosd((gb+gc)/2.)
	   ca = tand(pa/2.)
	   atb = atan2d(cb*ca,cc)
	   sb = sind((gb-gc)/2.)
	   sc = sind((gb+gc)/2.)
	   atc = atan2d(sb*ca,sc)
	   pb = atb+atc
	   pc = atb-atc
	   sgb = sind((pb-pc)/2.)
	   ssa = sind(pa/2.)
	   sga = sb*ssa/sgb
	   ga = 2.*acosd(sga)	
	   return
	else if (method.eq.2) then
	   cga = -cosd(gb)*cosd(gc) + sind(gb)*sind(gc)*cosd(pa)
	   ga = acosd(cga)
	   cpb = (sind(gc)*cosd(gb)+sind(gb)*cosd(gc)*cosd(pa))/sind(ga)
	   pb = acosd(cpb)
	   cpc = (cosd(gc)*sind(gb)+sind(gc)*cosd(gb)*cosd(pa))/sind(ga)
	   pc = acosd(cpc)
	   return
	endif

	end
