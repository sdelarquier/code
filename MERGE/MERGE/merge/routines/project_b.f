	subroutine project_b(rho,lat,lon,rho0,lat0,lon0,bxl,byl,bzl)
c	************************************************************

c************************************************************************
c
c Projects the point of global spherical coordinates (gsc) 
c (rho0,lat0,lon0) to the geocentric distance rho. 
c The projection is made along the vector b (bxl, byl, bzl)
c given by its local cartesian components.
c The gsc coordinates of the projected point are rho, lat, lon.
c The calculation is made in global cartesian coordinates.
c
c	$Log:	project_b.f,v $
c Revision 1.1  94/11/04  14:31:42  14:31:42  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $
c************************************************************************

	real*4 rho0,lat0,lon0
	real*4 rho,lat,lon,r
c----------------

	call NORM_VEC(bxl,byl,bzl)
	call GLBTHOR (-1,lat0,lon0,bxg,byg,bzg,bxl,byl,bzl)
	call GSPTCAR(+1,rho0,lat0,lon0,x0,y0,z0)
	cxy=bxg*bxg+byg*byg
	dxy=bxg*x0+byg*y0
	exy=x0*x0+y0*y0-rho*rho
	bb=-cxy*z0+bzg*dxy
	cc=cxy*z0*z0-2.*bzg*z0*dxy+bzg*bzg*exy
	det=sqrt(bb*bb-cc)
	z1=-bb+det
	z2=-bb-det
c
c***  choice of the closest point of intersection
c
	if (abs(z1-z0).lt.abs(z2-z0))then
	   zz=z1
        else
	   zz=z2
	endif
	xx=x0+(zz-z0)*bxg/bzg
	yy=y0+(zz-z0)*byg/bzg
	call GSPTCAR(-1,r,lat,lon,xx,yy,zz)

	return
	end

