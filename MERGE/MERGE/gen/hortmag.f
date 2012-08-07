c****************************************************************************
	subroutine HORTMAG(iopt,bx,by,bz,tx,ty,tz,vx,vy,vz)

c Converts a vector from components (tx,ty,tz) defined in terms of the
c the local horizon (x:southwards  y:eastwards  z:vertical) to components
c (vx,vy,vz) defined in terms of the orientation of the local geomagnetic
c field (bx,by,bz). (iopt >= 0)

c In this new coordinate system the z axis is parallel to B.  The y axis
c lies in the plane of the local horizon; it is obtained by a first rotation
c of the x axis about the z axis into the direction of the projection of B
c onto this plane.  The second rotation is about the y axis and moves the
c z axis into the direction of B.
c (For Goose Bay the y axis is generally westward while the x axis is 
c  equatorward.) 

c The inverse transformation is effected by iopt < 0.

c       $Log:	hortmag.f,v $
c Revision 1.1  94/08/03  16:22:27  16:22:27  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
c****************************************************************************
	if (iopt.ge.0) then
	  alpha = atan2(by,bx)
	  ux  =  cos(alpha)*tx + sin(alpha)*ty
	  uy  = -sin(alpha)*tx + cos(alpha)*ty
	  uz  =  tz
	  bxp = cos(alpha)*bx + sin(alpha)*by
	  beta  = atan2(bxp,bz)
	  vx  =  cos(beta)*ux - sin(beta)*uz
	  vy  =  uy
	  vz  =  sin(beta)*ux + cos(beta)*uz
	end if
	if (iopt.lt.0) then
	  alpha = atan2(by,bx)
	  bxp = cos(alpha)*bx + sin(alpha)*by
	  beta  = atan2(bxp,bz)
	  ux  =  cos(beta)*vx  + sin(beta)*vz
	  uy  =  vy
	  uz  = -sin(beta)*vx  + cos(beta)*vz
	  tx  =  cos(alpha)*ux - sin(alpha)*uy
	  ty  =  sin(alpha)*ux + cos(alpha)*uy
	  tz  =  uz
	end if
	return
	end
