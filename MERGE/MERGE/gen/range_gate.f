C File: /home/darn/gen/range_gate.f
C Last Modification: 13-MAR-2002
C Modified by: Dieter Andre
C
	integer*2 function range_gate( frang, rsep, rxris_km, range_edge,slant_range)
c
C Revision 2..0; 20020313; Dieter Andre
C Use frang and rsep
c       $Log:	range_gate.f,v $
c Revision 1.1  94/08/03  16:50:05  16:50:05  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
	implicit  none
	REAL*4    FRANG, RSEP
	real*4    rxris_km,range_edge,slant_range
c
c	this function calculates the range gate (rounded off) given the radar
c	parameters and the slant_range.  The value range_edge is either
c	0 (for the center of a range gate) or -0.5* rsep.
c
	range_gate = (slant_range - range_edge - frang + rxris_km)/rsep + 1.5
	return
	end

