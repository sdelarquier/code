C File: /home/darn/gen/slant_range.f
C Last Modification; 13-MAR-2002
C Modified by: Dieter Andre
C
	real*4    function slant_range( frang, rsep, rxris_km, range_edge, range_gate)
c
C Revision 2.0 13-MAR-2002; Dieter Andre
C Modified to use distances [frang and srep]
C
c Revision 1.1  94/08/03  16:58:39  16:58:39  miker (Michael Ruohoniemi S1G)
c Initial revision
c 
c
	implicit  none
	real*4    frang, rsep
	integer*2 range_gate
	real*4    rxris_km, range_edge
c
c	this function calculates the slant range given the radar
c	parameters and the range_gate.  The value range_edge is either
c	0 (for the center of a range gate) or -0.5*rsep.
C	First range gate has number 1.
c
	slant_range = frang - rxris_km + (range_gate - 1)* rsep + range_edge
	return
	end


