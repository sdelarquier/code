
c	$Log:	tests.f,v $
c Revision 1.1  94/09/01  16:14:04  16:14:04  senior (Catherine SENIOR)
c Initial revision
c 
c	$Revision: 1.1 $

	function XMOD(a)
c	****************

c****************************************************
c Brings angle a between ]-180.,180.] degrees
c****************************************************

	xmod = a
	if (a.gt.180.) then
	  xmod = a - 360.
        else if (a.le.-180.) then
	  xmod = a + 360.
        end if
	return
	end


	function TEST1(a)
c	*****************

c****************************************************
c Brings angle a between 0. and 180. degrees
c****************************************************

	b = xmod(a)
	test1 = abs(b)
	return
	end


        function TEST2(a,b)
c	*******************

c****************************************************
c Changes the sign of b if a<0
c****************************************************

	test2 = b
        if (a.lt.0.) test2 = -b
        return
        end
