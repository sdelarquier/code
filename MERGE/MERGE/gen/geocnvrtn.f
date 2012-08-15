C File: /home/darn/gen/geocnvrtn.f
C last Modification: 13-MAR-2002
C Modified by: Dieter Andre
C
        subroutine GEOCNVRTn(gdlat,gdlon,xal,xel,ral,rel)

c Converts from azimuth and elevation angles (ral,rel)
c to pointing azimuth and elevation angles (xal,xel)
C More documentation in: geocnvrt.f

        implicit none
        real*4  gdlat,gdlon,xal,xel,ral,rel
        real*4  kxg,kyg,kzg,kxr,kyr,kzr
        real*4  rrad,rlat,rlon,del

        kxr      =  cos( 0.017453292* rel)* sin( 0.017453292* ral)
        kyr      =  cos( 0.017453292* rel)* cos( 0.017453292* ral)
        kzr      =  sin( 0.017453292* rel)
        call geodtgc(1,gdlat,gdlon,rrad,rlat,rlon,del)
        del      = -del
        kxg      =  kxr
        kyg      =  kyr* cos( 0.017453292* del) + kzr* sin( 0.017453292* del)
        kzg      = -kyr* sin( 0.017453292* del) + kzr* cos( 0.017453292* del)

        xal      =  57.29577951* atan2( kxg, kyg)
        xel      =  57.29577951* atan( kzg/ sqrt(kxg**2+kyg**2))
        return
        end

