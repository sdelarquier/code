c****************************************************************************8
C File: /home/darn/gen/locbg.f
C Last Modification: 13-MAR-2002
C Modified by: Dieter Andre
C
        subroutine LOCBG(st_id,year,yr_sec, frang, rsep,flat,flon,height,beam,gate)
c
c This program LOCates the Beam and Gate indices corresponding to a given
c set of radar operating parameters (st_id, frang, rsep) and a field point
c position specified by geocentric latitude and longitude (flat,flon)
c and height above the surface of the earth (i.e. the output mapping parameters
c of RBPOS).
c <SD_END
        implicit   none
        integer*2  st_id, beam, gate
	real*4     frang, rsep
        real*4     flat,flon,height
        real*4     position(5),gdlat,gdlon,bore,bmwd,rxris
        real*4     grho,glat,glon,del,dum1,dum2,rrho,ral,rel,r,frho,xal,xel
        real*4     azi,psi
        integer*2  range_gate

        integer*2  radar_pos_year
        integer*4  radar_pos_sec
	integer*2  year
	integer*4  yr_sec


c call radar_pos to provide radar-specific parameters
        radar_pos_year  =  year
        radar_pos_sec   =  yr_sec
        call radar_pos(st_id,radar_pos_year,radar_pos_sec,position)
        gdlat  =  position(1)
        gdlon  =  position(2)
        bore   =  position(3)
        bmwd   =  position(4)
        rxris  =  position(5)

c find geocentric spherical coordinates of radar position (grho,glat,glon)
        call geodtgc( 1,gdlat,gdlon,grho,glat,glon,del)

c find radius of the earth below the field point....
        call geodtgc(-1,dum1,dum2,rrho,flat,flon,del)
        frho  =  rrho + height
c ..add height to obtain the distance from the centre of the earth (frho)

c find azimuth, elevation, and slant range to field point (spherical/virtual)
        call FLDPNTn(grho,glat,glon,ral,rel,r,frho,flat,flon)

c invert slant range algorithm for range gate
        gate = range_gate( frang, rsep, rxris* 0.15, 0.0, r)

c find pointing azimuth and elevation at the radar (xal,xel)
        call GEOCNVRTn(gdlat,gdlon,xal,xel,ral,rel)
        xel = rel
c analogous to the usage in FLDPNTH, approximate the pointing elevation with
c the spherical earth elevation angle

c convert from pointing azimuth and elevation to radar cone angle (psi)
c (azi represents the off-boresight azimuth)
        azi     =  xal - bore
        psi     =  57.29577951* asin( sin( 0.017453292* azi)*cos( 0.017453292* xel))

c find nearest beam number
        beam    =  NINT((psi/bmwd)+7.5)

        return
        end



