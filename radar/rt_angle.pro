pro	rt_angle

elev = 10.

Re = 6378.
z0 = 300. - findgen(300) + Re

angle = asin( Re/z0*sin(!PI/2. + elev*!PI/180.) )*180./!PI

plot, z0-Re, angle, yrange=[90.-elev-10., 90.-elev], title='Altitude dependance of incident angle', $
    xtitle='altitude of interface (km)', ytitle='incident angle (degrees)' , $
    thick=4, charthick=4, xthick=4, ythick=4

end