function mrg_str_def

A = {mrg_str, sdate : intarr(2,6), $
            edate : intarr(2,6), $
            mhz : intarr(2), $
            khz : intarr(2), $
            lagfr : intarr(2), $
            smsep : intarr(2), $
            map_qlty : 0L, $
            ngood : 0, $
            niter : 0L, $
            height_r : 0., $
            mlt : 0.0, $
            lat_g : fltarr(16,16), $
            lon_g : fltarr(16,16), $
            lat_m : fltarr(16,16), $
            lon_m : fltarr(16,16), $
            nlos : intarr(16,16), $
            vlos : fltarr(2,16,16), $
            vmag : fltarr(16,16), $
            vx : fltarr(16,16), $
            vy : fltarr(16,16), $
            vz : fltarr(16,16), $
            bx : fltarr(16,16), $
            by : fltarr(16,16), $
            bz : fltarr(16,16), $
            azim : fltarr(16,16) }

return, A
end
