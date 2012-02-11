pro hwm_ati, date, glat, glon

cd, '~/Documents/Research/HWM/HWM07'

sfjul, date, [0,0], sjul, fjul, no_hours=nhrs
juls = sjul + dindgen(nhrs)/24.d

parse_date, date, year, month, day
iyd = year*1000L + day_no(date)

time = [0,2400]
parse_time, time, hour, minutes
hour = hour + minutes/60.
sec = hour*3600.

shourut = abs(0. - glon/15.)
ssec = shourut*3600.
fhourut = shourut
fsec = fhourut*3600.
hrstp = 1.
secstp = hrstp*3600.

alt = 100.
dalt = 5.
nalt = (250.-alt)/dalt

; Execute hwm code
wm = fltarr(nhrs,nalt)
wz = fltarr(nhrs,nalt)
for is=0,nalt-1 do begin
    input = STRTRIM(iyd,2)+',0,'+STRTRIM(alt,2)+','+STRTRIM(glat,2)+','+STRTRIM(glon,2)+','+ $
	STRTRIM(ssec,2)+','+STRTRIM(fsec,2)+','+STRTRIM(secstp,2)+',1'
    print, input
    spawn, 'rm inp_file'
    spawn, 'echo '+input+' >> inp_file'
    spawn, '/home/sebastien/Documents/Research/HWM/HWM07/hwmOUT < inp_file'


    twm = fltarr(500)
    twz = fltarr(500)
    openr, unit, 'hwm.dat', /get_lun
    readf, unit, twm, format='(500F8.3)'
    readf, unit, twz, format='(500F8.3)'
    free_lun, unit

    wm[*,is] = -twm[0:FIX(nhrs)-1]
    wz[*,is] = twz[0:FIX(nhrs)-1]

    alt += dalt
endfor

set_format, /portrait
clear_page
legend = 'Wind speed [m/s]'
scale = [-100.,100.]
charsize = 1.
position = define_panel(1, 2, 0, 0, /bar, with_info=with_info)
plot_ati, wm, scale, title='Meridional winds (HWM07 +S) '+format_date(date,/human), $
    legend=legend, position=position, charsize=charsize
position = define_panel(1, 2, 0, 1, /bar, with_info=with_info)
plot_ati, wz, scale, title='Zonal winds (HWM07 +E) '+format_date(date,/human), $
    legend=legend, position=position, charsize=charsize

end