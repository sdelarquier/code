pro	hwm_season, year, glat, glon, alt=alt

cd, '~/Documents/Research/HWM/HWM07'

if ~keyword_set(alt) then $
    alt = 150.

sfjul, year*10000L+0101L, [0,0], sjul, fjul, no_hours=nhrs
juls = sjul + dindgen(nhrs)/24.d
iyd = year*1000L
sfjul, [year*10000L+0101L,year*10000L+1231L], sjul, fjul, no_days=nd

shourut = abs(0. - glon/15.)
ssec = shourut*3600.
fhourut = shourut
fsec = fhourut*3600.
hrstp = 1.
secstp = hrstp*3600.

; Execute hwm code
wm = fltarr(nd,nhrs)
wz = fltarr(nd,nhrs)
for id=0,nd-1 do begin
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

    wm[id,*] = -twm[0:FIX(nhrs)-1]
    wz[id,*] = twz[0:FIX(nhrs)-1]
    
    iyd++
endfor

set_format, /portrait
clear_page
legend = 'Wind speed [m/s]'
scale = [-100.,100.]
charsize = 1.
position = define_panel(1, 2, 0, 0, /bar, with_info=with_info)
plot_sti, wm, scale, title='Meridional winds (HWM07 +S) '+STRTRIM(alt,2)+'km', $
    legend=legend, position=position, charsize=charsize
position = define_panel(1, 2, 0, 1, /bar, with_info=with_info)
plot_sti, wz, scale, title='Zonal winds (HWM07 +E) '+STRTRIM(alt,2)+'km', $
    legend=legend, position=position, charsize=charsize

end