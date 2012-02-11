pro	hwm_plot_map_weff, xmaps, ymaps, xmap, ymap, date, time, alt=alt, res=res

cd, '~/Documents/Research/HWM/HWM07'

; set_format, /portrait
; clear_page
if ~keyword_set(alt) then $
    alt = 150.
if ~keyword_set(res) then $
    res = 2.

parse_date, date, year, month, day
iyd = year*1000L + day_no(date)

parse_time, time, hour, minutes
hour = hour + minutes/60.
sec = hour*3600.

slon = 0.
flon = 360.
lonstp = res

nlats = 180./res
nlons = 360./res
lat = -90. + findgen(nlats+1)*res
lon = -180. + findgen(nlons+1)*res

wm = fltarr(nlons+1,nlats+1)
wz = fltarr(nlons+1,nlats+1)
scale = [-40.,40.]
wmin = scale[0]
wmax = scale[1]

; clear_page
charsize=0.5
yrange = [-90.,90.]
xrange = [-180.,180.]

; Execute hwm code
glat = 90.
for nlat=0,nlats do begin
    input = STRTRIM(iyd,2)+','+STRTRIM(sec,2)+','+STRTRIM(alt,2)+','+STRTRIM(glat,2)+','+STRTRIM(0.,2)+','+ $
	STRTRIM(slon,2)+','+STRTRIM(flon,2)+','+STRTRIM(lonstp,2)+',2'
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

    wm[*,nlat] = -twm[0:nlons]
    wz[*,nlat] = twz[0:nlons]
    
    glat -= res
endfor
wm = shift(wm,nlons/2,0)
wz = shift(wz,nlons/2,0)

; igrf_plot, date
openr, unit, 'igrf_idl.dat', /get_lun
nla = 0.
nlo = 0.
readu, unit, nla, nlo
comb = fltarr(nlo+1,nla+1)
dip = fltarr(nlo+1,nla+1)
dec = fltarr(nlo+1,nla+1)
readu, unit, comb, dip, dec
free_lun, unit

weff = fltarr(nlo+1,nla+1)

; TOTAL wind efficiency
position = define_panel(xmaps, ymaps, xmap, ymap, /bar, with_info=with_info)
title = 'HWM07+IGRF total '+STRMID(STRTRIM(alt,2),0,3)+'km!C'+format_date(date,/human)+' - '+STRMID(STRTRIM(hour,2),0,2)+'LT'
legend = 'Effective vertical wind [m/s]'
pinds = where(dip ge 0.)
weff[pinds] = -(wm[pinds]*cos(dec[pinds]*!PI/180.) - wz[pinds]*sin(dec[pinds]*!PI/180.))* $
		cos(dip[pinds]*!PI/180.)*sin(dip[pinds]*!PI/180.)
ninds = where(dip le 0.)
weff[ninds] = (-wm[ninds]*cos(dec[ninds]*!PI/180.) + wz[ninds]*sin(dec[ninds]*!PI/180.))* $
		cos(dip[ninds]*!PI/180.)*sin(dip[ninds]*!PI/180.)
plot_map_data, weff, scale, $
    position=position, title=title, $
    legend=legend, charsize=charsize

; ; ZONAL wind efficiency
; ymap = 1
; position = define_panel(xmaps, ymaps, xmap, ymap, /bar, with_info=with_info)
; title = 'HWM07+IGRF zonal '+STRMID(STRTRIM(alt,2),0,3)+'km!C'+format_date(date,/human)+' - '+STRMID(STRTRIM(hour,2),0,2)+'LT'
; legend = 'Wind efficiency (zonal)'
; pinds = where(dip ge 0.)
; weff[pinds] = - wz[pinds]*sin(dec[pinds]*!PI/180.)*cos(dip[pinds]*!PI/180.)*sin(dip[pinds]*!PI/180.)
; ninds = where(dip le 0.)
; weff[ninds] = wm[ninds]*sin(dec[ninds]*!PI/180.)*cos(-dip[ninds]*!PI/180.)*sin(-dip[ninds]*!PI/180.)
; plot_map_data, weff, scale, $
;     position=position, title=title, $
;     legend=legend, charsize=charsize
; 
; ; MERIDIONAL wind efficiency
; ymap = 2
; position = define_panel(xmaps, ymaps, xmap, ymap, /bar, with_info=with_info)
; title = 'HWM07+IGRF meridional '+STRMID(STRTRIM(alt,2),0,3)+'km!C'+format_date(date,/human)+' - '+STRMID(STRTRIM(hour,2),0,2)+'LT'
; legend = 'Wind efficiency (zonal)'
; pinds = where(dip ge 0.)
; weff[pinds] = wm[pinds]*cos(dec[pinds]*!PI/180.)*cos(dip[pinds]*!PI/180.)*sin(dip[pinds]*!PI/180.)
; ninds = where(dip le 0.)
; weff[ninds] = wm[ninds]*cos(dec[ninds]*!PI/180.)*cos(-dip[ninds]*!PI/180.)*sin(-dip[ninds]*!PI/180.)
; plot_map_data, weff, scale, $
;     position=position, title=title, $
;     legend=legend, charsize=charsize


end