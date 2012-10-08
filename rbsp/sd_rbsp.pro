; Generate RBSP footprints maps
; The RBSP orbit coordinates are downloaded from athena.jhuapl.edu
; using the online position calculator. This function reads and parses
; the files outputed by the aforementioned calculator.
; 
; Written by Sebastien.
; Aug. 1, 2012
; --------------------------------------------------------------------
pro sd_rbsp, fp_pos=fp_pos, rbpos=rbpos, anim=anim, ext=ext

common radarinfo

months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']


; RBSP A
; Read file
file = 'orbitposA.dat'
if keyword_set(ext) then $
    file = file+'.'+strtrim(ext,2)
openr, fin, file, /get_lun
; Skip header
bla = ''
readf, fin, bla, format='(A)'
; Read all
template = {version: 1.0, $
            datastart: 1L, $
            commentsymbol: '#', $
            delimiter: 9B, $
            missingvalue: '', $
            fieldcount: 9L, $
            fieldtypes: [3l,3l,3l,3l,3l,3l,5l,5l,5l], $
            fieldnames: ['year','month','day','hour','minute','second','altitude','geolat','geolon'], $
            fieldlocations: [0L,5L,8L,11L,14L,17L,20L,33L,43L], $
            fieldgroups: lindgen(9)}
datA = read_ascii(file, template=template)
nelemA = n_elements(datA.year)
; Close file
free_lun, fin

; RBSP B
; Read file
file = 'orbitposB.dat'
if keyword_set(ext) then $
    file = file+'.'+strtrim(ext,2)
openr, fin, file, /get_lun
; Skip header
bla = ''
readf, fin, bla, format='(A)'
; Read all
datB = read_ascii(file, template=template)
nelemB = n_elements(datB.year)
; Close file
free_lun, fin

; Set postscript if necessary
if ~keyword_set(anim) then $
	ps_open, '~/Desktop/rbpos.ps'

; Plot map
load_usersym, /circle
range = [-50.,50.]
radar = network[where(network.status eq 1 and network.site[0].geolat gt 0.)].code[0]
if keyword_set(anim) then begin
	psname = 'rbpos'+strtrim(string(0,format='(I04)'),2)
	ps_open, psname+'.ps'
	rad_plot_fov, radar, xrange=range, yrange=range, coords='geog', /fillanno
		xyouts, .5, .9, strtrim(datA.day[0],2)+'/'+ $
		strtrim(datA.month[0],2)+'/'+ $
		strtrim(datA.year[0],2)+' - '+ $
		strtrim(string(datA.hour[0],format='(I02)'),2)+':'+ $
		strtrim(string(datA.minute[0],format='(I02)'),2), $
		align=.5, /normal
	ps_close, /no_f
	spawn, 'gs -sDEVICE=jpeg -dJPEGQ=100 -dNOPAUSE -dBATCH -dSAFER -r300 -sOutputFile='+psname+'.png '+psname+'.ps'
	spawn, 'mogrify +repage -trim -rotate 270 -resize 1000x1000 '+psname+'.png'
	spawn, 'rm '+psname+'.ps'
endif else $
	rad_plot_fov, radar, xrange=range, yrange=range, coords='geog', /fillanno

; Print dates
strdate = strtrim(datA.day[0],2)+'/'+months[datA.month[0]-1]+'/'+ $
	strtrim(datA.year[0],2)+' - '+ $
	strtrim(string(datA.hour[0],format='(I02)'),2)+':'+ $
	strtrim(string(datA.minute[0],format='(I02)'),2)+'!C'+ $
	strtrim(datA.day[nelemA-1],2)+'/'+months[datA.month[nelemA-1]-1]+'/'+ $
	strtrim(datA.year[nelemA-1],2)+' - '+ $
	strtrim(string(datA.hour[nelemA-1],format='(I02)'),2)+':'+ $
	strtrim(string(datA.minute[nelemA-1],format='(I02)'),2)
if ~keyword_set(anim) then $
	xyouts, .5, .9, strdate, align=.5, /normal

; Iterate through positions to get the footprint of each orbit position
loadct, 33
nelem = max([nelemA,nelemB])
; Index is: [satellite(A/B), #records, projected/original point, latitude/longitude/altitude]
fp_pos = fltarr(2,nelem,2,3)
for ipos=0,nelem-1 do begin
	if keyword_set(anim) then begin
		psname = 'rbpos'+strtrim(string(ipos+1,format='(I04)'),2)
		ps_open, psname+'.ps'
		rad_plot_fov, '', xrange=range, yrange=range, $
			coords='geog', /fillanno, /nofill
		xyouts, .5, .9, strtrim(datA.day[ipos],2)+'/'+ $
			strtrim(datA.month[ipos],2)+'/'+ $
			strtrim(datA.year[ipos],2)+' - '+ $
			strtrim(string(datA.hour[ipos],format='(I02)'),2)+':'+ $
			strtrim(string(datA.minute[ipos],format='(I02)'),2), $
			align=.5, /normal
		loadct, 33
	endif
	if ipos lt nelemA then begin
		igrf_run, $
			datA.year[ipos]*10000L+datA.month[ipos]*100L+datA.day[ipos], $
			lati=datA.geolat[ipos], longi=datA.geolon[ipos], $
			alti=datA.altitude[ipos]*[1.,1.,1.], $
			cgm=cgm, geo=geo
		;print, 'A-->'
		;print, reform(geo[0,*,*])
		if geo[0,1,0] gt 0. then begin
			fp_pos[0,ipos,0,0] = geo[0,1,0]
			fp_pos[0,ipos,0,1] = geo[0,1,1]
		endif else begin
			fp_pos[0,ipos,0,0] = geo[0,2,0]
			fp_pos[0,ipos,0,1] = geo[0,2,1]
		endelse
		fp_pos[0,ipos,1,0] = geo[0,0,0]
		fp_pos[0,ipos,1,1] = geo[0,0,1]
		fp_pos[0,ipos,1,2] = datA.altitude[ipos]
		stpos = calc_stereo_coords(fp_pos[0,ipos,0,0], fp_pos[0,ipos,0,1])
		x = stpos[0]
		y = stpos[1]
		if x ge range[0] and x le range[1] and y ge range[0] and y le range[1] then begin
			plots, stpos[0], stpos[1], psym=8, symsize=.5, color=50
		endif
	endif
	if ipos lt nelemB then begin
		igrf_run, $
			datB.year[ipos]*10000L+datB.month[ipos]*100L+datB.day[ipos], $
			lati=datB.geolat[ipos], longi=datB.geolon[ipos], $
			alti=datB.altitude[ipos]*[1.,1.,1.], $
			cgm=cgm, geo=geo
		;print, 'B-->'
		;print, reform(geo[0,*,*])
		if geo[0,1,0] gt 0. then begin
			fp_pos[1,ipos,0,0] = geo[0,1,0]
			fp_pos[1,ipos,0,1] = geo[0,1,1]
		endif else begin
			fp_pos[1,ipos,0,0] = geo[0,2,0]
			fp_pos[1,ipos,0,1] = geo[0,2,1]
		endelse
		fp_pos[1,ipos,1,0] = geo[0,0,0]
		fp_pos[1,ipos,1,1] = geo[0,0,1]
		fp_pos[1,ipos,1,2] = datA.altitude[ipos]
		stpos = calc_stereo_coords(fp_pos[1,ipos,0,0], fp_pos[1,ipos,0,1])
		x = stpos[0]
		y = stpos[1]
		if x ge range[0] and x le range[1] and y ge range[0] and y le range[1] then begin
			plots, stpos[0], stpos[1], psym=8, symsize=.5, color=200
		endif
	endif
	if keyword_set(anim) then begin
		ps_close, /no_f
		spawn, 'gs -sDEVICE=jpeg -dJPEGQ=100 -dNOPAUSE -dBATCH -dSAFER -r300 -sOutputFile='+psname+'.png '+psname+'.ps'
		spawn, 'mogrify +repage -trim -rotate 270 -resize 1000x1000 '+psname+'.png'
		spawn, 'rm '+psname+'.ps'
	endif
endfor

plots, range[0], range[0] - (range[1]-range[0])*.1, psym=8, symsize=.5, color=200
plots, range[0] + (range[1]-range[0])/2., range[0] - (range[1]-range[0])*.1, psym=8, symsize=.5, color=50
loadct, 0
xyouts, range[0] + (range[1]-range[0])/2. + (range[1]-range[0])*.05, range[0] - (range[1]-range[0])*.11, 'RBSP A' 
xyouts, range[0] + (range[1]-range[0])*.05, range[0] - (range[1]-range[0])*.11, 'RBSP B' 

if keyword_set(anim) then $
	spawn, 'convert -delay 20 rbpos*.png rbpos_anim.gif'
	
; ; Clearly mark 1st point
; stpos = calc_stereo_coords(fp_pos[0,0,0], fp_pos[0,0,1])
; plots, stpos[0], stpos[1], psym=4, symsize=1, color=20
; stpos = calc_stereo_coords(fp_pos[1,0,0], fp_pos[1,0,1])
; plots, stpos[0], stpos[1], psym=4, symsize=1, color=250
; 
; ; Clearly mark nth point
; seePt = 20
; stpos = calc_stereo_coords(fp_pos[0,seePt,0], fp_pos[0,seePt,1])
; plots, stpos[0], stpos[1], psym=1, symsize=1, color=20
; stpos = calc_stereo_coords(fp_pos[1,seePt,0], fp_pos[1,seePt,1])
; plots, stpos[0], stpos[1], psym=1, symsize=1, color=250

; Close postscript
if ~keyword_set(anim) then begin
	ps_close, /no_f
	if ~keyword_set(ext) then $
		spawn, 'ps2pdf ~/Desktop/rbpos.ps ~/Desktop/rbpos.pdf' $
	else $
		spawn, 'ps2pdf ~/Desktop/rbpos.ps ~/Desktop/rbpos.'+strtrim(ext,2)+'.pdf'
	spawn, 'rm -f ~/Desktop/rbpos.ps'
endif

rbpos = fp_pos

end
