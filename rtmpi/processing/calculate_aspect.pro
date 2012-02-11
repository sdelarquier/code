pro calculate_aspect, radar

common rt_data_blk

rt_run, 20110101, radar

Rav = 6370.

abeams = reform(rt_data.beam[0,*])
nbeams = n_elements(abeams)

aranges = 180. + findgen(50)*45.
aelev = rt_info.elev_beg + findgen((rt_info.elev_end-rt_info.elev_beg)/.5)*.5
nranges = n_elements(aranges)
nelevs = n_elements(aelev)
dran = aranges[1] - aranges[0]
del = aelev[1] - aelev[0]

; latitude/longitude distribution (expressed as angle from radar with Earth center)
ndip = n_elements(rt_data.dip[0,0,0,*])
thtdip = findgen(ndip)*2500./Rav/ndip
dthtdip = thtdip[1]-thtdip[0]

; Calculate aspect angle in each range-beam-elevation cell
aspect = fltarr(nbeams,nranges,nelevs)
for ib=0,nbeams-1 do begin
	for ir=0,nranges-1 do begin
		for iel=0,nelevs-1 do begin
			alt = sqrt(Rav^2 + aranges[ir]^2 - 2.*Rav*aranges[ir]*cos(!pi/2.+aelev[iel]*!dtor))
			ttht = asin(aranges[ir]/alt*sin(!pi/2.+aelev[iel]*!dtor))
			thtind = (ttht - thtdip[0])/dthtdip

			; Dip and declination at this position
			middip = rt_data.dip[0,ib,0,thtind]
			middec = rt_data.dip[0,ib,1,thtind]

			; calculate vector magnetic field
			Bx = cos(-middip*!dtor) * cos(rt_data.azim[0,ib]*!dtor - middec*!dtor)
			Bz = sin(-middip*!dtor)
			reldip = abs(acos(Bx/sqrt(Bx^2+Bz^2))*!radeg)

			aspect[ib,ir,iel] = 90.-abs(90. - (aelev[iel] + reldip + ttht*!radeg))
		endfor
	endfor
endfor

; Write to files
openw, unit, '~/tmp/rt/'+rt_info.name+'/aspect.dat', /get_lun
writeu, unit, nbeams, nranges, nelevs
writeu, unit, abeams, aranges, aelev
writeu, unit, aspect
free_lun, unit


end