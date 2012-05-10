pro rt_serial, radar

common radarinfo

; radar = 'fhe'

; Find radar location
radID = where(network.code[0,*] eq radar)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
endfor
radarsite = network[radID].site[s]


; juls = timegen(start=julday(10,15,2010), final=julday(06,15,2011), units='days')
; caldat, juls, month, day, year
; date = year*10000L+month*100L+day
; 
; ; Elevation time series
; edges = fltarr(n_elements(juls),4)
; slopes = fltarr(n_elements(juls),4)
; nmf2 = fltarr(n_elements(juls))
; hmf2 = fltarr(n_elements(juls))
; for id=0,n_elements(juls)-1 do begin
; 	iri_run, date[id], 600, param='alti', lati=radarsite.geolat, longi=radarsite.geolon, /ut, nmf2=tnmf2, hmf2=thmf2
; 	rad_fit_plot_ionoscat_elevation, date[id], radar, 12, edges=tedges, slope=tslopes, ps='is.ps', radhist=radhist, rthist=rthist
; 	if id eq 0 then begin
; 		hist = fltarr(n_elements(juls), n_elements(radhist[0,*,0]), n_elements(radhist[0,0,*]))
; 		rayhist = fltarr(n_elements(juls), n_elements(rthist[0,*,0]), n_elements(rthist[0,0,*]))
; 	endif
; ; 	caldat, juls[id], month, day, year
; ; 	print, date[id], reform(tedges[12,0,0:1])
; 	edges[id,0] = tedges[0,0]
; 	edges[id,1] = tedges[0,1]
; 	slopes[id,0] = tslopes[0,0]
; 	slopes[id,1] = tslopes[0,1]
; 	edges[id,2] = tedges[1,0]
; 	edges[id,3] = tedges[1,1]
; 	slopes[id,2] = tslopes[1,0]
; 	slopes[id,3] = tslopes[1,1]
; 	print, date[id], reform(slopes[id,*])
; 	hist[id,*,*] = reform(radhist[0,*,*])
; 	rayhist[id,*,*] = reform(rthist[0,*,*])
; 	nmf2[id] = tnmf2
; 	hmf2[id] = thmf2
; 	spawn, 'ps2png.sh is.ps'
; 	spawn, 'mv is.png ~/Desktop/IS_stats/elev'+strtrim(date[id],2)+'.png' 
; endfor
; 
; openw, unit, 'ionoscat_elev_edges.dat', /get_lun
; writeu, unit, n_elements(juls), juls, edges, slopes
; writeu, unit, n_elements(radhist[0,*,0]), n_elements(radhist[0,0,*]), hist, rayhist;/max(rayhist, /nan)
; writeu, unit, nmf2, hmf2
; free_lun, unit




; juls = timegen(start=julday(02,01,2008), final=julday(06,01,2011), units='days')
juls = timegen(start=julday(01,01,2011), final=julday(01,01,2012), units='days')
caldat, juls, month, day, year
date = year*10000L+month*100L+day

; Range-beam time series
edges = fltarr(n_elements(juls),4)
nmf2 = fltarr(n_elements(juls))
hmf2 = fltarr(n_elements(juls))
; hist = fltarr(n_elements(juls),16,71)
; rayhist = fltarr(n_elements(juls),16,71)
for id=0,n_elements(juls)-1 do begin
; 	iri_run, date[id], 600, param='alti', lati=42.6, longi=288.5, /ut, nmf2=tnmf2, hmf2=thmf2
	iri_run, date[id], 600, param='alti', lati=radarsite.geolat, longi=radarsite.geolon, /ut, nmf2=tnmf2, hmf2=thmf2
	print, '****************', tnmf2, thmf2
	rad_fit_plot_ionoscat, date[id], radar, edges=tedges, ps='is.ps', radhist=radhist, rthist=rthist, /rtrun
	if id eq 0 then begin
		hist = fltarr(n_elements(juls), n_elements(radhist[*,0]), n_elements(radhist[0,*]))
		rayhist = fltarr(n_elements(juls), n_elements(rthist[*,0]), n_elements(rthist[0,*]))
	endif
	edges[id,0] = tedges[12,0,0]
	edges[id,1] = tedges[12,0,1]
	ttedges = tedges[*,0,0]
	ind = where(ttedges ne 0, cc)
	if cc gt 0 then begin
		edges[id,2] = ind[0]
		edges[id,3] = ind[cc-1]
	endif
	hist[id,*,*] = radhist
	rayhist[id,*,*] = rthist
	nmf2[id] = tnmf2
	hmf2[id] = thmf2
	spawn, 'ps2png.sh is.ps'
	spawn, 'mv is.png ~/Desktop/IS_stats/'+radar+strtrim(date[id],2)+'.png' 
endfor

openw, unit, 'ionoscat_edges_'+radar+'.dat', /get_lun
writeu, unit, n_elements(juls), juls, edges
writeu, unit, n_elements(hist[0,*,0]), n_elements(hist[0,0,*]), hist, rayhist;/max(rayhist, /nan)
writeu, unit, nmf2, hmf2
free_lun, unit
; help, n_elements(juls), juls, edges, n_elements(hist[0,*,0]), n_elements(hist[0,0,*]), hist, rayhist, nmf2, hmf2


end