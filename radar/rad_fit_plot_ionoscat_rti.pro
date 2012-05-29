;- Plots a range-time scatter distribution for a given radar on a given day from 0 to 12 UT
; Both radar data and ray tracing are compared for a given beam
; For now, the aspect contours are only available for bks and for the configuration in 2011 (before the new electronics)
;
; Last update: Feb, 23, 2012
pro rad_fit_plot_ionoscat_rti, date, radar, beam, ps=ps

common rt_data_blk
common radarinfo
common rad_data_blk

Rav = 6371.

set_format, /portrait, /sardines, /tokyo
if keyword_set(ps) then $
	ps_open, '~/Desktop/ionoscat_rti_'+radar+'_'+strtrim(date,2)+'.ps'
clear_page

; Parse date
parse_date, date, yy, mm, dd

; Find range-gate locations
yrsec = (julday(mm,dd,yy) - julday(1,1,yy))*86400.d
radID = where(network.code[0,*] eq radar)
tval = TimeYMDHMSToEpoch(yy, mm, dd, 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]
nbeams = radarsite.maxbeam
beams = beam

; Set plot limits
charsize = get_charsize(n_elements(beams),2)

; Set elevation range
elrange = [5.,55.]
elstep = 0.5
nelev_steps = (elrange[1]-elrange[0])/elstep
elev_steps = elrange[0] + findgen(nelev_steps)*(elrange[1]-elrange[0])/nelev_steps

; Read aspect angle contours from file
openr, unit, '~/tmp/rt/'+radar+'/aspect.dat', /get_lun
aspnbeams = 0L
aspnranges = 0L
aspnelevs = 0L
readu, unit, aspnbeams, aspnranges, aspnelevs
aspabeams = intarr(aspnbeams)
asparanges = fltarr(aspnranges)
aspaelev = fltarr(aspnelevs)
readu, unit, aspabeams, asparanges, aspaelev
aspect = fltarr(aspnbeams,aspnranges,aspnelevs)
readu, unit, aspect
free_lun, unit


;*******************************************
; Ray-tracing
;*******************************************
rt_run, date, radar, time=[0,1200]

rt_plot_rti_panel, 1,2,0,1, /bar, beam=beam, time=[0,1200], /grid, yrange=[0.,3000.], coords='rang', /first, /last, scale=[-30.,0.]

pos = define_panel(1,2,0,1)
bpos = define_cb_position(pos, gap=-.1)
plot_colorbar, /vert, charthick=charthick, $
	nlevels=8, scale=[0,30], position=bpos, charsize=charsize, $
	legend='Power [dB]', /no_rotate, $
	level_format='(I3)', /keep_first_last_label


xyouts, .5, .88, $
		radar+', '+STRMID(format_juldate(rt_data.juls[0]),0,11), $
		align=.5, /normal, charsize=charsizeze



;*******************************************
; Radar: SCATTER 
;*******************************************
rad_fit_read, date, radar, time=[0,1200], /filter, /ajground, /catfile, catpath='~/tmp/'
rad_fit_calculate_elevation, date=date, time=[0,1200], /overwrite, tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8.
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

rad_fit_plot_rti_panel, 1,2,0,0, /bar, beam=beam, time=[0,1200], param='power', /grid, yrange=[0.,3000.], coords='rang', /first, scale=[0,30]

pos = define_panel(1,2,0,0)
bpos = define_cb_position(pos, gap=-.1)
plot_colorbar, /vert, charthick=charthick, $
	nlevels=8, scale=[0,30], position=bpos, charsize=charsize, $
	legend='Power [dB]', /no_rotate, $
	level_format='(I3)', /keep_first_last_label


if keyword_set(ps) then $
	ps_close, /no_f

end
