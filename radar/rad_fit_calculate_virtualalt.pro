pro	rad_fit_calculate_virtualalt

common rad_data_blk
common radarinfo

Rav = 6370.

; Find data index
data_index = rad_fit_get_data_index()
nrecs = (*rad_fit_info[data_index]).nrecs
nbeams = (*rad_fit_info[data_index]).nbeams
ngates = (*rad_fit_info[data_index]).ngates
radar = (*rad_fit_info[data_index]).code

; Find run date and time
caldat, (*rad_fit_info[data_index]).sjul, mm1, dd1, yy1, hh1, mn1
caldat, (*rad_fit_info[data_index]).fjul, mm2, dd2, yy2, hh2, mn2
date = yy1*10000L + mm1*100L + dd1
time = [hh1*100L, (hh2+1)*100L]

; radar structure
radID = where(network.ID eq (*rad_fit_info[data_index]).id)
tval = TimeYMDHMSToEpoch(yy1, mm1, dd1, 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]

; Range array
ajul = ((*rad_fit_info[data_index]).sjul+(*rad_fit_info[data_index]).fjul)/2.d
yrsec = (ajul-julday(1,1,yy1,0,0,0))*86400.d
rad_define_beams, (*rad_fit_info[data_index]).id, nbeams, ngates, yy1, yrsec, coords='rang', $
		/normal, fov_loc_center=range_loc

; Calculate elevation
rad_fit_calculate_elevation, date=date, time=time, /overwrite, phidiff=1., tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8.

; Calculate virtual altitude
hf = fltarr(nrecs,ngates)
for ib=0,(*rad_fit_info[data_index]).nbeams-1 do begin
	binds = where((*rad_fit_data[data_index]).beam eq ib,ccb)
	ranges = transpose(reform(range_loc[0,ib,*])) ## (1.+fltarr(ccb))
	hf[binds,*] = sqrt(ranges^2 + Rav^2 + 2.*ranges*Rav*sin((*rad_fit_data[data_index]).elevation[binds,*]*!dtor)) - Rav
endfor

; Passes virtual altitude to width parameter of rad_fit_data structure
noelevinds = where((*rad_fit_data[data_index]).elevation eq 10000., ccnoelevinds, complement=elevinds, ncomplement=ccelevinds)
if ccelevinds gt 0 then $
	(*rad_fit_data[data_index]).width[elevinds] = hf[elevinds]
; Enforce no value
if ccnoelevinds gt 0 then (*rad_fit_data[data_index]).width[noelevinds] = 10000.

; Plot it
ps_open, '~/Desktop/'+radar+'.'+strtrim(date,2)+'.'+strtrim(time[0],2)+'.'+strtrim(time[1],2)+'altv.ps'
clear_page
set_colorsteps, 250
rad_fit_plot_rti_panel, 1,2,0,1, param='width', scale=[200.,500.], beam=12, time=[0,1200], coords='rang', yrange=[0.,2500.], /last, /first, /bar
plot_colorbar, 1,2,0,1, scale=[200.,500.], legend='Virtual height [km]', /no_rotate, $
	level_format='(F5.0)', /keep_first_last_label, nlevels=4, /continuous
ps_close, /no_f


end