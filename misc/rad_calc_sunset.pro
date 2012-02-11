PRO	rad_calc_sunset, date, radar, beam, ngates, $
	    risetime=risetime, settime=settime, solnoon=solnoon

common radarinfo

; Define beams range-cell geographic locations
nid = where(network.code[0] eq radar)
stid = network[nid].ID
nbeams = network[nid].SITE[where(network[nid].SITE[*].TVAL eq -1)].MAXBEAM
yrsec = day_no(date[0])*24.*3600.
rad_define_beams, stid, nbeams, ngates, date[0]/10000L, yrsec, $
    coords='geog', fov_loc_center=fov_loc, /normal

; initialize arrays to receive julian values
rise = dblarr(ngates)
sset = dblarr(ngates)
noon = dblarr(ngates)

; Calculate sunset/sunrise/solarnoon in each range gate
for r=0,ngates-1 do begin
    calculate_sunset, date[0], fov_loc[0,beam,r], fov_loc[1,beam,r], $
	risetime=trisetime, settime=tsettime, solnoon=tsolnoon

    parse_date, date[0], year, month, day
    parse_time, trisetime, hour, minutes
    rise[r] = julday(month,day,year,hour,minutes)
    
    parse_time, tsettime, hour, minutes
    shift_d = 0
    if tsettime lt 1200 then $
	shift_d = 1
    sset[r] = julday(month,day+shift_d,year,hour,minutes)
    
    parse_time, tsolnoon, hour, minutes
    noon[r] = julday(month,day,year,hour,minutes)
;     print, trisetime, tsolnoon, tsettime, fov_loc[0,beam,r], fov_loc[1,beam,r]
endfor

; Return arrays
risetime = rise
settime = sset
solnoon = noon

END