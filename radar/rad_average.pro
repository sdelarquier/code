pro	rad_average

common rad_data_blk

beam = 7
date = [20100901,20101001]
sfjul, date, [0,2400], sjul, fjul, no_d=nd
hist = dindgen(60,nd)*0.+10000.
for i=0L, nd-1L do begin
	sfjul, adate, atime, sjul+double(i), /jul_to
	rad_fit_set_data_index, -1
	rad_fit_read, adate, 'bks', time=[1200,2400]
	ns = (*rad_fit_info[0]).nscans
	for s=0, ns-1 do begin
		inds = where((*rad_fit_data[0]).beam_scan eq s and (*rad_fit_data[0]).beam eq beam $
			      and (*rad_fit_data[0]).tfreq le 12e3 and (*rad_fit_data[0]).tfreq ge 10e3, sc)
		if sc eq 0 then $
			continue
		tbeams = (*rad_fit_data[0]).beam[inds]
		sinds = sort(tbeams)
		ubeams = uniq(tbeams, sinds)
		thist = (*rad_fit_data[0]).power[inds[sinds[ubeams]],0:59]
		cinds = where(thist ne 10000., gc)
		if gc gt 0 then $
			hist[cinds,i] += thist[cinds]
	endfor
	hist[*,i] = hist[*,i]/TOTAL(hist[*,i])
endfor

openw, unit, './rad_average.dat', /get_lun
writeu, unit, long(nd), long(date), hist
free_lun, unit


end