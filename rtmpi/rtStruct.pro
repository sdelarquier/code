; rtStruct.pro
; =======
; Author: S. de Larquier
;
; $License$
;
;
; Functions
; ---------------
;
; rtStruct
; rtStructInit
; rtWriteStruct
; rtReadStruct
; rtFileTest
;
; ---------------------------------------------------------------


; ********************************************************************
; Dummy procedure for compilation purposes
; ********************************************************************
pro rtStruct

	return

end


; ********************************************************************
; Creates structure to store ray tracing results
; You can add fields to these structure without having to modify the read and write procedures
; as long as you respect a few rules:
; - no strings in rt_dataD
; - rt_data expect all its arrays to be indexed with [# time steps, # beams, ...]
; - no arrays with more than 1 dimension in rt_info
; ********************************************************************
pro	rtStructInit, nhour, nazim, ngates, rt_data, rt_info
	start_mem = memory(/current)
		
	rt_data = { $
		juls: dblarr(nhour), $
		tfreq: fltarr(nhour), $
		beam: intarr(nhour,nazim), $
		azim: fltarr(nhour,nazim), $
		lagpower: fltarr(nhour,nazim,ngates), $
		power: fltarr(nhour,nazim,ngates), $
		gscatter: bytarr(nhour,nazim,ngates), $
		altitude: fltarr(nhour,nazim,ngates), $
		valtitude: fltarr(nhour,nazim,ngates), $
		elevation: fltarr(nhour,nazim,ngates), $
		grange: fltarr(nhour,nazim,ngates), $
		latitude: fltarr(nhour,nazim,ngates), $
		longitude: fltarr(nhour,nazim,ngates), $
		nr: fltarr(nhour,nazim,ngates), $
		aspect: fltarr(nhour,nazim,ngates), $
		edens: fltarr(nhour,nazim,50,250), $
		dip: fltarr(nhour,nazim,2,250) $
	}
; 		edens: fltarr(nhour,nazim,100,250), $
; 		dip: fltarr(nazim,2,250) $

	rt_info = { $
		name: '', $
		id: 0L, $
		sjul: 0.d, $
		fjul: 0.d, $
		timez: '', $
		ngates: 0L, $
		glat: 0., $
		glon: 0., $
		elev_beg: 0., $
		elev_end: 0., $
		elev_stp: 0., $
		bin: '', $
		nhop: 0L, $
		outdir: ''$
	}

end


; ********************************************************************
; Outputs filename for given radar date and time
; ********************************************************************
function rtFileName, date, time, radar, beam=beam, freq=freq, nhop=nhop, back=back, rays=rays, ground=ground, outdir=outdir

	strtime = strtrim(string(time,format='(I04)'),2)
	strdate = strtrim(date,2)

	if ~keyword_set(outdir) then $
		dir = '/sd-data/rt/'+strtrim(date/10000L,2)+'/'+radar+'/' $
	else $
		dir = outdir

	strsupl = ''
	if n_elements(beam) eq 1 then $
		strsupl = strsupl + '.'  + strtrim(string(beam, format='(I02)'), 2)
	if n_elements(freq) eq 1 then $
		strsupl = strsupl + '.'  + strtrim(string(freq, format='(I3)'), 2)
	if n_elements(nhop) eq 1 then $
		strsupl = strsupl + '.'  + strtrim(string(nhop, format='(I1)'), 2) + 'hop'
	if n_elements(ground) eq 1 then $
		strsupl = strsupl + '.'  + 'ground'
	if n_elements(back) eq 1 then $
		strsupl = strsupl + '.'  + 'back'

	if ~keyword_set(rays) then $
		file = strdate + '.' + strtime + '.' + radar + strsupl + '.rt.dat' $
	else $
		file = strdate + '.' + strtime + '.' + radar + strsupl + '.rays'

	filename = dir + file
;   	print, filename

	return, filename
	
end


; ********************************************************************
; Write structure to file
; The way the structure is written is automatically defined by the above
; structures. You can add field to the existing structures and this code
; will take them into account.
; ********************************************************************
pro	rtWriteStruct, rt_data, rt_info, rt_rays, ground=ground, code=code, outdir=outdir

	; Check if there is anything in the structure
	ntags_data = n_tags(rt_data)
	ntags_info = n_tags(rt_info)
	if ntags_data eq 0 then begin
		prinfo, 'No data present: try reading file instead.'
		code = 0
		return
	endif

	; Read number of steps
	ntime = n_elements(rt_data.power[*,0,0,0])
	nazim = n_elements(rt_data.power[0,*,0,0])

	for nh=0,ntime-1 do begin

		; read date and time
		caldat, rt_data.juls[nh], month, day, year, hour, minute

		for nb=0,nazim-1 do begin
			beam = rt_data.beam[nh,nb] 
			freq = rt_data.tfreq[nh]

			; name fit file
			output = rtFileName(year*10000L+month*100L+day, hour*100L+minute, rt_info.name, beam=rt_data.beam[nh,nb], freq=rt_data.tfreq[nh], nhop=rt_info.nhop, ground=ground, outdir=outdir)

			; open fit file
			openw, unit, output, /get_lun
			
			; write number of tags for each structure
			writeu, unit, ntags_data, ntags_info

			; write structure
			for n=0,ntags_info-1 do begin
				if size(rt_info.(n),/type) eq 7 then $
					writeu, unit, size(rt_info.(n),/type), strlen(rt_info.(n)) $
				else $
					writeu, unit, size(rt_info.(n),/type), size(rt_info.(n),/n_elements)
				writeu, unit, rt_info.(n)
			endfor
			for n=0,ntags_data-1 do begin
				writeu, unit, size(rt_data.(n), /type), size(rt_data.(n), /n_dimensions)
				writeu, unit, size(rt_data.(n), /dimensions)
                                case size(rt_data.(n), /n_dimensions) of
					1: writeu, unit, rt_data.(n)[nh]
					2: writeu, unit, rt_data.(n)[nh,nb]
					3: writeu, unit, rt_data.(n)[nh,nb,*]
					4: writeu, unit, rt_data.(n)[nh,nb,*,*]
				endcase
			endfor
			; close fit file
			free_lun, unit
		; End beam loop
		endfor
	; End time loop
	endfor
	code = 1

end


; ********************************************************************
; Read structure from file
; ********************************************************************
pro	rtReadStruct, date, time, radar, beams, freq, rt_data, rt_info, dhour=dhour, nhop=nhop, back=back, ground=ground, code=code, outdir=outdir

	parse_date, date[0], year, month, day
	parse_time, time, shour, sminute, fhour, fminute

	if ~keyword_set(dhour) then $
		dhour = .5

	; find number of files
	if fhour gt 0 then begin
		if fhour le shour then $
			nfiles = ceil( (fhour+fminute/60.)/dhour + (24./dhour - (shour+sminute/60.)/dhour) ) $
		else $
			nfiles = ceil( (fhour+fminute/60. - shour+sminute/60.)/dhour )
	endif else $
		nfiles = 1
	print, nfiles

	; calculate maximum number of time steps (each file can contain 2 hours with a max resolution of 1 min)
	maxnh = nfiles
	; maximum number of beams
	maxna = 24
	; maximum number of gates
	maxngates = 100

	; Initialize temp structure with maximum size possible
	rtStructInit, maxnh, maxna, maxngates, temp_rt_data, temp_rt_info

	; Initialize
	ntags_data = 0L
	ntags_info = 0L
	nazim = n_elements(beams)
	nelev = 0L
	nstep = 0L
	ttype = 0L
	ndims = 0L
	nelem = 0L

	; cycle through files
	tdate = date[0]
	nh = 0L
	hour = shour
	for nf=1,nfiles do begin
		parse_date, date, year, month, day
		if nf eq 1 then $
			sjul = julday(month, day, year, hour)
		if nf eq nfiles then $
			fjul = julday(month, day, year, hour)

		for nb=0,nazim-1 do begin
			; name fit file
			input = rtFileName(tdate, floor(hour)*100L+round((hour-floor(hour))*60.), radar, beam=beams[nb], freq=freq, nhop=nhop, back=back, ground=ground, outdir=outdir)
			print, input
			; test for file
			code = file_test(input, /read)
			if ~code then $
				return

			; open fit file
			openr, unit, input, /get_lun

			; read from file
			readu, unit, ntags_data, ntags_info

			for n=0,ntags_info-1 do begin
				readu, unit, ttype, nelem
				if ttype eq 7 then $
					temp = string('', format='(A'+strtrim(nelem, 2)+')') $
				else $
					temp = make_array(nelem, type=ttype)
				readu, unit, temp
				temp_rt_info.(n) = temp
			endfor
			for n=0,ntags_data-1 do begin
				readu, unit, ttype, ndims
				tnelem = lonarr(ndims)
				readu, unit, tnelem
				tnelem[0] = 1
                                if ndims gt 1 then $
				    tnelem[1] = 1
				temp = make_array(tnelem, type=ttype)
				readu, unit, temp
				case ndims of
					1: temp_rt_data.(n)[nh] = temp
					2: temp_rt_data.(n)[nh,nb] = temp
					3: temp_rt_data.(n)[nh,nb,0:tnelem[2]-1] = temp
					4: temp_rt_data.(n)[nh,nb,0:tnelem[2]-1,0:tnelem[3]-1] = temp
				endcase
			endfor

		free_lun, unit
		endfor		
		
		nh = nh + 1
		hour = hour + dhour
		if hour ge 24. then begin
			hour = hour - 24.
			tdate = calc_date(tdate, 1)
		endif

	endfor

	; Initialize properly sized structure
	rtStructInit, nh, nazim, temp_rt_info.ngates+1, rt_data, rt_info

	; Populate structures
	rt_info = temp_rt_info
	rt_info.sjul = sjul
	rt_info.fjul = fjul
	temp_rt_info = 0
	for n=0,ntags_data-1 do begin
		ndims = size(rt_data.(n), /n_dimensions)
		dims = size(rt_data.(n), /dimensions)
		case ndims of
			1: rt_data.(n) = temp_rt_data.(n)[0:nh-1]
			2: rt_data.(n) = temp_rt_data.(n)[0:nh-1,0:nazim-1]
			3: rt_data.(n) = temp_rt_data.(n)[0:nh-1,0:nazim-1,0:dims[2]-1]
			4: rt_data.(n) = temp_rt_data.(n)[0:nh-1,0:nazim-1,0:dims[2]-1,0:dims[3]-1]
		endcase
	endfor
	temp_rt_data = 0

end


; ********************************************************************
; Test if file is present
; ********************************************************************
pro rtFileTest, date, time, radar, beam=beam, freq=freq, dhour=dhour, nhop=nhop, back=back, rays=rays, ground=ground, code=code, outdir=outdir

	if ~keyword_set(dhour) then $
		dhour = .5

	parse_date, date[0], year, month, day
	parse_time, time, shour, sminute, fhour, fminute

	; find number of files
	if fhour gt 0 then begin
		if fhour le shour then $
			nfiles = ceil( (fhour+fminute/60.)/dhour + (24./dhour - (shour+sminute/60.)/dhour) ) $
		else $
			nfiles = ceil( (fhour+fminute/60. - shour+sminute/60.)/dhour )
	endif else $
		nfiles = 1

	; cycle through files
	tdate = date[0]
	nh = 0L
	hour = shour
	for nf=1,nfiles do begin
		for nb=0,n_elements(beam)-1 do begin
			input = rtFileName(tdate, floor(hour)*100L+round((hour-floor(hour))*60.), radar, beam=beam[nb], freq=freq, nhop=nhop, back=back, ground=ground, rays=rays, outdir=outdir)

			; test for file
			code = file_test(input, /read)
			if ~code then $
				return

		endfor
		hour = hour + dhour
		if hour ge 24. then begin
			hour = hour - 24.
			tdate = calc_date(tdate, 1)
	    	endif
	endfor

end
