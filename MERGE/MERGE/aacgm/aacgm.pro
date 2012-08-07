;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;       CNV_MDHMS_SEC
;
; PURPOSE:
;       convert time from the form year, month, day, hour, minute, sec
;       to seconds of the year
;
; Calling sequence:
;	t = cnv_mdhms_sec(yr, month, day, hour, minute, sec)
;	   if the arguments are arrays, they must be the same size
;
;	   OR
;
;	t = cnv_mdhms_sec(time_array)
;	   where time_array is a 2-d intarr (6,n)
;	   the first dimension gives the year, month, day, hour, minute, sec
;
;---------------------------------------------------------------------
;
function cnv_mdhms_sec,yr,mo,day,hr,mn,sc

jday=[0,31,59,90,120,151,181,212,243,273,304,334]
mday=[31,28,31,30,31,30,31,31,30,31,30,31]

if n_params() eq 1 then begin
	sz = size(yr)
	if (sz(0) eq 1) then begin
		y = yr(0)
		m = yr(1)
		d = yr(2)
		h = yr(3)
		n = yr(4)
		s = yr(5)
	endif else begin
		if (sz(1) eq 6) then begin
			y=intarr(sz(2))
			m=y & d=y & h=y & n=y & s=y
			y = yr(0,*)
			m = yr(1,*)
			d = yr(2,*)
			h = yr(3,*)
			n = yr(4,*)
			s = yr(5,*)
		endif else begin 
		  if sz(2) eq 6 then begin
			y=intarr(sz(1))
			m=y & d=y & h=y & n=y & s=y
			y= yr(*,0)
			m= yr(*,1)
			d= yr(*,2)
			h= yr(*,3)
			n= yr(*,4)
			s= yr(*,5)
		  endif else begin
			print,"input array must be 6 x n  or n x 6"
			help,yr
			return,-1
		  endelse
		endelse
	endelse
endif else begin
	y = yr
	m = mo
	d = day
	h = hr
	n = mn
	s = sc
endelse
;
t = long(jday(m-1)+d - 1)
if (n_elements(m) gt 1) then begin
	leap = where (m gt 2 AND ((y mod 4) EQ 0))
	ls = size(leap)
	if ls(0) ne 0 then t(leap)=t(leap)+1
endif $
else if (m gt 2) AND ((y mod 4) EQ 0) then t= t+1
;
t = ((t*24 + h)*60 + n)*60 + s
return,t
end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;	CNVTIME
;
; PURPOSE:
; 	This provides an alternate entry point to CNV_MDHMS_SEC
;
;----------------------------------------------------------------
;
function cnvtime,yr,mo,dy,hr,mn,sc
return,cnv_mdhms_sec(yr,mo,dy,hr,mn,sc)
end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;	CNV_SEC_MDHMS
;
; PURPOSE:
;	Convert the time in seconds of the year to the form month, day
;	hour, minutes, and seconds
;
; CALLING SEQUENCE:
;	status = cnv_sec_mdhms(yr, mo, day, hour, minute, sec, yr_secs)
;
;	All the arguments must be given. yr must be assigned a value (this
;	determines whether you are in  aleap year or not).  Mo, day, hour,
;	minute and sec must be declared to be normal integers and yr_secs
;	must be given a value and must be a long integer.
;
;	The status will be 0 for success and -1 for failure
;-----------------------------------------------------------------
;
function cnv_sec_mdhms,yr,mo,dy,hr,mn,sec,seconds

modys=[31,28,31,30,31,30,31,31,30,31,30,31]

if (n_elements(yr) eq 1) then $
  if (yr mod 4 eq 0) then modys(1)=29 else modys(1)=28 $
else if (yr(0) mod 4 eq 0) then modys(1)=29 else modys(1)=28

sec = fix(seconds mod 60)
tmin = (seconds - sec)/60
mn = fix(tmin mod 60)
thr = (tmin - mn)/60
hr = fix(thr mod 24)
tdays = fix((thr - hr)/24)
if n_elements(tdays) eq 1 then begin
  m = 0
  while tdays GT 0 AND m LT 12 do begin
    tdays = tdays - modys(m)
    m = m + 1
    endwhile
  if (m GT 0) then begin
    if (tdays EQ 0) then begin
      mo = m + 1
      dy = 1
    endif else begin
      mo = m
      dy = tdays + modys(m-1) + 1
    endelse
  endif else begin
    mo = 1
    dy = tdays + 1
  endelse
  return,0
endif else begin
  for i = 0,n_elements(tdays)-1 do begin
    m=0
    while tdays(i) GT 0 AND m LT 12 do begin
	tdays(i) = tdays(i) - modys(m)
	m = m + 1
	endwhile
    if (m GT 0) then begin
      if (tdays(i) EQ 0) then begin
        mo(i) = m + 1
        dy(i) = 1
      endif else begin
        mo(i) = m
        dy(i) = tdays(i) + modys(m-1) + 1
      endelse
    endif else begin
      mo(i) = 1
      dy(i) = tdays(i) +1
    endelse
  endfor
endelse
return,0
end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;	CNVCOORD
;
; PURPOSE:
;	Convert coordinates from geographic to PACE magnetic or
;	from PACE magnetic to geographic
;
;	calling sequence:
;	  pos = cnvcoord(inpos,[inlong],[height],[/GEO], [MODEL=year])
;	     the routine can be called either with a 3-element floating
;	     point array giving the input latitude, longitude and height
;	     or it can be called with 3 separate floating point values
;	     giving the same inputs.  The default conversion is from
;	     geographic to PACE geomagnetic coordinates.  If the keyword
;	     GEO is set (/GEO) then the conversion is from magnetic to
;	     geographic.  The optional keyword MODEL can be used
;            to select a particular magnetic coordinates model.
;            It should be set to the year of the model (for example,
;            MODEL=1990).  If the model is not specified, the most
;            recent model is used by default.  Once a model has
;            been specified it will continue to be used on subsequent
;            calls, even if the keyword is not included in the call.
;;
;	     The input array can also be given in the form 
;		inpos(3,d1,d2,. . .)
;	     The output array will be in the same form.
;----------------------------------------------------------------------------
function cnvcoord, in1, in2, in3, geo = geo, model = model
;
	if (keyword_set(geo)) then mgflag = 2 else mgflag = 1
        if (keyword_set(model)) then myear = model else myear = 1995
        myear = fix(myear)
        if (n_params() GE 3) then inp = float([in1,in2,in3]) $
		else inp = float(in1)
        if (n_elements(inp) MOD 3 NE 0) then begin
          print,'input position must be fltarr(3) [lat,long,height]'
          return,[0,0,0]
          end
	order=4
	err=0
	s0 = size(inp)
	tmp = reform(inp,3,n_elements(inp)/3)
	outpos=tmp
	s1 = size(tmp)
	for j=0,s1(2)-1 do begin
	  invec = fltarr(3)
	  invec(0:2) = tmp(0:2,j)
	  outvec = invec
	  ret_val = call_external(getenv ('SD_LIB_PGM'),$
		'cnvcoord_idl',invec,order,outvec,mgflag,err,myear)
          if (ret_val NE 0) then begin 
            print,"cnvcoord error = ",ret_val,err
            return,0
            endif
	  outpos(*,j)=outvec
	endfor
	outpos=reform(outpos,s0(1:n_elements(s0)-3))
	return,outpos
	end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;	MLT
;
; PURPOSE:
;
;	convert UT time to MLT
;
;	calling sequence:
;	  mt = mlt(year, ut_seconds, mag_long)
;		inputs:  year, time in seconds from Jan. 1 at 00;00:00 UT
;		         magnetic longitude of the observation point
;	
;	        the time in seconds can be found with routine "cnvtime"
;		the magnetic longitude of the obs. point can be found
;		  by using routine "cnvcoord"
;
;-----------------------------------------------------------------------------
function mlt, year, t, mlong
	year = fix(year)
	t = long(t)
	mlong = float(mlong)
	if (n_elements(t) EQ 1) then mt=0.0 else mt=fltarr(n_elements(t))
	if (n_elements(t) EQ 1) then $
          mt = call_external(getenv('SD_LIB_PGM'), 'mlt_idl', $
		year, t, mlong,/f_value) $
	else $
	  for i=0,n_elements(t)-1 do mt(i)=call_external(getenv('SD_LIB_PGM'),$
	     'mlt_idl', year, t(i), mlong, /f_value)

	return, mt
	end
;

