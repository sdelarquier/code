; Download data from Madrigal and saves it into a binary file
PRO	milh_read_to_idl, exdate

; Select one of the joint SD/MH observation dates
case exdate of 
	20101117: expdir = '2010/mlh/17nov10/mlh101117g.001'
	20110210: expdir = '2011/mlh/10feb11/mlh110210g.001'
	20110211: expdir = '2011/mlh/11feb11/mlh110211g.001'
endcase

; Fetches data from Madrigal
results = madPrint('http://madrigal.haystack.mit.edu/madrigal', '/opt/madrigal/experiments/'+expdir, $
		    'year,month,day,hour,min,sec,gdalt,az1,el1,range,ti,dti,tr,dtr,ne,dne,vo,dvo,gdlat,glon', '', $
		    'Sebastien de Larquier', 'sdelarquier@vt.edu', 'Virginia Tech')

; distributes data to individual arrays
tyear = results[0,*]
tmonth = results[1,*]
tday = results[2,*]
thour = results[3,*]
tmn = results[4,*]
tsec = results[5,*]
talt = results[6,*]
taz = results[7,*]
tel = results[8,*]
trange = results[9,*]
tti = results[10,*]
tdti = results[11,*]
ttr = results[12,*]
tdtr = results[13,*]
tnel = results[14,*]
tdnel = results[15,*]
tvo = results[16,*]
tdvo = results[17,*]
tvo = results[16,*]
tdvo = results[17,*]
tlat = results[18,*]
tlon = results[19,*]

inds = where(tyear gt 2000. and tyear le 2100. $
    and tmonth ge 0. and tmonth le 12.$
    and tday ge 0. and tday le 31.$
    and thour ge 0. and thour le 24.$
    and tmn ge 0. and tmn le 60.$
    and tsec ge 0. and tsec le 60.$
    and talt ge 0. and talt le 1000.$
    and taz ge -180. and taz le 180.$
    and tel ge 0. and tel le 90.$
    and trange ge 0. and trange le 2000.$
    and tlat ge -90. and tlat le 90.$
    and tlon ge -180. and tlon le 180., cinds)

juls = JULDAY(tmonth[inds],tday[inds],tyear[inds],thour[inds],tmn[inds],tsec[inds])
alt = talt[inds]
az = taz[inds]
el = tel[inds]
lat = tlat[inds]
lon = tlon[inds]
range = trange[inds]
ti = tti[inds]
dti = tdti[inds]
tr = ttr[inds]
dtr = tdtr[inds]
nel = tnel[inds]
dnel = tdnel[inds]
vo = tvo[inds]
dvo = tdvo[inds]

openw, final, '~/Documents/IDL/milh/milh_data_idl'+strtrim(exdate,2)+'.dat', /get_lun
writeu, final, cinds, juls, alt, az, el, range, lat, lon, ti, tr, nel, vo, dti, dtr, dnel, dvo
; writeu, final, tyear, tmonth, tday, thour, tmn, tsec, talt, taz, tel, trange, tlat, tlon, tti, ttr, tnel, tvo, tdti, tdtr, tdnel, tdvo
free_lun, final


; nlines = n_elements(tyear)
; openw, final, '~/Desktop/milh_data_asci.dat', /get_lun
; printf,final, nlines, format='(I6)'
; for nl = 0, nlines/2 do begin
; ; 	printf, final, juls[nl], az[nl], el[nl], range[nl], ti[nl,*], tr[nl,*], popl[nl,*], vo[nl,*], $
; ; 	    format = '(x,D,11F10.2)'
; 	printf, final, tyear[nl], tmonth[nl], tday[nl], thour[nl], tmn[nl], tsec[nl], taz[nl], tel[nl], trange[nl], tti[nl], ttr[nl], tnel[nl], tvo[nl], $
; 	    format = '(x,6I5,7F10.2)'
; endfor
; free_lun, final


END