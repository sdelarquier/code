pro rt_serial

year = 2010

; Set radar list
radar = ['wal','fhe','fhw','cve','cvw']
nradars = n_elements(radar)

juls = timegen(start=julday(12,01,2010), final=julday(06,01,2011), units='month')
caldat, juls, month, day, year
date = year*10000L+month*100L+day

for ir=0,nradars-1 do begin
	rt_plot_map_all_season, radar[ir], year
endfor

; year = 2011
; 
; ; Set radar list
; radar = ['wal','bks','fhe','fhw','cve','cvw']
; nradars = n_elements(radar)
; 
; juls = timegen(start=julday(01,01,year), final=julday(01,01,year+1), units='month')
; caldat, juls, month, day, year
; date = year*10000L+month*100L+day
; 
; for ir=0,nradars-1 do begin
; 	print, radar[ir]
; 	for i=0,n_elements(juls)-1 do begin
; 		print, date[i]
; 		rt_run, date[i], radar[ir]
; 	endfor
; endfor
; 
; ; Set radar list
; radar = ['wal','bks','fhe','fhw','cve','cvw']
; nradars = n_elements(radar)
; 
; juls = timegen(start=julday(01,15,year), final=julday(01,15,year+1), units='month')
; caldat, juls, month, day, year
; date = year*10000L+month*100L+day
; 
; for ir=0,nradars-1 do begin
; 	print, radar[ir]
; 	for i=0,n_elements(juls)-1 do begin
; 		print, date[i]
; 		rt_run, date[i], radar[ir], /force
; 	endfor
; endfor
; 
; 
; 
; year = 2010
; 
; ; Set radar list
; radar = ['wal','bks','fhe','fhw','cve','cvw']
; nradars = n_elements(radar)
; 
; juls = timegen(start=julday(01,01,year), final=julday(01,01,year+1), units='month')
; caldat, juls, month, day, year
; date = year*10000L+month*100L+day
; 
; for ir=0,nradars-1 do begin
; 	print, radar[ir]
; 	for i=0,n_elements(juls)-1 do begin
; 		print, date[i]
; 		rt_run, date[i], radar[ir], /force
; 	endfor
; endfor
; 
; ; Set radar list
; radar = ['wal','bks','fhe','fhw','cve','cvw']
; nradars = n_elements(radar)
; 
; juls = timegen(start=julday(01,15,year), final=julday(01,15,year+1), units='month')
; caldat, juls, month, day, year
; date = year*10000L+month*100L+day
; 
; for ir=0,nradars-1 do begin
; 	print, radar[ir]
; 	for i=0,n_elements(juls)-1 do begin
; 		print, date[i]
; 		rt_run, date[i], radar[ir], /force
; 	endfor
; endfor

; for ir=0,nradars-1 do begin
; 	date = 20110701
; 	rt_run, date, radar[ir]
; 	rt_plot_rti, param='power', /ps
; 	rad_calc_sunset, date, radar[ir], 7, 20, settime=settime
; 	print, radar[ir], settime[19]
; endfor


end