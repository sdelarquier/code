pro rad_rti_hist_year, year

; date = year*10000L + (indgen(12) + 1L)*100L + 1L
; sjul = julday(01,01,year)
; fjul = julday(01,01,year+1)
; ddays = 30.
; juls = dindgen( CEIL((fjul-sjul)/ddays) )*ddays + sjul
; caldat, juls, month, day, years
; date = years*10000L+month*100L+day
; print, date
; 
; for d=0,n_elements(date)-2 do begin
;     adate = date[d:d+1]
;     dirname = '~/Desktop/rad_year/'
;     filename = 'rad_rti_'+STRTRIM(year,2)+STRTRIM(STRING(month[d],format='(I02)'),2)+STRTRIM(STRING(day[d],format='(I02)'),2)
;     print, filename
;     ps_open, dirname+filename+'.ps'
; 	rad_rti_hist, adate
;     ps_close
;     spawn, 'ps2png.sh '+dirname+filename+'.ps'
;     spawn, 'mv '+filename+'.png '+dirname+filename+'.png' 
; endfor

; spawn, 'convert -delay 100  Desktop/rad_year/rad_'+STRTRIM(year,2)+'????.png Desktop/rad_year/rad_year.gif'

date = [20100101,20100201]
parse_date, date[0], year, month, day
adate = date
dirname = '~/Desktop/rad_year/'
filename = 'rad_rti_'+STRTRIM(year,2)+STRTRIM(STRING(month,format='(I02)'),2)+STRTRIM(STRING(day,format='(I02)'),2)
print, filename
ps_open, dirname+filename+'.ps'
    rad_rti_hist, adate
ps_close
spawn, 'ps2png.sh '+dirname+filename+'.ps'
spawn, 'mv '+filename+'.png '+dirname+filename+'.png' 

date = [20100401,20100501]
parse_date, date[0], year, month, day
adate = date
dirname = '~/Desktop/rad_year/'
filename = 'rad_rti_'+STRTRIM(year,2)+STRTRIM(STRING(month,format='(I02)'),2)+STRTRIM(STRING(day,format='(I02)'),2)
print, filename
ps_open, dirname+filename+'.ps'
    rad_rti_hist, adate
ps_close
spawn, 'ps2png.sh '+dirname+filename+'.ps'
spawn, 'mv '+filename+'.png '+dirname+filename+'.png' 

date = [20100601,20100701]
parse_date, date[0], year, month, day
adate = date
dirname = '~/Desktop/rad_year/'
filename = 'rad_rti_'+STRTRIM(year,2)+STRTRIM(STRING(month,format='(I02)'),2)+STRTRIM(STRING(day,format='(I02)'),2)
print, filename
ps_open, dirname+filename+'.ps'
    rad_rti_hist, adate
ps_close
spawn, 'ps2png.sh '+dirname+filename+'.ps'
spawn, 'mv '+filename+'.png '+dirname+filename+'.png' 

date = [20100801,20100901]
parse_date, date[0], year, month, day
adate = date
dirname = '~/Desktop/rad_year/'
filename = 'rad_rti_'+STRTRIM(year,2)+STRTRIM(STRING(month,format='(I02)'),2)+STRTRIM(STRING(day,format='(I02)'),2)
print, filename
ps_open, dirname+filename+'.ps'
    rad_rti_hist, adate
ps_close
spawn, 'ps2png.sh '+dirname+filename+'.ps'
spawn, 'mv '+filename+'.png '+dirname+filename+'.png' 

date = [20100901,20101001]
parse_date, date[0], year, month, day
adate = date
dirname = '~/Desktop/rad_year/'
filename = 'rad_rti_'+STRTRIM(year,2)+STRTRIM(STRING(month,format='(I02)'),2)+STRTRIM(STRING(day,format='(I02)'),2)
print, filename
ps_open, dirname+filename+'.ps'
    rad_rti_hist, adate
ps_close
spawn, 'ps2png.sh '+dirname+filename+'.ps'
spawn, 'mv '+filename+'.png '+dirname+filename+'.png' 

date = [20101101,20101201]
parse_date, date[0], year, month, day
adate = date
dirname = '~/Desktop/rad_year/'
filename = 'rad_rti_'+STRTRIM(year,2)+STRTRIM(STRING(month,format='(I02)'),2)+STRTRIM(STRING(day,format='(I02)'),2)
print, filename
ps_open, dirname+filename+'.ps'
    rad_rti_hist, adate
ps_close
spawn, 'ps2png.sh '+dirname+filename+'.ps'
spawn, 'mv '+filename+'.png '+dirname+filename+'.png' 


END