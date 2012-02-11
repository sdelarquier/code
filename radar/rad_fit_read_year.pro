pro rad_fit_read_year, year

sjul = julday(04,01,year)
fjul = julday(10,31,year)
juls = dindgen(FIX(fjul-sjul)) + sjul
caldat, juls, month, day, years
date = years*10000L+month*100L+day

for d=0,n_elements(juls)-1 do begin
  rad_fit_read, date[d], 'bks', time=[500,500]
  dirname = 'Desktop/rad_year/'
  filename = 'rad_'+STRTRIM(year,2)+STRTRIM(STRING(month[d],format='(I02)'),2)+STRTRIM(STRING(day[d],format='(I02)'),2)
  ps_open, dirname+filename+'.ps'
    rad_fit_plot_rti, time=[500,500], param='power', beam=7, yrange=[0,75]
  ps_close
  spawn, 'ps2png.sh '+dirname+filename+'.ps'
  spawn, 'mv '+filename+'.png '+dirname+filename+'.png' 
endfor

; spawn, 'convert -delay 100  Desktop/rad_year/rad_'+STRTRIM(year,2)+'????.png Desktop/rad_year/rad_year.gif'

END