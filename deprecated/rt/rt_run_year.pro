pro rt_run_year, year

date = year*10000L + (indgen(12) + 1L)*100L + 1L
sjul = julday(01,15,year)
fjul = julday(01,01,year+1)
ddays = 15.
; juls = dindgen( FIX((fjul-sjul)/ddays) )*ddays + sjul
juls = timegen(12,units='Month',year=year,start=sjul)
caldat, juls, month, day, years
date = years*10000L+month*100L+day

for d=0,n_elements(date)-1 do begin
  dirname = 'Desktop/rt_year/'
  filename = 'rt_'+STRTRIM(year,2)+STRTRIM(STRING(month[d],format='(I02)'),2)+STRTRIM(STRING(day[d],format='(I02)'),2)
  dirTest = FILE_TEST(dirname+filename+'.png', /READ)
  if (dirTest ne 1) then begin
    rt_run, date[d], 'bks', time=[500,500], beam=7, /hhour, freq=11
    ps_open, dirname+filename+'_Nel.ps'
      rt_plot_rti, param=['power','edens'], yrange=[0,75]
    ps_close
    spawn, 'ps2png.sh '+dirname+filename+'_Nel.ps'
    spawn, 'mv '+filename+'_Nel.png '+dirname+filename+'_Nel.png' 
    ps_open, dirname+filename+'.ps'
      rt_plot_rti, param='power', yrange=[0,75], /sun
    ps_close
    spawn, 'ps2png.sh '+dirname+filename+'.ps'
    spawn, 'mv '+filename+'.png '+dirname+filename+'.png' 
  endif
endfor

; spawn, 'convert -delay 100  Desktop/rt_year/rt_'+STRTRIM(year,2)+'????.png Desktop/rt_year/rt_year.gif'
; spawn, 'convert -delay 100  Desktop/rt_year/rt_'+STRTRIM(year,2)+'????_Nel.png Desktop/rt_year/rt_year_Nel.gif'

END