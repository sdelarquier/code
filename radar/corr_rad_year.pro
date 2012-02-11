pro corr_rad_year, year

date = year*10000L + (indgen(12) + 1L)*100L + 1L
sjul = julday(01,25,year)
fjul = julday(07,01,year)
juls = timegen(start=sjul, final=fjul, units='days')
caldat, juls, month, day, years
date = years*10000L+month*100L+day
print, date

for d=0,n_elements(date)-2 do begin
    adate = date[d]
    dirname = '~/Desktop/rad_corr_year/'
    filename = 'rad_rti_'+STRTRIM(year,2)+STRTRIM(STRING(month[d],format='(I02)'),2)+STRTRIM(STRING(day[d],format='(I02)'),2)
    print, filename
    milh_rad_tsr, date=adate, ps=dirname+filename+'.ps'
    spawn, 'ps2png.sh '+dirname+filename+'.ps'
    spawn, 'mv '+filename+'_pg1.png '+dirname+'png/'+filename+'_pg1.png' 
    spawn, 'mv '+filename+'_pg2.png '+dirname+'png/'+filename+'_pg2.png' 
    spawn, 'mv '+filename+'_pg3.png '+dirname+'png/'+filename+'_pg3.png' 
endfor

end