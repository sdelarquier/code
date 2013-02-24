pro rad_fov_movie

endDate = julday(07,01,2013)

; Make field-of-view movie
juls = timegen(start=julday(01,01,1983), final=endDate, units='months',step_size=6)
caldat, juls, month, day, year
date = year*10000L+month*100L+day
 
for it=0,n_elements(juls)-1 do begin
    radarFoVs, date[it], 'fovs'+strtrim(string(it,format='(I03)'),2), /fillfov, range=[-55,55], /bardate, /legend
;   spawn, 'ps2png.sh fovs'+strtrim(string(it,format='(I03)'),2)+'.ps'
    spawn, 'gs -sDEVICE=jpeg -dJPEGQ=100 -dNOPAUSE -dBATCH -dSAFER -r300 -sOutputFile=fovs'+strtrim(string(it,format='(I03)'),2)+'.png fovs'+strtrim(string(it,format='(I03)'),2)+'.ps'
    spawn, 'mogrify +repage -rotate 270 -resize 800x800 -crop 650x618+100+0 fovs'+strtrim(string(it,format='(I03)'),2)+'.png'
    spawn, 'rm fovs'+strtrim(string(it,format='(I03)'),2)+'.ps'
endfor
spawn, 'convert -delay 100 fovs000.png -delay 50 fovs???.png -delay 500 fovs'+strtrim(string(n_elements(juls)-1, format='(I03)'),2)+'.png fovs.gif'
spawn, 'rm fovs???.png'


; Make field-of-view movie
juls = timegen(start=julday(01,01,2005), final=endDate, units='months',step_size=6)
caldat, juls, month, day, year
date = year*10000L+month*100L+day
 
for it=0,n_elements(juls)-1 do begin
    radarFoVs, date[it], 'fovsN'+strtrim(string(it,format='(I03)'),2), /fillfov, range=[-55,55], /bardate, /legend
    spawn, 'gs -sDEVICE=jpeg -dJPEGQ=100 -dNOPAUSE -dBATCH -dSAFER -r300 -sOutputFile=fovsN'+strtrim(string(it,format='(I03)'),2)+'.png fovsN'+strtrim(string(it,format='(I03)'),2)+'.ps'
;   spawn, 'mogrify +repage -rotate 270 -resize 800x800 -crop 650x618+100+0 fovs'+strtrim(string(it,format='(I03)'),2)+'.png'
    ;spawn, 'mogrify +repage -rotate 270 -resize 800x800 -crop 710x500+50+100 fovs'+strtrim(string(it,format='(I03)'),2)+'.png'
    spawn, 'mogrify +repage -rotate 270 -resize 800x800 -trim fovsN'+strtrim(string(it,format='(I03)'),2)+'.png'
    spawn, 'rm fovsN'+strtrim(string(it,format='(I03)'),2)+'.ps'
endfor
spawn, 'convert -delay 100 fovsN000.png -delay 50 fovsN???.png -delay 500 fovsN'+strtrim(string(n_elements(juls)-1, format='(I03)'),2)+'.png fovsNmidlat.gif'
; spawn, 'convert -delay 100 fovsS000.png -delay 50 fovsS???.png -delay 500 fovsS'+strtrim(string(n_elements(juls)-1, format='(I03)'),2)+'.png fovsSmidlat.gif'
spawn, 'rm fovs???.png'


end