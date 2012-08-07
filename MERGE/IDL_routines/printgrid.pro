pro printgrid

filename = ''
print, ' '
print, 'Enter GRID data file to be plotted'
print, 'It should have been created first by the grid.exe'
print, 'program in the /misc directory. The present program'
print, 'reads the file from the misc/Grid directory'
print, ' '
read, 'Enter File... ', filename
file = getenv('SD_MISC') + '/Grid/' + filename
print, file
get_lun, ug
openr, ug, file

get_lun, up
openw, up, 'grid.prt'

recl1 = 0L
recl2 = 0L
recl3 = 0L
recl4 = 0L
st1 = 0
st2 = 0
height = 0.0
;latg = fltarr(16,16)
;long = fltarr(16,16)
;latm = fltarr(16,16)
;lonm = fltarr(16,16)
latg = fltarr(256)
long = fltarr(256)
latm = fltarr(256)
lonm = fltarr(256)

readu, ug, recl1,st1,st2,height,recl2
readu, ug, recl3,latg, long, latm, lonm,recl4

;for ib=0,15 do begin
;   for jb=0,15 do begin
;	print, ib, jb
;	printf,up, ib,jb,latg(ib,jb),long(ib,jb),latm(ib,jb),lonm(ib,jb)
;   endfor
;endfor

for k=0,255 do begin
   print, k, latg(k), long(k), latm(k), lonm(k)
endfor

free_lun, ug
free_lun, up
end
