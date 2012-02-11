pro	igrf_coefgen
; create coeff files for igrf model as used by IRI and conductivities

nl = FILE_LINES('~/Desktop/igrf11coeffs.txt')
openr, unit, '~/Desktop/igrf11coeffs.txt', /get_lun

bla = ''
readf, unit, bla
readf, unit, bla
readf, unit, bla
readf, unit, bla

format = '(A1,2I3,20I7,F9.1,F10.2,F9.1,F7.1)'
oldgrf = fltarr(21)
gh = ''
n = 0
m = 0
d05 = 0.
i10 = 0.
sv = 0.
gharr = fltarr(nl-4,8)
nn = 0L
while ~eof(unit) do begin
    readf, unit, gh, n, m, oldgrf, d05, i10, sv, format=format
    if strcmp(gh,'g') then begin
	gharr(nn,0) = n
	gharr(nn,1) = m
	gharr(nn,2) = d05
	gharr(nn,4) = i10
	gharr(nn,6) = sv
	nn++
    endif else begin
	gharr(nn-1,0) = n
	gharr(nn-1,1) = m
	gharr(nn-1,3) = d05
	gharr(nn-1,5) = i10
	gharr(nn-1,7) = sv
    endelse
endwhile
free_lun, unit


openw, file_d05, '~/Desktop/dgrf05.dat', /get_lun
printf, file_d05, '    dgrf05'
printf, file_d05, ' 10  6371.2 2005.0'
for nn=0,nl-5 do begin
    printf, file_d05, gharr(nn,0:1), gharr(nn,2:3), format='(x,2I3,F9.1,F8.1)'
endfor
openw, file_i10, '~/Desktop/igrf10.dat', /get_lun
printf, file_i10, '    igrf10'
printf, file_i10, ' 10  6371.2 2010.0'
for nn=0,nl-5 do begin
    printf, file_i10, gharr(nn,0:1), gharr(nn,4:5), format='(x,2I3,F9.1,F8.1)'
endfor
openw, file_i10s, '~/Desktop/igrf10s.dat', /get_lun
printf, file_i10s, '    igrf10s'
printf, file_i10s, '  8  6371.2 2015.0'
for nn=0,nl-5 do begin
    printf, file_i10s, gharr(nn,0:1), gharr(nn,6:7), format='(x,2I3,F8.1,F9.1)'
endfor
free_lun, unit, file_d05, file_i10, file_i10s

end