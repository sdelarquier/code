;+ 
; NAME: 
; RT_READ_IONOS
;
; PURPOSE: 
; This procedure reads ionos.dat: ionospheric scatter predictions.
; 
; CATEGORY: 
; Input/Output
; 
; CALLING SEQUENCE:
; RT_READ_IONOS, rantheta, grpran, ranhour, ranazim, ranelv, ranalt, ran, grndran, weights, nrefract, dir=dir
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORDS:
;
; COMMON BLOCKS:
;
; EXAMPLE:
;
; COPYRIGHT:
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.
;
; MODIFICATION HISTORY:
; Written by Sebastien de Larquier, Aug. 2011
;-
pro 	rt_read_ionos, rantheta, grpran, ranhour, ranazim, ranelv, ranalt, ran, gdran, weights, nrefract, $
			lat=lat, lon=lon, dir=dir, silent=silent, aspect=aspect


; Read rays.dat header to format output arrays
rt_read_header, txlat, txlon, azim_beg, azim_end, azim_stp, $
		elev_beg, elev_end, elev_stp, freq, $
		year, mmdd, hour_beg, hour_end, hour_stp, $
		nhour, nazim, nelev, dir=dir

if ~keyword_set(dir) then $
	filename = 'ionos.dat' $
else $
	filename = dir+'/ionos.dat'


naspstep = 0.
tnaspstep = 0.
trayhour = 0.
trayazim = 0.
trayelev = 0.

; Count lines first
openr, lun, filename, /get_lun
nr = 0L
while ~eof(lun) do begin
	readu, lun, tnaspstep
	readu, lun, trayhour, trayazim, trayelev
	if tnaspstep gt 0. then begin
		tmp = fltarr(tnaspstep)
		readu, lun, tmp
		readu, lun, tmp
		readu, lun, tmp
		readu, lun, tmp
		readu, lun, tmp
		readu, lun, tmp
		readu, lun, tmp
		readu, lun, tmp
		readu, lun, tmp
	endif

	if tnaspstep gt naspstep then naspstep = tnaspstep
	nr = nr + 1
endwhile
free_lun, lun


; Save to arrays
openr, lun, filename, /get_lun
IF naspstep GT 1 THEN BEGIN
	rantheta = fltarr(nhour, nazim, nelev*naspstep)
	grpran = fltarr(nhour, nazim, nelev*naspstep)
	ranhour = fltarr(nhour, nazim, nelev*naspstep)
	ranazim = fltarr(nhour, nazim, nelev*naspstep)
	ranelv = fltarr(nhour, nazim, nelev*naspstep)
	ranalt = fltarr(nhour, nazim, nelev*naspstep)
	ran = fltarr(nhour, nazim, nelev*naspstep)
	gdran = fltarr(nhour, nazim, nelev*naspstep)
	weights = fltarr(nhour, nazim, nelev*naspstep)
	nrefract = fltarr(nhour, nazim, nelev*naspstep)
	lat = fltarr(nhour, nazim, nelev*naspstep)
	lon = fltarr(nhour, nazim, nelev*naspstep)
	aspect = fltarr(nhour, nazim, nelev*naspstep)
	
	nh = 0L
	na = 0L
        nev = 0L
	nevarr = lonarr(nhour,nazim)
        while ~eof(lun) do begin
		readu, lun, tnaspstep
		readu, lun, trayhour, trayazim, trayelev
			
		; find hour, beam and elevation index
		nh = round((trayhour - hour_beg)/hour_stp)
		if (trayhour lt hour_beg) then $
			nh = ((24. - hour_beg)/hour_stp + trayhour/hour_stp)
		na = round((trayazim - azim_beg)/azim_stp)
		nel = round((trayelev - elev_beg)/elev_stp)
		
		if tnaspstep gt 0. then begin
			nev = nevarr[nh,na]
			tmp = fltarr(tnaspstep)
			readu, lun, tmp
			ranalt[nh,na,nev:nev+tnaspstep-1] = tmp*1e-3 - 6370.
			readu, lun, tmp
			rantheta[nh,na,nev:nev+tnaspstep-1] = tmp
			readu, lun, tmp
			grpran[nh,na,nev:nev+tnaspstep-1] = tmp
			readu, lun, tmp
			ran[nh,na,nev:nev+tnaspstep-1] = tmp
			readu, lun, tmp
			weights[nh,na,nev:nev+tnaspstep-1] = tmp
			readu, lun, tmp
			nrefract[nh,na,nev:nev+tnaspstep-1] = tmp
			readu, lun, tmp
			lat[nh,na,nev:nev+tnaspstep-1] = tmp
			readu, lun, tmp
			lon[nh,na,nev:nev+tnaspstep-1] = tmp
			readu, lun, tmp
			aspect[nh,na,nev:nev+tnaspstep-1] = tmp
		        
                        ranelv[nh,na,nev:nev+tnaspstep-1] = trayelev
                        ranazim[nh,na,nev:nev+tnaspstep-1] = trayazim
                        ranhour[nh,na,nev:nev+tnaspstep-1] = trayhour
		        
                        nevarr[nh,na] = nev + round(tnaspstep)
                endif
	endwhile
	gdran = 6370.*rantheta

ENDIF ELSE BEGIN
	grpran = fltarr(nhour, nazim)
	ranhour = fltarr(nhour, nazim)
	ranazim = fltarr(nhour, nazim)
	ranelv = fltarr(nhour, nazim)
	ranalt = fltarr(nhour, nazim)
	ran = fltarr(nhour, nazim)
	gdran = fltarr(nhour, nazim)
	weights = fltarr(nhour, nazim)
	nrefract = fltarr(nhour, nazim)
	lat = fltarr(nhour, nazim)
	lon = fltarr(nhour, nazim)
	aspect = fltarr(nhour, nazim)
ENDELSE
free_lun, lun

END
