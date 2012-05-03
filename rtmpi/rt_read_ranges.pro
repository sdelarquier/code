;+ 
; NAME: 
; RT_READ_RANGES
;
; PURPOSE: 
; This procedure reads ranges.dat: ground scatter predictions.
; 
; CATEGORY: 
; Input/Output
; 
; CALLING SEQUENCE:
; RT_READ_RANGES, rantheta, grpran, ranhour, ranazim, ranelv, ranalt, ran, grndran, dir=dir
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
pro 	rt_read_ranges, rantheta, grpran, ranhour, ranazim, ranelv, ranalt, ran, grndran, lat=lat, lon=lon, dir=dir, silent=silent


; Read rays.dat header to format output arrays
rt_read_header, txlat, txlon, azim_beg, azim_end, azim_stp, $
		elev_beg, elev_end, elev_stp, freq, $
		year, mmdd, hour_beg, hour_end, hour_stp, $
		nhour, nazim, nelev, dir=dir

if ~keyword_set(dir) then $
	filename = 'ranges.dat' $
else $
	filename = dir+'/ranges.dat'

tr = 0.
ttheta = 0.
tgrpran = 0.
trayhour = 0.
trayazim = 0.
trayelev = 0.
tran = 0.
tlat = 0.
tlon = 0.

; Count lines first
openr, lun, filename, /get_lun
nr = 0L
while ~eof(lun) do begin
	readu, lun, tr, ttheta, tgrpran, trayhour, trayazim, trayelev, tran, tlat, tlon
	nr = nr + 1
endwhile
free_lun, lun

; Save to arrays
openr, lun, filename, /get_lun
IF nr GT 1 THEN BEGIN
	rantheta = fltarr(nhour, nazim, nelev)
	grpran = fltarr(nhour, nazim, nelev)
	ranhour = fltarr(nhour, nazim, nelev)
	ranazim = fltarr(nhour, nazim, nelev)
	ranelv = fltarr(nhour, nazim, nelev)
	ranalt = fltarr(nhour, nazim, nelev)
	ran = fltarr(nhour, nazim, nelev)
	lat = fltarr(nhour, nazim, nelev)
	lon = fltarr(nhour, nazim, nelev)
	
	nr = 0L
	nh = 0L
	na = 0L
	nev = 0L
	while ~eof(lun) do begin
		readu, lun, tr, ttheta, tgrpran, trayhour, trayazim, trayelev, tran, tlat, tlon

		nh = round((trayhour - hour_beg)/hour_stp)
		if (trayhour lt hour_beg) then $
                    nh = round((24. - hour_beg)/hour_stp + trayhour/hour_stp)
                na = round((trayazim - azim_beg)/azim_stp)
		nev = round((trayelev - elev_beg)/elev_stp)
		
		rantheta[nh,na,nev] = ttheta
		grpran[nh,na,nev] = tgrpran
		ranhour[nh,na,nev] = trayhour
		ranazim[nh,na,nev] = trayazim
		ranelv[nh,na,nev] = trayelev
		ranalt[nh,na,nev] = tr*1e-3 - 6370.
		ran[nh,na,nev] = tran
		lat[nh,na,nev] = tlat
		lon[nh,na,nev] = tlon

		nr = nr + 1
	endwhile
	
	grndran = 6370.*rantheta

ENDIF ELSE BEGIN
	grpran = fltarr(nhour, nazim)
	ranhour = fltarr(nhour, nazim)
	ranazim = fltarr(nhour, nazim)
	ranelv = fltarr(nhour, nazim)
	ranalt = fltarr(nhour, nazim)
	ran = fltarr(nhour, nazim)
	grndran = fltarr(nhour, nazim)
	lat = fltarr(nhour, nazim)
	lon = fltarr(nhour, nazim)
ENDELSE
free_lun, lun


END
