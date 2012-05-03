;+ 
; NAME: 
; RT_READ_RANGES
;
; PURPOSE: 
; This procedure reads output files from rt_read_rays2files
; 
; CATEGORY: 
; Input/Output
; 
; CALLING SEQUENCE:
; RT_READ_RANGES, date, radar, time, radpos=radpos, thtpos=thtpos, grppth=grppth, $
;			range=range, nrefl=nrefl, raysteps=raysteps
;
; INPUTS:
; DATE: YYYYMMDD
;
; RADAR: 3 letters code
;
; TIME: HHMM
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
; Created by Sebastien, Nov. 2011
;-
pro rt_read_rays, date, radar, time, beam, freq, radpos=radpos, thtpos=thtpos, grppth=grppth, $
			range=range, nrefl=nrefl, raysteps=raysteps, lats=lats, lons=lons, nhop=nhop, back=back, $
			code=code, dirname=dirname


if ~keyword_set(nhop) then $
	nhop = 1L

; Check for data availability
filename = rtFileName(date, time, radar, beam=beam, freq=freq, nhop=nhop, back=back, /rays, outdir=dirname)
if ~FILE_TEST(filename, /read) then begin
	print, 'No data present'
	; fail
	code = 0
	return
endif

; header allocation
rayhour = 0.
rayazim = 0.
rayelev = 0.
nelev = 0L
nel = 0

; open file
openr, unit, filename, /get_lun

; read nazim, nelev and number of steps per ray
readu, unit, nelev
raysteps = lonarr(nelev)
readu, unit, raysteps

; content matrices
maxsteps = max(raysteps)
radpos = fltarr(nelev,maxsteps)
thtpos = fltarr(nelev,maxsteps)
grppth = fltarr(nelev,maxsteps)
range = fltarr(nelev,maxsteps)
nrefl = fltarr(nelev,maxsteps)

; read file into matrices

while ~eof(unit) do begin
	; read header
	readu, unit, rayhour, rayazim, rayelev
	readu, unit, nel

	if raysteps[nel] gt 0 then begin
		tmp = fltarr(raysteps[nel])
		readu, unit, tmp
		radpos[nel,0:raysteps[nel]-1] = tmp

		readu, unit, tmp
		thtpos[nel,0:raysteps[nel]-1] = tmp

		readu, unit, tmp
		grppth[nel,0:raysteps[nel]-1] = tmp

		readu, unit, tmp
		range[nel,0:raysteps[nel]-1] = tmp

		readu, unit, tmp
		nrefl[nel,0:raysteps[nel]-1] = tmp
	endif

endwhile
free_lun, unit

; Success!
code = 1

END
