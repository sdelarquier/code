;+ 
; NAME: 
; RT_READ_EDENS
;
; PURPOSE: 
; This procedure reads electron density for rt_plot_rays
; 
; CATEGORY: 
; Input/Output
; 
; CALLING SEQUENCE:
; RT_READ_EDENS, edens, lats, lons, alts, dir=dir
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
; Modified by S. de Larquier, Aug. 2011
;-
pro rt_read_edens, edens, dip=dip, lats=lats, lons=lons, alts=alts, thtedens=thtedens, dir=dir

; Read rays.dat header to format output arrays
rt_read_header, txlat, txlon, azim_beg, azim_end, azim_stp, $
		elev_beg, elev_end, elev_stp, freq, $
		year, mmdd, hour_beg, hour_end, hour_stp, $
		nhour, nazim, nelev, dir=dir
	
if ~keyword_set(dir) then $
	openr, lun, 'edens.dat', /get_lun $
else $
	openr, lun, dir+'/edens.dat', /get_lun

; Number of points in horizontal direction
npoints = 500

; Initialize arrays
pos = fltarr(nhour, nazim, npoints, 2)
tht = fltarr(nhour, nazim, npoints)
edens = fltarr(nhour, nazim, npoints, npoints)
dip = fltarr(nhour, nazim, 2, npoints)

nel = 0L
while ~eof(lun) do begin
	readu, lun, hour, azim
	
	nh = round((hour - hour_beg)/hour_stp)
        if hour lt hour_beg then $
            nh = round((24. - hour_beg)/hour_stp + hour/hour_stp)
        na = round((azim - azim_beg)/azim_stp)

	tmp = fltarr(npoints,2)
	readu, lun, tmp
	pos[nh, na, *, *] = tmp

	tmp = fltarr(npoints)
	readu, lun, tmp
	tht[nh, na, *] = tmp

	tmp = fltarr(npoints, npoints)
	readu, lun, tmp
	edens[nh, na, *, *] = tmp

	tmp = fltarr(npoints,2)
	readu, lun, tmp
	dip[nh, na, *, *] = transpose(tmp)

	nel = nel + 1
endwhile
free_lun, lun

alts = 60. + findgen(npoints)*1.
lats = pos[*,*,*,0]
lons = pos[*,*,*,1]
end
