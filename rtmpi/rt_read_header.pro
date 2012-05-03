;+ 
; NAME: 
; RT_READ_HEADER
;
; PURPOSE: 
; This procedure reads header of rays.dat
; 
; CATEGORY: 
; Input/Output
; 
; CALLING SEQUENCE:
; RT_READ_HEADER, txlat, txlon, azim, $
; 	elev_beg, elev_end, elev_stp, freq, $
; 	year, mmdd, hour, nrays=nrays, dir=dir
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
;-
pro rt_read_header, txlat, txlon, azim_beg, azim_end, azim_stp, $
		elev_beg, elev_end, elev_stp, freq, $
		year, mmdd, hour_beg, hour_end, hour_stp, $
		nhour, nazim, nelev, dir=dir

if ~keyword_set(dir) then $
	openr, lun, 'rays.dat', /get_lun $
else $
	openr, lun, dir+'/rays.dat', /get_lun

dum = 0L
; Header info
txlat = 0.
txlon = 0.
azim_beg = 0.
azim_end = 0.
azim_stp = 0.
elev_beg = 0.
elev_end = 0.
elev_stp = 0.
freq = 0.
nhop = 0L
year = 0L
mmdd = 0L
hour_beg = 0.
hour_end = 0.
hour_stp = 0.
hmf2 = 0.
nmf2 = 0.
nhour = 0L
nazim = 0L
nelev = 0L

READU, lun,$
	txlat, txlon, azim_beg, azim_end, azim_stp, $
	elev_beg, elev_end, elev_stp, freq, nhop, $
	year, mmdd, hour_beg, hour_end, hour_stp, hmf2, nmf2

READU, lun, nhour, nazim, nelev

; print, txlat, txlon, azim_beg, azim_end, azim_stp, $
; 	elev_beg, elev_end, elev_stp, freq, nhop, $
; 	year, mmdd, hour_beg, hour_end, hour_stp
; print, nhour, nazim, nelev

free_lun, lun

END
