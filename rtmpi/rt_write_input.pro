;+ 
; NAME: 
; RT_WRITE_INPUT
;
; PURPOSE: 
; This procedure generates input file for raytracing code
; 
; CATEGORY: 
; Input/Output
; 
; CALLING SEQUENCE:
; RT_WRITE_INPUT
;
; INPUTS:
; RADAR: the radar code for which you want to run the raytracing
;
; KEYWORD PARAMETERS:
;
; COMMON BLOCKS:
; RADARINFO
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
; Written by Sebastien de Larquier, Sept. 2010
;	Last modified 17-09-2010
;-
PRO	rt_write_input, lat, lon, azim, date, hour, freq=freq, elev=elev, nhop=nhop, nmf2=nmf2, hmf2=hmf2, $
		outdir=outdir, filename=filename, silent=silent

common radarinfo

parse_date, date, year, month, day

if ~keyword_set(outdir) then $
  outdir = '/tmp/'

if ~file_test(outdir, /dir) then begin
  prinfo, 'Output directory does not exist: '+outdir
  return
endif

if ~keyword_set(filename) then $
  filename = 'Input_RT.inp'

davit_lib = getenv("DAVIT_LIB")
; davit_lib = ''
; Read base input file
openr, unit, davit_lib+'/vt/fort/rtmpi/Inputs_tpl.inp', /get_lun

; Skip first 2 lines
bla = ''
readf, unit, bla, format='(A)'
readf, unit, bla, format='(A)'

; Then go on until end of file
fmt 	= '(I3,F17.2)'
tdata 	= {ID:0S, VAL:0.0}
data 	= REPLICATE(tdata,20)
j = 0
status = FSTAT(unit)
POINT_LUN, -unit, pos 
WHILE (~EOF(unit) AND pos lt status.size - 2) DO BEGIN
	readf, unit, tdata, format=fmt
	data[j] = tdata
	j++
	POINT_LUN, -unit, pos 
ENDWHILE

free_lun, unit

; Set new radar position
data[where(data.ID eq 1)].VAL = lat
data[where(data.ID eq 2)].VAL = lon
data[where(data.ID eq 3)].VAL = azim[0]
data[where(data.ID eq 4)].VAL = azim[1]
data[where(data.ID eq 5)].VAL = azim[2]

; Set elevation limits
if ~keyword_set(elev) then $
	elev = [5., 55., .1]
data[where(data.ID eq 6)].VAL = elev[0]
data[where(data.ID eq 7)].VAL = elev[1]
data[where(data.ID eq 8)].VAL = elev[2]

; Set frequency
if ~keyword_set(freq) then $
	freq = 11.
data[where(data.ID eq 9)].VAL = freq

; Set number of hops
if ~keyword_set(nhop) then $
	nhop = 1
data[where(data.ID eq 10)].VAL = nhop

; Set date and time
data[where(data.ID eq 11)].VAL = year
data[where(data.ID eq 12)].VAL = month*100L + day
data[where(data.ID eq 13)].VAL = hour[0]
data[where(data.ID eq 14)].VAL = hour[1]
data[where(data.ID eq 15)].VAL = hour[2]

; Set frequency
if ~keyword_set(hmf2) then $
	hmf2 = 0.
data[where(data.ID eq 16)].VAL = hmf2
if ~keyword_set(nmf2) then $
	nmf2 = 0.
data[where(data.ID eq 17)].VAL = nmf2

; Write input file
openw, unit, outdir+filename, /get_lun

; Skip first 2 lines
printf, unit, bla
printf, unit, bla
; Write input to file
FOR i=0,j-1 DO BEGIN
	tdata = data[i]
	if tdata.ID eq 10 or $
		 tdata.ID eq 11 or $
		 tdata.ID eq 12 then $
				ifmt = '(I3,I17)' $
	else $
		ifmt = fmt
	printf, unit, tdata, format=ifmt
ENDFOR

free_lun, unit

END
