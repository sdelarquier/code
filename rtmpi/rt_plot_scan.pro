;+
; NAME:
; RT_PLOT_SCAN
;
; PURPOSE:
; This procedure creates range-time plot from raytracing data
;
; CATEGORY:
; Graphics
;
; CALLING SEQUENCE:
; RT_PLOT_SCAN
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORDS PARAMETERS:
; PARAM: 'power' or 'elevation' or 'altitude' or 'valtitude'
;
; COMMON BLOCKS:
; RT_DATA_BLK
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
; Written by Sebastien de Larquier, Jan. 2012
;	Last modified 02-2012
;-
pro rt_plot_scan, time, date=date, $
	param=param, coords=coords, rotate=rotate, $
	xrange=xrange, yrange=yrange, scale=scale, $
	silent=silent, ground=ground, $
	charthick=charthick, charsize=charsize, $
	no_title=no_title, ionos=ionos, grid=grid, ps=ps

common	rt_data_blk

help, rt_info, /st, output=infout
if n_elements(infout) le 2 then begin
	print, 'No data present'
	return
endif

if ~keyword_set(param) then $
	param = 'power'
nparams = n_elements(param)
set_format, /sardines, /tokyo, /portrait

if ~keyword_set(coords) then $
	coords = 'magn'

legend = param
for p=0,nparams-1 do begin
	case param[p] of
		'power': legend[p] = textoidl('Relative power [dB]')
		'elevation': legend[p] = textoidl('Elevation angle [\circ]')
		'altitude': legend[p] = textoidl('Reflection alt. [km]')
		'valtitude': legend[p] = textoidl('Virtual height [km]')
		'nr': legend[p] = textoidl('Refractive index')
		'off aspect': legend[p] = textoidl('off aspect [\circ]')
	else: print, 'Parameter '+param+' unknown'
	endcase
endfor

if ~keyword_set(scale) then begin
	scale = 0.*findgen(2*nparams)
	for p=0,nparams-1 do begin
		case param[p] of
			'power': scale[2*p:2*p+1]=[-30.,0.]
			'elevation': scale[2*p:2*p+1]=[10.,35.]
			'altitude': scale[2*p:2*p+1] =[100.,500.]
			'valtitude': scale[2*p:2*p+1] =[100.,500.]
			'nr': scale[2*p:2*p+1] =[0.8,1.]
			'aspect': scale[2*p:2*p+1] =[0.,1.]
		else: print, 'Parameter '+param+' unknown'
		endcase
	endfor
endif

if ~keyword_set(date) then begin
	if ~keyword_set(silent) then $
		prinfo, 'No DATE given, trying for scan date.'
	caldat, rt_data.juls[0], month, day, year
	date = year*10000L + month*100L + day
endif

; Number of panels
npanels = n_elements(param)

; calculate number of panels per page
xmaps = floor(sqrt(npanels)) > 1
ymaps = ceil(npanels/float(xmaps)) > 1

; take into account format of page
; if landscape, make xmaps > ymaps
fmt = get_format(landscape=ls, sardines=sd)
if ls then begin
	if ymaps gt xmaps then begin
		tt = xmaps
		xmaps = ymaps
		ymaps = tt
	endif
; if portrait, make ymaps > xmaps
endif else begin
	if xmaps gt ymaps then begin
		tt = ymaps
		ymaps = xmaps
		xmaps = tt
	endif
endelse

; for multiple parameter plot
; always stack them
if n_elements(param) gt 1 then begin
	ymaps = npanels
	xmaps = 1
endif

if ~keyword_set(charsize) then $
	charsize = get_charsize(xmaps, ymaps)

; clear output area
if keyword_set(ps) then $
	ps_open, '~/Desktop/rt_scan'+rt_info.name+'.ps', /no_init
clear_page

; loop through panels
loadct, 0, file='/tmp/colors2.tbl'
for p=0,npanels-1 do begin

	if strcmp(param[p],'altitude') or strcmp(param[p],'valtitude') then begin
		level_format = 0
		set_colorsteps, 250
	endif else if strcmp(param[p],'power') then begin
		level_format = '(f4.0)'
		set_colorsteps, 250
	endif else begin
		level_format = '(f5.1)'
		set_colorsteps, 250
	endelse

	ascale = 0

	if n_elements(param) gt 1 then begin
		aparam = param[p]
		if keyword_set(scale) then $
			ascale = scale[p*2:p*2+1]
	endif else begin
		aparam = param[0]
		if keyword_set(scale) then $
			ascale = scale
	endelse

	xmap = p mod xmaps
	ymap = p/xmaps

	first = 0
	if xmap eq 0 then $
		first = 1

	last = 0
	if ymap eq ymaps-1 then $
		last = 1

	; plot an rti panel
	rt_plot_scan_panel, time, xmaps, ymaps, xmap, ymap, /bar, $
		date=date, xrange=xrange, yrange=yrange, $
		param=param[p], coords=coords, scale=ascale, $
		silent=silent, ground=ground, ionos=ionos, $
		charthick=charthick, charsize=charsize, $
		last=last, first=first, legend=legend[p]
; 	plot_colorbar, xmaps, ymaps, xmap, ymap, scale=ascale, charsize=charsize, $
; 		param=param[p], /with_info, legend=legend[p], level_format=level_format
endfor

; plot a title for the page
strtim = strmid(strtrim(time,2),0,2)+':'+strmid(strtrim(time,2),2,2)+' UT'
strdat = format_juldate(rt_info.sjul, /date)
strrad = rt_info.name+'@'+strtrim(string(rt_data.tfreq[0],format='(I3)'),2)+'MHz'
xyouts, .1, .94, 'RAY-TRACING & IRI', align=0, /normal, charsize=2
xyouts, .1, .9, strrad+' - '+strdat, align=0, /normal, charsize=1.5
xyouts, .1, .86, strtim, align=0, /normal, charsize=1.5
xyouts, .101, .941, 'RAY-TRACING & IRI', align=0, /normal, charsize=2
xyouts, .101, .901, strrad+' - '+strdat, align=0, /normal, charsize=1.5
xyouts, .101, .861, strtim, align=0, /normal, charsize=1.5

if keyword_set(ps) then $
	ps_close, /no_init

end



