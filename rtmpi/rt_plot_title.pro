;+ 
; NAME: 
; RT_PLOT_TITLE
; 
; PURPOSE: 
; This procedure plots a generall title on the top of a page.
; 
; CATEGORY: 
; Graphics
; 
; CALLING SEQUENCE: 
; RT_PLOT_TITLE
;
; OPTIONAL INPUTS:
; Title: A string used as the title, default is RAYTRACING PARAMETER PLOT.
;
; Subtitle: A string used as the subtitle, default radar name: parameter.
;
; KEYWORD PARAMETERS:
; NO_DATE: Set this keyword to surpress plotting of date/time information.
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
; Written by Lasse Clausen, Nov, 30 2009
;-
pro rt_plot_title, title, subtitle, no_date=no_date, $
	param=param, date=date, time=time, voffset=voffset

common rt_data_blk

if n_elements(title) eq 0 then $
	title = ''

if n_elements(subtitle) eq 0 then $
	subtitle = ''

if n_elements(voffset) eq 0 then $
	voffset = -0.14

if strlen(title) eq 0 then $
	title = 'Ray-tracing & IRI'

legend = ''
if ~keyword_set(param) then $
	param = '' $
else begin
	for ip=0,n_elements(param)-1 do begin
		legend = legend + param[ip] + (ip eq n_elements(param)-1 ? '' : '; ')
	endfor
endelse

if strlen(subtitle) eq 0 then $
	subtitle = rt_info.name+'@'+strtrim(string(rt_data.tfreq[0],format='(f5.1)'),2)+'MHz: '+legend

if keyword_set(no_date) then begin
	right_title = ''
	right_subtitle = ''
endif else begin
	if keyword_set(date) and keyword_set(time) then $
		sfjul, date, time, sjul, fjul $
	else begin
		sjul = rt_info.sjul
		fjul = rt_info.fjul
	endelse
	sfjul, sdate, stime, sjul, /jul_to
	sfjul, fdate, ftime, fjul, /jul_to
	if ftime eq 0 then $
		fdate = sdate
	right_title = ''
	right_subtitle = format_juldate(sjul, /date)
	if sdate ne fdate and ftime gt 5 then $
		right_subtitle += ' - '+format_juldate(fjul, /date)
endelse

plot_title, title, subtitle, top_right_title=right_title, top_right_subtitle=right_subtitle, voffset=voffset

end
