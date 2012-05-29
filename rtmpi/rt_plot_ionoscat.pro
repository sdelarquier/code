;+ 
; NAME: 
; RT_PLOT_IONOSCAT
;
; PURPOSE: 
; This procedure plots the ionosphric scatter curve as a function of range and elevation or altitude
; PREREQUISITE: RT_RUN
; 
; CATEGORY: 
; Graphics
; 
; CALLING SEQUENCE:
; RT_PLOT_IONOSCAT, time, zparam=zparam, xparam=xparam
;
; INPUTS:
; TIME: time of your raytracing run for which you want to plot the ray paths
;
; KEYWORD PARAMETERS:
; ZPARAM: 'elevation' or 'altitude' (Default is 'elevation')
;
; XPARAM: 'ground' or 'slant' range (Default is 'ground')
;
; KEYWORDS:
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
; Written by Sebastien de Larquier, Nov.2010
;-
pro rt_plot_ionoscat, time, xparam=xparam, zparam=zparam, date=date, beam=beam, ps=ps

common rt_data_blk

Rav = 6370.

if ~keyword_set(beam) then begin
	nbeams = n_elements(rt_data.beam[0,*])
	beam = rt_data.beam[0,round(nbeams/2.)-1]
	beam = 0
endif
binds = where(rt_data.beam[0,*] eq beam)
ib = binds[0]

if ~keyword_set(xparam) then $
	xparam = 'ground'
if ~keyword_set(zparam) then $
	zparam = 'elevation'

; Retrieve raytracing parameters from structure
radar = rt_info.name
caldat, rt_data.juls[*,0], month, day, year, hours, minutes
tdate 	= year*10000L + month*100L + day

parse_time, time, hour, minute
if ~keyword_set(date) then $
	date = tdate[0]
timeind = where(tdate eq date and hours eq hour and minutes eq minute)
juls = julday(month[timeind], day[timeind], year[timeind], hours[timeind], minutes[timeind])

; Test for data availability
rt_read_rays, date, radar, time, radpos=radpos, thtpos=thtpos, grppth=grppth, raysteps=raysteps, code=code
if ~code then $
	return

; If data is present, limit it to selected beam
radpos = reform(radpos[ib,*,*])
thtpos = reform(thtpos[ib,*,*])
grppth = reform(grppth[ib,*,*])
raysteps = reform(raysteps[ib,*])

; Count element of matrix
nrays = n_elements(radpos[*,0])

; Open postscript if desired
if keyword_set(ps) then begin
	ps_open, '~/Desktop/ionoscat_'+radar+STRTRIM(beam,2)+'_'+STRTRIM(date,2)+'_'+ $
		STRTRIM(STRING(time,format='(I04)'),2)+rt_info.timez+'.ps', /no_init
	clear_page
endif

; Set plot limits
case xparam of
	'ground': 	begin
						xrange = [0.,2000.]
						xtitle = 'Ground range [km]'
						xdata = thtpos*Rav
				end
	'slant': 	begin
						xtitle = 'Slant range [x 45 km]'
						xrange = [0.,50.]
						xdata = (grppth-180e3)/45e3
				end
endcase
case zparam of
	'elevation': 	begin
						yrange = [0.,60.]
						ztitle = 'Elevation'
					end
	'altitude': 	begin
						ztitle = 'Altitude [km]'
						yrange = [0.,500.]
					end
endcase
plot, xrange, yrange, /nodata, xstyle=5, ystyle=5

; Set tolerance (in degree) for how much deviation from perfect aspect condition is allowed
tol = 1.

; latitude/longitude distribution (expressed as angle from radar with Earth center)
ndip = n_elements(rt_data.dip[timeind,ib,0,*])
thtdip = findgen(ndip)*2500./Rav/ndip

for nr=0,nrays-1 do begin
	nrsteps = raysteps[nr]
	for ns=1,nrsteps-1 do begin
		if grppth[nr,ns-1] gt 180e3 and xdata[nr,ns] le xrange[1] then begin

			; Calculate k vector
			kx = radpos[nr,ns]*sin(thtpos[nr,ns]-thtpos[nr,ns-1])
			kz = radpos[nr,ns]*cos(thtpos[nr,ns]-thtpos[nr,ns-1]) - radpos[nr,ns-1]
			kvect = sqrt( kx^2. + kz^2. )

			; Plot coordinates
			xx = [xdata[nr,ns-1], xdata[nr,ns]]
			case zparam of
				'elevation': zz = [1., 1.]*(rt_info.elev_beg + nr*rt_info.elev_stp)
				'altitude': zz = [radpos[nr,ns-1], radpos[nr,ns]]*1e-3 - Rav
			endcase
			if zz[1] ge yrange[1] then $
				continue
			
			; Middle of the step: position and index in B grid
			midtht = (thtpos[nr,ns]-thtpos[nr,ns-1])/2. + thtpos[nr,ns-1]
			diff = min(midtht-thtdip, thtind, /abs)
			
			; Dip and declination at this position
			middip = rt_data.dip[timeind,ib,0,thtind]
			middec = rt_data.dip[timeind,ib,1,thtind]

			; calculate vector magnetic field
			Bx = cos(-middip*!dtor) * cos(rt_data.azim[timeind[0],ib]*!dtor - middec*!dtor)
			Bz = sin(-middip*!dtor)
			
			; calculate cosine of aspect angle
			cos_aspect = (Bx*kx + Bz*kz)/kvect
			
			if abs(cos_aspect) le cos(!pi/2. - tol*!dtor) then begin
				plots, xx, zz, color=200, thick=2
; 					print, nr*.1+10., ns-1, (grppth[nr,ns-1]*1e-3-180.)/45., radpos[nr,ns-1]*1e-3-Rav
			endif

		endif
	; end ray steps loop
	endfor
; end rays loop
endfor


; Over plot weighed scatter location
s = execute('ydata = rt_data.'+zparam)
case xparam of 
	'ground': oplot, rt_data.grange[timeind,ib,*], ydata[timeind,ib,*]*rt_data.ionoscatter[timeind,ib,*], psym=4, thick=2, symsize=1, min_val=.1
	'slant': oplot, findgen(rt_info.ngates+1), ydata[timeind,ib,*]*rt_data.ionoscatter[timeind,ib,*], psym=4, thick=2, symsize=1, min_val=.1
endcase

; Plot axis
title = STRMID(format_juldate(rt_data.juls[timeind]),0,17)
subtitle = 'Radar: '+rt_info.name+', Beam '+STRTRIM(beam,2)+', Freq. '+$
	STRTRIM(string(rt_data.tfreq[timeind],format='(F5.2)'),2)+' MHz'
plot, xrange, yrange, /nodata, xstyle=1, ystyle=1, charsize=charsize, $
	xtitle=xtitle, ytitle=ztitle, title=title+'!C'+subtitle

; Close potscript if necessary
if keyword_set(ps) then $
	ps_close, /no_init

end