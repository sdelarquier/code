;+
; NAME:
; RAD_GRD_DGNSTC
;
; PURPOSE:
; This procedure reads grid files and plots different parameters
; Param options
; plot_hmb -> Plot the Heppner Maynard Boundary
; plot_grd_echoes -> plot the number of echoes in each individual radar/or all of them
; plot_grd_vel_sctr -> make a scatter plot of the grid velocities in each individual radar/or all of them
;
; CATEGORY:
; Diagnostics
;
; CALLING SEQUENCE:
; rad_grd_dgnstc, date
;
;
; KEYWORD PARAMETERS:
; Date: The date of which to read data. scalar in YYYYMMDD format 
;
; PROCEDURE:
;
; COMMON BLOCKS:
; RAD_DATA_BLK: The common block holding the currently loaded radar data and
; information about that data.
;
; RADARINFO: The common block holding data about all radar sites (from RST).
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
; Written by Bharat Kunduri, Jul, 13, 2012


pro rad_grd_dgnstc, date,time=time, hemisphere=hemisphere, north=north, $
		south=south,plot_hmb=plot_hmb,plot_grd_echoes=plot_grd_echoes, $
		plot_grd_vel_sctr=plot_grd_vel_sctr,sel_rad_ids=sel_rad_ids


set_format,/sard

common rad_data_blk
common radarinfo

coords='mlt'

load_usersym,/circle

;; If time is not set --- set it to 0 to 24 UT
if (~keyword_set(time)) then $
	time=[0000,2400]

;;check for the hemisphere
if (keyword_set(north)) then $
	hemisphere=1
if (keyword_set(south)) then $
	hemisphere=-1

if (~keyword_set(hemisphere) and ~keyword_set(north) and ~keyword_set(south)) then $
	hemisphere=1

if (~keyword_set(plot_hmb) and ~keyword_set(plot_grd_echoes) and ~keyword_set(plot_grd_vel_sctr)) then $
	plot_grd_vel_sctr=1


;;; Make an array of radar names from the sel_rad_ids

if (n_elements(sel_rad_ids) gt 0 and sel_rad_ids[0] ne 0) then begin
	sel_rad_names=strarr(n_elements(sel_rad_ids))
	for rn=0,n_elements(sel_rad_ids)-1 do begin
		Jinds_rad_names=where(network[*].id[0] eq sel_rad_ids[rn])
		sel_rad_names[rn]=STRUPCASE(network[Jinds_rad_names].code[0])
	endfor
endif

if (n_elements(sel_rad_ids) gt 6) then $
			charsize='0.3' $
else $
			charsize='0.5' 


;;; Plotting for the HM-Boundary

if (keyword_set(plot_hmb)) then begin

;;; Read the map files
	rad_map_read,date,hemisphere=hemisphere

	sfjul,date,time,sjul,fjul
;;; int_hemi is 0 for north and 1 for south
	int_hemi=(hemisphere lt 0)

;;;;Calculate number of points to loop through
	ntimes = round((fjul-sjul)*1440.d/2.d)

	lat_hm=fltarr(ntimes)
	jul_sel=dblarr(ntimes)

;;; Start the loop for reading the boundary
	for b=0, ntimes-1 do begin

		asjul = sjul + double(b)*2.d/1440.d

; calculate index from date and time 
		dd = min( abs( (*rad_map_data[int_hemi]).mjuls-asjul ), _index)
		index=_index
; check if time ditance is not too big 
		if dd*1440.d gt 60. then begin
			prinfo, 'Map found, but '+string(dd*1440.d,format='(I4)')+' minutes off chosen time.'
		endif 
; get boundary data 
		bnd = (*(*rad_map_data[int_hemi]).bvecs[index])
		num_bnd = (*rad_map_data[int_hemi]).bndnum[index] 

; get longitude shift 
		lon_shft = (*rad_map_data[int_hemi]).lon_shft[index]
		caldat, asjul, mm, dd, year
		utsec = (asjul - julday(1, 1, year, 0, 0))*86400.d

; calculate lon_shft, i.e. shift magnetic longitude into mlt coordinates

		if coords eq 'mlt' then begin
			lon_shft += mlt(year, utsec, 0.)*15. 
			lons = ((bnd[*].lon+lon_shft)/15.) mod 24.
		endif else $
			lons = (bnd[*].lon+lon_shft)

		lat_hm[b] = MIN(bnd[*].lat,/ABSOLUTE)
		jul_sel[b]=asjul
	endfor

;;;Plot the data
	clear_page
	plot,jul_sel,lat_hm,xtickformat='label_date',xticks='6',xminor=4,xstyle=1,$
			xrange=[sjul,fjul],yrange=[45*hemisphere,80*hemisphere],ytitle='HM-Boundary (Lat)',xtitle='UT-Time'

endif



;;; Plotting for the GRID echoes or the scatter velocities -- similar reading procedure

if (keyword_set(plot_grd_echoes) or keyword_set(plot_grd_vel_sctr)) then begin
	clear_page

	rad_grd_read,date,hemisphere=hemisphere

;;; If you select the sel_rad_ids part you need multiple panels
	npanels=n_elements(sel_rad_ids)

	sfjul,date,time,sjul,fjul

;;; int_hemi is 0 for north and 1 for south
	int_hemi=(hemisphere lt 0)

;;;;Calculate number of points to loop through time
	ntimes = round((fjul-sjul)*1440.d/2.d)

	total_echoes=intarr(ntimes)
	jul_sel_all=dblarr(ntimes)
 if (n_elements(sel_rad_ids) gt 0 and sel_rad_ids[0] ne 0) then begin
		ind_rad_echoes=intarr(npanels,ntimes) ;; 2d array to store individual radar echoes
		jul_sel_ind_rad=dblarr(npanels,ntimes) ;; 2d array to store individual radar time
		ind_rad_vels=fltarr(1000,npanels,ntimes);; 1000 points each radar each instance of time
	endif
;; Make arrays for velocities
	total_vels=fltarr(1000,ntimes) ;; we need a scatter plot. - so assuming there are 1000 points at each instance of time (at the most).


;;; Start the loop through time for reading data
	for b=0, ntimes-1 do begin

		asjul = sjul + double(b)*2.d/1440.d
		jul_sel_all[b]=asjul
; calculate index from date and time 
		dd = min( abs( (*rad_grd_data[int_hemi]).mjuls-asjul ), index)
		nradars_rpt  =  (*(*rad_grd_data[int_hemi]).gvecs[index])[*].st_id ;; This array has station ids 
		total_echoes[b]=n_elements(nradars_rpt);;;Total number of points/echoes

		radar_ids=nradars_rpt[uniq(nradars_rpt)]
		nv_per_radar=intarr(n_elements(radar_ids)) ;; array holding no.of echoes in each radar 

;;;;Check the velocities and put them in an array
		vel_data = (*(*rad_grd_data[int_hemi]).gvecs[index]).vel.median
		total_vels[0:n_elements(vel_data)-1,b]=vel_data


		for rr=0,n_elements(radar_ids)-1 do begin ;; loop through the radars observing echoes --> not the radars we wanted to see (sel_rad_ids)
			jinds_rad_nvecs=where(nradars_rpt eq radar_ids[rr],xx) ;; nv_per_radar now holds how many echoes are observed by that radar
			nv_per_radar[rr]=xx
;; Check if the radars observing echoes are indeed matching any in sel_rad_ids
;; If so loop store there values in the array
   if (n_elements(sel_rad_ids) gt 0 and sel_rad_ids[0] ne 0) then begin
				for ss=0,npanels-1 do begin
					jinds_sel_rad_chk=where(radar_ids[rr] eq sel_rad_ids[ss],cc) 
					if (cc gt 0) then begin
						ind_rad_echoes[ss,b]=nv_per_radar[rr]
						jul_sel_ind_rad[ss,b]=asjul
						ind_rad_vels[0:n_elements(jinds_rad_nvecs)-1,ss,b]=vel_data[jinds_rad_nvecs]
					endif 
				endfor
			endif
		endfor

	endfor

;;; Now we need to change all those values with zeros to NaN's.......helps in plotting

	jinds_jul_all_nan=where(jul_sel_all eq 0.,cc1)
	if (cc1 gt 0.) then $
		jul_sel_all[jinds_jul_all_nan]=!values.f_nan

	jinds_total_vels_nan=where(total_vels eq 0.,cc4)
	if (cc4 gt 0.) then $
		total_vels[jinds_total_vels_nan]=!values.f_nan

	jinds_total_echoes_nan=where(total_echoes eq 0.,cc5)
	if (cc5 gt 0.) then $
		total_echoes[jinds_total_echoes_nan]=!values.f_nan

 if (n_elements(sel_rad_ids) gt 0 and sel_rad_ids[0] ne 0) then begin
		jinds_jul_selrad_nan=where(jul_sel_ind_rad eq 0.,cc2)
		if (cc2 gt 0.) then $
			jul_sel_ind_rad[jinds_jul_selrad_nan]=!values.f_nan

		jinds_ind_rad_echoes_nan=where(ind_rad_echoes eq 0,ccre)

		if (ccre gt 0.) then $
			ind_rad_echoes[jinds_ind_rad_echoes_nan]=-1 ;; This is not made NaN because, we ind_rad_echoes is an intarr and it wouldnt take NaN

		jinds_ind_rad_vels_nan=where(ind_rad_vels eq 0.,cc6)
		if (cc6 gt 0.) then $
			ind_rad_vels[jinds_ind_rad_vels_nan]=!values.f_nan
	endif

;;;Plotting now---depending on param choice
	if (keyword_set(plot_grd_echoes)) then begin

;;check if sel_rad_ids is set then loop through panels to plot
  if (n_elements(sel_rad_ids) gt 0 and sel_rad_ids[0] ne 0) then begin

			if (n_elements(jinds_total_echoes_nan) lt n_elements(total_echoes)) then begin
				plot,jul_sel_all,total_echoes,xtickformat=_xtickformat,$
					xticks=4,xminor=6,xstyle=1,xrange=[sjul,fjul],$
					ytitle='Total Echoes',xtickname=replicate(' ', 40),$
					position=define_panel(1,n_elements(sel_rad_ids)+1,0,0),charsize=charsize
			endif else begin
				plot,[0],[0],/nodata,xtickformat='label_date',$
					xticks='6',xminor=4,xstyle=1,xrange=[sjul,fjul],$
					ytitle='Total Echoes',xtitle='UT-Time'

					xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
						'No-Data', color=get_blue(), charsize='1', charthick=2
			endelse
  
			for npanels1=0,n_elements(sel_rad_ids)-1 do begin
				if (npanels1 eq n_elements(sel_rad_ids)-1) then begin
					plot, jul_sel_ind_rad[npanels1,*],ind_rad_echoes[npanels1,*],xtickformat='label_date',charsize=charsize,$
						xticks=4,xminor=6,xstyle=1, xrange=[sjul,fjul],position=define_panel(1,n_elements(sel_rad_ids)+1,0,npanels1+1),$
						ytitle='Echoes-'+sel_rad_names[npanels1],xtitle='UT-Time'
					Jinds_panel_echo_data=where(ind_rad_echoes[npanels1,*] gt -1)
					if (Jinds_panel_echo_data[0] eq -1) then begin
						xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
							'No-Data', color=get_blue(), charsize='1', charthick=2
					endif
				endif else begin
					plot, jul_sel_ind_rad[npanels1,*],ind_rad_echoes[npanels1,*],xtickformat=_xtickformat,charsize=charsize,$
						xticks=4,xminor=6,xrange=[sjul,fjul],position=define_panel(1,n_elements(sel_rad_ids)+1,0,npanels1+1),$
						ytitle='Echoes-'+sel_rad_names[npanels1],xstyle=1,xtickname=replicate(' ', 40)
					Jinds_panel_echo_data=where(ind_rad_echoes[npanels1,*] gt -1)
					if (Jinds_panel_echo_data[0] eq -1) then begin
						xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
							'No-Data', color=get_blue(), charsize='1', charthick=2
					endif
				endelse
			endfor
		endif else begin
			if (n_elements(jinds_total_echoes_nan) lt n_elements(total_echoes)) then begin
				plot,jul_sel_all,total_echoes,xtickformat='label_date',$
					xticks='6',xminor=4,xstyle=1,xrange=[sjul,fjul],$
					ytitle='Total Echoes',xtitle='UT-Time'
			endif else begin
				plot,[0],[0],/nodata,xtickformat='label_date',$
					xticks='6',xminor=4,xstyle=1,xrange=[sjul,fjul],$
					ytitle='Total Echoes',xtitle='UT-Time'

					xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
						'No-Data', color=get_blue(), charsize='1', charthick=2
			endelse
		endelse
	endif

	if (keyword_set(plot_grd_vel_sctr)) then begin
;;check if sel_rad_ids is set then loop through panels to plot
  if (n_elements(sel_rad_ids) gt 0 and sel_rad_ids[0] ne 0) then begin

		plot,[0],[0],/nodata,xtickformat=_xtickformat,$
					xticks=4,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
					ytitle='Vel-All Radars',yrange=[0,100*(fix(max(total_vels)/100)+1)],$
					position=define_panel(1,n_elements(sel_rad_ids)+1,0,0),charsize=charsize
				if (n_elements(jinds_total_vels_nan) lt n_elements(total_vels)) then begin
					for pp3= 0, n_elements(jul_sel_all)-1 do begin
						for pp4=0,n_elements(total_vels[*,0])-1 do begin
							oplot, [jul_sel_all[pp3]],[total_vels[pp4,pp3]],psym='3',symsize='3'
						endfor
					endfor
				endif else begin
					xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
						'No-Data', color=get_blue(), charsize='1', charthick=2
				endelse

  
			for npanels1=0,n_elements(sel_rad_ids)-1 do begin
				if (npanels1 eq n_elements(sel_rad_ids)-1) then begin
					plot,[0],[0],/nodata, xtickformat='label_date',$  
						xrange=[sjul,fjul],yrange=[0,100*(fix(max(ind_rad_vels[*,npanels1,*])/100)+1)], $
						position=define_panel(1,n_elements(sel_rad_ids)+1,0,npanels1+1), $
						ytitle='Velocity-'+sel_rad_names[npanels1], $
						xtitle='UT-Time',xstyle=1,charsize=charsize, xticks=4,xminor=6
					Jinds_panel_chk_data=where(finite(ind_rad_vels[*,npanels1,*]))
					if (Jinds_panel_chk_data[0] ne -1) then begin
						for pp1= 0, n_elements(jul_sel_ind_rad[npanels1,*])-1 do begin
							for pp2=0,n_elements(ind_rad_vels[*,npanels1,pp1])-1 do begin
								oplot, [jul_sel_ind_rad[npanels1,pp1]],[ind_rad_vels[pp2,npanels1,pp1]],psym='3',symsize='3'
							endfor
						endfor
					endif else begin
					xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
						'No-Data', color=get_blue(), charsize='1', charthick=2
					endelse
				endif else begin
					plot,[0],[0],/nodata, xtickformat=_xtickformat, $  
						xrange=[sjul,fjul],yrange=[0,100*(fix(max(ind_rad_vels[*,npanels1,*])/100)+1)], $
						position=define_panel(1,n_elements(sel_rad_ids)+1,0,npanels1+1), $
						ytitle='Velocity-'+sel_rad_names[npanels1], $
						xtickname=replicate(' ', 40),xstyle=1,charsize=charsize, xticks=4,xminor=6
					Jinds_panel_chk_data=where(finite(ind_rad_vels[*,npanels1,*]))
					if (Jinds_panel_chk_data[0] ne -1) then begin
						for pp1= 0, n_elements(jul_sel_ind_rad[npanels1,*])-1 do begin
							for pp2=0,n_elements(ind_rad_vels[*,npanels1,pp1])-1 do begin
								oplot, [jul_sel_ind_rad[npanels1,pp1]],[ind_rad_vels[pp2,npanels1,pp1]],psym='3',symsize='3'
							endfor
						endfor
					endif else begin
					xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
						'No-Data', color=get_blue(), charsize='1', charthick=2
					endelse
				endelse
			endfor
		endif else begin
				plot,[0],[0],/nodata,xtickformat='label_date',$
					xticks='6',xminor=4,xstyle=1,xrange=[sjul,fjul],$
					ytitle='Velocity',xtitle='UT-Time',yrange=[0,100*(fix(max(total_vels)/100)+1)]
				if (n_elements(jinds_total_vels_nan) lt n_elements(total_vels)) then begin
					for pp3= 0, n_elements(jul_sel_all)-1 do begin
						for pp4=0,n_elements(total_vels[*,0])-1 do begin
							oplot, [jul_sel_all[pp3]],[total_vels[pp4,pp3]],psym='3',symsize='3'
						endfor
					endfor
				endif else begin
					xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
						'No-Data', color=get_blue(), charsize='1', charthick=2
				endelse
		endelse

	endif
endif



end