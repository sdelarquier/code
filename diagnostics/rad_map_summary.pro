;+
; NAME:
; RAD_MAP_SUMMARY
;
; PURPOSE:
; This procedure reads ACE, OMNI, geomagnetic indices, grid files and map files and plots a summary of various parameters
; Param options
;
; CATEGORY:
; Diagnostics
;
; CALLING SEQUENCE:
; rad_map_summary, date, north=north, south=south, hemisphere=hemisphere
;
;
; KEYWORD PARAMETERS:
; Date: The date of which to read data. scalar in YYYYMMDD format 
;
; Time: The date of which to read data. scalar in HHII format
;
; param_plot: Enter the parameters you want to plot, if you dont want the default one's
;							eg: param_plot = ['ACEPOS','ACEIMF','OMN','SYMKP','AE','NVTOT','NVMID','HMB','PCPOT']
; PROCEDURE:
;
; COMMON BLOCKS:
; RAD_DATA_BLK: The common block holding the currently loaded radar data and
; information about that data.
; RADARINFO: The common block holding data about all radar sites (from RST).
;
; ACE_DATA_BLK
; OMN_DATA_BLK
; AUR_DATA_BLK
; KPI_DATA_BLK
; RAD_DATA_BLK
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
; Written by Bharat Kunduri, Aug, 11, 2012



pro rad_map_summary, date, time=time, north=north, south=south, $
						hemisphere=hemisphere, param_plot=param_plot, $
						line_marktime = line_marktime

common rad_data_blk
common radarinfo
common omn_data_blk
common aur_data_blk
common ace_data_blk
common kpi_data_blk

set_format,/sard,/portrait

;;Im setting this here because we switch to loadct,0 in program and switch back again.........basically not to mess up colors
rad_load_colortable,/leicester


if (n_elements(param_plot) eq 0) then begin
	npanels=8
endif else begin
	npanels=n_elements(param_plot)
endelse


;;;;;If not set; set the time from 00 to 24 UT, since we are reading for the entire day

if (~keyword_set(param_plot)) then begin
	param_plot='-1'
endif

if (~keyword_set(line_marktime)) then begin
	line_marktime='-1'
endif

;; This step is for the website thingy......where when i want to plot default parameters i put plot_param = 'none'
if (n_elements(param_plot) eq 1) then begin
if (param_plot[0] eq 'none') then begin
  npanels=8
	param_plot='-1'
endif
endif
; print,param_plot

if (~keyword_set(time)) then begin
	time=[0000,2400]
endif

sfjul,date,time,sjul,fjul

;;set charsize
charsize='0.4'

;;set the hemisphere
if (keyword_set(north)) then $
	hemisphere=1
if (keyword_set(south)) then $
	hemisphere=-1

if (~keyword_set(hemisphere) and ~keyword_set(north) and ~keyword_set(south)) then $
	hemisphere=1


;; This part is for calculating the line markings
If ((n_elements(line_marktime) gt 0) and (line_marktime[0] ne -1)) then begin

				if (n_elements(line_marktime) eq 1) then $
								sfjul,date,line_marktime[0], jul_marktime1

				if (n_elements(line_marktime) eq 2) then $
								sfjul,date,line_marktime, jul_marktime1, jul_marktime2


endif


;;;; Print the date in a proper format on the plot, so get year, month and day from date variable.
year_plot=fix(date/1e4)
mndy=double(date)-double(year_plot*1e4)
month_plot=fix(mndy/1e2)
day_plot=fix(mndy-month_plot*1e2)
month_list_plot=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

date_in_plot_format=month_list_plot[month_plot-1]+'/'+strtrim(string(day_plot),2)+'/'+strtrim(string(year_plot),2)


clear_page
;;; Start with the ACE panel
;;; Start with the ACE panel
;;; Start with the ACE panel

;;check if param_plot has ACEPOS in it.
if ((n_elements(param_plot) gt 0)) then begin
	check_ace_pos_param_plot=where(param_plot eq 'ACEPOS')
endif


if ((param_plot[0] eq '-1') or (check_ace_pos_param_plot[0] ne -1)) then begin

check_ace_files=ace_mag_find_files(date);; check if ace data exists--- plot only if it does exist
check_ace_swe_files=ace_swe_find_files(date)


	if (check_ace_swe_files ne '') then begin
;;set the panel position
		pos_ace_pos_panel=define_panel(1,npanels,0,0)
		ace_read,date,time=time

;; We are not using the ACE POS yrange scale

		yrange_ace_pos_min_val=min(ace_mag_data.rx_gse)
		yrange_ace_pos_max_val=max(ace_mag_data.rx_gse)
		yrange_ace_pos=[0,260];[20*(round(yrange_ace_pos_min_val/20)-1),20*(round(yrange_ace_pos_max_val/20)+1)];
		yrange_ace_np=[0,max([24,2*(round(max(ace_swe_data.np)/2)+1)])];[0,10];



		ace_swe_plot_panel,date=date,time=time, position=pos_ace_pos_panel,ytickname=replicate(' ', 40), $
			param='vt',charsize=charsize,xtickformat=_xtickformat, xstyle=1, xticks=12,xminor=6,$
			linecolor=get_green(),linethick=2, ytitle=' ',ystyle=1,yminor=4,yticks=1,yrange=[200,800]

		loadct,0
		ace_swe_plot_panel,date=date,time=time, position=pos_ace_pos_panel,ytickname=replicate(' ', 40), $
			param='np',charsize=charsize,xtickformat=_xtickformat, xstyle=1, xticks=12,xminor=6,$
			linecolor=100,linethick=2, ytitle=' ',ystyle=1,yminor=4,yticks=1,yrange=yrange_ace_np
		rad_load_colortable,/leicester

		if (npanels-1 gt 0) then begin

			ace_mag_plot_panel,date=date,time=time, position=pos_ace_pos_panel,yrange=yrange_ace_pos, $
				param='rx_gse',charsize=charsize,xtickformat=_xtickformat, xstyle=1, xticks=12,xminor=6,$
				linecolor=get_red(), ytitle='ACE-POS[Re]', linethick=2,ystyle=1,yminor=4,yticks=4

		endif else begin

			ace_mag_plot_panel,date=date,time=time, position=pos_ace_pos_panel,yrange=yrange_ace_pos, $
				param='rx_gse',charsize=charsize,xtickformat='label_date', xstyle=1, xticks=12,xminor=6,$
				xtitle='TIME (UT)',linecolor=get_red(), ytitle='ACE-POS[Re]', linethick=2,ystyle=1,yminor=4,yticks=4
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse


		xyouts, sjul + 20.d/1440.d, !y.crange[1]-.18*(!y.crange[1]-!y.crange[0]), $
			'Rx', color=get_red(), charsize='0.5', charthick=2
		xyouts, sjul + 20.d/1440.d, !y.crange[0]+.4*(!y.crange[1]-!y.crange[0]), $
			'Vt', color=get_green(), charsize='0.5', charthick=2
		loadct,0
		xyouts, sjul + 20.d/1440.d, !y.crange[0]+.13*(!y.crange[1]-!y.crange[0]), $
			'Np', color=100, charsize='0.5', charthick=2
		rad_load_colortable,/leicester
		xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[1]+.1*(!y.crange[1]-!y.crange[0]), $
			date_in_plot_format, color=get_black(), charsize='0.7', charthick=2

;;; marking the lines

		if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
    endif
    if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]) , !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]) , !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
    endif
;; Loading IDL colortable 0 as mike wanted a darker gray for some plots
;; So we loadct,0 and then get back to the normal davit colortable
		axis,yaxis=1, ytitle='Vel[Km/s]',color=get_green(),charsize=charsize,ystyle=1,yminor=4,yticks=4,yrange=[200,800];[0,100*(fix(max_ace_vt/100)+1)]

;; set up the axis for ACE-Np
		loadct,0
		axis,fjul+0.045*(fjul-sjul),yaxis=1, ytitle=textoidl('ACE-N_{p} [cm^{-3}]'),color=100,charsize=charsize,ystyle=1,yminor=4,yticks=4,yrange=yrange_ace_np,ticklen=-0.005
		rad_load_colortable,/leicester

	endif else begin

		if (npanels-1 gt 0) then begin
			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_ace_pos_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='ACE-POS[Re]',charsize=charsize,ystyle=1,yminor=4,yticks=4,yrange=[0,1]
		endif else begin
			plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_ace_pos_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
				ytitle='ACE-POS[Re]',charsize=charsize,ystyle=1,yminor=4,yticks=4,yrange=[0,1]
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse
		xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
			'No ACE Data', color=get_blue(), charsize='0.5', charthick=2

;;; Indicate the date on the plot
		xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
			date_in_plot_format, color=get_black(), charsize='0.7', charthick=2

	endelse

endif



;;check if param_plot has ACEIMF in it.
if ((n_elements(param_plot) gt 0)) then begin
	check_ace_imf_param_plot=where(param_plot eq 'ACEIMF')
endif


if ((param_plot[0] eq '-1') or (check_ace_imf_param_plot[0] ne -1)) then begin

	check_ace_files=ace_mag_find_files(date);; check if ace data exists--- plot only if it does exist
	if (check_ace_files ne '') then begin

;; set the panel position depending on what parameters are chosen to plot
		if (param_plot[0] eq '-1') then begin
			panel_ace_imf=1
		endif

		if ((param_plot[0] ne '-1') and (check_ace_pos_param_plot[0] ne -1)) then begin
			panel_ace_imf=1
		endif
		if ((param_plot[0] ne '-1') and (check_ace_pos_param_plot[0] eq -1)) then begin
			panel_ace_imf=0
		endif

		ace_read,date,time=time

		jinds_ace_by_finite=where(finite(ace_mag_data.by_gsm))
		jinds_ace_bz_finite=where(finite(ace_mag_data.bz_gsm))

		yrange_ace_min_val=min([min(ace_mag_data.by_gsm[jinds_ace_by_finite]),min(ace_mag_data.bz_gsm[jinds_ace_bz_finite])])
		yrange_ace_max_val=max([10,max(ace_mag_data.by_gsm[jinds_ace_by_finite]),max(ace_mag_data.bz_gsm[jinds_ace_bz_finite])])
		yrange_ace=[5*(round(yrange_ace_min_val/5)-1),5*(round(yrange_ace_max_val/5)+1)]

		pos_ace_panel=define_panel(1,npanels,0,panel_ace_imf)

		if (panel_ace_imf lt npanels-1) then begin
			ace_mag_plot_panel,date=date,time=time, position=pos_ace_panel, yrange=yrange_ace, $
				param='bz_gsm',charsize=charsize,xtickformat=_xtickformat, xstyle=1, xticks=12,xminor=6,$
				linecolor=get_red(), ytitle='ACE-IMF[nT]', linethick=2,ystyle=1,yminor=4,yticks=min([10,fix(((yrange_ace[1])-(yrange_ace[0]))/5)])
		endif else begin
			ace_mag_plot_panel,date=date,time=time, position=pos_ace_panel, yrange=yrange_ace, $
				param='bz_gsm',charsize=charsize,xtickformat='label_date', xstyle=1, xticks=12,xminor=6,$
				xtitle='TIME (UT)',linecolor=get_red(), ytitle='ACE-IMF[nT]', linethick=2,ystyle=1,yminor=4,yticks=min([10,fix(((yrange_ace[1])-(yrange_ace[0]))/5)])
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse


		oplot,[ace_mag_data.juls[0],ace_mag_data.juls[n_elements(ace_mag_data.juls)-1]],[0,0],linestyle='2

		xyouts, sjul + 20.d/1440.d, !y.crange[0]+.13*(!y.crange[1]-!y.crange[0]), $
			'Bz', color=get_red(), charsize='0.5', charthick=2

		loadct,0
		oplot,ace_mag_data.juls,ace_mag_data.by_gsm,color=100
		xyouts, sjul + 20.d/1440.d, !y.crange[1]-.18*(!y.crange[1]-!y.crange[0]), $
			'By', color=100, charsize='0.5', charthick=2
		rad_load_colortable,/leicester

				if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
        if (panel_ace_imf eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
				endif		
    endif
    if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				if (panel_ace_imf eq 0) then begin
				xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
    endif

	endif else begin


		if (panel_ace_imf lt npanels-1) then begin
			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_ace_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='ACE-IMF[nT]',charsize=charsize,ystyle=1,yminor=4,yticks=4,yrange=[0,1]
		endif else begin
			plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_ace_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
				ytitle='ACE-IMF[nT]',charsize=charsize,ystyle=1,yminor=4,yticks=4,yrange=[0,1]
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse
		xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
			'No ACE Data', color=get_blue(), charsize='0.5', charthick=2

	endelse

endif




;;check if param_plot has OMN-Data in it.
if ((n_elements(param_plot) gt 0)) then begin
	check_omn_param_plot=where(param_plot eq 'OMN')
endif


if ((check_omn_param_plot[0] ne -1) and (param_plot[0] ne '-1')) then begin


	omn_read,date,time=time

;;check if omni data is present if not print 'NO-DATA'
	jinds_omn_exist=where(finite(omn_data.bz_gsm),check_omn)


	if (check_omn ne 0) then begin

;;set the position
		if ((check_ace_pos_param_plot[0] ne -1) and (check_ace_imf_param_plot[0] ne -1)) then begin
			panel_omn=2
		endif
		if ((check_ace_imf_param_plot[0] ne -1) and (check_ace_pos_param_plot[0] eq -1)) then begin
			panel_omn=1
		endif
		if ((check_ace_pos_param_plot[0] ne -1) and (check_ace_imf_param_plot[0] eq -1)) then begin
			panel_omn=1
		endif
		if ((check_ace_pos_param_plot[0] eq -1) and (check_ace_imf_param_plot[0] eq -1)) then begin
			panel_omn=0
		endif

;;; time to set the position

		pos_omn_panel=define_panel(1,npanels,0,panel_omn)


;;Calculate the yranges - you would want to round them off to nearest 5 nT in case of IMF....

		jinds_omn_by_finite=where(finite(omn_data.by_gsm))
		jinds_omn_bz_finite=where(finite(omn_data.bz_gsm))

		yrange_omn_min_val=min([min(omn_data.by_gsm[jinds_omn_by_finite]),min(omn_data.bz_gsm[jinds_omn_bz_finite])])
		yrange_omn_max_val=max([10,max(omn_data.by_gsm[jinds_omn_by_finite]),max(omn_data.bz_gsm[jinds_omn_bz_finite])])
		yrange_omn=[5*(round(yrange_omn_min_val/5)-1),5*(round(yrange_omn_max_val/5)+1)]



		if (panel_omn lt npanels-1) then begin

			omn_plot_panel, date=date, time=time, position=pos_omn_panel, yrange=yrange_omn, $
				param='bz_gsm',charsize=charsize,xtickformat=_xtickformat, xstyle=1, xticks=12,xminor=6,$
				linecolor=get_red(), ytitle='OMNI-IMF[nT]', linethick=2,ystyle=1,yminor=4,yticks=fix(((yrange_omn[1])-(yrange_omn[0]))/5)

		endif else begin
			omn_plot_panel, date=date, time=time, position=pos_omn_panel, yrange=yrange_omn, $
				param='bz_gsm',charsize=charsize,xtickformat='label_date',xtitle='TIME (UT)', xminor=6, xticks=12, $
				xstyle=1, linecolor=get_red(), ytitle='OMNI-IMF[nT]', linethick=2,ystyle=1,yminor=4,yticks=fix(((yrange_omn[1])-(yrange_omn[0]))/5)
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2

		endelse


		xyouts, sjul + 20.d/1440.d, !y.crange[0]+.13*(!y.crange[1]-!y.crange[0]), $
			'Bz', color=get_red(), charsize='0.5', charthick=2

		oplot,[omn_data.juls[0],omn_data.juls[n_elements(omn_data.juls)-1]],[0,0],linestyle='2'

		loadct,0
		oplot,omn_data.juls,omn_data.by_gsm,color=100
		xyouts, sjul + 20.d/1440.d, !y.crange[1]-.18*(!y.crange[1]-!y.crange[0]), $
			'By', color=100, charsize='0.5', charthick=2
		rad_load_colortable,/leicester

		if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				if (panel_omn eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
				endif
    endif
    if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				if (panel_omn eq 0) then begin
				xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
    endif

	endif else begin ;; If there is no OMN data

		if (panel_omn lt npanels-1) then begin

			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_omn_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='OMNI-IMF[nT]',yrange=[0,1],charsize=charsize

		endif else begin

			plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_omn_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
				ytitle='OMNI-IMF[nT]',yrange=[0,1],charsize=charsize
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2

		endelse
		xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
			'No OMNI Data', color=get_blue(), charsize='0.5', charthick=2

	endelse


endif




if ((n_elements(param_plot) gt 0)) then begin
	check_symh_kp_param_plot=where(param_plot eq 'SYMKP')
endif


if ((param_plot[0] eq '-1') or (check_symh_kp_param_plot[0] ne -1)) then begin

	aur_read,date,time=time
;;check if the data exists for the date
	symh_data=aur_data.sym_h
	jinds_check_symh=where(finite(symh_data),check_symh)


	if (param_plot[0] eq '-1') then begin
		panel_symh_kp=2
	endif

	if ((param_plot[0] ne '-1')) then begin
		jinds_check_panel_symkp=where((param_plot eq 'ACEPOS') or (param_plot eq 'ACEIMF') or (param_plot eq 'OMN'),count_symkp_panel_pos)
		panel_symh_kp=count_symkp_panel_pos
	endif

	pos_symh_panel=define_panel(1,npanels,0,panel_symh_kp)

	if (check_symh ne 0) then begin

		kpi_read,date,time=time
;; We are autoscaling symh yrange data here but we dont use it in plotting
;; Mike wanted it to be fixed between -120 and 60 (to coorespond 0 symh with kp=6), but just in case sometime later we want it to autoscale
;;we can use yrange_symh instead of -120,60] in line 212
		yrange_symh=[50*(round(min(aur_data.sym_h)/50)-1),50*(round(max(aur_data.sym_h)/50)+1)]

		if (panel_symh_kp lt npanels-1) then begin
			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_symh_panel,xticklen=0.05, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='Symh Ind[nT]',charsize=charsize,ystyle=1,yminor=4,yticks=3,yrange=[-120,60];yrange_symh
		endif else begin
			plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_symh_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',xticklen=0.05,$
				ytitle='Symh Ind[nT]',charsize=charsize,ystyle=1,yminor=4,yticks=3,yrange=[-120,60];yrange_symh
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse

		oplot,aur_data.juls,aur_data.sym_h
;;; this stuff is for the shading part of the sym-H index
		sym_xval=[aur_data.juls[0],aur_data.juls,aur_data.juls[n_elements(aur_data.juls)-1]]
		sym_y_minval=0
		polyfill,sym_xval,[sym_y_minval,aur_data.sym_h,sym_y_minval],color=get_gray()

;;; In Kpi_plot_panel, I put the charsize to 0.001 since I couldnt remove a tickmark from kpi panel so I made it very small
		kpi_plot_panel,date=date,time=time,pos=pos_symh_panel,xticks=12,xminor=6,xstyle=1,xtickname=replicate(' ', 40),$
			ytickname=replicate(' ', 40),yrange=[0,9],charsize=0.001,ystyle=1,$
			yminor=4,linecolor=get_blue(),yticks=1,ytitle=' '
		for kk=0,n_elements(kpi_data.juls)-1 do begin	
; 		oplot,kpi_data.juls,kpi_data.kp_index,color=get_blue(),linestyle=1,thick=2
		oplot,[kpi_data.juls[kk],kpi_data.juls[kk]+3.d/24.d],[kpi_data.kp_index[kk],kpi_data.kp_index[kk]],color=get_blue(),linestyle=1,thick=2
		endfor
		oplot,[aur_data.juls[0],aur_data.juls[n_elements(aur_data.juls)-1]],[3,3],linestyle='2',thick='2'
		oplot,[aur_data.juls[0],aur_data.juls[n_elements(aur_data.juls)-1]],[6,6],linestyle='2',thick='2'

		if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				if (panel_symh_kp eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
				endif		
    endif
    if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				if (panel_symh_kp eq 0) then begin
				xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
    endif

		axis,yaxis=1, ytitle='Kp-Index',color=get_blue(),charsize='0.5',yrange=[0,9],ystyle=1,yminor=4,yticks=3

	endif else begin

		if (panel_symh_kp lt npanels-1) then begin
			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_symh_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='Symh Ind[nT]',charsize=charsize,ystyle=1,yminor=4,yticks=3,yrange=[0,1];yrange_symh
		endif else begin
			plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_symh_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
				ytitle='Symh Ind[nT]',charsize=charsize,ystyle=1,yminor=4,yticks=3,yrange=[0,1];yrange_symh
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse
		xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
			'No Sym-H Data', color=get_blue(), charsize='0.5', charthick=2

	endelse



endif







if ((n_elements(param_plot) gt 0)) then begin
	check_symh_ae_plot=where(param_plot eq 'AE')
endif


if ((param_plot[0] eq '-1') or (check_symh_ae_plot[0] ne -1)) then begin

	aur_read,date,time=time

	ae_data=aur_data.ae_index
	jinds_check_ae=where(finite(ae_data),check_ae)

	if (param_plot[0] eq '-1') then begin
		panel_ae_pos=3
	endif

	if ((param_plot[0] ne '-1')) then begin
		jinds_check_panel_ae=where((param_plot eq 'ACEPOS') or (param_plot eq 'ACEIMF') or (param_plot eq 'OMN') or (param_plot eq 'SYMKP'),count_ae_panel_pos)
		panel_ae_pos=count_ae_panel_pos
	endif

	pos_ae_panel=define_panel(1,npanels,0,panel_ae_pos)

	if (check_ae ne 0) then begin


		yrange_ae=[100*(fix(min(aur_data.ae_index)/100)-1),max([1000,100*(fix(max(aur_data.ae_index)/100)+1)])]

		if (panel_ae_pos lt npanels-1) then begin
			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_ae_panel,xticklen=0.05, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='AE Ind[nT]',yrange=yrange_ae,charsize=charsize,ystyle=1,yminor=4
		endif else begin
			plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_ae_panel,xticklen=0.05, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
				ytitle='AE Ind[nT]',yrange=yrange_ae,charsize=charsize,ystyle=1,yminor=4
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse

		oplot,aur_data.juls,aur_data.ae_index


		if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
       	  if (panel_ae_pos eq 0) then begin
						xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
							strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
				  endif
    endif
    if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
				oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
				if (panel_ae_pos eq 0) then begin
				xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
    endif
		

	endif else begin
		if (panel_ae_pos lt npanels-1) then begin
			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_ae_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='AE Ind[nT]',yrange=yrange_ae,charsize=charsize,ystyle=1,yminor=4
		endif else begin
			plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_ae_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
				ytitle='AE Ind[nT]',yrange=yrange_ae,charsize=charsize,ystyle=1,yminor=4
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse
		xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
			'No AE Data', color=get_blue(), charsize='0.5', charthick=2

	endelse

endif



if ((n_elements(param_plot) gt 0)) then begin
	check_nvecs_tot_plot=where(param_plot eq 'NVTOT')
	check_nvecs_mid_plot=where(param_plot eq 'NVMID')
endif


if ((param_plot[0] eq '-1') or (check_nvecs_tot_plot[0] ne -1) or (check_nvecs_mid_plot[0] ne -1)) then begin

	;;; set the high and mid-lat radar ids depending on the hemisphere
	if (hemisphere eq 1) then begin
		sel_rad_ids_midlat=[206,207,204,203,33,32]
		sel_rad_ids_highlat=[64,65,16,7,6,5,3,1,8,9,10,40] ;;; Note here HOK is put down as high-lat radar, just bcoz when we mean mid-lat I felt we only mean north american one's
	endif else begin
		sel_rad_ids_highlat=[20,19,13,12,11,4,18,14,15]
		sel_rad_ids_midlat=[21]
	endelse

	check_grd_file_exist=rad_grd_find_files(date,hemisphere=hemisphere)


	if (param_plot[0] eq '-1') then begin
		pos_totvec_panel=4
		pos_midvec_panel=pos_totvec_panel+1
		count_totvec_panel_pos=pos_totvec_panel
		pos_npnt_panel=define_panel(1,npanels,0,pos_totvec_panel)
		pos_npnt_mdlat_panel=define_panel(1,npanels,0,pos_midvec_panel)
	endif

	if ((param_plot[0] ne '-1') and (check_nvecs_tot_plot[0] ne -1) and (check_nvecs_mid_plot[0] ne -1)) then begin
		jinds_check_panel_totvec=where((param_plot eq 'ACEPOS') or (param_plot eq 'ACEIMF') or $
			(param_plot eq 'OMN') or (param_plot eq 'SYMKP') or (param_plot eq 'AE'),count_totvec_panel_pos)
		pos_npnt_panel=define_panel(1,npanels,0,count_totvec_panel_pos)
		pos_npnt_mdlat_panel=define_panel(1,npanels,0,count_totvec_panel_pos+1)
	endif

	if ((param_plot[0] ne '-1') and (check_nvecs_tot_plot[0] ne -1) and (check_nvecs_mid_plot[0] eq -1)) then begin
		jinds_check_panel_totvec=where((param_plot eq 'ACEPOS') or (param_plot eq 'ACEIMF') or $
			(param_plot eq 'OMN') or (param_plot eq 'SYMKP') or (param_plot eq 'AE'),count_totvec_panel_pos)
		pos_npnt_panel=define_panel(1,npanels,0,count_totvec_panel_pos)
	endif

	if ((param_plot[0] ne '-1') and (check_nvecs_tot_plot[0] eq -1) and (check_nvecs_mid_plot[0] ne -1)) then begin
		jinds_check_panel_totvec=where((param_plot eq 'ACEPOS') or (param_plot eq 'ACEIMF') or $
			(param_plot eq 'OMN') or (param_plot eq 'SYMKP') or (param_plot eq 'AE'),count_totvec_panel_pos)
		pos_npnt_mdlat_panel=define_panel(1,npanels,0,count_totvec_panel_pos)
	endif



	if (check_grd_file_exist ne '') then begin

		rad_grd_read,date,time=time,hemisphere=hemisphere
		ntimes = round((fjul-sjul)*1440.d/2.d)

		total_echoes=intarr(ntimes)
		total_echoes_highlat=intarr(ntimes)
		total_echoes_midlat=intarr(ntimes)
		jul_sel_all=dblarr(ntimes)
		int_hemi= (hemisphere lt 0)

	;;; Start the loop through time for reading data
		for b=0, ntimes-1 do begin

			asjul = sjul + double(b)*2.d/1440.d
			jul_sel_all[b]=asjul
	; calculate index from date and time
			dd = min( abs( (*rad_grd_data[int_hemi]).mjuls-asjul ), index)
	;;; Check if there indeed are any data points
		check_nrad_rpt=(*rad_grd_data[0]).vcnum[index]

		if (check_nrad_rpt gt 0) then begin
				nradars_rpt  =  (*(*rad_grd_data[int_hemi]).gvecs[index])[*].st_id ;; This array has station ids
				endif else begin
				nradars_rpt=[0]
				endelse
				total_echoes[b]=n_elements(nradars_rpt);;;Total number of points/echoes

				radar_ids=nradars_rpt[uniq(nradars_rpt)]
				nv_per_radar=intarr(n_elements(radar_ids)) ;; array holding no.of echoes in each radar
				for rr=0,n_elements(radar_ids)-1 do begin ;; loop through the radars observing echoes --> not the radars we wanted to see (sel_rad_ids)
					jinds_rad_nvecs=where(nradars_rpt eq radar_ids[rr],xx) ;; nv_per_radar now holds how many echoes are observed by that radar
					nv_per_radar[rr]=xx
		;; Check if the radars observing echoes are indeed matching any in sel_rad_ids
		;; If so loop store there values in the array
					for ss=0,n_elements(sel_rad_ids_midlat)-1 do begin
						jinds_sel_rad_chk=where(radar_ids[rr] eq sel_rad_ids_midlat[ss],cc)
						if (cc gt 0) then begin
							total_echoes_midlat[b]=total_echoes_midlat[b]+nv_per_radar[rr]
						endif
					endfor
					for ss=0,n_elements(sel_rad_ids_highlat)-1 do begin
						jinds_sel_rad_chk=where(radar_ids[rr] eq sel_rad_ids_highlat[ss],cc)
						if (cc gt 0) then begin
							total_echoes_highlat[b]=total_echoes_highlat[b]+nv_per_radar[rr]
						endif
					endfor
				endfor
			endfor



		if ((param_plot[0] eq '-1') or ((param_plot[0] ne '-1') and (check_nvecs_tot_plot[0] ne -1) and (check_nvecs_mid_plot[0] ne -1))) then begin
			plot,jul_sel_all,total_echoes,xtickformat=_xtickformat,pos=pos_npnt_panel,xticklen=0.05, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='Num.Vecs',charsize=charsize,ystyle=1,yminor=4,yrange=[0,max([400,50*(fix(max(total_echoes)/50)+1)])]

			oplot,jul_sel_all,total_echoes_highlat,color=get_red()
			xyouts, sjul + 20.d/1440.d, !y.crange[1]-.18*(!y.crange[1]-!y.crange[0]), $
					'Total', color=get_black(), charsize='0.5', charthick=2
			xyouts, sjul + 20.d/1440.d, !y.crange[0]+.13*(!y.crange[1]-!y.crange[0]), $
					'High-lat', color=get_red(), charsize='0.5', charthick=2
					
   	if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
     	  if (count_totvec_panel_pos eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			  endif
   endif
   if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			if (count_totvec_panel_pos eq 0) then begin
			xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
   endif

			if (count_totvec_panel_pos+1 lt npanels-1) then begin
				plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_npnt_mdlat_panel,xticklen=0.05, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
					ytitle='MidLat-NumVecs',charsize=charsize,ystyle=1,yminor=4,yrange=[0,max([100,50*(fix(max(total_echoes_midlat)/50)+1)])]
			endif else begin
				plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_npnt_mdlat_panel, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',xticklen=0.05,$
					ytitle='MidLat-NumVecs',charsize=charsize,ystyle=1,yminor=4,yrange=[0,max([100,50*(fix(max(total_echoes_midlat)/50)+1)])]
				xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
					date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
			endelse

			oplot,jul_sel_all,total_echoes_midlat,color=get_blue()
				xyouts, sjul + 20.d/1440.d, !y.crange[1]-.18*(!y.crange[1]-!y.crange[0]), $
					'Mid-lat', color=get_blue(), charsize='0.5', charthick=2

   	if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
     	  if (count_totvec_panel_pos+1 eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			  endif
   endif
   if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			if (count_totvec_panel_pos+1 eq 0) then begin
			xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
   endif
					
		endif





		if ((param_plot[0] eq '-1') or ((param_plot[0] ne '-1') and (check_nvecs_tot_plot[0] ne -1) and (check_nvecs_mid_plot[0] eq -1))) then begin


			if (count_totvec_panel_pos lt npanels-1) then begin
				plot,jul_sel_all,total_echoes,xtickformat=_xtickformat,pos=pos_npnt_panel, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xticklen=0.05,xtickname=replicate(' ', 40),$
					ytitle='Num.Vecs',charsize=charsize,ystyle=1,yminor=4,yrange=[0,max([400,50*(fix(max(total_echoes)/50)+1)])]
			endif else begin

				plot,jul_sel_all,total_echoes,xtickformat='label_date',pos=pos_npnt_panel, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',xticklen=0.05,$
					ytitle='Num.Vecs',charsize=charsize,ystyle=1,yminor=4,yrange=[0,max([400,50*(fix(max(total_echoes)/50)+1)])]
				xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
					date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
			endelse



			oplot,jul_sel_all,total_echoes_highlat,color=get_red()
			xyouts, sjul + 20.d/1440.d, !y.crange[1]-.18*(!y.crange[1]-!y.crange[0]), $
					'Total', color=get_black(), charsize='0.5', charthick=2
			xyouts, sjul + 20.d/1440.d, !y.crange[0]+.13*(!y.crange[1]-!y.crange[0]), $
					'High-lat', color=get_red(), charsize='0.5', charthick=2


   	if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
     	  if (count_totvec_panel_pos eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			  endif
   endif
   if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			if (count_totvec_panel_pos eq 0) then begin
			xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
   endif



		endif



		if ((param_plot[0] eq '-1') or ((param_plot[0] ne '-1') and (check_nvecs_tot_plot[0] eq -1) and (check_nvecs_mid_plot[0] ne -1))) then begin


			if (count_totvec_panel_pos lt npanels-1) then begin
				plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_npnt_mdlat_panel,xticklen=0.05, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
					ytitle='MidLat-NumVecs',charsize=charsize,ystyle=1,yminor=4,yrange=[0,max([100,50*(fix(max(total_echoes_midlat)/50)+1)])]
			endif else begin
				plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_npnt_mdlat_panel,xticklen=0.05, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
					ytitle='MidLat-NumVecs',charsize=charsize,ystyle=1,yminor=4,yrange=[0,max([100,50*(fix(max(total_echoes_midlat)/50)+1)])]
				xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
					date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
			endelse

			oplot,jul_sel_all,total_echoes_midlat,color=get_blue()
				xyouts, sjul + 20.d/1440.d, !y.crange[1]-.18*(!y.crange[1]-!y.crange[0]), $
					'Mid-lat', color=get_blue(), charsize='0.5', charthick=2

   	if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
     	  if (count_totvec_panel_pos eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			  endif
   endif
   if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			if (count_totvec_panel_pos eq 0) then begin
			xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
   endif
					
		endif


	endif else begin

		if ((param_plot[0] eq '-1') or ((param_plot[0] ne '-1') and (check_nvecs_tot_plot[0] ne -1) and (check_nvecs_mid_plot[0] ne -1))) then begin
			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_npnt_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='Num.Vecs',yrange=[0,1],charsize=charsize

			xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
				'No GRDEX Files', color=get_blue(), charsize='0.5', charthick=2

		if (count_totvec_panel_pos+1 lt npanels-1) then begin
			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_npnt_mdlat_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='MidLat-NumVecs',yrange=[0,1],charsize=charsize
		endif else begin
			plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_npnt_mdlat_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname='TIME (UT)',$
				ytitle='MidLat-NumVecs',yrange=[0,1],charsize=charsize
				xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
					date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse


			xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
				'No GRDEX Files', color=get_blue(), charsize='0.5', charthick=2
		endif


		if ((param_plot[0] eq '-1') or ((param_plot[0] ne '-1') and (check_nvecs_tot_plot[0] ne -1) and (check_nvecs_mid_plot[0] eq -1))) then begin

			if (count_totvec_panel_pos lt npanels-1) then begin
				plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_npnt_mdlat_panel, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
					ytitle='Num.Vecs',yrange=[0,1],charsize=charsize
			endif else begin
				plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_npnt_mdlat_panel, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname='TIME (UT)',$
					ytitle='Num.Vecs',yrange=[0,1],charsize=charsize
				xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
					date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
			endelse


			xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
				'No GRDEX Files', color=get_blue(), charsize='0.5', charthick=2
		endif



		if ((param_plot[0] eq '-1') or ((param_plot[0] ne '-1') and (check_nvecs_tot_plot[0] eq -1) and (check_nvecs_mid_plot[0] ne -1))) then begin

			if (count_totvec_panel_pos lt npanels-1) then begin
				plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_npnt_mdlat_panel, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
					ytitle='Num.Vecs',yrange=[0,1],charsize=charsize
			endif else begin
				plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_npnt_mdlat_panel, $
					xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname='TIME (UT)',$
					ytitle='Num.Vecs',yrange=[0,1],charsize=charsize
				xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
					date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
			endelse


			xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
				'No GRDEX Files', color=get_blue(), charsize='0.5', charthick=2
		endif





	endelse


endif






if ((n_elements(param_plot) gt 0)) then begin
	check_HMB_plot=where(param_plot eq 'HMB')
	check_PCP_plot=where(param_plot eq 'PCPOT')
endif


if ((param_plot[0] eq '-1') or (check_HMB_plot[0] ne -1) or (check_PCP_plot[0] ne -1)) then begin


	;;set the default coords
	coords='mlt'

	;;; set the high and mid-lat radar ids depending on the hemisphere
	if (hemisphere eq 1) then begin
		sel_rad_ids_midlat=[206,207,204,203,33,32]
		sel_rad_ids_highlat=[64,65,16,7,6,5,3,1,8,9,10,40] ;;; Note here HOK is put down as high-lat radar, just bcoz when we mean mid-lat I felt we only mean north american one's
	endif else begin
		sel_rad_ids_highlat=[20,19,13,12,11,4,18,14,15]
		sel_rad_ids_midlat=[21]
	endelse

	;;;; Start with HM-Boundary and PHI-PC ---> we need to read map files for this
	;;;; Start with HM-Boundary and PHI-PC ---> we need to read map files for this
	;;;; Start with HM-Boundary and PHI-PC ---> we need to read map files for this

	check_map_file_exist=rad_map_find_files(date,hemisphere=hemisphere)

	pos_hmb_panel=define_panel(1,npanels,0,7)
	pos_phipc_panel=define_panel(1,npanels,0,8)

	if (check_map_file_exist ne '') then begin

		rad_map_read,date,time=time,hemisphere=hemisphere

	;;; int_hemi is 0 for north and 1 for south
		int_hemi=(hemisphere lt 0)

	;;;;Calculate number of points to loop through
		ntimes = round((fjul-sjul)*1440.d/2.d)

		lat_hm=fltarr(ntimes)
		jul_sel=dblarr(ntimes)
		pot_drop=fltarr(ntimes)

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
			pot_drop[b]=(*rad_map_data[int_hemi]).pot_drop[index]/1000 ;;; ---We are calculating KV
		endfor
	endif

endif


if ((param_plot[0] eq '-1') or (check_HMB_plot[0] ne -1)) then begin


	if (param_plot[0] eq '-1') then begin
		panel_hmb_pos=6
	endif

	if ((param_plot[0] ne '-1')) then begin
		jinds_check_panel_ae=where((param_plot eq 'ACEPOS') or (param_plot eq 'ACEIMF') or (param_plot eq 'OMN') $
				or (param_plot eq 'SYMKP') or (param_plot eq 'AE') or (param_plot eq 'NVTOT') or (param_plot eq 'NVMID'),count_hmb_panel_pos)
		panel_hmb_pos=count_hmb_panel_pos
	endif

	pos_hmb_panel=define_panel(1,npanels,0,panel_hmb_pos)

	if (check_map_file_exist ne '') then begin

		if (panel_hmb_pos lt npanels-1) then begin
			plot,jul_sel,lat_hm,xtickformat=_xtickformat,pos=pos_hmb_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='HM-BND (MLAT)',yrange=[50*hemisphere,75*hemisphere],charsize=charsize,ystyle=1,yminor=4,xticklen=0.05
		;;zero line plotting for HM-BND at 60 MLAT - as a reference
			oplot,[jul_sel[0],jul_sel[n_elements(jul_sel)-1]],[60*hemisphere,60*hemisphere],linestyle='2',thick='2'
		   	if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
     	  if (panel_hmb_pos eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			  endif
   endif
   if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			if (panel_hmb_pos eq 0) then begin
			xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
   endif

			
		endif else begin

			plot,jul_sel,lat_hm,xtickformat='label_date',pos=pos_hmb_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
				ytitle='HM-BND (MLAT)',yrange=[50*hemisphere,75*hemisphere],charsize=charsize,ystyle=1,yminor=4,xticklen=0.05
				xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
					date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		;;zero line plotting for HM-BND at 60 MLAT - as a reference
			oplot,[jul_sel[0],jul_sel[n_elements(jul_sel)-1]],[60*hemisphere,60*hemisphere],linestyle='2',thick='2'

			   	if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
     	  if (panel_hmb_pos eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			  endif
   endif
   if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			if (panel_hmb_pos eq 0) then begin
			xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
   endif

   endelse

	endif else begin
		if (panel_hmb_pos lt npanels-1) then begin
			plot,[0],[0],/nodata,xtickformat=_xtickformat,pos=pos_ae_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtickname=replicate(' ', 40),$
				ytitle='HMB',yrange=[0,1],charsize=charsize,ystyle=1,yminor=4
		endif else begin
			plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_ae_panel, $
				xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
				ytitle='HMB',yrange=[0,1],charsize=charsize,ystyle=1,yminor=4
				xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
					date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2
		endelse
			xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
				'No MAPEX Files', color=get_blue(), charsize='0.5', charthick=2
	endelse

endif







; ;;;;;; Now for the lengthier part --- part with HM boundary, Num-data points and Phi-PC
; ;;;;;; Most of this part is taken from rad_grd_dgnstc.pro --- refer that for more details
;



if ((param_plot[0] eq '-1') or (check_PCP_plot[0] ne -1)) then begin


	if (param_plot[0] eq '-1') then begin
		panel_pcp_pos=7
	endif

	if ((param_plot[0] ne '-1')) then begin
		jinds_check_panel_pcp=where((param_plot eq 'ACEPOS') or (param_plot eq 'ACEIMF') or (param_plot eq 'OMN') $
				or (param_plot eq 'SYMKP') or (param_plot eq 'AE') or (param_plot eq 'HMB') or (param_plot eq 'NVTOT') or (param_plot eq 'NVMID'),count_pcp_panel_pos)
		panel_pcp_pos=count_pcp_panel_pos
	endif

	pos_phipc_panel=define_panel(1,npanels,0,panel_pcp_pos)

	if (check_map_file_exist ne '') then begin

		plot,jul_sel,pot_drop,xtickformat='label_date',pos=pos_phipc_panel, $
			xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
			ytitle=textoidl('\Phi_{pc} [kV]'),yrange=[0,max([100,5*(fix(max(pot_drop)/5)+1)])],charsize=charsize,ystyle=1,yminor=4,xticklen=0.05

			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2

			xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2

			   	if ((n_elements(line_marktime) eq 1) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
     	  if (panel_pcp_pos eq 0) then begin
					xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
						strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			  endif
   endif
   if ((n_elements(line_marktime) eq 2) and line_marktime[0] ne -1) then begin
			oplot,[jul_marktime1,jul_marktime1],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			oplot,[jul_marktime2,jul_marktime2],[!y.crange[0],!y.crange[1]],linestyle=2,thick=2, color=get_black()
			if (panel_pcp_pos eq 0) then begin
			xyouts, jul_marktime1-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[0]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			xyouts, jul_marktime2-0.025*(!x.crange[1]-!x.crange[0]), !y.crange[1]-.15*(!y.crange[1]-!y.crange[0]), $
			strtrim(string(line_marktime[1]),2)+' UT', color=get_black(), charsize='0.35', charthick=2
			endif
   endif




	endif else begin
		plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_phipc_panel, $
			xticks=12,xminor=6,xstyle=1,xrange=[sjul,fjul],xtitle='TIME (UT)',$
			ytitle=textoidl('\Phi_{pc} [kV]'),yrange=[0,1],charsize=charsize
		xyouts, (sjul+fjul)/2, !y.crange[0]+.5*(!y.crange[1]-!y.crange[0]), $
			'No MAPEX Files', color=get_blue(), charsize='0.5', charthick=2
			xyouts, (sjul+fjul)/2-0.07*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				date_in_plot_format, color=get_black(), charsize='0.7', charthick=2
				xyouts, fjul-0.1*(fjul-sjul), !y.crange[0]-.05*npanels*(!y.crange[1]-!y.crange[0]), $
				'VT-SD:Bharat/Mike', color=get_black(), charsize='0.35', charthick=2

	endelse

endif





end

