pro ionoscat_multi, option, xparam=xparam, zparam=zparam, $
		xrange=xrange, yrange=yrange, beam=beam

common rt_data_blk

if option eq 4 then begin
	for im=1,6 do $
		rad_fit_plot_ionoscat_hourly, im, 2011, /ps

; 	ps_open, '~/Desktop/radstat.ps'
; 	for id=15,28 do begin
; 		clear_page
; 		date = 20110200 + id
; 		rad_fit_read, date, 'bks', time=[0,1200], /filter, /ajground, /catfile, catpath='/tmp/'
; 		rad_set_scatterflag, 0
; 		set_colorsteps, 250
; 		rad_fit_plot_rti_panel, 1, 2, 0, 0, param='elevation', scale=[0,55], beam=7, time=[0,1200]
; 		set_colorsteps, 4
; 		rad_fit_plot_rti_panel, 1, 2, 0, 1, param='gscatter', scale=[0,3], beam=7, time=[0,1200]
; 	endfor
; 	ps_close
	return
endif

if ~keyword_set(beam) then $
	beam = 0

; Retrieve raytracing parameters from structure
radar = rt_info.name
caldat, rt_data.juls, month, day, year, hours, minutes
tdate 	= year*10000L + month*100L + day
date = tdate[0]


case option of
	0:	begin
			if ~keyword_set(xparam) then $
				xparam = 'slant'
			if ~keyword_set(zparam) then $
				zparam = 'elevation'
			time = lindgen(6)*2*100
			ps_open, '~/Desktop/ionoscatm_'+radar+'_'+STRTRIM(date,2)+'_'+xparam+'_'+zparam+'_'+STRTRIM(beam,2)+'.ps', /no_init
			set_format, /portrait
			!X.MARGIN = [4,3]	
			clear_page

			for it=0,n_elements(time)-1 do begin
				!P.MULTI = [n_elements(time)-it,2,3]
				rt_plot_ionoscat, time[it], zparam=zparam, xparam=xparam, beam=beam
			endfor

			ps_close
			!P.MULTI = 0
		end
	1:	begin
			time = lindgen(24)*1*100
			dir = '~/Desktop/'
			for it=0,n_elements(time)-1 do begin
				filen = 'ionoscatm_'+radar+'_'+STRTRIM(date,2)+'_'+STRTRIM(STRING(time[it],format='(I04)'),2)
				ps_open, dir+filen+'.ps', /no_init
				loadct, 0, file='/tmp/colors2.tbl'

				rt_plot_ionoscat3, time[it]

				ps_close, /no_init
				spawn, 'ps2png.sh '+dir+filen+'.ps'
				spawn, 'rm -f '+dir+filen+'.ps'
				spawn, 'mv '+filen+'.png '+dir+filen+'.png' 
			endfor

			spawn, 'convert -delay 100  '+dir+'ionoscatm_'+radar+'_'+STRTRIM(date,2)+'_'+'????.png '+dir+'ionoscat_multi.gif'
			spawn, 'rm -f '+dir+'ionoscatm_'+radar+'_'+STRTRIM(date,2)+'_'+'????.png'
		end
	2: 	begin
			time = lindgen(6)*2*100
			ps_open, '~/Desktop/refrel_'+radar+'_'+STRTRIM(date,2)+'_'+STRTRIM(beam,2)+'.ps', /no_init
			set_format, /portrait
			!X.MARGIN = [4,3]	
			clear_page

			for it=0,n_elements(time)-1 do begin
				!P.MULTI = [n_elements(time)-it,2,3]
				rt_plot_nrelev, time[it], xrange=xrange, yrange=yrange, beam=beam
			endfor

			ps_close, /no_filename
			!P.MULTI = 0
		end
	3:	begin
			time = lindgen(6)*2*100
			ps_open, '~/Desktop/nmf2bias_'+radar+'_'+STRTRIM(date,2)+'_'+STRTRIM(beam,2)+'.ps', /no_init
			set_format, /portrait
			!X.MARGIN = [4,3]	
			clear_page

			for it=0,n_elements(time)-1 do begin
				!P.MULTI = [n_elements(time)-it,2,3]
				rt_plot_nmf2_bias, time[it], beam=beam
			endfor

			ps_close
			!P.MULTI = 0
		end
endcase


end