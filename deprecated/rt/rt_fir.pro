pro	rt_fir, year, beam=beam

common rt_data_blk

if ~keyword_set(beam) then $
	beam=7

months = 1 + indgen(12)
date = year*10000L + months*100L + 1L

npanels = 4
set_format, /portrait, /sardines, /tokyo
ps_open, '~/Desktop/fir_'+STRTRIM(year,2)+'.ps', /no_init
clear_page
charsize = get_charsize(2,npanels)	
xyouts, .5, .9, 'FIR - '+strtrim(year,2), /normal, $
	align=.5, charsize=(2*charsize)
for nd=0,n_elements(date)-1 do begin
	if ~((nd+1) mod npanels) then begin
		xtitle = 'Time (UT)'
		last = 1
	endif else begin
		xtitle = ''
		last = 0
	endelse
	rt_run, date[nd], 'fir', /hhour, beam=beam;, /no_comment
	rt_plot_rti_panel, 2, npanels, 0, (nd mod npanels), param='power', date=date[nd], $
		ytitle='Range', yrange=[0,70], /first, $
		xtitle=xtitle, last=last, scale=[0.,18.]
	title='Front - beam '+strtrim(rt_info.beam,2)
	pos = define_panel(2,npanels,0,(nd mod npanels))
	xyouts, pos[0]+.01, pos[1]+.01, format_juldate(rt_info.sjul, /date), $
		/normal, charsize=charsize
	pos = define_panel(2,npanels,0,0)
	if (nd mod npanels) then $
		xyouts, pos[0] + (pos[2]-pos[0])/2., pos[3]+.01, title, align=.5, /normal, $
			charsize=charsize
	rt_run, date[nd], 'fir', /hhour, beam=beam, /back;, /no_comment
	rt_plot_rti_panel, 2, npanels, 1, (nd mod npanels), param='power', date=date[nd], $
		ytitle='', yrange=[0,70], $
		xtitle=xtitle, last=last, scale=[0.,18.]
	title='Back'
	pos = define_panel(2,npanels,1,0)
	if (nd mod npanels) then $
		xyouts, pos[0] + (pos[2]-pos[0])/2., pos[3]+.01, title, align=.5, /normal, $
			charsize=charsize
	if ~((nd+1) mod npanels) and (nd ne n_elements(date)-1) then $
		clear_page
endfor 
ps_close, /no_init


end