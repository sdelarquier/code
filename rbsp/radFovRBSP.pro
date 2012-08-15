; Plots a field of view map of all radars in a given hemisphere with 3 highlighted
; beams (for a themis mode on steroids)
pro radFovRBSP, hemisphere=hemisphere

common radarinfo

if ~keyword_set(hemisphere) then $
	hemisphere = 1

; Get radar list and center beam
radars = network[where(network.status eq 1 and hemisphere*network.site[0].geolat gt 0.)]
codes = radars.code[0]
sites = radars.site
beams = sites[where(radars.site.tval eq -1)].maxbeam/2


; Open postscript
ps_open, '~/Desktop/radFovRBSP.ps'

; First layer with center beam
rad_plot_fov, codes, beam=beams, charsize=1., /fillanno

; Second layer at n-1
rad_plot_fov, codes, beam=beams-1, charsize=1., /overlay, /fillanno, bcolor=1

; Third layer at n+2
rad_plot_fov, codes, beam=beams+2, charsize=1., /overlay, /fillanno, bcolor=3


; Close postscript
ps_close, /no_f
spawn, 'ps2pdf ~/Desktop/radFovRBSP.ps ~/Desktop/radFovRBSP.pdf'
spawn, 'rm -f ~/Desktop/radFovRBSP.ps'


end
