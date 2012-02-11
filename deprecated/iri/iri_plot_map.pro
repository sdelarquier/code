pro iri_plot_map, res=res

if ~keyword_set(res) then $
    res = 1.

set_format, /portrait
clear_page

charsize = 0.5

xmaps = 2
ymaps = 3

xmap = 1
ymap = 0
date = 20100621
time = 2200
scale = [1e10, 1.1e12]
iri_plot_map_panel, xmaps, ymaps, xmap, ymap, date, time, $
	alt=300., scale=scale, charsize=charsize, res=res

xmap = 1
ymap = 1
date = 20100621
time = 1200
scale = [1e10, 1.1e12]
iri_plot_map_panel, xmaps, ymaps, xmap, ymap, date, time, $
	alt=300., scale=scale, charsize=charsize, res=res

xmap = 0
ymap = 0
date = 20101221
time = 2200
scale = [1e10, 1.2e12]
iri_plot_map_panel, xmaps, ymaps, xmap, ymap, date, time, $
	alt=300., scale=scale, charsize=charsize, res=res

xmap = 0
ymap = 1
date = 20101221
time = 1200
scale = [1e10, 1.2e12]
iri_plot_map_panel, xmaps, ymaps, xmap, ymap, date, time, $
	alt=300., scale=scale, charsize=charsize, res=res

xmap = 0
ymap = 2
date = 20101221
scale = [-9e10, 9e10]
iri_plot_map_diff, xmaps, ymaps, xmap, ymap, date, $
	alt=300., scale=scale, charsize=charsize, res=res

xmap = 1
ymap = 2
date = 20100621
scale = [-9e10, 9e10]
iri_plot_map_diff, xmaps, ymaps, xmap, ymap, date, $
	alt=300., scale=scale, charsize=charsize, res=res

end