pro iri_test

ps_open, '~/Desktop/iri_test.ps'

clear_page
iri_plot_map,20101221, 1200, param='tec', /ut, /stereo, hemisphere=1, panel=[0,0,2], /iso, /no_bar
iri_plot_map,20101221, 1200, param='tec', /ut, /stereo, hemisphere=-1, panel=[1,0,2], /iso

ps_close

end