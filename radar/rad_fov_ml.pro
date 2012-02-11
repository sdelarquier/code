pro	rad_fov_ml

ps_open, '/home/sebastien/Desktop/rad_fov.ps'
clear_page
map_plot_panel, yrange=[-45,0], /no_fill, coords='magn',xrange=[-35,10]
overlay_fov, name=['bks','kap'], fov_linecolor=get_black(),fov_linethick=2., nranges=75, fov_fillcolor=254, /grid, /annotate, nbeams=16
; overlay_fov, name=['wal','fhe','fhw','cve','cvw','hok'], fov_linecolor=get_black(),fov_linethick=1., nranges=75, /annotate, /no_fill, nbeams=16
overlay_coast, coords='magn', /no_fill, coast_linethick=2,coast_linecolor=get_gray()
overlay_isr, 'msh', azim=[-70.,-30.]
ps_close

end
