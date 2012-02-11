PRO	milh_gbm

date=[20101117,20101118]
time=[2100,1300]
parse_date, date[0], year, month, day
parse_time, time[0], hr, mn
jul = julday(month, day, year, hr, mn)

map_plot_panel, jul=jul, coords='mlt', /no_fill, $
  xrange=[-50,0], yrange=[0,50], $
  coast_linethick=1,coast_linecolor=get_black()
; overlay radar fov and ISR beams stations
overlay_fov, name='bks', jul=jul, /grid, /no_fill, fov_linecolor=get_black(),fov_linethick=1., nranges=60, coords='mlt'
overlay_isr, 'MSH', jul=jul, coord='mlt',azim=[-85.,-90.,-95.,-100.,-105.]
overlay_gbm, ['BMLS','SWNO','OTT'], /annotate, jul=jul, coord='mlt', /gbm_themis

END