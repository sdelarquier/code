pro	rad_hist
; Lasse's function

common rad_data_blk
set_format, /landscape

; date=[20100901,20101101]
date=[20110209,20110209]
; sfjul, date, [0,2400], sjul, fjul, no_d=nd
sfjul, date, [2200,1200], sjul, fjul, no_d=nd
hist = dblarr(16, 60)
for i=0L, nd-1L do begin
  sfjul, adate, atime, sjul+double(i), /jul_to
  rad_fit_set_data_index, -1
  ;        rad_fit_read, adate, 'bks', time=[0,1400]
  rad_fit_read, adate, 'bks', time=[2100,1300]
  ns = (*rad_fit_info[0]).nscans
  for s=0, ns-1 do begin
    inds = where((*rad_fit_data[0]).beam_scan eq s, sc)
    if sc ne 16 then $
      continue
    tbeams = (*rad_fit_data[0]).beam[inds]
    ubeams = uniq(tbeams, sort(tbeams))
    thist = (*rad_fit_data[0]).power[inds[ubeams],0:59]
    cinds = where(thist ne 10000., gc)
    if gc gt 0 then $
      hist[cinds] += thist[cinds]
  endfor
endfor

; hist = reverse(hist/max(hist),1)
hist = hist/max(hist)

ps_open, '~/Desktop/bks_hist.ps'
clear_page
map_plot_panel, yrange=[-50.,0.], /no_fill,xrange=[-30.,20.]
rad_overlay_scan, hist, 33, scale=[0.,1.]
overlay_fov, name=['bks','kap'], /grid, /no_fill, fov_linecolor=get_black(),fov_linethick=1., nranges=60
overlay_coast, /no_fill, coast_linethick=1,coast_linecolor=get_black()
overlay_isr, 'msh', azim=[-70.,-30.]
plot_colorbar, scale=[0.,1.], legend='Counts', nlevels=10, colorsteps=10, level_format='(f3.1)', /no_rotate
ps_close


end