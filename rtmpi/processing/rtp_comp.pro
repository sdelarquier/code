pro	rtp_comp

common rt_data_blk


radar = 'bks'
date = 20101118
time = [0,2400]
beam = 12
param = ['power','elevation']
 
rt_run, date, radar
ps_open, '~/Desktop/rt_'+strtrim(date,2)+'_'+radar+'_'+strtrim(beam,2)+'.ps', /no_init
rt_plot_rti, beam=beam, param=param, scale=[0,1,10,25], /sun, /grid
ps_close, /no_init, /no_filename

rad_fit_read, date, radar, time=time, /filter, /ajground, /catfile, catpath='/tmp/'
rad_fit_calculate_elevation, date=date, time=time, /overwrite, tdiff=-.324, interfer_pos=[0.,-58.9,-2.7], scan_boresite_offset=8.
ps_open, '~/Desktop/rad_'+strtrim(date,2)+'_'+radar+'_'+strtrim(beam,2)+'.ps', /no_init
rad_fit_plot_rti, beam=beam, param=param, scale=[0,30,10,25], /sun
ps_close, /no_init, /no_filename


end