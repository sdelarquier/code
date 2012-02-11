pro milh_rad_read, date

rad_fit_read, [date,date+1], 'bks', time=[2100,1300], /filter, /force
rad_fit_calculate_elevation, date=[date,date+1],time=[2100,1300],tdiff=-.324,/overwrite,scan_boresite_offset=8

END