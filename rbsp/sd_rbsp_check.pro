; This routine uses the output of sd_rbsp to do some sanity check on the I/O from IGRF mapping
pro sd_rbsp_check, rbpos

set_format, /portrait
ps_open, '~/Desktop/rbposB_check.ps'
clear_page

xyouts, .5, .95, 'RBSP B', align=.5, /normal

xticknames = replicate(' ', 60)

pos = [.1,.79,.9,.9]
plot, rbpos[1,*,1,2], xtickname=xticknames, ytitle='Sat. altitude [km]', yrange=[500., 32e3], ystyle=1, position=pos, chars=.5
pos = [.1,.67,.9,.78]
plot, rbpos[1,*,1,0], xtickname=xticknames, ytitle='Sat. latitude [km]', yrange=[-90., 90.], ystyle=1, position=pos, chars=.5
pos = [.1,.55,.9,.66]
plot, rbpos[1,*,1,1], xtickname=xticknames, ytitle='Sat. longitude [km]', yrange=[-180., 180.], ystyle=1, position=pos, chars=.5
xyouts, .93, .75, 'Sat. geographic position', align=.5, orient=90., /normal, chars=.8

pos = [.1,.39,.9,.5]
; plot, rbpos[1,*,0,2], xtickname=xticknames, ytitle='Sat. altitude [km]', yrange=[500., 32e3], ystyle=1, position=pos, chars=.5
pos = [.1,.27,.9,.38]
plot, rbpos[1,*,0,0], xtickname=xticknames, ytitle='FP latitude [km]', yrange=[-90., 90.], ystyle=1, position=pos, chars=.5
pos = [.1,.15,.9,.26]
plot, rbpos[1,*,0,1] - 180., xtickname=xticknames, ytitle='FP longitude [km]', yrange=[-180., 180.], ystyle=1, position=pos, chars=.5
xyouts, .93, .25, 'Footpoint geographic position', align=.5, orient=90., /normal, chars=.8

ps_close, /no_f


ps_open, '~/Desktop/rbposA_check.ps'
clear_page

xyouts, .5, .95, 'RBSP A', align=.5, /normal

xticknames = replicate(' ', 60)

pos = [.1,.79,.9,.9]
plot, rbpos[0,*,1,2], xtickname=xticknames, ytitle='Sat. altitude [km]', yrange=[500., 32e3], ystyle=1, position=pos, chars=.5
pos = [.1,.67,.9,.78]
plot, rbpos[0,*,1,0], xtickname=xticknames, ytitle='Sat. latitude [km]', yrange=[-90., 90.], ystyle=1, position=pos, chars=.5
pos = [.1,.55,.9,.66]
plot, rbpos[0,*,1,1], xtickname=xticknames, ytitle='Sat. longitude [km]', yrange=[-180., 180.], ystyle=1, position=pos, chars=.5
xyouts, .93, .75, 'Sat. geographic position', align=.5, orient=90., /normal, chars=.8

pos = [.1,.39,.9,.5]
; plot, rbpos[0,*,0,2], xtickname=xticknames, ytitle='Sat. altitude [km]', yrange=[500., 32e3], ystyle=1, position=pos, chars=.5
pos = [.1,.27,.9,.38]
plot, rbpos[0,*,0,0], xtickname=xticknames, ytitle='FP latitude [km]', yrange=[-90., 90.], ystyle=1, position=pos, chars=.5
pos = [.1,.15,.9,.26]
plot, rbpos[0,*,0,1] - 180., xtickname=xticknames, ytitle='FP longitude [km]', yrange=[-180., 180.], ystyle=1, position=pos, chars=.5
xyouts, .93, .25, 'Footpoint geographic position', align=.5, orient=90., /normal, chars=.8

ps_close, /no_f

end