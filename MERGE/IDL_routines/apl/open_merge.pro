function open_merge,fname,u_mrg,u_los,u_mik

 status = 0
 on_ioerror, exit
 openr,u_mrg,fname+'.mrg',/get_lun
 openr,u_los,fname+'.los',/get_lun

 openw,u_mik,fname+'.mik',/get_lun				;JMR

 recl1    =  0L
 recl2    =  0L
 version  =  1.0
 infi     =  intarr(27)
 infr     =  fltarr(8)

 readu,u_mrg, recl1,version,infi,infr,recl2

 writeu,u_mik, recl1,version,infi,infr,recl2			;JMR

 status = 1

exit:
 return,status
 end
