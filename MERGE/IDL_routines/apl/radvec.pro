	pro radvec,menu=mnsl,file=flnm,scale=scl,cbar=colbar,ncon=nocon, $
                                                                 mt=mt

	common magblk, b_field	

	if (KEYWORD_SET(flnm)) then begin
	  file = flnm
	endif else begin
	  print,' Need to provide a filename or an array of filenames!'
	  stop
	endelse

	if (KEYWORD_SET(scl)) then scale=scl else scale=1000

; This program will plot the velocities in the diamonds as provided by
; RAD_MERGE.
;
; IOPT = -1 plots overlapping k-vectors in all the diamonds
; IOPT =  0 plots LOS velocities
; IOPT =  2 plots 2-D vectors
;
; Bring up the plot options menus with the switch:  menu=1
; 
;; Plot options are read from the file RADVEC_input, which presently contains
; just two lines:
;
; west lon, east lon (deg, 2*I)
;  low lat, high lat (deg, 2*I)
;
;
; I modified the attached routines (open_merge, write_merge) in order to
; produce a psuedo .mrg file made up of (possibly) L shell fit vectors.
; The file is of the type 941019C.mik and is produced by running radvec_tot
; once with (presumably) the L shell fit option.  In order to see it, first
;   $cp 941019C.mik 941019X.mrg
;   $cp 941019C.los 941019X.los
; then run like any other .mrg/.fit combination.
; Currently, this .mik file contains output only for the last radar pair.
;
; 		**	Notes	**
;
; i) For multi radar pairs, no effort is made to sync the files in time,
;    because this is achieved in RAD_MERGE by starting the processing at the
;    same times and using the same time step
; 
; ii) Ajusting the CHAR parameter does not affect sizes in x-window mode
;
; iii) A scan devoid of vectors can cause problems.  A sure sign of this
;      happening is an inconsistency condition between NVECTS and A = 
;      where (varr ne 9999), which happens because of 'scan-skipped' mismatch.
;      To avoid a problem with writing the .mik file, I have the body of
;      the program execute even when NVECTS=0.
;      A good quality check is to compare the file sizes of the .mrg and .mik
;      files when they should be identical (i.e., no added vectors)
;
; Subroutines called:
;
;	lshell_fit.pro
;	open_merge.pro
;	sub_pntazm.pro
;	sub_velazm.pro		
;	write_merge.pro
;	read_merge.pro
;
;
; Last written: Nov 1/95
;
;*****************************************************************************


title_r  =  strarr(n_elements(file))
for i = 0,n_elements(file)-1 do begin
  title_r(i) = name_pair(file(i))
endfor
nf       =  size(file)                               ;consider input file names
if nf(0) gt 0 then begin
    jmax = nf(1)-1
endif else jmax = 0
iopt     =  INTARR(jmax+1)
iopl     =  iopt
iopt_d   =  0
iopt3    =  0
sc_hld   =  1.5
char     =  1.0

if (KEYWORD_SET(mnsl)) then begin		;user selects menu 

  iopt_d   =  wmenu(['X-window','PSC file (col)','PS file (bw)','Movie'], $
                                                                        init=0)

  iopt1    =  wmenu(['Standard','2-d vectors','LOS vectors', $
					              'Grid vectors'], init=0)

  if (iopt1 eq 0) then begin               	;standard plotting
    for j = 0,jmax do begin			;only merged vectors
      iopt(j) = 0
    endfor
    ncrd = 0
  endif

  if (iopt1 ne 0) then begin			;non-standard plotting
 
    if (iopt1 eq 1) then begin
      title =  ['1st pair','2nd pair','3rd pair']
      for j = 0,jmax do begin
        np      =  wmenu([title(j),'Merge','L shell','M + L','L + M'], init=1)
        iopt(j) =  np-1
        if (np ge 2) then begin
          nl    =  wmenu([title(j),'L-fit 1st radar','L-fit 2nd radar'],init=1)
          iopl(j) =  nl-1           
        endif
      endfor
      q = where (iopt ge 1, count) 
;      if (iopt_d eq 0 and count ne 0) then $
;	                    iopt3 =  wmenu(['No-see fits','See fits'], init=0)
      iopt3 = 0
    endif


  endif

  ncrd   =  wmenu(['Geographic','Geomagnetic'],init=0)

  if (iopt_d eq 0) then begin
    sc_h   =  wmenu(['No wait','2.5-sec','5-sec'],init=0)
  endif else begin
    sc_h   =  0
  endelse
  sc_hld =  sc_h * 2.5

endif else begin
						;user selected
  iopt1 = 0					;standard plotting
  for j = 0,jmax do begin		
    iopt(j) = 0
  endfor
  ncrd = 0
  
endelse

if (iopt_d eq 1) then begin			;create PS file (idl.ps) 
  psport
  device,/color
endif

if (iopt_d eq 2) then begin
  psport
endif


imp:						;<<<<<<<<<<  goto point


dev       =  !d.name

FMAX      =  10
!P.FONT   =   0
new_vel_col


get_lun,u
openr,u,'radvec_input'					;input parameters
readf,u,xmin,xlim
readf,u,ymin,ylim
close,u
free_lun,u


lshlat  =  fltarr(4,360)
lshlon  =  fltarr(4,360)
get_lun,u1
openr,u1,'/homes/bristow/gravity/idl/lsh_geog.dat'
readf,u1,lshlon
readf,u1,lshlat
close,u1
free_lun,u1

S       =  findgen(16)*(!PI*2/16.)			;dot symbols
usersym,cos(S),sin(S),/FILL

ncol    =  13						;set the colors
ccol    =  ncol - 2
mtcol   =  ncol - 3
tcol    =  ncol - 1
if (!d.name eq 'PS') then tcol = ccol              

ind_arr = [200,800]	                      	;colours for radar points


A = mrg_str_def()


u_mrg   = intarr(FMAX)
u_los   = intarr(FMAX)
u_mik   = intarr(FMAX)					
mrg_str = replicate(A,FMAX)

mrg_str(*).sdate(*,*) = 99
mrg_str(*).edate(*,*) = 0

rot = 0
if ymin gt ylim then begin
    hold = ylim
    ylim = ymin
    ymin = hold
    rot = 180.
endif


fcount  = 0
nvects  = 1

for j = 0,jmax do begin					;open files
  status   =  open_merge(file(j),um,ul,uk)                   ;JMR
  u_mrg(j) =  um
  u_los(j) =  ul
  u_mik(j) =  uk
  if status ne 1 then begin
    print,'ERROR OPENING FILE:',file(j)
    goto, exit
  endif
  status = read_merge(u_mrg(j),u_los(j),m_str)		;first read, all later
  mrg_str(j) = m_str					;at end of while loop
  if status ne 1 then begin
    print,'ERROR READING FILE:',file(j),u_mrg(j),u_los(j)
    goto, exit
  endif
endfor


if (iopt_d eq 3) then begin				;open movie file(s)
  fname = "991332_PICT0.000"
  frame=bytarr(550,480)
  set_plot,'z'
  device,z_buffer=0
  b=intarr(2)
  b(1) = 480
  b(0) = 550
  device,set_resolution=b
  char = 1.5
endif


while ( status eq 1 ) do begin				;big read and plot loop

  fcount =  fcount + 1
  lat_g  =  fltarr(16,16)
  lon_g  =  fltarr(16,16)
  lat_m  =  fltarr(16,16)
  lon_m  =  fltarr(16,16)
  nlos   =  intarr(16,16)
  vlos   =  fltarr(2,16,16)
  vmag   =  fltarr(16,16)
  vazm   =  fltarr(16,16)

; the map plotting stuff must be called outside of the j-loop 

  dlat = abs(ylim-ymin)/5.
  dlon = abs(xlim-xmin)/5.
  dlat = float((fix(dlat)/5)*5)
  dlon = float((fix(dlon)/5)*5)
  if dlat EQ 0 then dlat = 2.0
  if (ymin MOD 5) EQ 0 AND dlat EQ 2.0 then dlat = 5.0
  if dlon EQ 0 then dlon = 2.0
  if (dlon GT 30.0) then dlon = 30.0
  if (ylim LT 0) then llab = ylim else llab = ymin + dlat
  pollat = (ymin+ylim)/2.
  pollon = (xmin+xlim)/2.
  if (iopt_d ne 3) then begin
    set_viewport,.0,1.0,.0,1.0
    map_set,pollat,pollon,limit=[ymin,xmin,ylim,xlim],/GRID,$
		 glinethick=0.5,/stereo,color=ccol,$
		 latdel=dlat,londel=dlon,/label,latlab=xmin+.25*dlon,$
		 lonlab=llab
  endif else begin
    set_viewport,.05,.95,.05,.95
    map_set,pollat,pollon,limit=[ymin,xmin,ylim,xlim],/GRID,$
		 glinethick=0.5,/stereo,color=ccol,$
		 latdel=dlat,londel=dlon,/label,latlab=xmin+.25*dlon,$
		 lonlab=llab,noborder=1
  endelse 


  if not keyword_set(nocon)and ncrd eq 0 then map_continents,color=ccol
  lthik    =   1.
  NLV      =   6
  btl      =   0
  tpl      =   6.*scale/5. 
  CIN      =  (1+INDGEN(NLV+1));*((8)/(NLV+1))
  LVL      =   BTL+(INDGEN(NLV))*(TPL-BTL)/(NLV)
  delt_lat =   abs(ylim-ymin)
  delt_lon =   abs(xlim-xmin)


  for j = 0,jmax do begin                            ;loop through radar pairs

    nvects   =  mrg_str(j).ngood
      
    lat_g    =  mrg_str(j).lat_g		     ;assign values for this
    lon_g    =  mrg_str(j).lon_g		     ;period
    lat_m    =  mrg_str(j).lat_m
    lon_m    =  mrg_str(j).lon_m
    nlos     =  mrg_str(j).nlos
    vlos     = -mrg_str(j).vlos                            ;note vel sign
    vmag     =  mrg_str(j).vmag
    vazm     =  mrg_str(j).azim     
    ilos     =  intarr(2,16,16)
    vazm_m   =  fltarr(16,16)

    q   =  where(mrg_str(j).vmag ne 9999, a)               ;just a check
    if (a ne nvects) then begin
      print,' inconsistent number of vects, nvects,a = ',nvects,a
      print,mrg_str(j).sdate(0,3),mrg_str(j).sdate(0,4),mrg_str(j).sdate(0,5)
      stop
    endif

    q        =  where(abs(vlos) lt 9999, lcount)
    if (lcount ne 0) then ilos(q)  =  1

    q        =  where (lon_g ge 180., count)
    if (count gt 0) then lon_g(q) = lon_g(q) - 360.

    alos   = fltarr(2,16,16)                             ;azimuth arrays
    alos_m = fltarr(2,16,16)	
	
; here we take the coordinates (geographic) of the cell centers and determine
; pointing azimuths for the two radars

    sub_pntazm,lon_g,lat_g,alos
    sub_pntazm,lon_m,lat_m,alos_m

; find the azimuth of the solved velocities in mag coordinates

    sub_velazm,nlos,alos_m,vlos,vazm_m

    if (ncrd eq 0) then begin
      lon_x  =  lon_g
      lat_x  =  lat_g
      alos_x =  alos
      vazm_x =  vazm
    endif
    if (ncrd eq 1) then begin
      lon_x  =  lon_m
      lat_x  =  lat_m
      alos_x =  alos_m
      vazm_x =  vazm_m
    endif

; if doing an L-shell fitting, go to it now
;
; which radar of the pair is fit is decided in s/r lshell_fit!

    if (iopt(j) ge 1) then begin

      ind_r = iopl(j)

      lshell_fit,lon_m,lat_m,alos_m,vlos,alos,ind_r,iopt3, $
						  nlsh,vmag_l,vazm_l,vazm_m_l

      if (iopt(j) eq 1) then begin              ;L shell sol only
	nlos      =    nlsh
	vmag      =    vmag_l
	if (ncrd eq 0) then vazm_x  =  vazm_l
	if (ncrd eq 1) then vazm_x  =  vazm_m_l
      endif
      if (iopt(j) eq 2) then begin              ;M first then L shell sol
	q         =    where(nlos ne 2,count)
	if (count ne 0) then begin
	  nlos(q)   =    nlsh(q)
	  vmag(q)   =    vmag_l(q)
	  if (ncrd eq 0) then vazm_x(q) =  vazm_l(q)
	  if (ncrd eq 1) then vazm_x(q) =  vazm_m_l(q)
	endif
      endif
      if (iopt(j) eq 3) then begin              ;rewrite M with L sol
	q         =    where(nlsh ne 0,count)
	if (count ne 0) then begin
	  nlos(q)   =    nlsh(q)
	  vmag(q)   =    vmag_l(q)
	  if (ncrd eq 0) then vazm_x(q) =  vazm_l(q)
	  if (ncrd eq 1) then vazm_x(q) =  vazm_m_l(q)
        endif
      endif

    endif


    if (iopt1 eq 3) then begin			;grid vectors

      for ib = 0,15 do begin
	for jb = 0,15 do begin

	  if (lon_x(ib,jb) gt xmin and lon_x(ib,jb) lt xlim and $
		      lat_x(ib,jb) gt ymin and lat_x(ib,jb) lt ylim) then begin

	    for k = 0,1 do begin

                  ind    =  max(where(LVL le ind_arr(k))) + 1
		  xazm   =  alos_x(k,ib,jb)
		  vscx   =  50.
		  vabs   =  250.
	          sub_sphcal,lon_x(ib,jb),lat_x(ib,jb),xazm,vscx,Clon,Clat

	          n_xy_s    =  convert_coord(lon_x(ib,jb),lat_x(ib,jb),/data, $
							           /to_normal)
	          n_xy_e    =  convert_coord(Clon,Clat,/data,/to_normal)
		  del_x     =  n_xy_e(0) - n_xy_s(0)
		  del_y     =  n_xy_e(1) - n_xy_s(1)
		  n_azm     =  atan( del_x,del_y )
		  n_azm_deg =  n_azm * (1./!dtor)
		  scx_len   =  vabs/1000. * .08
		  n_xy_e    =  n_xy_s + [sin(n_azm)*scx_len,cos(n_azm)*scx_len]
		  plots,n_xy_s(0),n_xy_s(1),psym=8,symsize=0.3, $
						   color=cin(ind),/normal
		  plots,[n_xy_s(0),n_xy_e(0)],[n_xy_s(1),n_xy_e(1)], $
		                            color=cin(ind),thick=lthik,/normal

            endfor
          endif
	endfor
      endfor
    endif


    if (iopt1 eq 2) then begin                     ;los vel vectors

      for ib = 0,15 do begin
        for jb = 0,15 do begin

	  if (lon_x(ib,jb) gt xmin and lon_x(ib,jb) lt xlim and $
		      lat_x(ib,jb) gt ymin and lat_x(ib,jb) lt ylim) then begin

	    for k = 0,1 do begin

	      if (ilos(k,ib,jb) eq 1) then begin

                    ind    =  max(where(LVL le ind_arr(k))) + 1
		    if (vlos(k,ib,jb) ge 0.) then xazm = alos_x(k,ib,jb)
		    if (vlos(k,ib,jb) lt 0.) then xazm = alos_x(k,ib,jb) + 180
		    vabs =  abs(vlos(k,ib,jb))
		    vscx =  100. * (vabs/500.)
	            sub_sphcal,lon_x(ib,jb),lat_x(ib,jb),xazm,vscx,Clon,Clat

	            n_xy_s    =  convert_coord(lon_x(ib,jb),lat_x(ib,jb), $
                                                            /data,/to_normal)
	            n_xy_e =  convert_coord(Clon,Clat,/data,/to_normal)
		    del_x     =  n_xy_e(0) - n_xy_s(0)
		    del_y     =  n_xy_e(1) - n_xy_s(1)
		    n_azm     =  atan( del_x,del_y )
		    n_azm_deg =  n_azm * (1./!dtor)
		    scx_len   =  vabs/scale * .08
		    n_xy_e    =  n_xy_s + $
				        [sin(n_azm)*scx_len,cos(n_azm)*scx_len]
		    if (iopt_d ne 2) then begin
		      plots,n_xy_s(0),n_xy_s(1),psym=8,symsize=0.3, $
			  			   color=cin(ind),/normal
		      plots,[n_xy_s(0),n_xy_e(0)],[n_xy_s(1),n_xy_e(1)], $
		                            color=cin(ind),thick=lthik,/normal
		    endif
		    if (iopt_d eq 2) then begin
		      plots,n_xy_s(0),n_xy_s(1),psym=8,symsize=0.3,/normal
		      plots,[n_xy_s(0),n_xy_e(0)],[n_xy_s(1),n_xy_e(1)], $
		                            thick=lthik,/normal
		    endif
              endif
	    endfor
	  endif
	endfor
      endfor
    endif

    if (iopt1 le 1) then begin			;2-d vel vectors

      for ib = 0,15 do begin
	for jb = 0,15 do begin

	  if (nlos(ib,jb) ge 2) then begin

	    if (lon_x(ib,jb) gt xmin and lon_x(ib,jb) lt xlim and $
		      lat_x(ib,jb) gt ymin and lat_x(ib,jb) lt ylim) then begin

                  ind       =  max(where( LVL le vmag(ib,jb) )) + 1

		  xazm      =  vazm_x(ib,jb)
		  vabs      =  abs(vmag(ib,jb))
		  vscx      =  100. * (vabs/500.)
	          sub_sphcal,lon_x(ib,jb),lat_x(ib,jb),xazm,vscx,Clon,Clat

	          n_xy_s    =  convert_coord(lon_x(ib,jb),lat_x(ib,jb),/data, $
							            /to_normal)
	          n_xy_e    =  convert_coord(Clon,Clat,/data,/to_normal)
		  del_x     =  n_xy_e(0) - n_xy_s(0)
		  del_y     =  n_xy_e(1) - n_xy_s(1)
		  n_azm     =  atan( del_x,del_y )
		  n_azm_deg =  n_azm * (1./!dtor)
		  scx_len   =  vabs/scale * .08
		  n_xy_e    =  n_xy_s + [sin(n_azm)*scx_len,cos(n_azm)*scx_len]

		  if (iopt_d ne 2) then begin
		    plots,n_xy_s(0),n_xy_s(1),psym=8,symsize=0.5, $
			  			   color=cin(ind),/normal
		    plots,[n_xy_s(0),n_xy_e(0)],[n_xy_s(1),n_xy_e(1)], $
		                            color=cin(ind),thick=lthik,/normal
		  endif
		  if (iopt_d eq 2) then begin
		    plots,n_xy_s(0),n_xy_s(1),psym=8,symsize=0.5,/normal
		    plots,[n_xy_s(0),n_xy_e(0)],[n_xy_s(1),n_xy_e(1)], $
		                            thick=lthik,/normal
		  endif


	    endif
	  endif
	endfor
      endfor
    endif

  endfor				;end of loop through radar pairs


  yr       =  mrg_str(0).sdate(0,0)
  mo       =  mrg_str(0).sdate(0,1)
  dy       =  mrg_str(0).sdate(0,2)
  shr      =  min(mrg_str(*).sdate(0,3))
  smin     =  min(mrg_str(*).sdate(0,4))
  ssec     =  min(mrg_str(*).sdate(0,5))
  ehr      =  max(mrg_str(*).edate(0,3))
  emin     =  max(mrg_str(*).edate(0,4))
  esec     =  max(mrg_str(*).edate(0,5))

  print,yr,mo,dy,fix(shr),fix(smin),fix(ssec),fix(ehr),fix(emin),fix(esec)

  if (iopt_d eq 3) then begin
    syr = string(yr)
    if yr gt 1000 then syr = strmid(strtrim(syr,2),2,2)
    smo = string(mo)
    sdy = string(dy)
    if mo lt 10 then smo = '0'+smo
    if dy lt 10 then sdy = '0'+sdy
    dstring = strcompress(syr+smo+sdy+"_pict",/remove_all)
    strput,fname,strcompress(dstring+string((fcount)/1000.),/remove_all)
    print,fname
  endif

  if ncrd eq 1 then begin				;plot MLTs for
    utsec = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)	;mag coordinates
    mlt_min = mlt(yr,utsec,xmin)
    mlt_max = mlt(yr,utsec,xlim)
    if (mlt_max lt mlt_min) then mlt_max = mlt_max + 24
    mlt_ref = indgen(17)*3
    q       = where (mlt_ref ge mlt_min and mlt_ref le mlt_max, mcount)
    if (mcount ne 0) then begin
      for m = 0,mcount-1 do begin
        mltx    =  mlt_ref(q(m))
        del_mlt =  mltx - mlt_min
        xtst    =  xmin + del_mlt*15.
        lats    =  ylim - findgen(ylim-ymin+1)
        lons    =  fltarr(n_elements(lats)) + xtst
        plots,lons,lats,col=tcol,thick=2.
	if (mltx ge 24) then mltx = mltx - 24
        tstring = "!5"+string(mltx)
        tstring = strcompress(tstring,/remove_all)+'MLT'
        xyouts,lons(n_elements(lats)-1),lats(n_elements(lats)-1),tstring, $
						color=tcol,alignment=.4,font=-1
      endfor
    endif
  endif



	
; after this point, the plot area is re-sized through PLOTS calls

  if (iopt_d ne 2) then begin
    plots,.84,.87,psym=8,symsize=0.6,color=cin(nlv),/normal      ;ref arrow
    plots,[.84,.92],[.87,.87],color=cin(nlv),thick=lthik,/normal
  endif else begin
    plots,.84,.87,psym=8,symsize=0.6,/normal      ;ref arrow
    plots,[.84,.92],[.87,.87],thick=lthik,/normal
  endelse
  scstring = strtrim(string(scale),2)+'m/s'
  xyouts,.84,.83,scstring,color=tcol,charsize=char,/normal	;from tcol

  if (ncrd eq 0) then begin
      if KEYWORD_SET(mt) then plot_mlt_meridian,mt,yr,mo,dy,shr, $
        smin,ssec,ymin,color=mtcol
    for ii=0,358 do begin
      for jj=0,3 do begin
	if lshlat(jj,ii) gt ymin then if lshlat(jj,ii) lt ylim then $
	  if lshlon(jj,ii) gt xmin then if lshlon(jj,ii) lt xlim then begin
	    plots,[lshlon(jj,ii),lshlon(jj,ii+1)]$
			        ,[lshlat(jj,ii),lshlat(jj,ii+1)],color=tcol
	  endif	
      endfor
    endfor
    !p.font=-1
    lambda= '!7K=80!9%'
    xyouts,-59,71,lambda,color=tcol,/data
    !p.font=0
  endif
	
  parameter='vel'

  if (iopt_d ne 2 and iopt1 lt 2) then $				
          MK_CONT,BTL,TPL,NLV,CIN,XTITLE=parameter,CHARSIZE=char,ncolor=10

  xyouts,0.05,0.025,systime(0),charsize=char/2.,/normal,color=tcol ;time info

  xtit  = ' '
    for i = 0,n_elements(file)-1 do begin
    xtit = xtit + ' ' + strcompress(title_r(i))
  endfor
  if (n_elements(file) eq 1) then begin
    xyouts,.87,0.95,xtit,charsize=char,/normal,color=tcol,alignment=0.5
  endif else begin
    xyouts,.99,0.95,xtit,charsize=char,/normal,color=tcol,alignment=1.0
  endelse

  thetitle = 'SUPERDARN VELOCITY MAP'
  xyouts,0.01,0.95,thetitle,charsize=char,/normal,color=tcol
 
  date     =  strcompress(string(fix(mo))+'/'+string(fix(dy))+'/'+ $
                                                   string(fix(yr)),/remove_all)
  if ssec ge 10. then begin
    sstring = string(fix(ssec))
  endif else begin
    sstring = '0'+string(fix(ssec))
  endelse
  if smin ge 10. then begin
    mstring = string(fix(smin))
  endif else begin
    mstring = '0'+string(fix(smin))
  endelse
  sttim = strcompress(string(fix(shr))+':'+mstring $
		                                      +':'+sstring,/remove_all)
  if esec ge 10. then begin
    sstring = string(fix(esec))
  endif else begin
    sstring = '0'+string(fix(esec))
  endelse
  if emin ge 10. then begin
    mstring = string(fix(emin))
    endif else begin
    mstring = '0'+string(fix(emin))
  endelse
  endtim = strcompress(string(fix(ehr))+':'+mstring +':'+sstring,/remove_all)
  times = sttim + ' - ' + endtim + ' UT'
  xyouts,0.08,0.90,date,charsize=char, /normal,color=tcol
  xyouts,0.01,0.85,times,charsize=char,/normal,color=tcol


  ;<<<<<<<<<<<<
  write_merge,uk,mrg_str(jmax),nlos,vmag,vazm_x           ;write to .mik file
  ;>>>>>>>>>>>>

  if (iopt_d eq 3) then begin
    frame(0,0)  =  tvrd()
    write_pict,fname,frame
  endif

  if !d.name eq 'X' then wait,sc_hld

  ;<<<<<<<<<<<
  for j = 0,jmax do begin				;read in next data recs
    status     =  read_merge(u_mrg(j),u_los(j),m_str)
    mrg_str(j) =  m_str
  endfor
  ;>>>>>>>>>>>

endwhile


for j=0,jmax do begin 
    free_lun,u_mrg(j)
    free_lun,u_los(j)
    free_lun,u_mik(j)                               	;JMR
endfor

if (!d.name eq 'PS') then begin
   device,/close
;   if (iopt_d eq 1) then begin
;     spawn,'lp -d color_s1g idl.ps'
;     cbar = 1
;  endif
;   if (iopt_d eq 2) then spawn,'lp -dplot_825   idl.ps'
  set_plot,'X'
endif


if (iopt_d eq 0) then begin
  np      =  wmenu(['Exit','Re-do','PSC file (col)','PS file (bw)'],init=0)
  iopt_d  =  np - 1
  if iopt_d eq 0 then goto,imp
  if iopt_d eq 1 then begin
    psport
    device,/color
    goto, imp
  endif
  if iopt_d eq 2 then begin
    psport
    goto, imp
  endif
endif 

exit:
end
















