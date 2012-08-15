;     Program "MERGE" IDL version
;   This program can perform several tasks
;	1 - read fit files and create a mrg file (vector velocity maps)
;	2 - plot a mrg file
;	3- perform both of the two previous actions 
;    mrg files are created with the possible options of the merge program
;    plots are made with the options of the pltmrg program
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
PRO MAIN63_Event, Event
common cplots6, go_to, answer
common cplots2, main63 
    answer=''
 CASE Event.Value OF  
 
  'next processing.stop': BEGIN 
    PRINT, 'Event for next processing.stop' 
    widget_control, main63, /DESTROY 
    go_to= 'endloop' 
    END 
  'next processing.next plot': BEGIN 
    PRINT, 'Event for next processing.next plot' 
    go_to='loop'
    widget_control, main63, /DESTROY 
    END 
  'next processing.postscript_fr1b': BEGIN 
    PRINT, 'Event for next processing.postscript' 
    widget_control, main63, /DESTROY 
    answer = 'PS_fr1b'
    init_ps_vec
    go_to='start'
    END 
  'next processing.postscript_tour': BEGIN 
    PRINT, 'Event for next processing.postscript' 
    widget_control, main63, /DESTROY 
    answer = 'PS_tour'
    init_ps_vec
    go_to='start'
    END 
  'next processing.color postscript': BEGIN 
    PRINT, 'Event for next processing.color postscript' 
    widget_control, main63, /DESTROY 
    answer='PSC'
    init_psc
    go_to='start'
    END 
  'next processing.encapsulated ps': BEGIN 
    PRINT, 'Event for next processing.encapsulated ps' 
    widget_control, main63, /DESTROY 
    answer='EPS'
    init_epsf
    go_to='start'
    END 
  'next processing.encapsulated color ps': BEGIN 
    PRINT, 'Event for next processing.encapsulated color ps' 
    widget_control, main63, /DESTROY 
    answer='EPSC'
    init_epsf, /color
    go_to='start'
    END 
  ENDCASE
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END MAIN63_Event 
 
PRO MAIN13SUI_Event, Event 
 
common cplots6, go_to, answer
common cplots2, main63 
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev 
 
  CASE Ev OF  
 
  'PDMENU3': MAIN63_Event, Event 
  ENDCASE 
END 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END MAIN13SUI_Event 
PRO PLOT_M,  GROUP=Group 
common cplots2, main63 
common cplots6, go_to, answer
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0 
  junk   = { CW_PDMENU_S, flags:0, name:'' } 
  MAIN63 = WIDGET_BASE(GROUP_LEADER=Group, $ 
      ROW=1, TITLE='next ?', $ 
      MAP=1, UVALUE='MAIN63') 
 
  BASE2 = WIDGET_BASE(MAIN63, $ 
      ROW=1, MAP=1,  $ 
      UVALUE='BASE2') 
 
  MenuDesc308 = [ $ 
      { CW_PDMENU_S,       3, 'next processing' }, $ ;        0 
        { CW_PDMENU_S,       0, 'stop' }, $ ;        1 
        { CW_PDMENU_S,       0, 'next plot' }, $ ;        1 
        { CW_PDMENU_S,       0, 'postscript_fr1b' }, $ ;        2 
        { CW_PDMENU_S,       0, 'postscript_tour' }, $ ;        2 
        { CW_PDMENU_S,       0, 'color postscript' }, $ ;        3 
        { CW_PDMENU_S,       0, 'encapsulated ps' }, $ ;        4 
        { CW_PDMENU_S,       2, 'encapsulated color ps' }] ;      5 
 
  PDMENU3 = CW_PDMENU( BASE2, MenuDesc308, /RETURN_FULL_NAME, $ 
      UVALUE='PDMENU3') 
 
  WIDGET_CONTROL, MAIN63, /REALIZE 
 
  XMANAGER, 'PLOT_M', MAIN63, EVENT_HANDLER='MAIN13SUI_Event',/MODAL 
END 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END PLOT_M            
                            
  PRO LAT_LON_Event, Event                                  
  common cplot1, main116 , text185, text186, text187, text188
  common cplot2, ymin, ylim, xmin, xlim   
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
                                    
  CASE Ev OF                                     
  'TEXT185': BEGIN                                    
      widget_control,text185,get_value = symin              
      Print, 'Event for lat_min  ',symin, ' degrees '           
      ymin = float (symin(0))
      END                                    
  'TEXT186': BEGIN                                    
      widget_control,text186,get_value = symax              
      Print, 'Event for lat_max  ',symax, ' degrees '           
      ylim = float(symax(0))
      END                                    
  'TEXT187': BEGIN                                    
      widget_control,text187,get_value = sxmin              
      Print, 'Event for lon_min  ',sxmin, ' degrees '           
      xmin = float(sxmin(0))
      END                                    
  'TEXT188': BEGIN                                    
      widget_control,text188,get_value = sxmax              
      Print, 'Event for lon_max  ',sxmax, ' degrees '           
      xlim = float(sxmax(0))
      END                                    
  'ok': BEGIN                                    
      CASE Event.Value OF                                    
      0: BEGIN                           
          Print,'Button     O K    Pressed'                           
          widget_control, main116, /DESTROY                           
	END                                    
      1: BEGIN                           
         Print,'Button      Help      Pressed'
         END                           
      ELSE: Message,'Unknown button pressed' 
      ENDCASE                                    
      END                                    
  ENDCASE
END                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END LAT_LON_Event                                    
                                    
PRO LAT_LON, GROUP
                                    
  common cplot1, main116, text185, text186, text187, text188
  common cplot2, ymin, ylim, xmin, xlim                         
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0
;  junk   = { CW_PDMENU_S, flags:0, name:''} 
                                    
  MAIN116 = WIDGET_BASE( $ GROUP_LEADER=Group, $
      MAP=1, UVALUE='MAIN116',        $                                    
      TITLE='Choice of limits for the velocity plot')
                                    
  BASE2 = WIDGET_BASE(MAIN116, $                                    
      COLUMN=1, FRAME=10, MAP=1,  $                                    
      UVALUE='BASE2')                                    
                                    
  BASE74 = WIDGET_BASE(BASE2, ROW=1, MAP=1,       $                                    
      TITLE='base_lat_lon', $                                    
      UVALUE='BASE74')                                    
      
	l84val = 'latmin = '+string(ymin)+'  latmax = '+string(ylim)+  $
	'  lonmin = '+string(xmin)+'  lonmax = '+string(xlim)                              
  LABEL84 = WIDGET_LABEL( BASE74, $                                    
      UVALUE='LABEL84', $                                    
      VALUE=l84val)    
  BASE75 = WIDGET_BASE(BASE2,ROW=1,MAP=1,TITLE='val',UVALUE='BASE75')                                
  TEXT185 = WIDGET_TEXT( BASE75,VALUE=string(ymin),EDITABLE=1,  $ 
      FRAME=1, UVALUE='TEXT185', YSIZE=1)  
                                    
  TEXT186 = WIDGET_TEXT( BASE75,VALUE=string(ylim),EDITABLE=1,  $ 
      FRAME=1, UVALUE='TEXT186', YSIZE=1)  
                                    
  TEXT187 = WIDGET_TEXT( BASE75,VALUE=string(xmin),EDITABLE=1,  $ 
      FRAME=1, UVALUE='TEXT187', YSIZE=1)  
                                    
  TEXT188 = WIDGET_TEXT( BASE75,VALUE=string(xlim),EDITABLE=1,  $ 
      FRAME=1, UVALUE='TEXT188', YSIZE=1)  
                                    
  Btns4353 = [ '      OK      ','      HELP      ']       
  BGROUP126 = CW_BGROUP( BASE2, Btns4353, $                        
      ROW=1, $
      LABEL_LEFT='                                  ', $  
      UVALUE='ok')                                    
  WIDGET_CONTROL, MAIN116, /REALIZE                                    
  XMANAGER, 'LAT_LON', MAIN116, /JUST_REG, /MODAL     
END                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END LAT-LON                           
  PRO MAIN16_Event, Event                                  
  common coptions4, main16 , text85                       
  common coptions5, v_scale, coordinates, cntrs                         
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev                                    
                                    
  CASE Ev OF                                     
                                    
  'TEXT85': BEGIN                                    
      widget_control,text85,get_value = viscale               
      v_scale = float(viscale(0))
      Print, 'Event for text_vscale  ',v_scale, 'm/s  '           
      END                                    
  'coordinates': BEGIN                                    
      CASE Event.Value OF                                    
      0: BEGIN              
         coordinates = 'geogr' 
         cntrs = 1                
         Print,'Button 1  Pressed  *  coordinates = ',coordinates ,cntrs             
         END                                             
      1: BEGIN
         coordinates = 'geogr'
         cntrs = 0 
         Print,'Button 2 pressed  *Coordinates : ',coordinates, cntrs
         END
      2: BEGIN              
         coordinates = 'geom'                 
         cntrs =0 
         Print,'Button 3 Pressed  *  coordinates = ', coordinates, cntrs              
         END                                             
         ELSE: Message,'Unknown button pressed'                                    
      ENDCASE                                    
      END                                    
  'ok': BEGIN                                    
      CASE Event.Value OF                                    
      0: BEGIN                           
          Print,'Button     O K    Pressed'                           
          widget_control, main16, /DESTROY                           
          END                                    
      1: BEGIN                           
         Print,'Button      Help      Pressed'                                    
         END                           
      ELSE: Message,'Unknown button pressed'                                    
      ENDCASE                                    
      END                                    
  ENDCASE                                    
END                                    
                                    
                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END MAIN16_Event                                    
                                    
PRO GRAPHSET, GROUP=Group                                    
                                    
  common coptions4, main16, text85                        
  common coptions5, v_scale, coordinates, cntrs                         
                                    
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0                                    
         v_scale = 1000.
         coordinates = 'geom'                    
         cntrs = 0                               
  junk   = { CW_PDMENU_S, flags:0, name:'' } 
                                    
  MAIN16 = WIDGET_BASE(GROUP_LEADER=Group, MAP=1, UVALUE='MAIN16',  $
      TITLE='Choice of Options for the velocity plot')                                    
  BASE2 = WIDGET_BASE(MAIN16, COLUMN=1, FRAME=10, MAP=1,  $  
      UVALUE='BASE2')                                    
                                    
  BASE74 = WIDGET_BASE(BASE2, ROW=1, MAP=1, $ 
      TITLE='base_avrtime', $                                    
      UVALUE='BASE74')                                    
                                    
  LABEL84 = WIDGET_LABEL( BASE74, $                                    
      UVALUE='LABEL84', $                                    
      VALUE='Velocity Scale  : 1cm =   (Default = 1000 m/s)                       ')                                    
  TEXT85 = WIDGET_TEXT( BASE74,VALUE='1000.', EDITABLE=1, FRAME=1,  $
      YSIZE=1, UVALUE='TEXT85')                                    
                                    
  Btns4543 = [ 'Geographic with continents','Geographic without continents','Geomagnetic']                                    
  BGROUP130 = CW_BGROUP( BASE2, Btns4543, ROW=1, $  
      LABEL_LEFT='Type of Coordinates  (Default = Geomagnetic)   ', $                                    
      UVALUE='coordinates')                                    
                                    
  Btns4353 = [ '      OK      ','      HELP      '] 
  BGROUP126 = CW_BGROUP( BASE2, Btns4353, ROW=1,  $ 
      LABEL_LEFT='                                  ',  $
      UVALUE='ok')                                    
  WIDGET_CONTROL, MAIN16, /REALIZE                                    
  XMANAGER, 'MAIN16', MAIN16                                    
END                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END GRAPHSET                           
PRO BADTIME_Event, Eventbadtime                            
                            
  common cbadtime, main81, button14                            
 common cfile1, main13,text50,text56, main33        
  WIDGET_CONTROL,Eventbadtime.Id,GET_UVALUE=Ev                   
                            
  CASE Ev OF                             
                            
  'BUTTON14': BEGIN                            
      Print, 'Event for OK'                            
      widget_control,MAIN81 , /DESTROY                      
          widget_control, main33, SENSITIVE=1
      END                            
  ENDCASE                            
END                            
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  END badtime_event                            
                            
PRO BADTIME, param,  GROUP=Group                          
                            
  common cbadtime, main81,button14                            
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0                            
                            
  junk   = { CW_PDMENU_S, flags:0, name:'' }                            
                            
  if param eq 't_start' then begin
     main1_title = 'Start time'
     lab3_value = 'The length of start time should be 17.  Please, enter a new value' 
     endif
if param eq 't_end' then begin
         main1_title = 'End time'          
         lab3_value = 'The length of end time should be 17.  Please, enter a new value'
         endif
     if param eq 'late' then begin
         main1_title = ' Start and End Times '
         lab3_value = 'End time should be later than Start time  *  Enter again both Start time and End time '
         endif
MAIN81 = WIDGET_BASE( GROUP_LEADER=Group,  $              
      MAP=1,TITLE = main1_title, $                            
      UVALUE='sorry')                            
                            
  BASE2 = WIDGET_BASE(MAIN81, ROW=1, MAP=1,    $                            
      UVALUE='BASE2')                            
                            
  LABEL3 = WIDGET_LABEL( BASE2, $                            
      UVALUE='LABEL3', $                            
      VALUE=lab3_value, YSIZE = 30)                            
                            
  BUTTON14 = WIDGET_BUTTON( BASE2, $                            
      UVALUE='BUTTON14', $                            
      VALUE='OK')                            
                            
  WIDGET_CONTROL, MAIN81, /REALIZE
  WIDGET_CONTROL, BUTTON14, /INPUT_FOCUS
  XMANAGER, 'badtime', MAIN81                            
END                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END BADTIME           
PRO SORRY, param                            
                            
  if param eq 'parameter' then begin
     main1_title = 'Choice of Parameter'
     lab_val = 'SORRY ! Option not yet ready. Please, choose another option'
     endif
  if param eq 'pairs' then begin
     main1_title='Radar Pairs' 
      lab_val=strarr(11)
      lab_val(1)='Nominal pairs of radars are :'
      lab_val(3)='   WEST            EAST            letters '
      lab_val(4)='  Radar 1         Radar 2                  '
      lab_val(6)='Saskatoon   -   Kapuskasing      ( t _ k )'
      lab_val(7)= 'Goose Bay   -   Stokkseyri       ( g _ w )'
      lab_val(8)= 'Thykkvibaer -   Hankasalmi       ( e _ f )'
   endif
 
 label31 = WIDGET_MESSAGE(lab_val, TITLE=main1_title)
END                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END SORRY          

pro sel_file,title_sel, r_selection, file_path, search_path, filtr  
  
r_selection=pickfile(PATH=search_path, /READ,FILTER=filtr, $  
             GET_PATH=file_path, TITLE=title_sel, /FIX_FILTER)  
 
END  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END SEL_FILE 
                                                                
PRO MAIN13_Event, Event                                  
 common cfile1, main13,text50,text56, main33                         
 common cfile2,file_path1, file_path2,file1, file2,t_start,t_end, file_vec                                  
common cfile4, main23, file_mrg, mrg
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev    
                                  
  CASE Ev OF 
                                     
  'BUTTON145': BEGIN  
      Print, 'Event for Select radar 1 (West) file'  
      title1='Select a file for radar 1 (West)'  
      filtr = '*.fit'
      search_path = getenv('SD_FITROPEN_PATH')
      sel_file, title1, file1, file_path1, search_path, filtr 
      print, file1, file_path1        
      if (strlen(file1) eq 0)   then begin
 	lab=widget_message('No File Selected', TITLE='Radar 1 (West)')
	mrg='y'
        ENDIF  else mrg='n'
      END  
 
  'BUTTON146': BEGIN  
      Print, 'Event for Select radar 2 (East) file'  
      title2='Select a file for radar2 (East)'  
      filtr = '*.fit'
      search_path = getenv('SD_FITROPEN_PATH')  
      sel_file, title2, file2, file_path2, search_path , filtr 
      file_vec = 'file_vecteur'
      print, file2, file_path2        
      if (strlen(file2) eq 0)   then begin
 	lab=widget_message('No File Selected', TITLE='Radar 2 (East)')
	mrg='y' 
        ENDIF ELSE mrg='n'
    END  

   'BUTTON61': BEGIN
       sorry, 'pairs'
	END
  'BUTTON246': BEGIN
 	  widget_control, main13, SENSITIVE=0
	time_sel
   END
'BUTTON59': BEGIN                                  
      Print, 'Event for but_close'                                  
        if (strlen(file1) eq 0)  or (strlen(file2) eq 0) then begin
 	  widget_control, main13, SENSITIVE=0
	  lab=widget_message('You have to select TWO Files to merge', $
	         TITLE='Selection of fit files')
	  widget_control, main13, SENSITIVE=1
	  mrg='y' 
        ENDIF ELSE begin
	  mrg='n'
	  widget_control, main13, /DESTROY 
          lab=widget_message('Now, select plot options', title='next')                          
          ENDELSE
       END
   'BUTTON159': widget_control, main13, /DESTROY
   'BUTTON60': BEGIN
       sorry, 'pairs'
  END                                  
  ENDCASE                                  
END                                  
                                  
                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END MAIN13_EVENT                                  
                                 
PRO FILESEL2, GROUP=Group                                  
                                  
 common cfile1, main13,text50,text56, main33        
 common cfile2, file_path1, file_path2, file1, file2, t_start, t_end, file_vec                                    
      file_path1 = 'unknown'
      file_path2 = 'unknown'
      file1=''
      file2=''
      t_start = ''
      t_end=''
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0                                  
                                  
  junk   = { CW_PDMENU_S, flags:0, name:'' }                  
                                  
  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, ROW=1, MAP=1, $ 
      UVALUE='MAIN13',title='MERGE : File and time selection')                                  
                                  
  BASE2 = WIDGET_BASE(MAIN13, ROW=1, MAP=1, $               
      TITLE='base _file_time', $                                  
      UVALUE='BASE2')                                  
                                  
  BASE24 = WIDGET_BASE(BASE2, COLUMN=1, MAP=1, $     
      TITLE='base1_file', $                                  
      UVALUE='BASE24')                                  
                                  
  LABEL30 = WIDGET_LABEL( BASE24, UVALUE='LABEL30',  $        
      VALUE=' *  RADAR 1  *  WEST *')                                  
   
  BUTTON145 = WIDGET_BUTTON( BASE24, FRAME=10, UVALUE='BUTTON145',    $  
      VALUE='Select radar 1 (West) file')  
      
  BUTTON60 =WIDGET_BUTTON(BASE24,FRAME=10,UVALUE='BUTTON60',VALUE='HELP RADAR 1')

  BASE114 = WIDGET_BASE(BASE2, COLUMN=1, MAP=1,  $                                  
      TITLE='base2_file', $                                  
      UVALUE='BASE114')                                  
                                  
  LABEL109 = WIDGET_LABEL( BASE114, $                                  
      UVALUE='LABEL109', $                                  
      VALUE=' *  RADAR 2  *  EAST  *')                                  
  
  BUTTON146 = WIDGET_BUTTON( BASE114, FRAME=10, $  
      UVALUE='BUTTON146', $  
      VALUE='Select radar 2 (East) file')  
 
  BUTTON61 =WIDGET_BUTTON(BASE114,FRAME=10,UVALUE='BUTTON61',VALUE='HELP RADAR 2')          

  BASE214 = WIDGET_BASE(BASE2, COLUMN=1, MAP=1)                                  
                                  
  BUTTON246 = WIDGET_BUTTON( BASE214, FRAME=10, $  
      UVALUE='BUTTON246', $  
      VALUE='Select Start and End Times')  
 
  BASE44 = WIDGET_BASE(BASE2, COLUMN=1, MAP=1,   $                   
      TITLE='base_ok', $                                  
      UVALUE='BASE44')                                  
                                  
  BUTTON59 = WIDGET_BUTTON( BASE44, $                    
      FRAME=30, VALUE='  OK  ',UVALUE='BUTTON59')                

  BUTTON159 = WIDGET_BUTTON( BASE44, $                    
      FRAME=30, VALUE=' QUIT ',UVALUE='BUTTON159')                

  WIDGET_CONTROL, MAIN13, /REALIZE                    
  XMANAGER, 'MAIN13', MAIN13                                  
END                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END FILESEL2                                    
                                                                
PRO TIME_SEL_Event, Event                                  
 common cfile1, main13,text50,text56, main33                         
 common cfile2,file_path1, file_path2,file1, file2,t_start,t_end, file_vec                                  
common cfile4, main23, file_mrg, mrg
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev    
                                  
  CASE Ev OF 
                                     
  'TEXT50': BEGIN                                  
      widget_control, text50,get_value = t_start
      if strlen(t_start(0)) ne 17 then begin
          widget_control, main33, SENSITIVE=0
          badtime, 't_start'
          Print, 'Event for text_begin',t_start        
      endif
      END
  'TEXT56': BEGIN                                  
      widget_control, text56, get_value = t_end
      if strlen(t_end(0)) ne 17 then begin
          widget_control, main33, SENSITIVE=0 
          badtime, 't_end'
          Print, 'Event for text_end',t_end          
       endif 
       END                                  
  'BUTTON79': BEGIN                                  
      Print, 'Event for but_close'                                  
       if t_end(0) LE t_start(0) then begin
          widget_control, main33, SENSITIVE = 0
          badtime, 'late'
       ENDIF ELSE BEGIN 
	  widget_control, main33, /DESTROY 
	widget_control, main13, SENSITIVE=1
          ENDELSE
       END
   'BUTTON179': BEGIN
	widget_control, main33, /DESTROY
	widget_control, main13, SENSITIVE=1
       END
   'BUTTON60': BEGIN
       sorry, 'pairs'
  END                                  
  ENDCASE                                  
END                                  
                                  
                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END MAIN13_EVENT                                  
                                 
PRO TIME_SEL, GROUP=Group                                  
                                  
 common cfile1, main13,text50,text56, main33        
 common cfile2, file_path1, file_path2, file1, file2, t_start, t_end, file_vec                                    
;      t_start = '93 10 22 21 14 00'
;      t_end = '93 10 22 21 56 34'
t_start = ''
t_end=''
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0                                  
                                  
  junk   = { CW_PDMENU_S, flags:0, name:'' }                  
                                  
  MAIN33 = WIDGET_BASE(GROUP_LEADER=Group, ROW=1, MAP=1, $ 
      UVALUE='MAIN33',title='MERGE : Time Selection')                                  
                                  
  BASE2 = WIDGET_BASE(MAIN33, ROW=1, MAP=1, $               
      TITLE='base_time', $                                  
      UVALUE='BASE2')                                  
                                  
  BASE25 = WIDGET_BASE(BASE2, COLUMN=1, MAP=1,  $                                  
      TITLE='base_time', $                                  
      UVALUE='BASE25')                                  
                                  
  LABEL130 = WIDGET_LABEL( BASE25, UVALUE='LABEL130', YSIZE=35,  $ 
      VALUE='START AND END TIMES ')                                  
                                  
  LABEL131 = WIDGET_LABEL( BASE25, UVALUE='LABEL131', YSIZE=35,  $ 
      VALUE=' Enter start time first')                                  
                                  
  LABEL47 = WIDGET_LABEL( BASE25, UVALUE='LABEL47',   $   
      VALUE='Start Time (yy,mm,dd,hh,mm,ss) CR')                                  
  Label48=widget_label(BASE25,VALUE='separate by a comma or a blank character') 
  
	fc =rstrpos(file1,'/')
	deb = strmid(file1,fc+1,8)
  TextVal193 = [strmid(deb,0,2)+' '+strmid(deb,2,2)+' '+strmid(deb,4,2)+' '+strmid(deb,6,2)+' 00 00' ]                                  
  TEXT50 = WIDGET_TEXT( BASE25,VALUE=TextVal193, $                                  
      EDITABLE=1, FRAME=5, YSIZE=1,  $                                  
      UVALUE='TEXT50')                                  
                                  
  LABEL53 = WIDGET_LABEL( BASE25, $                                  
      UVALUE='LABEL53', $                                  
      VALUE='End Time (yy,mm,dd,hh,mm,ss) CR')                   
  
  hr=strcompress(string(2+fix(strmid(deb,6,2))), /remove_all)                                    
  TextVal193 = [strmid(deb,0,2)+' '+strmid(deb,2,2)+' '+strmid(deb,4,2)+' '+hr+' 00 00' ]                                  
  TEXT56 = WIDGET_TEXT( BASE25,VALUE=TextVal193, $    
      EDITABLE=1, FRAME=5, YSIZE=1,  $                                  
      UVALUE='TEXT56')                                  
                                  
  BASE44 = WIDGET_BASE(BASE2, COLUMN=1, MAP=1,   $                   
      TITLE='base_ok', $                                  
      UVALUE='BASE44')                                  
                                  
  BUTTON79 = WIDGET_BUTTON( BASE44, $                    
      FRAME=30, VALUE='  OK  ',UVALUE='BUTTON79')                

  BUTTON179 = WIDGET_BUTTON( BASE44, $                    
      FRAME=30, VALUE=' QUIT ',UVALUE='BUTTON179')                

  WIDGET_CONTROL, MAIN33, /REALIZE                    
  XMANAGER, 'TIME_SEL', MAIN33                                  
END                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END FILESEL2                                    
                                                                                                   
PRO MAIN23_Event, Event                                  
 common cfile1, main13,text50,text56, main33                         
 common cfile2,file_path1, file_path2,file1, file2,t_start,t_end, file_vec                                  
common cfile4, main23, file_mrg, mrg
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev                                  
                                  
  CASE Ev OF 
                                     
  'BUTTON155': BEGIN
      title3= 'Selection of a non-standard merged data file'
      filtr='*.mrg'
      search_path = getenv('SD_MRGOPEN_PATH') + '/nostnd/'
      sel_file, title3, file_mrg, mrg_path, search_path, filtr
      Print, 'mrg file selected :  ', file_mrg
      if strlen(file_mrg) eq 0  then begin
	mrg='n'
 	  lab=widget_message(' No MRG File Selected', $
	         TITLE='Selection of a MRG file')
        ENDIF ELSE begin
	  mrg='y'
	  widget_control, main23, /DESTROY                           
          lab=widget_message('Now, select "Type of Output.Screen Display"',title='Next')
          ENDELSE
	print, 'mrg selection = ', mrg
      END
 
  'BUTTON156': BEGIN
      title3= 'Selection of a Standard merged data file'
      filtr='*.mrg'
      search_path = getenv('SD_MRGOPEN_PATH') + '/stnd/'     
      sel_file, title3, file_mrg, mrg_path, search_path, filtr
      Print, 'mrg file selected :  ', file_mrg
      if strlen(file_mrg) eq 0 then begin
	mrg='n'
 	  lab=widget_message(' No MRG File Selected', $
	         TITLE='Selection of a MRG file')
        ENDIF ELSE begin
	  mrg='y'
	  widget_control, main23, /DESTROY                           
          lab=widget_message('Now, select "Type of Output.Screen Display"',title='Next')
          ENDELSE
      print, 'mrg selection = ' , mrg
    END
 
  ENDCASE                                  
END                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END MAIN23_EVENT                                  
                                 
PRO FILESEL1, GROUP=Group                                  
                                  
 common cfile1, main13,text50,text56, main33        
 common cfile2, file_path1, file_path2, file1, file2, t_start, t_end, file_vec                                    
  common cfile4, main23, file_mrg, mrg
IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0                                  
                                  
  junk   = { CW_PDMENU_S, flags:0, name:'' }              
                                  
  MAIN23 = WIDGET_BASE(GROUP_LEADER=Group, ROW=1, MAP=1,  $         
      UVALUE='MAIN23',title='MERGE : Mrg File selection')                                  
                                  
  BASE2 = WIDGET_BASE(MAIN23, ROW=1, MAP=1, $      
      TITLE='base _mrgfile', $                                  
      UVALUE='BASE2')                                  
                                  
  BASE24 = WIDGET_BASE(BASE2, COLUMN=1, MAP=1, $ 
      TITLE='base1_file', $                                  
      UVALUE='BASE24')                                  
                                  
  LABEL60 = WIDGET_LABEL( BASE24, UVALUE='LABEL60', YSIZE=35, $        
      VALUE=' * Plot already merged data *', FRAME=5)  
   
  BUTTON155 = WIDGET_BUTTON( BASE24, FRAME=10, UVALUE='BUTTON155',    $
      VALUE='Select non-standard .mrg file')
 
  BASE114 = WIDGET_BASE(BASE2, COLUMN=1, MAP=1,  $  
      TITLE='base2_file', $                                  
      UVALUE='BASE114')                                  
                                  
  LABEL66 = WIDGET_LABEL( BASE114, UVALUE='LABEL66', YSIZE=35,  $ 
      VALUE=' * Plot already merged data *', FRAME=5)  
   
  BUTTON156 = WIDGET_BUTTON( BASE114, FRAME=10, UVALUE='BUTTON156',    $
      VALUE='Select standard .mrg file')
                                  
  WIDGET_CONTROL, MAIN23, /REALIZE                    
  XMANAGER, 'MAIN23', MAIN23                                  
END                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END FILESEL1                                    
;          
PRO STANDARD                                    
                         
; initiates standard values for optional parameters            
                         
  common coptions5, v_scale, coordinates, cntrs                         
  common coptions2, stnd, av_time, lf1, lf2, hf1, hf2, v_height, pwr, min_vel, max_vel, $                         
                   max_err, g_scat, e_echoes, v_filter, div_free, one_scan, ialt, frng_min, ires, iext, inter
                           
  stnd = 1
  av_time = 196.                         
  lf1 = 8                         
  lf2 = 8                         
  hf1 = 20                         
  hf2 = 20                         
  v_height = 400.                                      
  pwr = 3.                         
  min_vel = 25.                         
  max_vel = 3000.                         
  max_err = 50.                                
  g_scat = 1                         
  frng_min = 900.
  inter = 0
  ires = 0 
  iext = 0 
  e_echoes = 1                         
  v_filter = 'y'                         
  div_free = 'n'                         
  one_scan = 'n'                     
  ialt = 1
END                                                           
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END STANDARD                                    
PRO MAIN1_Event, Event                                    
                                    
  common coptions1, main1                        
  common coptions2, stnd, av_time, lf1, lf2, hf1, hf2, v_height, pwr, min_vel, max_vel, $                         
                   max_err, g_scat, e_echoes, v_filter, div_free, one_scan, ialt, frng_min, ires, iext,inter                                    
  common coptions3, text85,btn92,btn93,btn94, text87,text103,text105,text108,text89,text116,text118, $               
                    text120,text122                        
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev                                    
    
       stnd = 0                                
  CASE Ev OF                                     
                                    
  'TEXT85': BEGIN                                    
      widget_control,text85,get_value = aver_time               
      av_time = FLOAT(aver_time)
      if av_time(0) eq 0 then lab = widget_message('Incorrect value * Enter a new value', title=' Averaging Time')
      one_scan = 'n'
      Print, 'Event for text_avtime  ', 'average time =  ',av_time           
      END                                    
  'btn92': BEGIN
     one_scan = 'y'
     print, 'Button  One map per scan pressed'
     av_time = 192.
     END
  'TEXT87': BEGIN                                    
      widget_control,text87,get_value = lfr1               
      Print, 'Event for text_lf1  ', lfr1                                    
      lf1 = FIX(lfr1)
      if lf1(0) eq 0 then lab = widget_message('Incorrect value of frequency * Enter a new value', title='Radar1 * Low frequency')
     END  
  'TEXT103': BEGIN                                    
      widget_control,text103,get_value = lfr2               
      Print, 'Event for text_lf2  ', lfr2                                    
      lf2 = FIX(lfr2)
      if lf2(0) eq 0 then lab = widget_message('Incorrect value of frequency * Enter a new value', title='Radar2 * Low frequency')
      END                                    
  'TEXT105': BEGIN                                    
      widget_control,text105,get_value = hfr1               
      Print, 'Event for text_hf1  ',hfr1                                    
      hf1 = FIX(hfr1)
      if hf1(0) eq 0 then lab = widget_message('Incorrect value of frequency * Enter a new value', title='Radar1 * High frequency')
      END                                    
  'TEXT108': BEGIN                                    
      widget_control,text108,get_value = hfr2               
      Print, 'Event for text_hf2  ' , hfr2                                   
      hf2 = FIX(hfr2)
      if hf2(0) eq 0 then lab = widget_message('Incorrect value of frequency * Enter a new value', title='Radar2 * High frequency')
      END                                    
  'TEXT89': BEGIN                                    
      widget_control,text89,get_value = vi_height               
      Print, 'Event for text_vheight  ' , vi_height  
      ialt = 1
      v_height = FLOAT(vi_height)
      if v_height(0) eq 0 then lab = widget_message('Incorrect value * Enter a new value', title=' Virtual Height')
      END                                    
  'btn93': BEGIN
     lab=widget_message('Option not yet ready * Constant virtual height (400km) assumed',  $
 title=' Model for the Virtual Height ')
     v_height = 400.
     END
  'btn94': BEGIN
     lab=widget_message('Option not yet ready * Constant virtual height (400km) assumed',  $
 title=' Virtual Height deduced from Elevation Angle ')
     END
  'TEXT116': BEGIN                                    
      widget_control,text116,get_value = pwrm               
      Print, 'Event for text_pwr  ', pwrm                                    
      pwr = FLOAT(pwrm)
      if  pwr(0) eq 0 then lab = widget_message('Incorrect value * Enter a new value', title=' Minimum Power')
      END                                    
  'TEXT118': BEGIN                                    
      widget_control,text118,get_value = mini_vel               
      Print, 'Event for text_minvel' ,mini_vel                     
      min_vel = FLOAT(mini_vel)
      if  min_vel(0) eq 0 then lab = widget_message('Incorrect value * Enter a new value', title=' Minimum Velocity to Process')
      END                                    
  'TEXT120': BEGIN                                    
      widget_control,text120,get_value = maxi_vel               
      Print, 'Event for text_maxvel  ',maxi_vel 
      max_vel = FLOAT(maxi_vel)
      if  max_vel(0) eq 0 then lab = widget_message('Incorrect value * Enter a new value', title=' Maximum Velocity to Process')
      END                                    
  'TEXT122': BEGIN                                    
      widget_control,text122,get_value = maxi_err               
      Print, 'Event for text_maxerr  ', maxi_err  
      max_err = FLOAT(maxi_err)
      if  max_err(0) eq 0 then lab = widget_message('Incorrect value * Enter a new value', title=' Maximum Error on Velocity')
      END                                    
  'gscat': BEGIN                                    
      CASE Event.Value OF                                    
      0: BEGIN              
         g_scat = 1                 
         Print,'Button 1  Pressed  *  g_scat = ',g_scat              
         END                                             
      1: BEGIN              
         g_scat = 2                 
         Print,'Button 2 Pressed  *  g_scat = ', g_scat              
         END                                             
      2: BEGIN                     
         Print,'Button INCLUDE/EXCLUDE Pressed'                     
         sorry, 'parameter'                      
         END                                         
      3: BEGIN                     
         Print,'Button 4 Pressed'                                    
         sorry, 'parameter'              
         END                     
         ELSE: Message,'Unknown button pressed'                                    
      ENDCASE                                    
      END                                    
  'eechoes': BEGIN                                    
      CASE Event.Value OF                     
      0: e_echoes = 1                             
      1: e_echoes = 2                         
      2: BEGIN                        
         Print,'Button 3 Pressed'                                    
         sorry, 'parameter'            
         END                        
      ELSE: Message,'Unknown button pressed'              
      ENDCASE                                    
      Print,'Button E region echoes  Pressed  *  e_echoes = ', e_echoes                  
      END                                    
  'vfilter': BEGIN                                    
      CASE Event.Value OF                                    
      0: v_filter = 'y'                               
      1: v_filter = 'n'                                    
      ELSE: Message,'Unknown button pressed'                                    
      ENDCASE                                    
      Print,'Button Velocity filter  ',v_filter,'  Pressed'                    
      END

  'interp' : BEGIN
      CASE Event.Value OF
      0: BEGIN
	inter = 1
	END
      1: BEGIN
	inter=0
      END
      ENDCASE
      Print, 'Button Interpolatio pressed. inter = ', inter
      END
                                    
  'div_free': BEGIN                                    
      CASE Event.Value OF                                    
      0: BEGIN
	div_free = 'y'                            
	iext =0
	ires = 0
      END
      1: BEGIN
	div_free = 'n'                                    
	iext = 0
	ires = 0
      END
      2: BEGIN
	div_free = 'y'
	iext = 1
	ires = 0
      END
      3: BEGIN
	div_free = 'y'
	ires = 1
      END
      ELSE: Message,'Unknown button pressed'                                    
      ENDCASE                                    
      Print,'Button Divergence Free -extension -Increased resolution  ',div_free, iext, ires              
      END                                    
  'ok': BEGIN                                    
      CASE Event.Value OF                                    
      0: BEGIN                           
          Print,'Button     O K    Pressed'                           
          widget_control, main1, /DESTROY                           
          END                                    
      1: BEGIN                           
         Print,'Button      Quit      Pressed'                           
         widget_control, main1, /DESTROY                           
         END                                           
      2: BEGIN                           
         Print,'Button      Help      Pressed'                                    
         END                           
      ELSE: Message,'Unknown button pressed'                                    
      ENDCASE                                    
      END                                    
  ENDCASE                                    
END                                    
                                    
                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END MAIN1_Event
                                    
PRO OPTIONS2, GROUP=Group                                    
                                    
  common coptions1, main1                                           
  common coptions3, text85,btn92,btn93,btn94, text87,text103,text105,text108,text89,text116,text118, $               
                    text120,text122                        
                                    
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0                                    
                             
                                        
  junk   = { CW_PDMENU_S, flags:0, name:'' }                                    
                                    
                                    
  MAIN1 = WIDGET_BASE(GROUP_LEADER=Group, $                                    
      MAP=1, $                                    
      UVALUE='MAIN1', $                         
      TITLE='Choice of Options for merging dual radar data')                                    
                                    
  BASE2 = WIDGET_BASE(MAIN1, $                                    
      COLUMN=1, $                                    
      FRAME=10, $                                    
      MAP=1, $                                    
      UVALUE='BASE2')                                    
                                    
  BASE74 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_avrtime', $                                    
      UVALUE='BASE74')                                    
                                    
  LABEL84 = WIDGET_LABEL( BASE74, $                                    
      UVALUE='LABEL84', $                                    
      VALUE='Averaging   time      (Standard = 192s)       ')                                                
  TextVal1167 =['', '192.']
  TEXT85 = WIDGET_TEXT( BASE74,VALUE=TextVal1167, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT85', $                                    
      YSIZE=1)                                    
      
  btn92 = WIDGET_BUTTON (BASE74, VALUE = 'One map / scan', UVALUE='btn92')
                              
  BASE75 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_lfreq1', $                                    
      UVALUE='BASE75')                                    
                                    
  LABEL86 = WIDGET_LABEL( BASE75, $                                    
      UVALUE='LABEL86', $                                    
      VALUE='Low frequency * Radar 1  (Standard = 8 Mhz)            ')                                    
  TextVal1330 = ['','8']                                    
  TEXT87 = WIDGET_TEXT( BASE75,VALUE=TextVal1330, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT87', $                                    
      YSIZE=1)                                    
                                    
  BASE76 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_lfreq2', $                                    
      UVALUE='BASE76')                                    
                                    
  LABEL102 = WIDGET_LABEL( BASE76, $                                    
      UVALUE='LABEL102', $                                    
      VALUE='Low frequency * Radar 2 (Standard = 8 MHz)      ')                                    
                                    
  TEXT103 = WIDGET_TEXT( BASE76,VALUE=TextVal1330, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT103', $                                    
      YSIZE=1)                                    
                                    
  BASE77 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_hfreq1', $                                    
      UVALUE='BASE77')                                    
                                    
  LABEL104 = WIDGET_LABEL( BASE77, $                                    
      UVALUE='LABEL104', $                                    
      VALUE='High frequency * Radar 1 (Standard = 20 MHz)    ')                                    
                                    
  TextVal2814 =['','20']                                    
  TEXT105 = WIDGET_TEXT( BASE77,VALUE=TextVal2814, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT105', $                                    
      YSIZE=1)                                    
                                    
  BASE78 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_hfreq2', $                                    
      UVALUE='BASE78')                                    
                                    
  BASE106 = WIDGET_BASE(BASE78, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      UVALUE='BASE106')                                    
           
                                    
  LABEL107 = WIDGET_LABEL( BASE78, $                                    
      UVALUE='LABEL107', $                                    
      VALUE='High frequency * Radar 2 (Standard = 20 MHz)      ')                                    
                                    
  TEXT108 = WIDGET_TEXT( BASE78,VALUE=TextVal2814, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT108', $                                    
      YSIZE=1)                                    
                                    
  BASE79 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_vheight', $                                    
      UVALUE='BASE79')                                    
                                    
  LABEL88 = WIDGET_LABEL( BASE79, $                                    
      UVALUE='LABEL88', $                                    
      VALUE='Virtual height  (Standard = 400 km)'   )                                    
                                    
  TextVal1493 =['','400.']                                  
  TEXT89 = WIDGET_TEXT( BASE79,VALUE=TextVal1493, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT89', $                                    
      YSIZE=1)                                    
                                    
  btn93 = WIDGET_BUTTON (BASE79, VALUE = ' Model ', UVALUE='btn93')
  btn94 = WIDGET_BUTTON (BASE79, VALUE = 'Elevation Angle', UVALUE='btn94')
  BASE80 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_pwr', $                                    
      UVALUE='BASE80')                                    
                                    
  LABEL115 = WIDGET_LABEL( BASE80, $                                    
      UVALUE='LABEL115', $                                    
      VALUE='Minimum power (Standard = 3 dB)             ')     
                                    
  TextVal3766 =['','3.']                                     
  TEXT116 = WIDGET_TEXT( BASE80,VALUE=TextVal3766, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT116', $                                    
      YSIZE=1)                                    
                                    
  BASE81 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_minvel', $                                    
      UVALUE='BASE81')                                    
                                    
  LABEL117 = WIDGET_LABEL( BASE81, $                                    
      UVALUE='LABEL117', $                                    
      VALUE='Minimum velocity (Standard = 25 m/s)                    ')                                    
                                    
  TextVal3929 =['','25.']                                    
  TEXT118 = WIDGET_TEXT( BASE81,VALUE=TextVal3929, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT118', $                                    
      YSIZE=1)                                    
                                    
  BASE82 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_maxvel', $                                    
      UVALUE='BASE82')                                    
                                    
  LABEL119 = WIDGET_LABEL( BASE82, $                                    
      UVALUE='LABEL119', $                                    
      VALUE='Maximum velocity (Standard = 3000 m/s)               ')                                    
                                    
  TextVal4092 =['','3000.']                                    
  TEXT120 = WIDGET_TEXT( BASE82,VALUE=TextVal4092, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT120', $                                    
      YSIZE=1)                                    
                                    
  BASE83 = WIDGET_BASE(BASE2, $                                    
      ROW=1, $                                    
      MAP=1, $                                    
      TITLE='base_maxerr', $                                    
      UVALUE='BASE83')                                    
                                    
  LABEL121 = WIDGET_LABEL( BASE83, $                                    
      UVALUE='LABEL121', $                                    
      VALUE='Maximum error on velocity (Standard = 50 m/s)      ')                                    
                                    
  TextVal4255 =['','50.']                                    
  TEXT122 = WIDGET_TEXT( BASE83,VALUE=TextVal4255, $                                    
      EDITABLE=1, $                                    
      FRAME=1, $                                    
      UVALUE='TEXT122', $                                    
      YSIZE=1)                                    
                                    
  Btns4543 = [ $                                    
    'Incl. GS', $                                    
    'Excl. GS', $                                    
    'Incl./Excl.', $                                    
    'Other' ]                                    
  BGROUP130 = CW_BGROUP( BASE2, Btns4543, $                                    
      ROW=1, $                                    
      LABEL_LEFT='Ground Scatter (Standard = Incl. GS)         ', $                                    
      UVALUE='gscat')                                    
                                    
  Btns4816 = [ $                                    
    'include all echoes', $                                    
    'incl. dist > 900km', $                                    
    '3-elevation angle' ]                                    
  BGROUP135 = CW_BGROUP( BASE2, Btns4816, $                                    
      ROW=1, $                                    
      LABEL_LEFT='E region Echoes (Standard = Include all echoes)    ', $                                    
      UVALUE='eechoes')                                    
                                    
  Btns4900 = [ $                                    
    'Yes ', $                                    
    'No' ]                                    
  BGROUP138 = CW_BGROUP( BASE2, Btns4900, $                                    
      ROW=1, $                                    
      LABEL_LEFT='Velocity Filter (Standard = Yes)             ', $                                    
      UVALUE='vfilter')                                    
                                    
  Btns4909 = [  ' YES ', '  NO  ']                                   
  BGROUP140 = CW_BGROUP( BASE2, Btns4909,ROW = 1, $                    
      LABEL_LEFT = 'Interpolation of radial velocities at isolated missing points (Standard = NO) ', $                                    
      UVALUE='interp')                                    
                                    

  Btns4917 = [  ' YES ', '  NO  ', 'DF Extension', 'DF Resolution']                                   
  BGROUP141 = CW_BGROUP( BASE2, Btns4917, $                                    
      ROW=1, $                                    
      LABEL_LEFT='Divergence Free (Standard = NO)        ', $                                    
      UVALUE='div_free')                                    
                                    
  Btns4353 = [ $                                    
    '          O K         ', $                                    
    '     Quit     ', $                                    
    '     Help     ' ]                                    
  BGROUP126 = CW_BGROUP( BASE2, Btns4353, $                                    
      ROW=1, $
      LABEL_LEFT='                                  ', $                                    
      UVALUE='ok')                                    
  WIDGET_CONTROL, MAIN1, /REALIZE                                    
  XMANAGER, 'MAIN1', MAIN1                                    
END                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END OPTIONS2                           
                             
PRO PDMENU10_Event, Event                                     
                                     
  common cfile4, main23, file_mrg, mrg
  common cmerge1,main103           
  common cmerge2,plot_type                                    
  common coptions5, v_scale, coordinates, cntrs                         
  common coptions6,std, file_v
  common coptions2, stnd, av_time, lf1, lf2, hf1, hf2, v_height, pwr, min_vel, max_vel, $
      max_err, g_scat, e_echoes, v_filter, div_free, one_scan, ialt, frng_min, ires, iext, inter            
  common cfile2, file_path1, file_path2, file1, file2, t_start, t_end, file_vec
  CASE Event.Value OF                                      
                                     
  'Files / Times.Fit Files / Times': BEGIN                                     
    PRINT, 'Event for Fitfiles / Times'                                     
    filesel2                                 
    END                                     
  'Files / Times.  Mrg Files  ': BEGIN                                     
    PRINT, 'Event for Mrg Files '                                     
    filesel1                                 
    END                                     
  'Plot Options.Standard': BEGIN                                     
    PRINT, 'Event for Plot Options.Standard'                         
    standard                                         
    END                                     
  'Plot Options.Non Standard': BEGIN                                     
    PRINT, 'Event for Plot Options.Non Standard'                         
    standard                                 
    options2                                         
    END                                     
  'Type of Output.mrg File Only': BEGIN                                     
    PRINT, 'Event for File Only'          
    plot_type = 'f'                                         
    END                                     
  'Type of Output.Screen Display (Default)': BEGIN 
    graphset                                    
    PRINT, 'Event for Screen Display'          
    plot_type = 's'                                         
    END                                     
  'OK ?.Continue': BEGIN                                     
    PRINT, 'Event for OK ?.Continue' 
        if mrg eq 'y' then goto, direct_plot 
        widget_control,  /HOURGLASS
	file1 = strcompress(file1, /remove_all)
	file2 = strcompress(file2, /remove_all)
	fc1 = rstrpos(file1,'/')
     	fc2 = rstrpos(file2,'/')
	deb = strmid(file1,fc1+1,6)
     	fl = strmid(file1,fc1+9,1)
	ll = strmid(file2,fc2+9,1)
	file_vec = deb +fl + ll + '.mrg' 
	file_path1= strmid(file1,0,fc1+1)
	file_path2 = strmid(file2,0,fc2+1)
	file1 = strmid(file1, fc1+1, 14)
	file2 = strmid(file2, fc2+1, 14)
	call_fortran
        if plot_type eq 's' then begin
          v_scale = FLOAT(v_scale)
          file_v = deb + fl + ll
          std=stnd
	  plotmerge
	  goto, fin
    direct_plot:
	v_scale = FLOAT(v_scale)
 	  fc1= rstrpos(file_mrg,'/')
	  file_v= strmid(file_mrg,fc1+1,8)
	  if strmid(file_mrg, fc1-5,5) eq '/stnd' then std = 1 else std=0 
	  plotmerge
     fin:
     ENDIF
    END                                     
  'OK ?.Quit': BEGIN                                     
    PRINT, 'Event for OK ?.Quit'                                 
    widget_control, main103, /DESTROY                                         
    END                                     
  ENDCASE                                     
END                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END PDMENU10_Event                                     
                                     
PRO MAIN103_Event, Event                                     
                                     
common cmerge1,main103                                     
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev                                     
                                     
  CASE Ev OF                                      
                                     
  ; Event for MERGE-VECTOR VELOCITY                                     
  'PDMENU10': PDMENU10_Event, Event                                     
  ENDCASE                                     
END                                     
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   END MAIN103_Event                                   
PRO phase1, GROUP=Group                                     
                                     
  common cmerge1,main103                                     
  common cmerge2, plot_type
  plot_type = 's'
    IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0                                     
                                     
  junk   = { CW_PDMENU_S, flags:0, name:'' }                                     
                                     
  MAIN103 = WIDGET_BASE(GROUP_LEADER=Group, $                                     
      COLUMN=1, $                                     
      MAP=1, $                                     
      UVALUE='MAIN13', TITLE= 'MERGE PROGRAM')                                     
                                     
  BASE2 = WIDGET_BASE(MAIN103, $                                     
      ROW=1, $                                     
      MAP=1, $                                     
      TITLE='MERGE', $                                     
      UVALUE='BASE2')                                     
                                     
  MenuDesc104 = [ $                                     
      { CW_PDMENU_S,       1, 'Files / Times' }, $ ;                     0 
        { CW_PDMENU_S,       0, 'Fit Files / Times' }, $ ;               1  
        { CW_PDMENU_S,       2, '  Mrg Files  ' }, $ ;                   2 
      { CW_PDMENU_S,       1, 'Plot Options' }, $ ;                      3           
        { CW_PDMENU_S,       0, 'Standard' }, $ ;                        4     
        { CW_PDMENU_S,       2, 'Non Standard' }, $ ;                    5       
      { CW_PDMENU_S,       1, 'Type of Output' }, $ ;                    6         
        { CW_PDMENU_S,       0, 'mrg File Only' }, $ ;                  7  
        { CW_PDMENU_S,       2, 'Screen Display (Default)' }, $ ;        8    
      { CW_PDMENU_S,       3, 'OK ?' }, $ ;                              9  
        { CW_PDMENU_S,       0, 'Continue' }, $ ;                       10  
        { CW_PDMENU_S,       2, 'Quit' }]   ;                           11
                                     
  PDMENU10 = CW_PDMENU( MAIN103, MenuDesc104, /RETURN_FULL_NAME, $        
      UVALUE='PDMENU10')                                     
  WIDGET_CONTROL, MAIN103, /REALIZE                    
  XMANAGER, 'MAIN103', MAIN103                                     
END            
;          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END PHASE1 
;
PRO call_fortran
  common cmerge2, plot_type
  common coptions2, stnd, av_time, lf1, lf2, hf1, hf2, v_height, pwr, min_vel, max_vel, $
    max_err, g_scat, e_echoes, v_filter, div_free, one_scan, ialt, frng_min, ires, iext, inter            
  common cfile2,file_path1,file_path2, file1, file2, t_start, t_end, file_vec        
s_yr = fix(strmid(t_start,0,2))
s_mo = fix(strmid(t_start,3,2))
s_day = fix(strmid(t_start,6,2))
s_hr = fix(strmid(t_start,9,2))
s_min = fix(strmid(t_start,12,2))
s_sec = fix(strmid(t_start,15,2))
e_yr = fix(strmid(t_end,0,2))
e_mo = fix(strmid(t_end,3,2))
e_day = fix(strmid(t_end,6,2))
e_hr = fix(strmid(t_end,9,2))
e_min = fix(strmid(t_end,12,2))
e_sec = fix(strmid(t_end,15,2))
get_lun, lun
OPENW, lun, 'options.dat'
printf,lun, FORMAT='( 11I7, 7F10.4, 12I2, 4A50, 1A12, 4A1 )',   $
      lf1, lf2, hf1, hf2, ialt, ires, iext, inter, stnd, fix(g_scat), fix(e_echoes),   $
      frng_min, av_time, min_vel, max_vel, max_err, v_height, pwr,      $
      s_yr, s_mo, s_day, s_hr, s_min, s_sec, e_yr, e_mo, e_day, e_hr, e_min, e_sec,   $
      file_path1, file_path2, file1, file2, file_vec, plot_type, v_filter, div_free, one_scan
free_lun, lun
print, plot_type,'   ',lf1,'  ',lf2,'  ',hf1,'  ',hf2,'  ',div_free,'  ',v_height,'  ',pwr,   $
               g_scat,'  ',v_filter,'  ',e_echoes,'  ',min_vel,'  ',max_vel,'  ',max_err
print, av_time
print, file1
print, file2
print, file_vec
print, file_path1
print, file_path2
print, t_start
print, t_end
print, 'standard options = ',stnd,'one map per scan = ',one_scan
SPAWN, getenv('SD_MERGE') + '/merge_idl.exe'
print, 'sortie merge fortran'
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END CALL_FORTRAN 

;=======================================================================

PRO READ_MERGE, mrg_unit, nvects,yr,mo,dy,shr,smin,ssec,ehr,emin,esec, $
	  		   niter, map_qlty, height_r, data


recl1    =  0L
recl2    =  0L
sdate    =  intarr(2,6)
edate    =  intarr(2,6)
mhz	 =  intarr(2)
khz	 =  intarr(2)
lagfr    =  intarr(2)
smsep    =  intarr(2)
map_qlty =  0L
ngood    =  0
niter    =  0L
height_r =  0.


  readu, mrg_unit, recl1,sdate,edate,mhz,khz,lagfr,smsep,map_qlty,ngood, $
		   niter,height_r,recl2

  nvects = ngood

  yr=     sdate(0,0)
  mo=     sdate(0,1)
  dy=     sdate(0,2)

  start_time = cnv_mdhms_sec(sdate)
  stop_time  = cnv_mdhms_sec(edate)

  time_min = min(start_time)
  time_max = max(stop_time)

  if (start_time(0) eq time_min) then begin
     shr  = sdate(0,3)
     smin = sdate(0,4)
     ssec = sdate(0,5)
  endif else begin
     shr  = sdate(1,3)
     smin = sdate(1,4)
     ssec = sdate(1,5)
  endelse

  if (stop_time(0) eq time_max) then begin
     ehr  = edate(0,3)
     emin = edate(0,4)
     esec = edate(0,5)
  endif else begin
     ehr  = edate(1,3)
     emin = edate(1,4)
     esec = edate(1,5)
  endelse

  if (ngood eq 0) then goto, jump
	
  ib      =  0L
  jb      =  0L
  ncnt    =  intarr(16,16)
  lat_g   =  fltarr(16,16)
  lon_g   =  fltarr(16,16)
  vx      =  fltarr(16,16)
  vy      =  fltarr(16,16)
  vz      =  fltarr(16,16)

  g1      =  0

  for n = 0,ngood-1 do begin

    readu, mrg_unit, recl1,ib,jb,a1,a2,b1,b2,c1,c2,c3,h1,h2,d1,d2,d3, $
                                              e1,e2,e3,f1,f2,f3,g1,recl2


    ncnt(ib,jb)  =   1
    lat_g(ib,jb) =  a1
    lon_g(ib,jb) =  a2
    vx(ib,jb)    =  d1
    vy(ib,jb)    =  d2
    vz(ib,jb)    =  d3

  endfor

  cnt    =  ncnt
  lat    =  lat_g
  lon    =  lon_g  


  data= FLTARR( 4, ngood)
  igood= 0

  for ib = 0,15 do begin
    for jb = 0,15 do begin

      if (cnt(ib,jb) gt 0) then begin

        vvx         =  vx(ib,jb)
        vvy         =  vy(ib,jb)
	vvz 	    =  vz(ib,jb)
        lon_f       =  lon(ib,jb)
        lat_f       =  lat(ib,jb)

	Vmag = sqrt(vvx^2+vvy^2+vvz^2)
	azim = atan(vvy,-vvx)*!radeg

        data( 0, igood)= lat_f
        data( 1, igood)= lon_f
        data( 2, igood)= Vmag
        data( 3, igood)= azim
        igood= igood + 1

      endif

    endfor
  endfor

;endwhile

jump:

return
END ; read_merge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END READ_MERGE
;=======================================================================
;
	PRO NEW_VEL_COL
;
; Load color bar vel
;
;****************************************************************************

	common colors, r_orig, g_orig, b_orig, RED,GREEN,BLUE
	ncol = (!d.n_colors < 256) -1 ;Colors we can use

	RED = INTARR(ncol)
	GREEN=INTARR(ncol)
	BLUE =INTARR(ncol)
	get_lun,colorfile
	OPENR,colorfile,getenv('SD_TABLES')+'/new_vel_col.dat'
	ar=intarr(4,11)
	READF,colorfile,ar
	free_lun,colorfile
	red(1) = 0
	blue(1) = 0
	green(1) = 0
	ind = 1
	if (!d.name eq 'PS') then ind = 2
	FOR I=0,10 DO BEGIN
	  RED(I+ind)=ar(1,i)
	  GREEN(I+ind)=ar(2,i)
	  BLUE(I+ind)=ar(3,i)
	ENDFOR
	red(0) = 0
	blue(0) = 0

	green(0) = 0
	red(ncol-1) = 255
	blue(ncol-1) = 255
	green(ncol-1) = 255
	red(ncol-2) = 0
	blue(ncol-2) = 75
	green(ncol-2) = 205

	TVLCT,RED,GREEN,BLUE
	END
;**************************************************************END NEW_VEL_COL*

	PRO MK_CONT,BOTTOMLEVEL,TOPLEVEL,NLEVELSIN,CINPUT,XTITLE=XTI, $
	CHARSIZE=CHAR,NCOLOR=NCOLO
;
; Given BOTTOMLEVEL, TOPLEVEL and NLEVELS and CIN, a vector with NLEVELS+1
; color indices, produces a color key for a contour plot.
;
	IF( KEYWORD_SET(NCOLO)) THEN NCOL = NCOLO ELSE $
		ncol = (!d.n_colors < 256) -1 ;Colors we can use

	NLEVELS =  NLEVELSIN

        CIN     =  CINPUT + (ncol-3)/(nlevels+1)	


		ncol = (!d.n_colors < 256) -1 ;Colors we can use
		if !d.name eq 'PS' then begin
			tcol = 1
		endif else tcol = ncol-1


	IF(KEYWORD_SET(XTI)) THEN XTL=XTI ELSE XTL=' '
	BOXES=REPLICATE(-1000.*(TOPLEVEL-BOTTOMLEVEL),4,NLEVELS+3)

	FOR I=1,2 DO BOXES(I,0:NLEVELS+1)=BOTTOMLEVEL + $
	(FINDGEN(NLEVELS+2)-1.)*(TOPLEVEL-BOTTOMLEVEL)/(NLEVELS-1)

	CLVLS=BOTTOMLEVEL+FINDGEN(NLEVELS)*(TOPLEVEL-BOTTOMLEVEL)/ $
	(NLEVELS-1)

	LVLS  = BOTTOMLEVEL + $
	            (FINDGEN(NLEVELS+3)-1)*(TOPLEVEL-BOTTOMLEVEL) / NLEVELS


	XTV   = BOTTOMLEVEL + $
	            (FINDGEN(NLEVELS+1))*(TOPLEVEL-BOTTOMLEVEL) / NLEVELS

	YAX=[0,1,2,3]



	CONTOUR,BOXES,yax,LVLS,XSTYLE=4,YSTYLE=4,POSITION=[.85,.15,.90,.85], $
	        /NOERASE,LEVELS=CLVLS,YTICKS=NLEVELS,YTICKV=XTV,XTICKLEN=.1, $
	        CHARSIZE=CHAR,color=tcol, /FILL,C_COLOR=CIN

;	POLYCONTOUR,'TEMP.DAT',COLOR_INDEX=CIN,/DELETE_FILE


	CONTOUR,BOXES,YAX,lvls,XSTYLE=4,YSTYLE=4,POSITION=[.85,.15,.90,.85], $
	        /NOERASE,LEVELS=CLVLS,YTICKS=NLEVELS,YTICKV=XTV,XTICKLEN=.1, $
	        XTITLE=XTL,CHARSIZE=CHAR,color=tcol


	decr = .7/(nlevels+2)

	pos0 = 0.14 + decr
	
	j0 = 0
	if ( xtl eq 'vel' ) then j0 = 1

	for j=j0,nlevels-1 do begin

		xpos = 0.89

		ypos = pos0 + j*decr
		value= strtrim(fix(xtv(j)),2)
		xyouts,xpos,ypos,value,/normal,color=tcol
	endfor

	if xtl eq 'pwr_l' then xtl = 'dB'
	if xtl eq 'width_l' then xtl = 'm/s'
	if xtl eq 'vel' then xtl = 'm/s'

		xpos = 0.86
		ypos = pos0 - .05
		value= strtrim(xtl,2)
		xyouts,xpos,ypos,value,/normal,color=tcol

	RETURN
	END
;*******************************************************END MK_CONT*
;
; Ce programme trace les vitesses MERGE carte par carte.
;
; Les vitesses sont d'abord tracees a l'ecran, et on peut produire des
; fichiers postscript (noir et blanc, ou couleur) ou postscript encapsule
; des cartes sur demande.
; Les fichiers postscript sortent un a un de l'imprimante.
; - Les noir et blanc sortent dans le batiment FR1 ou dans la tour, avec les indications de
;   "Processing Parameters"
; - Les fichiers couleur sortent dans le batiment informatique, sur l'imprimante
;   couleur
; - Les fichiers postscript encapsule sont gardes un a un dans des fichiers
;   idlxxx.epsf, ou xxx est le numero de la carte
;
; UTILISATION:
;	plotmerge
;*******************************************************************************

PRO plotmerge

common cplots6, go_to, answer
common cplots2, main13 
common pair_data, pair_info
common cplot3, pair, st1, st2 
  common cplot2, ymin, ylim, xmin, xlim                         
  common coptions6,std, file_v
  common cplot2, ymin, ylim, xmin, xlim                         
  common coptions5, scale, coordinates, cntrs                         
  common coptions6,std, file_v


;Fetch the .mrg file
;-------------------

f_path = getenv('SD_MRGOPEN_PATH')+'/nostnd/'
if std eq 1 then f_path = getenv('SD_MRGOPEN_PATH')+'/stnd/'

file=file_v
file_v = f_path + file + '.mrg'

lthik = 1.7
!P.FONT = 0
new_vel_col
get_pair_info


;READING THE GENERAL HEADER OF THE MERGE DATA FILE
;-------------------------------------------------
get_lun,u
openr, u, file_v

recl1    =  0L
recl2    =  0L
version  =  '01234567890123456789'
infi     =  intarr(27)
infr     =  fltarr(8)

readu, u, recl1,version,infi,infr,recl2

vers = 'MERGE: version ' + string(strmid(version,11,3))
print, vers

st1 = infi(0)
st2 = infi(1)

sdate = intarr(6)
edate = intarr(6)
sdate = infi(2:7)
edate = infi(8:13)
print, ' '
print, format='("The data in this file are from:   19",i2,5i4)',sdate
print, format='("                            to:   19",i2,5i4)',edate

t = intarr(6)
t(0) = sdate(0)
t(1) = sdate(1)
t(2) = sdate(2)
t(5) = 0
t = sdate
stm = cnv_mdhms_sec(t)

f_low1 	= infi(14)
f_low2 	= infi(15)
f_high1 = infi(16)
f_high2 = infi(17)
idur 	= infi(18)
ialt 	= infi(19)
igs 	= infi(20)
iereg	= infi(21)
ifltr	= infi(22)
inter	= infi(23)
idiv	= infi(24)
iext	= infi(25)
ires	= infi(26)
tperiod    = infr(0)
height     = infr(1)
frang_min  = infr(3)
pwr_min    = infr(4)
vel_max    = infr(5)
velerr_max = infr(6)
vel_min    = infr(7)


;DEFINING THE PAIR NUMBER AND THE CORRESPONDING RADAR NAMES
;----------------------------------------------------------
pair = 0
if (st1 eq  7) and (st2 eq  6) then pair = 1
if (st1 eq  5) and (st2 eq  3) then pair = 2
if (st1 eq  1) and (st2 eq  8) then pair = 3
if (st1 eq  9) and (st2 eq 10) then pair = 4
if (st1 eq  2) and (st2 eq  8) then pair = 5

if (pair eq 0) then begin		;Not a nominal pair
lab = widget_message(' This is not a Nominal Pair',  $
	title = 'Radar Pair Warning ! ')
	goto, endloop
endif



;SETTING THE PLOT LIMITS, GRID AND COLORS
;----------------------------------------
misc = fltarr(3)
if coordinates eq 'geom' then lims = pair_info(pair).maglim $
else lims = pair_info(pair).geolim & misc = pair_info(pair).misc

ymin = lims(0)
ylim = lims(1)
xmin = lims(2)
xlim = lims(3)
lambx = misc(0)
lamby = misc(1)
reflon = misc(2)
GROUP='group2'
lat_lon, GROUP
thetitle = 'SUPERDARN VELOCITY MAP'
radars = pair_info(pair).name

if coordinates eq 'geom' then lims = pair_info(pair).maglim $
else lims = pair_info(pair).geolim & misc = pair_info(pair).misc
misc = fltarr(3)
lambx = misc(0)
lamby = misc(1)
reflon = misc(2)

dlat = abs(ylim-ymin)/5.
dlon = abs(xlim-xmin)/5.

pollat = (ymin+ylim)/2.
pollon = (xmin+xlim)/2.

S = findgen(16)*(!PI*2/16.)
usersym,cos(S),sin(S),/FILL

NLV = 6
btl = 0
tpl = 6.0*scale/5.

CIN   =  (1+INDGEN(NLV+1));*((8)/(NLV+1))
LVL   =   BTL+(INDGEN(NLV))*(TPL-BTL)/(NLV)

Re    =  6362.
dist_ref = 120.
dt = dist_ref/scale

;GETTING THE L-SHELL CONTOURS
;----------------------------
if coordinates ne 'geom' then begin
  lshlat=fltarr(4,360)
  lshlon=fltarr(4,360)
  get_lun,u1
  openr,u1,getenv('SD_TABLES')+'/lsh_geog.dat'
  readf,u1,lshlon
  readf,u1,lshlat
  close,u1
  free_lun,u1
endif

;READING THE DATA
;----------------
count2 = 0
while( NOT EOF(u) ) do begin

	count2 = count2 + 1
	nvects = 0

	read_merge,u, nvects, yr, mo, dy, shr, smin, ssec, ehr, emin, esec, $
	    niter, map_qlty, height_r, data
	
	  print, 'the map contains ',nvects, ' vectors'
	if (nvects eq 0) then goto, loop

	start_time = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
	if (start_time lt stm) then goto, loop

	lats = fltarr(nvects)
	lons = fltarr(nvects)
	Vmag   = fltarr(nvects)
	azim   = fltarr(nvects)

	lats = data(0,0:nvects-1)
	lons = data(1,0:nvects-1)
	Vmag   = data(2,0:nvects-1)
	azim   = data(3,0:nvects-1)
	

; Calculating the positions (late,lone) of the the points, initially at
; (lats,lons), and moving with the velocity Vmag during the time dt
;----------------------------------------------------------------------	
	cside = (90. - lats)*!dtor
	bside = Vmag*dt/Re	
	angs  = azim*!dtor
	
	arg = cos(bside)*cos(cside) + sin(bside)*sin(cside)*cos(angs)

	q = where( arg lt -1, count )
 		if count ne 0 then arg(q) = -1.0
	q = where( arg gt  1, count )
		if count ne 0 then arg(q) = 1.0

	aside = acos(arg)
	late = 90. - aside*!radeg

	arg = (cos(bside)-cos(aside)*cos(cside))/(sin(aside)*sin(cside))

	q = where( arg lt -1,count )
 		if count ne 0 then arg(q) = -1.0
	q = where( arg gt  1 ,count )
		if count ne 0 then arg(q) = 1.0

	bang = acos(arg)*!radeg

	q = where (angs lt 0,count)
		if count ne 0 then bang(q) = -1.*bang(q)
	lone = lons + bang

	if coordinates eq 'geom' then begin
	  outlats = fltarr(nvects)
	  outlons = fltarr(nvects)
	  outlate = fltarr(nvects)
	  outlone = fltarr(nvects)
	  for i = 0,nvects-1 do begin
	     inlat = lats(i)
	     inlon = lons(i)
	     outpos = cnvcoord(inlat,inlon,height_r)
	     outlats(i) = outpos(0)
	     outlons(i) = outpos(1)
	     inlat = late(i)
	     inlon = lone(i)
	     outpos = cnvcoord(inlat,inlon,height_r)
	     outlate(i) = outpos(0)
	     outlone(i) = outpos(1)
	  endfor
	  lats = outlats
	  lons = outlons
	  late = outlate
	  lone = outlone
	endif


;SETTING THE MAP
;---------------
	print,'Plotting map :', count2

	answer = 'c'
	colbar = 'y'

	start:

	ncol = (!d.n_colors < 256) -1 ;Colors we can use
	if !d.name eq 'PS' then begin
		tcol = 1
	endif else tcol = 10 ;ncol-1

	if (answer eq 'PS_fr1b' ) or (answer eq 'PS_tour' ) then colbar = 'n'
	if (answer eq 'PSC')  then colbar = 'y'
	if (answer eq 'EPS')  then colbar = 'n'

	if (!d.name eq 'PS') and (!d.y_size gt 17790) then  $
	  map_set,pollat,pollon,limit=[ymin,xmin,ylim,xlim],/GRID,$
		glinethick=0.5,/stereo,color=tcol,ymargin=[13.36,2.],$
		latdel=dlat,londel=dlon,/label,latlab=xmin+.25*dlon,$
		lonlab=ymin+dlat,charsize=0.9                       $
	else $
	  map_set,pollat,pollon,limit=[ymin,xmin,ylim,xlim],/GRID,$
		glinethick=0.5,/stereo,color=tcol,$
		latdel=dlat,londel=dlon,/label,latlab=xmin+.25*dlon,$
		lonlab=ymin+dlat,title='VELOCITY MAP'

	if cntrs eq 1 then $
	map_continents,color=tcol

;Calculating the final position of the arrows whose lengths are proportional to
;Vmag, and which pass through the points (late,lone) calculated before
;------------------------------------------------------------------------------
	tlat = fltarr(nvects)
	tlon = fltarr(nvects)
	for i=0,nvects-1 do begin
	   nx = convert_coord(lons(i),lats(i),/data,/to_device)
	   x0 = nx(0)
	   y0 = nx(1)
	   nx = convert_coord(lone(i),late(i),/data,/to_device)
	   x1 = nx(0)
	   y1 = nx(1)

; If P0 (x0,y0) are the paper coordinates of the origin of the arrow,
;    P1 (x1,y1) are the paper coordinates of the point calculated above,
;    P2 (x2,y2) are the paper coordinates of the end of the arrow,
; we solve for (x2,y2) the two equations:
;	(1)	sqrt[(x2-x0)^2+(y2-y0)^2] = Vmag*0.08/scale
;	(2)	(x2-x0)/(x1-x0) = (y2-y0)/(y1-y0)
; where (1) expresses that the length of the arrow is proportional to the scaled velocity
; and (2) expresses that P0-P1 and P0-P2 are parallel

	   size = Vmag(i)*0.08*!d.x_size/scale
	   rap = (y1-y0)/(x1-x0)
	   sqdifx = size^2/(1+rap^2)
	   sqdify = rap^2*sqdifx
	   difx = sqrt(sqdifx)
	   dify = sqrt(sqdify)
	   if ((x1-x0) lt 0.) then difx = -1.*difx
	   if ((y1-y0) lt 0.) then dify = -1.*dify
	   x2 = difx + x0
	   y2 = dify + y0

	   nx = convert_coord(x2,y2,/device,/to_data)
	   tlon(i) = nx(0)
	   tlat(i) = nx(1)
	endfor

;PLOT THE VELOCITY ARROWS
;------------------------
	for i = 0,nvects-1 do begin
	  if (lons(i) gt xmin) and (lons(i) lt xlim) and (lats(i) gt ymin) $
	      	  	       and (lats(i) lt ylim) then begin
             ind = max(where(LVL le Vmag(i))) + 1
	     ttcol = cin(ind)	      
	      
; For a two-color plot, then comment out the following lines
	     if (tlon(i) ge lons(i)) then ttcol = cin(5) $
	     else ttcol = cin(1)
	     
	     if (answer eq 'ps') or (answer eq 'PS') or (answer eq 'eps') $
		or (answer eq 'EPS') then ttcol = tcol

 	     plots,lons(i),lats(i),psym=8,symsize=0.4,color=ttcol
	     plots,[lons(i),tlon(i)],[lats(i),tlat(i)], $
		   color=ttcol,thick=lthik
	
	  endif
	endfor

;PLOT THE SCALE (ARROW AND COLOR BAR)
;------------------------------------
	if (!d.name eq 'PS') and (!d.y_size gt 17790) then begin
	  y = 0.907 & y1 = 0.879
	endif else begin
	  y = 0.87 & y1 = 0.83
	endelse
	plots,.84,y,psym=8,symsize=0.4,color=tcol,/normal
	plots,[.84,.84+0.08],[y,y]$
		,color=tcol,thick=lthik,/normal
	scstring = strtrim(string(scale),2)+'m/s'
	xyouts,.84,y1,scstring,color=tcol,/normal


;PLOTS THE L-SHELL CONTOURS
;--------------------------
	if coordinates ne 'geom' then begin
	  for i=0,358 do begin
	    for j=0,3 do begin
	      if lshlat(j,i) gt ymin then if lshlat(j,i) lt ylim then $
	      if lshlon(j,i) gt xmin then if lshlon(j,i) lt xlim then begin
	         plots,[lshlon(j,i),lshlon(j,i+1)]$
			,[lshlat(j,i),lshlat(j,i+1)],color=tcol
	      endif	
	    endfor
	  endfor

	  !p.font=-1
	  lambda= '!7K=70!9%'
	  xyouts, lambx,lamby,lambda,color=tcol,/data
   	endif	

;WRITES COMMENTS ON MAPS
;-----------------------

	!p.font = 0	
	xyouts, 0.02, 0.965, thetitle, /normal, color=tcol
	xyouts, 0.98, 0.965, radars, /normal, alignment=1.0,color=tcol

	name_month = month_name(mo)
	date = strcompress(string(fix(dy)),/remove_all) + name_month + $
	       strcompress(string(fix(yr)),/remove_all)

	if (ssec ge 10.) then begin
	   sstring = string(fix(ssec))
	endif else begin
	   sstring = '0'+string(fix(ssec))
	endelse

	if (smin ge 10.) then begin
	   mstring = string(fix(smin))
	endif else begin
	   mstring = '0'+string(fix(smin))
	endelse


	sttim = strcompress(string(fix(shr))+':'+mstring $
		+':'+sstring,/remove_all)

	if (esec ge 10.) then begin
	   sstring = string(fix(esec))
	endif else begin
	   sstring = '0'+string(fix(esec))
	endelse

	if (emin ge 10.) then begin
	   mstring = string(fix(emin))
	endif else begin
	   mstring = '0'+string(fix(emin))
	endelse


	endtim = strcompress(string(fix(ehr))+':'+mstring $
		+':'+sstring,/remove_all)

	times=sttim+' - '+endtim+' UT'

	if (!d.name eq 'PS') and (!d.y_size gt 17790) then begin
	  y1 = 0.921 & y2 = 0.886
	endif else begin
	  y1 = 0.89 & y2 = 0.84
	endelse
	xyouts,0.09, y1,date,/normal,color=tcol
	xyouts,0.05, y2,times,/normal,color=tcol

	b1 = cnv_mdhms_sec(yr,mo,dy,shr,smin,ssec)
	b2 = cnv_mdhms_sec(yr,mo,dy,ehr,emin,esec)
	b = (b1 + b2) / 2

	if coordinates ne 'geom' then begin	  
           mlt = MLT(yr,b,reflon)
	   mlthr = fix(mlt)
	   mltmin= fix((mlt-mlthr)*60.)
	   if (mltmin ge 10.) then mstring = string(fix(mltmin)) $
	   else mstring = '0'+string(fix(mltmin))
	   !p.font=-1
	   smlt = strcompress('!9A!5'+string(mlthr)+':'$
	 	+mstring,/remove_all)+' MLT'
	   if (!d.name eq 'PS') and (!d.y_size gt 17790) then y = 0.85 $
	   else y = 0.79
	   xyouts,0.1, y,smlt,/normal,color=tcol
	endif else begin
	   yy = ymin + 1.
	   for i=1,3 do begin
	      xx = xmin + (2*i-1) * dlon
	      mlt = MLT(yr, b, xx)
	      mlthr = fix(mlt)
	      mltmin= fix((mlt-mlthr)*60.)
	      if (mltmin ge 10.) then mstring = string(fix(mltmin)) $
	      else mstring = '0'+string(fix(mltmin))
	      !p.font = -1
	      smlt = strcompress(string(mlthr)+':'+mstring,/remove_all)+' MLT'
	      xyouts, xx,yy,smlt,alignment=0.5,color=tcol
	   endfor
	endelse

	parameter='vel'
	char = 1.0
;	if (!d.name eq 'WIN') or (colbar eq 'y') then $
; For two-color plot, then uncomment the two following lines
;	if (colbar eq 'y') then $
;	    MK_CONT,BTL,TPL,NLV,CIN,XTITLE=parameter,CHARSIZE=CHAR,ncolor=10

; adds the French SuperDARN logo on the display
;----------------------------------------------
if !d.name ne 'PS' then begin 
	file_logo = getenv('SD_LOGO') + '/frnc_nb.bmp'
	image = read_bmp(file_logo)
	sd_logo=congrid(image,72,82)
	tvscl,sd_logo,0.02, 0.6, /normal
	file_logo = getenv('SD_LOGO') + '/cetp.bmp'
	image2= read_bmp(file_logo)
	tvscl,congrid(image2,64,64),0.85,0.6, /normal
	print, 'logo'
	endif


;WRITES PROCESSING INFORMATIONS
;------------------------------

if (!d.name eq 'PS') and (!d.y_size gt 17790) then begin

x = 0.02
if (!d.y_size gt 20000) then y = 0.20 $
else y = 0.25
pstring = vers
xyouts, 0.02,y,pstring,/normal,charsize=0.8,color=tcol
y = y - 0.01
pstring = '_________________'
xyouts, x,y,pstring,/normal,charsize=0.8,color=tcol

y = y - 0.025
y1 = y
pstring =  'Processing Parameters:'
xyouts, x,y,pstring,/normal,charsize=0.8,color=tcol

y = y - 0.015
if (idur eq 1) then begin
   pstring = '    Map for Each Scan'
endif
if (idur eq 2) then begin
   pstring = '    Averaged Map, integ. time = ' $
             + strcompress(string(fix(tperiod)),/remove_all) + ' s'
endif
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
if (ialt eq 1) then begin
   pstring = '    Constant Virtual Height of ' $
             + strcompress(string(fix(height)),/remove_all) + ' km'
endif
if (ialt eq 2) then begin
   pstring = '    Model for Virtual Height'
endif
if (ialt eq 3) then begin
   pstring = '    Virtual Height from elev. angle'
endif
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    ifltr  = '+ strcompress(string(fix(ifltr)),/remove_all)
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    inter = '+ strcompress(string(fix(inter)),/remove_all)
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    idiv  = '+ strcompress(string(fix(idiv)),/remove_all) $
	 + ',   niter = '+ strcompress(string(fix(niter)),/remove_all)
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    iext  = '+ strcompress(string(fix(iext)),/remove_all)
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    ires  = '+ strcompress(string(fix(ires)),/remove_all)
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.025
pstring = 'Map Quality Index = '+ strcompress(string(fix(map_qlty)), $
	  /remove_all)
xyouts, x,y,pstring,/normal,charsize=0.8,color=tcol

y = y1
x = 0.5
pstring =  'Data Selection Parameters:'
xyouts, x,y,pstring,/normal,charsize=0.8,color=tcol

y = y - 0.015
pstring = '    Frequency Range for Radar ' $
	  + strcompress(string(fix(st1)),/remove_all) + ': ' $
	  + strcompress(string(fix(f_low1 )),/remove_all) + '-' $
	  + strcompress(string(fix(f_high1)),/remove_all) + ' MHz'
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring = '    Frequency Range for Radar ' $
	  + strcompress(string(fix(st2)),/remove_all) + ': ' $
	  + strcompress(string(fix(f_low2 )),/remove_all) + '-' $
	  + strcompress(string(fix(f_high2)),/remove_all) + ' Mhz'
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    Minimum Power = ' $
	   + strcompress(string(fix(pwr_min)),/remove_all) + ' db'
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    Minimum Vlos = ' $
	   + strcompress(string(fix(vel_min)),/remove_all) + ' m/s'
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    Maximum Vlos = ' $
	   + strcompress(string(fix(vel_max)),/remove_all) + ' m/s'
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    Max Error on Vlos = ' $
	   + strcompress(string(fix(velerr_max)),/remove_all) + ' m/s'
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    igs   = ' + strcompress(string(fix(igs)),/remove_all)
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

y = y - 0.015
pstring =  '    iereg = ' + strcompress(string(fix(iereg)),/remove_all)
xyouts, x,y,pstring,/normal,charsize=0.7,color=tcol

; adds the French SuperDARN logo on the postscript file
;----------------------------------------------------------
; file_logo = getenv('SD_LOGO') + '/frnc_nb.bmp'
; image = read_bmp(file_logo)
 tvscl,image,0.02,0.31,xsize=0.08, ysize=0.093, /normal
; file_logo = getenv('SD_LOGO') + '/cetp.bmp'
; image2=read_bmp(file_logo)
 tvscl,image2,0.908,0.31,xsize=0.072,ysize=0.072, /normal
endif

  	EMPTY

;CONTINUE or PRINT THE MAP
;-------------------------
	if (!d.name ne 'PS') then begin
	   plot_m
	   if go_to eq 'loop' then goto, loop
	   if go_to eq 'start' then goto, start
 	   if go_to eq 'endloop' then goto, endloop
 	endif  else begin
	   if answer eq 'PS_fr1b'  then begin
		output_ps, 'fr1b'
		spawn, 'rm -f idl.ps'
		wait, 1.
	   endif 
	   if answer eq 'PS_tour'  then begin
		output_ps, 't'
		spawn, 'rm -f idl.ps'
		wait, 1.
	   endif 
	   if answer eq 'PSC' then begin
		output_psc
		spawn, 'rm -f idl.psc'
	   	wait, 1.
	   endif
	   if (answer eq 'EPSC') or (answer eq 'EPS') then begin
		spawn, "mv idl.epsf idl"+strcompress(string(fix(count2)),/remove_all)+".epsf"
	   endif
	   init_default
	endelse
loop:
endwhile
endloop: 
wdelete
init_default
close,u
free_lun,u
END ; plotmerge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END PLOTMERGE
;
;;;;;;;;;;;;;;;;;;;END MERGE.PRO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
