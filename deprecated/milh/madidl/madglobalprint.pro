; globalIsprint is a procedure to search through the entire Madrigal database
; for appropriate data to print in ascii to a file
;
;    Inputs:
;
;        madurl - url to homepage of site to be searched (Example: 
;              'http://www.haystack.mit.edu/madrigal/'
;
;        parms - a comma delimited string listing the desired Madrigal 
;            parameters in mnemonic form.  
;            (Example: 'year,month,day,hour,min,sec,gdalt,dte,te').  
;            Ascii space-separated data will be returned in the same  
;            order as given in this string. See 
;            http://madrigal.haystack.mit.edu/cgi-bin/madrigal/getMetadata
;            "Parameter code table" for all possible parameters.
;
;        output - the local file name to store the resulting ascii data.
;                 (Example: '/tmp/isprint.txt')
;
;        user_fullname - the full user name (Example: 'Bill Rideout')
;
;        user email -  Example: 'brideout@haystack.mit.edu'
;
;        user_affiliation - Example: 'MIT'
;
;        startDate - a time in IDL Julian Date format at which to begin the search
;
;        endDate - a time in IDL Julian Date format at which to end the search
;
;        inst - instrument code (integer).  See 
;            http://madrigal.haystack.mit.edu/cgi-bin/madrigal/getMetadata
;            "Instrument Table" for this list. Examples: 30 for Millstone
;            Hill Incoherent Scatter Radar, 80 for Sondrestrom Incoherent 
;            Scatter Radar
;
;    Optional inputs
;
;        filters - is the optional filters requested in exactly the form given in isprint
;         command line (example = 'filter=gdalt,,500 filter=ti,500,1000')
;         See:  http://www.haystack.mit.edu/madrigal/ug_commandLine.html for details
;
;        kindats - is an optional array of kindat (kinds of data) codes to accept.
;           The default is a null pointer, which will accept all kindats.
;  
;        expName - a case insensitive string as used by strmatch that matches the experiment
;           name.  Default is zero-length string, which matches all experiment names.
;
;        fileDesc - a case insensitive string as used by strmatch that matches the file description.
;           Default is zero-length string, which matches all file descriptions.
;
;    Returns: Nothing.
;
;    Affects: Writes results to output file
;    
;
;  Example: 
;   madglobalprint, 'http://www.haystack.mit.edu/madrigal/',  $
;                         'year,month,day,hour,min,sec,gdalt,dte,te', $
;                         '/tmp/isprint.txt', $
;                         'Bill Rideout',  'brideout@haystack.mit.edu', 'MIT', $
;                         julday(1,19,1998,0,0,0),  julday(1,21,1998,23,59,59), 30
;
;  $Id: madglobalprint.pro,v 1.2 2010/07/27 00:49:11 brideout Exp $
;
pro madglobalprint, madurl, parms, output, user_fullname, user_email, user_affiliation, startDate, endDate, $
                           inst, filters, kindats, expName,  fileDesc
                           
    ; determine if any default parameters need to be set
    if (n_params() lt 9) then begin
        message, 'Too few parameters specified - see usage in madglobalprint.pro'
    endif
    
    if (n_params() lt 10) then begin
        filters = ''
    endif
    
    if (n_params() lt 11) then begin
        kindats = ptr_new()
    endif
    
    if (n_params() lt 12) then begin
        expName = ''
    endif
    
    if (n_params() lt 13) then begin
        fileDesc = ''
    endif
    
    ; get numParms
    parmList = strsplit(parms, ',', /EXTRACT)
    numParms = n_elements(parmList)
    if (strlen(parmList[0]) eq 0) then begin
         message, 'At least one parameter must be specified'
    endif
    
    ; build a format string
    formatStr = '(%"%f '
    for i=1, numParms-1 do begin
        formatStr = formatStr + '%f '
    endfor
    formatStr = formatStr + '")'
    
    ; open output file
    openw, 10, output
    printf, 10, parms
    
    ; convert time arguments
    startYear = 0
    startMonth = 0
    startDay = 0
    startHour = 0
    startMin = 0
    startSec = 0
    startYear = 0
    endMonth = 0
    endDay = 0
    endHour = 0
    endMin = 0
    endSec = 0
    
    CALDAT, startDate, startMonth, startDay, startYear, startHour , startMin, startSec
    CALDAT, endDate, endMonth, endDay, endYear, endHour , endMin, endSec
    
    ; handle the case when experiments extend outside time range
    timeFiltStr1 = string(format='(%" date1=%02i/%02i/%04i time1=%02i:%02i:%02i ")', $
                            startMonth, startDay, startYear, startHour , startMin, startSec)
    timeFiltStr2 = string(format='(%"date2=%02i/%02i/%04i time2=%02i:%02i:%02i ")', $
                            endMonth, endDay, endYear, endHour , endMin, endSec)
    filters = filters + timeFiltStr1 + timeFiltStr2
    
    ; get an array of experiments that match the input arguments
    expArr = madgetexperiments(madurl, inst, startYear, startMonth, startDay, startHour , startMin, startSec, $
                                                          endYear, endMonth, endDay, endHour , endMin, endSec, 1)
                                                          
    ; print error message if no experiments found
    resultSize = size(expArr)
    if (resultSize[1] eq 10) then begin
        close, 10
        message, 'No experiments found that matched the input arguments'
    endif
    
    for i=0, n_elements(expArr)-1 do begin
        
        ; apply experiment name filter, if needed
        if (strlen(expName) gt 0) then begin
            if (strmatch(expArr[i].name, expName, /FOLD_CASE) eq 0) then continue
        endif
        
        ; for each experiment, find all default files
        fileArr = madgetexperimentfiles(madUrl, expArr[i].id)
        
        ; check for empty result
        resultSize = size(fileArr)
        if (resultSize[1] eq 10) then continue
        
        ; loop through each default file
        for j=0, n_elements(fileArr)-1 do begin
            
            ; apply kindat filter, if needed
            resultSize = size(kindats)
            if (resultSize[1] ne 10) then begin
                found = 0 ; loop state variable
                for k=0, n_elements(kindats)-1 do begin
                    if (fileArr[j].kindat eq kindats[k]) then found = 1
                endfor
                if (found eq 0) then continue ; skip this default file
            endif
            
            ; apply fileDesc filter, if needed
            if (strlen(fileDesc) gt 0) then begin
                if (strmatch(filArr[j].status, fileDesc, /FOLD_CASE) eq 0) then continue ; skip this default file
            endif
            
            print , 'Working on file ', fileArr[j].name
            
            ; run madprint
            data = madprint(madurl, fileArr[j].name, parms, filters, user_fullname, user_email, user_affiliation, 1)
            
            dataSize = size(data)
            if (dataSize[1] eq 10) then continue ; no data found in that file
         
            ; loop through each data row
            result = size(data)
            for k=0, result[2]-1 do begin
                printf, 10, FORMAT=formatStr,  data[*,k]
            endfor
            
        endfor ; end loop through files
        
    endfor ; end loop through experiments
                                                          
   
    ; close output file
    close, 10
    
    print, 'madglobalprint complete'
                           
end