;+
; NAME: 
;    madprint(madurl, fullFilename, parms, filters, user_fullname, user_email, user_affiliation, ignore_no_data)
;
; PURPOSE: 
;    madPrint prints the data in the given file is a simple ascii format based on user-specified parmeters and filters.
;
;       See madSimplePrint to print a file with all data and just those parameters in the file itself.
;
;        madPrint allows you to choose which parameters to print, to print derived parameters, or
;        to filter the data.
;
; INPUTS: 
;      madurl - scalar string giving a fully qualified url to the Madrigal site
;      fullFilename - full path to experiment file as returned by madGetExperimentFiles.
;      parms - Comma delimited string listing requested parameters (no spaces allowed).
;      filters - Space delimited string listing filters desired, as in isprint command (see
;                   http://madrigal.haystack.mit.edu/madrigal/ug_commandLine.html#isprint for details)
;      user_fullname - full name of user making request
;      user_email - email address of user making request
;      user_affiliation - affiliation of user making request
;      ignore_no_data - if 0 (the default), raises error if no data, If 1, ignores no data
;
; OUTPUT:  a two dimensional array of doubles.  Number of columns = number of parameters requested.  Number of rows
;         is the number of measurments.  Note that Madrigal often contains a number a measurments at the same time
;         (such as when a radar makes different range measurements at the same time), so two or more rows may have the same time.
; EXAMPLE: 
;     result = madPrint('http://madrigal.haystack.mit.edu/madrigal', '/opt/madrigal/blah/mlh980120g.001', $
;                                   'year,month,day,hour,min,sec,gdalt,ti', 'filter=gdalt,500,600 filter=ti,1900,2000' $
;                                   'Bill Rideout', 'brideout@haystack.mit.edu', 'MIT')
;
; $Id$
;
FUNCTION madPrint, madurl,  fullFilename, parms, filters, user_fullname, user_email, user_affiliation, ignore_no_data

    scriptName = 'isprintService.py'
    
    ; determine if needed parameters set
    if (n_params() lt 7) then begin
      message, 'Too few parameters specified - see usage in madprint.pro'
    endif
    
    ; set ignore_no_data if needed
    if (n_params() lt 8) then begin
      ignore_no_data = 0
    endif
    
    ; get cgiUrl
    cgiUrl = madgetcgiurl(madurl)
    
    result = strmatch(parms, '* *')
    if (result eq 1) then begin
        message, 'parms string must not contain spaces'
    endif
    
    ; get numParms
    parmList = strsplit(parms, ',', /EXTRACT)
    numParms = n_elements(parmList)
    if (strlen(parmList[0]) eq 0) then begin
         message, 'At least one parameter must be specified'
    endif
    
    ; build url
    expUrl = cgiUrl + scriptName + '?'
    fullFilename2 = String(Replicate(32B, strlen(fullFilename)))
    strput, fullFilename2, fullFilename
    strreplace, fullFilename2, '/',  '%2F'
    expUrl += string(format='(%"file=%s&")', fullFilename2)
    parms2 = String(Replicate(32B, strlen(parms)))
    strput, parms2, parms
    strreplace, parms2, '+', '%2B'
    strreplace, parms2, ',', '+'
    expUrl += string(format='(%"parms=%s&")', parms2)
    if (strlen(filters) gt 0) then begin
        filters2 = String(Replicate(32B, strlen(filters)))
    endif else begin
        filters2 = ''
    endelse
    strput, filters2, filters
    strreplace, filters2, '=', '%3D'
    strreplace, filters2, ',', '%2C'
    strreplace, filters2, '/', '%2F'
    strreplace, filters2, '+', '%2B'
    strreplace, filters2, ' ', '+'
    expUrl += string(format='(%"filters=%s&")', filters2)
    user_fullname2 = String(Replicate(32B, strlen(user_fullname)))
    strput, user_fullname2, user_fullname
    strreplace, user_fullname2, ' ', '+'
    expUrl += string(format='(%"user_fullname=%s&")', user_fullname2)
    user_email2 = String(Replicate(32B, strlen(user_email)))
    strput, user_email2, user_email
    strreplace, user_email2, ' ', '+'
    expUrl += string(format='(%"user_email=%s&")', user_email2)
    user_affiliation2 = String(Replicate(32B, strlen(user_affiliation)))
    strput, user_affiliation2, user_affiliation
    strreplace, user_affiliation2, ' ', '+'
    expUrl += string(format='(%"user_affiliation=%s&")', user_affiliation2)
    
    result = webget(expUrl)
    lines = result.Text
    
    ; this first pass through the data is simply to get a count
    totalLines = long64(0)
    buffer = ''
    for i=long64(0), N_ELEMENTS(lines)-1 do begin
        if (strcmp(lines[i], '1400') eq 1) then continue ; I have no idea why this shows up 
        thisLine = buffer + lines[i]
        words = strsplit(thisLine, ' ', /EXTRACT)
        ; check for an error message in the first few lines
        if (i lt 10 and N_ELEMENTS(words) gt 1) then begin
            for j=0, N_ELEMENTS(words)-1 do begin
                ; skip assumed, missing, knownbad
                if (strcmp(strlowcase(words[j]), 'missing') eq 1) then continue
                if (strcmp(strlowcase(words[j]), 'assumed') eq 1) then continue
                if (strcmp(strlowcase(words[j]), 'knownbad') eq 1) then continue
                if (num_chk(words[j]) eq 1) then begin
                    if (ignore_no_data) then return, ptr_new()
                    print, 'Error: ', lines
                    message, 'Error: ' + lines[i]
                endif
            endfor
        endif
        if (N_ELEMENTS(words) LT numParms) then begin
            ; debug coded print, 'Error: two few words in line - buffering: ' + thisLine
            buffer = thisLine
            continue
        endif else begin
            buffer = ''
        endelse
        totalLines += 1
    endfor
    
    if (totalLines eq 0) then begin
        return, ptr_new()
    endif
    
    ; create dataArr set to all zeros
    dataArr = fltarr(numParms, totalLines)
    
    ; this pass fills out that array
    index = long64(0)
    buffer = ''
    for i=long64(0), N_ELEMENTS(lines)-1 do begin
        if (strcmp(lines[i], '1400') eq 1) then continue ; I have no idea why this shows up 
        thisLine = buffer + lines[i]
        words = strsplit(thisLine, ' ', /EXTRACT)
        if (N_ELEMENTS(words) LT numParms) then begin
            ; debug coded print, 'Error: two few words in line - buffering: ' + thisLine
            buffer = thisLine
            continue
        endif else begin
            buffer = ''
        endelse
        ; loop through each parm
        for j=0, numParms - 1 do begin
            ; set all missing, assumed, and knownbad to NaN
            if (strcmp(strlowcase(words[j]), 'missing') eq 1 or $
                 strcmp(strlowcase(words[j]), 'assumed') eq 1 or $
                 strcmp(strlowcase(words[j]), 'knownbad') eq 1) then begin
                 dataArr[j,index] = !values.F_NAN
            endif else begin
                if (num_chk(words[j]) eq 1 ) then begin
                    dataArr[j,index] = !values.F_NAN
                endif else begin
                    dataArr[j,index] = float(words[j])
                endelse
            endelse
        endfor
        index += 1
    endfor
    
	RETURN, dataArr
END