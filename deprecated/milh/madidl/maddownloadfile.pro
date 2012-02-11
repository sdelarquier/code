;+
; NAME: 
;    maddownloadfile, madurl, fullFilename, outputFile, user_fullname, user_email, user_affiliation
;
; PURPOSE: 
;    downloads a Madrigal file in simple ascii format to local computer
;
; INPUTS: 
;      madurl - scalar string giving a fully qualified url to the Madrigal site
;      fullFilename - full path to experiment file as returned by madGetExperimentFiles.
;      outputFile - path to save file locally
;      user_fullname - full name of user making request
;      user_email - email address of user making request
;      user_affiliation - affiliation of user making request
;
; EXAMPLE: 
;     maddownloadfile, 'http://madrigal.haystack.mit.edu/madrigal', '/opt/madrigal/blah/mlh980120g.001', '/tmp/junk.txt', &
;                                   'Bill Rideout', 'brideout@haystack.mit.edu', 'MIT'
;
; $Id: maddownloadfile.pro,v 1.1 2010/07/15 16:33:23 brideout Exp $
;
PRO maddownloadfile, madurl,  fullFilename, outputFile, user_fullname, user_email, user_affiliation

    ; determine if needed parameters set
    if (n_params() lt 6) then begin
      message, 'Too few parameters specified - see usage in maddownloadfile.pro'
    endif

    ; the first step is to call madGetExperimentFileParameters to find out what parameters to request
    parms = madGetExperimentFileParameters(madurl, fullFilename)
    parmStr = 'year,month,day,hour,min,sec'
    numParms = 6
    for i=0, n_elements(parms)-1 do begin
        if (parms[i].isMeasured eq 0) then continue ; skip derived parameters
        if (parms[i].isAddIncrement eq 1) then continue ; skip additional increment
        parmStr = parmStr + ',' + parms[i].mnemonic
        numParms = numParms + 1
    endfor

    ; now call madprint without filters
    simpleData = madprint(madurl,  fullFilename, parmStr, '', user_fullname, user_email, user_affiliation)
     
    ; build a format string
    formatStr = '(%"%f '
    for i=1, numParms-1 do begin
        formatStr = formatStr + '%f '
    endfor
    formatStr = formatStr + '")'
    
    ; write result to file
    openw, 11, outputFile
    printf, 11, parmStr
    
    ; loop through each data row and write it to file
    result = size(simpleData)
    for k=0, result[2]-1 do begin
        printf, 11, FORMAT=formatStr,  simpleData[*,k]
    endfor
    
    close, 11
END