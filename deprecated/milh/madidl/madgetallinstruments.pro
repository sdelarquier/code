;+
; NAME: 
;    madgetallinstruments(madurl)
;
; PURPOSE: 
;    Get information on all instruments with data at the Madrigal site specified
;    by madUrl
;
; INPUTS: 
;      madurl - scalar string giving a fully qualified url to the Madrigal site
;
; OUTPUT: an array of structures with the following fields:
;		1. name - (string) Example: 'Millstone Hill Incoherent Scatter Radar'
;		2. code - (int) Example: 30
;		3. mnemonic - (3 char string) Example: 'mlh'
;		4. latitude - (double) Example: 45.0
;		5. longitude (double) Example: 110.0
;		6. altitude (double)  Example: 0.015 (km)
; EXAMPLE: 
;     result = madGetAllInstruments('http://madrigal.haystack.mit.edu/madrigal')
;
; $Id$
;
FUNCTION madgetallinstruments, madurl

    scriptName = 'getInstrumentsService.py'
    
    ; determine if needed parameters set
  if (n_params() lt 1) then begin
      message, 'Too few parameters specified - see usage in madgetallinstruments.pro'
  endif
    
    ; get cgiUrl
    cgiUrl = madgetcgiurl(madurl)
    
    instUrl = cgiUrl + scriptName ; no arguments needed
    
    result = webget(instUrl)
    
    lines = result.Text
    
    ; this first pass through the data is simply to get a count
    totalLines = 0
    for i=0, N_ELEMENTS(lines)-1 do begin
        words = strsplit(lines[i], ',', /EXTRACT)
        if (N_ELEMENTS(words) LT 6) then continue
        totalLines += 1
    endfor
    
    ; create default instArr
    instDefault = {instrument, name:"", code:0, mnemonic:"", latitude:0.0, longitude:0.0, altitude:0.0} 
    instArr = REPLICATE({instrument}, totalLines) 
    
    ; this pass fills out that array
    index = 0
    for i=0, N_ELEMENTS(lines)-1 do begin
        words = strsplit(lines[i], ',', /EXTRACT)
        if (N_ELEMENTS(words) LT 6) then continue
        instArr[index].name = words[0]
        instArr[index].code = uint(words[1])
        instArr[index].mnemonic = words[2]
        instArr[index].latitude = double(words[3])
        instArr[index].longitude = double(words[4])
        instArr[index].altitude = double(words[5])
        index += 1
    endfor
	
	RETURN, instArr
END