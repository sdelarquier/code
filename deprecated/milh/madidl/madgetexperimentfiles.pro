;+
; NAME: 
;    madgetexperimentfiles(madurl, expid, getnondefault)
;
; PURPOSE: 
;    returns a list of all  Madrigal Experiment Files for a given experiment id
;
; INPUTS: 
;      madurl - scalar string giving a fully qualified url to the Madrigal site
;      expid - experiment id as returned by madgetexperiments
;      getnondefault - an optional argument.  If 1, get all files, including non-default.  If 0 (the default)
;             get only default files
;
; OUTPUT: an array of structures representing experiment files for that experiment with the following fields:
;     1. name (string) Example '/opt/madrigal/blah/mlh980120g.001'
;     2. kindat (int) Kindat code.  Example: 3001
;     3. kindatdesc (string) Kindat description: Example 'Basic Derived Parameters'
;     4. category (int) (1=default, 2=variant, 3=history, 4=real-time)
;     5. status (string)('preliminary', 'final', or any other description)
;     6. permission (int)  0 for public, 1 for private
;     7. expId - experiment id (int) of the experiment this MadrigalExperimentFile belongs in
;   If no experiement files found, returns a Null Pointer (IDL does not allow empty arrays)
; EXAMPLE: 
;     result = madGetExperimentFiles('http://madrigal.haystack.mit.edu/madrigal', 300041, 0)
;
; $Id: madgetexperimentfiles.pro,v 1.3 2010/07/13 16:28:24 brideout Exp $
;
FUNCTION madGetExperimentFiles, madurl,  expId, getNonDefault

    scriptName = 'getExperimentFilesService.py'
    
    ; determine if needed parameters set
    if (n_params() lt 2) then begin
      message, 'Too few parameters specified - see usage in madgetexperimentfiles.pro'
    endif
    
    if (n_params() lt 3) then begin
        getNonDefault = 0
    endif
    
    ; get cgiUrl
    cgiUrl = madgetcgiurl(madurl)
    
    
    ; now call the main method to get experiment files
    expUrl = cgiUrl + scriptName + '?'
    expUrl += string(format='(%"id=%i")', expId)
    
    result = webget(expUrl)
    
    lines = result.Text
    
    ; this first pass through the data is simply to get a count
    totalLines = 0
    for i=0, N_ELEMENTS(lines)-1 do begin
        words = strsplit(lines[i], ',', /EXTRACT)
        if (N_ELEMENTS(words) LT 6) then continue
        if (getNonDefault eq 0) then begin
            if (uint(words[3]) ne 1) then continue ; skip non-default file
        endif
        totalLines += 1
    endfor
    
    if (totalLines eq 0) then begin
        return, ptr_new()
    endif
    
    ; create default expFileArr
    expFileDefault = {experimentFile, name:"", kindat:0L, kindatdesc:"", category:0, status:"", permission:0,expid:0L} 
    expFileArr = REPLICATE({experimentFile}, totalLines) 
    
    ; this pass fills out that array
    index = 0
    for i=0, N_ELEMENTS(lines)-1 do begin
        words = strsplit(lines[i], ',', /EXTRACT)
        if (N_ELEMENTS(words) LT 6) then continue
        if (getNonDefault eq 0) then begin
            if (uint(words[3]) ne 1) then continue ; skip non-default file
        endif
        expFileArr[index].name = words[0]
        expFileArr[index].kindat = long(words[1])
        expFileArr[index].kindatdesc = words[2]
        expFileArr[index].category = uint(words[3])
        expFileArr[index].status = words[4]
        expFileArr[index].permission = uint(words[5])
        expFileArr[index].expid = long(expid)

        index += 1
    endfor
    
	RETURN, expFileArr
END