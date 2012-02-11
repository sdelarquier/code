;+
; NAME: 
;    madgetcgiurl(madurl)
;
; PURPOSE: 
;    Get the Madrigal CGI url based on the base Madrigal url.  This is meant
;    to be a private method used internal by other methods.
;
; INPUTS: 
;      madurl - scalar string giving a fully qualified url to the base Madrigal site
;
; OUTPUT: a string giving the base Madrigal CGI url
; EXAMPLE: 
;      result = madgetcgiurl('http://madrigal.haystack.mit.edu/madrigal')
;
; $Id$
;
FUNCTION madgetcgiurl, madurl
	
	; constants
	cgiName = 'accessData.cgi'
	
	; determine if needed parameters set
  if (n_params() lt 1) then begin
      message, 'Too few parameters specified - see usage in madgetcgiurl.pro'
  endif
	
	; make sure url ends index.html
	result = stregex(madurl, '/index.html', /BOOLEAN)
	if (~ result) then madurl += '/index.html'
	
	; make sure url starts with http
	result = stregex(madurl, 'http://', /BOOLEAN)
	if (~ result) then begin
		message, 'madurl must start with http://'
	endif
	
	; get the rootUrl
	index = strpos(madurl, '/', 8) ; 8 is to skip http://? part
	rootUrl = strmid(madurl, 0, index)
	
	text = webget(madurl)
	regex = '".*' + cgiName
	cgiUrlList = stregex(text.Text, regex, /EXTRACT)
	
	; find the first non-zero length string
	cgiUrl = ""
	for i=0, N_ELEMENTS(cgiUrlList)-1 do begin
	    if (strlen(cgiUrlList[i]) gt strlen(cgiName)) then begin
	        cgiUrl = cgiUrlList[i]
	        break
	    endif
	endfor
	
	if (strlen(cgiUrl) eq 0) then begin
		message, 'error parsing home page'
	endif
	
	index1 = strpos(cgiUrl, '"')
	index2 = strpos(cgiUrl, cgiName)
	baseUrl = strmid(cgiUrl, index1+1, index2-(index1+1))
	
	retStr =rootUrl + baseUrl
	RETURN, retStr
END