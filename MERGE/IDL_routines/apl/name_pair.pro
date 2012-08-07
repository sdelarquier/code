;------------------------------------------------------------------------------
function name_pair,file
;
; This function returns the name (string format) of a radar pair deduced
; from the name of a MERGE file.
;
;
pairs = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N']
names = [' ','SAS-KAP','GOOSE-STOK','ICW-FIN',' ',' ',' ',' ',' ',' ',$
         ' ',' ',' ','HALLEY-SYOWA']
q = where (pairs eq strmid(file,6,1),ncount)
if (ncount ne 0) then begin 
  pair_name    =  names(q(0))
endif else begin
  print,' pair ID ',strmid(file,6,1),' not found'
  pair_name    =  ' '
endelse

return,pair_name
end
