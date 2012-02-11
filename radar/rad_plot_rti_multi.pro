pro	rad_plot_rti_multi, date, radars, time=time

if ~keyword_set(time) then $
	time=[0000,2400]

for r=0, n_elements(radars)-1 do begin
	rad_fit_read, date, radars[r], time=time
	rad_fit_plot_rti, beams=indgen(16), param='power', time=time
endfor

END