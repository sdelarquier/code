pro	rt_trend_fft, date=date, time=time

clear_page

trendPTR = rt_trend(date=date, time=time)

trendSize = 1
Nold = n_elements(*trendPTR)/2

; Remove zeros
ttrend = (*trendPTR)[where((*trendPTR)[*,1] gt 15.),*]
Ns = n_elements(ttrend)/2

print, 'Array size changed from '+STRTRIM(Nold,2)+' to '+STRTRIM(Ns,2)


; format trend array size to next power of 2 greater than current size
; and uniformly sampled
REPEAT trendSize = trendSize *2 UNTIL trendSize gt Ns
trend = dindgen(trendSize,2)*0.
trend[*,0] = INTERPOL(ttrend[*,0],trendSize)
trend[*,1] = INTERPOL(ttrend[*,1],ttrend[*,0],trend[*,0])

print, 'Array size changed from '+STRTRIM(Ns,2)+' to '+STRTRIM(trendsize,2)

; plot, trend[*,0], SMOOTH(trend[*,1],20,/edge), thick=2
; oplot, trend[*,0], trend[*,1],linestyle=3

; Time and frequency arrays
dt = trend[1,0]-trend[0,0]
Freq = FINDGEN(trendSize/2+1)/(trendSize*dt*86400.469)
time = dt*dindgen(trendSize)

; rad_fit_plot_rti, time=[1200,0500], /trend, param='power', yrange=[0,75]

; Finally the FFT 
trendFFT = FFT(SMOOTH(trend[*,1],20,/edge),/double)
plot, Freq[1:trendSize/2], ABS(trendFFT[1:trendSize/2]); , xrange=[2e-5,1e-4]


temp = ABS(trendFFT[where(Freq ge 2e-5 AND Freq le 6e-5)])
ftemp = Freq[where(Freq ge 2e-5 AND Freq le 6e-5)]


end