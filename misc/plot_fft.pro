; Plott the fft of a regularily sampled variable y
pro plot_fft, y, ts=ts, trange=trange, frange=frange, xrange=xrange, title=title


; number of points
npoints = n_elements(y)

; sampling (Hz)
if ~keyword_set(ts) then $
    ts = 1.
fs = 1./ts
print, 'Sampling frequency/interval: ', fs, ts

; time vector
t = findgen(npoints)/fs

; frequency vector
f = [findgen(npoints/2.)/(npoints/fs), 1./2.*fs, -reverse( findgen(npoints/2.-1)/(npoints/fs) )]

; Signal
s = y

; Fourier transform
X = fft(s)

; Plot signal and transform
if (!d.name eq 'X') then $
    window, xsize=800, ysize=1000
plot, t, s, xtitle='Time', ytitle='Signal', position=[.1,.7,.9,.95], $
    xrange=xrange, xstyle=1
plot, f[1:npoints/2], abs(X[1:npoints/2]), xtitle='Frequency', position=[.1,.4,.9,.65], $
    xrange=frange, xstyle=1, xticklen=1., xgridstyle=1
oplot, f[1:npoints/2], abs(X[1:npoints/2]), psym=1
plot, 1./f[1:npoints/2], abs(X[1:npoints/2]), xtitle='Period', position=[.1,.1,.9,.35], $
    xrange=trange, xstyle=1, xticklen=1., xgridstyle=1
oplot, 1./f[1:npoints/2], abs(X[1:npoints/2]), psym=1

if keyword_set(title) then begin
    xyouts, .5, .97, title, align=.5, /normal
endif

end