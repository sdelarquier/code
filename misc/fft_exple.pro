pro fft_exple


; time length (s)
dur = 20.

; sampling (Hz)
fs = 20.

; time vector
npoints = dur*fs
t = findgen(npoints)/fs

; frequency vector
f = [findgen(npoints/2.)/(npoints/fs), 1./2.*fs, -reverse( findgen(npoints/2.-1)/(npoints/fs) )]

; Signal
T1 = 1.
T2 = 0.5
s = cos(2.*!pi*t/T1) + .6*cos(2.*!pi*t/T2)

; Fourier transform
X = fft(s)

; Plot signal and transform
window, 0, xsize=800, ysize=1000
plot, t, s, xtitle='Time', ytitle='Signal', position=[.1,.7,.9,.95]
plot, f[0:npoints/2], abs(X[0:npoints/2]), xtitle='Frequency', ytitle='Power', position=[.1,.4,.9,.65]
plot, 1./f[0:npoints/2], abs(X[0:npoints/2]), xtitle='Period', ytitle='Power', position=[.1,.1,.9,.35]


;*******************************************************************
; display the power spectrum of a 100-element vector sampled at a rate of 0.1 seconds per point. Show the 0 frequency component at the center of the plot and label the abscissa with frequency:
;*******************************************************************

; ; Define the number of points and the interval:  
; N = 100  
; T = 0.1  
;   
; ; Midpoint+1 is the most negative frequency subscript:  
; N21 = N/2 + 1  
; ; The array of subscripts:  
; F = INDGEN(N)  
; ; Insert negative frequencies in elements F(N/2 +1), ..., F(N-1):  
; F[N21] = N21 -N + FINDGEN(N21-2)  
;   
; ; Compute T0 frequency:  
; F = F/(N*T)  
;   
; ; Shift so that the most negative frequency is plotted first:  
; PLOT, /YLOG, SHIFT(F, -N21), SHIFT(ABS(FFT(F, -1)), -N21)

  
;*******************************************************************
; Compute the FFT of a two-dimensional image by entering:
;*******************************************************************

; ; Create a cosine wave damped by an exponential.  
; n = 256  
; x = FINDGEN(n)  
; y = COS(x*!PI/6)*EXP(-((x - n/2)/30)^2/2)  
;   
; ; Construct a two-dimensional image of the wave.  
; z = REBIN(y, n, n)  
; ; Add two different rotations to simulate a crystal structure.  
; z = ROT(z, 10) + ROT(z, -45)  
; WINDOW, XSIZE=540, YSIZE=540  
; LOADCT, 39  
; TVSCL, z, 10, 270  
;   
; ; Compute the two-dimensional FFT.  
; f = FFT(z)  
; logpower = ALOG10(ABS(f)^2)   ; log of Fourier power spectrum.  
; TVSCL, logpower, 270, 270  
;   
; ; Compute the FFT only along the first dimension.  
; f = FFT(z, DIMENSION=1)  
; logpower = ALOG10(ABS(f)^2)   ; log of Fourier power spectrum.  
; TVSCL, logpower, 10, 10  
;   
; ; Compute the FFT only along the second dimension.  
; f = FFT(z, DIMENSION=2)  
; logpower = ALOG10(ABS(f)^2)   ; log of Fourier power spectrum.  
; TVSCL, logpower, 270, 10

end  