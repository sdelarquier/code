pro wave1dProp

; Useful constants
eps0 = 8.854e-12
mu0 = 4.*!pi*1e-7
c0 = 3e8

; Domain grid
L = 60.; [m]
dl = 1.; [m]
ngrid = round(L/dl/2.)
xx = findgen(ngrid)*2.*dl
x = findgen(ngrid*2)*dl

; Signal
F = 10.; [MHz]
pL = 5.; [ms]

; Background densities
N0 = 1e11; [m-3]
ldiff = 15.; [m]
Ndiff = .1
N = N0*( 1. + Ndiff/2.*sin(2.*!pi/ldiff*x) )
nr = sqrt(1. - 80.5e-12*N/F^2)

; Time
T = 10.; [s]
dt = 1. *dl/c0; [s]
iiterations = round(T/dt)
tt = findgen(iiterations)*dt

; Parameters
eps = eps0*nr^2

; Variables
EE = fltarr(ngrid,2)
HH = fltarr(ngrid,2)

; Source
HH[*,0] = 
EE[*,0] = sqrt(mu0/eps[0:2*ngrid-1:2])*HH[*,0]

; Propagation
it = 0L
while it le iiterations and it ge 0 then begin
	; Set current time
	time = it*dt

	; Move one step forward (Leapfrog)
	HH[2:ngrid-1,1] = HH[2:ngrid-1,0] - dt/dl/mu0 * (EE[2:ngrid-1,0] - EE[1:ngrid-2,0])
	HH[0,1] = HH[1,0]
	EE[1:ngrid-2,1] = EE[1:ngrid-2,0] - dt/dl/eps[0:2*ngrid-2:2] * (HH[2:ngrid-1,1] - HH[1:ngrid-2,1])
	EE[brgid-1,1] = EE[ngrid-2,0]
	
	; Pass data to next step
	EE[*,0] = EE[*,1]
	HH[*,0] = HH[*,1]

	; Increment time loop
	it += 2
endwhile



end