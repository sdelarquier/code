pro rt_calculate_phase, jul, $
	tdiff=tdiff, phidiff=phidiff, $
	interfer_pos=interfer_pos, scan_boresite_offset=scan_boresite_offset, $
	theta=theta, chi_max=chi_max, phi_temp=phi_temp, psi=psi, phi0=phi0

common rt_data_blk
common radarinfo

; get time
caldat, jul, month, day, year, hour, minute, second

; get hardware configuration at the time
radar = radargetradar(network, rt_info.id)
site = radarymdhmsgetsite(radar, year, month, day, hour, minute, second)

; get default tdiff from hardware file
if ~keyword_set(tdiff) then $
	tdiff = site.tdiff

; get default phidiff from hardware file
if ~keyword_set(phidiff) then $
	phidiff = site.phidiff

; get default interferometer position
; or the one that's in the hdw file, anyway
if n_elements(interfer_pos) ne 3 then $
	interfer_pos = site.interfer

; get default interferometer position
; or the one that's in the hdw file, anyway
if total(interfer_pos^2) le 0. then $
	interfer_pos = site.interfer

; this is the offset in degree between
; the physical boresite and the scanning boresite
; the physical boresite is the antenna array normal direction
; the scanning boresite is the direction of the center beam
; as far as I know, these values differ only at Blackstone.
if n_elements(scan_boresite_offset) eq 0 then $
	scan_boresite_offset = 0.

; get dimensions of data array
sz = size(rt_data.elevation, /dim)

; antenna separation in meters
antenna_separation = sqrt(total(interfer_pos^2))

;print, tdiff, phidiff, interfer_pos, scan_boresite_offset

; elevation angle correction, if antennas are at different heights; rad
elev_corr = phidiff * asin( interfer_pos[2] / antenna_separation)

; +1 if interferometer antenna is in front of main antenna, -1 otherwise
; interferometer in front of main antenna
if interfer_pos[1] gt 0.0 then $
	phi_sign = 1.0 $
; interferometer behind main antenna
else $
	phi_sign = -1.0
elev_corr *= phi_sign

; offset in beam widths to the edge of the array
offset = site.maxbeam/2.0 - 0.5


; beam direction off boresight; rad
phi = ( site.bmsep*( rt_data.beam - offset ) + scan_boresite_offset ) * !dtor

; cosine of phi
c_phi = cos( phi )
; replicate c_phi to match dimensions of phi0
c_phi = rebin(c_phi, sz[0], sz[1], sz[2])

; wave number; 1/m
k = 2. * !PI * rt_data.tfreq * 1.0e6/ 2.99792458e8
; replicate k to match dimensions of phi0
k = rebin(k, sz[0], sz[1], sz[2])

; the phase difference phi0 is between -pi and +pi and gets positive,
; if the signal from the interferometer antenna arrives earlier at the
; receiver than the signal from the main antenna.
; If the cable to the interferometer is shorter than the one to
; the main antenna, than the signal from the interferometer
; antenna arrives earlier. tdiff < 0  --> dchi_cable > 0

; phase shift caused by cables; rad
dchi_cable = -2. * !PI * rt_data.tfreq * 1.0e6 * tdiff * 1.0e-6
; replicate dchi_cable to match dimensions of phi0
dchi_cable = rebin(dchi_cable, sz[0], sz[1], sz[2])

; If the interferometer antenna is in front of the main antenna
; then lower elevation angles correspond to earlier arrival
; and greater phase difference.
; If the interferometer antenna is behind of the main antenna 
; then lower elevation angles correspond to later arrival
; and smaller phase difference

; maximum phase shift possible; rad
chi_max = phi_sign * k * antenna_separation * c_phi + dchi_cable
; replicate chi_max to match dimensions of phi0
chi_max = rebin(chi_max, sz[0], sz[1], sz[2])


; Get uncorrected elevation
theta = rt_data.elevation*!dtor
vinds = where(rt_data.elevation eq 10000., ni, complement=ninds, ncomplement=nn)
if ni gt 0L then begin
	theta[vinds] = 20.*!dtor
endif
theta = theta - elev_corr

; Add off-boresite effect
s_theta = sin(theta)
c_theta = sqrt(c_phi*c_phi - s_theta*s_theta)

; Get actual phase angle (no cable)
psi = phi_sign * c_theta * (k * antenna_separation)

; Actual phase (incl. cable)
phi_temp = psi + dchi_cable

; Get phi0 between -pi and pi
if phi_sign lt 0 then $
	phi_temp = phi_temp - 2.*!pi
phi0 = phi_temp - floor(chi_max/2./!pi)*2.*!pi
inds = where(phi0 lt -!pi, cc)
if cc gt 0 then phi0[inds] = ( (phi0[inds]-!pi) mod (2.*!pi) ) + !pi
inds = where(phi0 gt !pi, cc)
if cc gt 0 then phi0[inds] = ( (!pi+phi0[inds]) mod (2.*!pi) ) - !pi

; Gets rid of bad values
if ni gt 0L then begin
	theta[vinds] = 10000.
	phi_temp[vinds] = 10000.
	psi[vinds] = 10000.
	phi0[vinds] = 10000.
endif

end