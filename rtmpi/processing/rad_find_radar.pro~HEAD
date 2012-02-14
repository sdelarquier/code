;+
; NAME:
; RAD_FIND_POS
;
; PURPOSE:
; This procedure finds radars that can 'see' a user provided position
; It also outputs beam and ground distance for each radar.
;
; CATEGORY:
; misc
;
; CALLING SEQUENCE:
; rad_find_radar, lat, lon,
; 		which_radar=which_radar, which_beam=which_beam, which_dist=which_dist,
;		coords=coords
;
; INPUTS:
; LAT: latitude of location
;
; LON: longitude of location
;
; WHICH_RADAR: radars that 'see' the given location
;
; WHICH_BEAM: beam of each radar that passes the closest over the given location
;
; WHICH_DIST: distance of given location to radars
;
; COORDS: coordinates in which the user location is given. Default is 'geog'
; for geographic coordinated, but you should specify coords='magn' if you provide
; a position in magnetic coordinates
;
; KEYWORD PARAMETERS:
;
; COMMON BLOCKS:
;
; EXAMPLE:
;
; COPYRIGHT:
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.
;
;
; MODIFICATION HISTORY:
; Written by Sebastien de Larquier 02/2012
; -
pro rad_find_radar, lat, lon, which_radar=which_radar, which_beam=which_beam, which_dist=which_dist, coords=coords

common radarinfo

if lat gt 0. then $
	hemisphere = 1. $
else $
	hemisphere = -1.

if ~keyword_set(coords) then $
	coords = 'geog'

if coords eq 'magn' then begin
	geopos = cnvcoord(lat, lon, /geo)
	lat = geo[0]
	lon = geo[1]
endif

which_radar = ''
which_beam = 0
which_dist = 0.
nr = 0L
for ir=1,n_elements(network)-1 do begin
	radarsite = network[ir].site[where(network[ir].site.tval eq -1)]
	; Only look at radars in the same hemisphere
	if radarsite.geolat*hemisphere gt 0. then begin
		dist = calc_azdist([radarsite.geolat, radarsite.geolon], [lat, lon], azimuth=az)
		if az lt 0. then $
			az = (az + 360.) mod 360.
		; Calculate radar extreme azimuths
		offset = radarsite.maxbeam/2. - .5
		b0 = radarsite.boresite
		if b0 lt 0. then $
			b0 = (b0 + 360.) mod 360.
		sradaz = b0 + (0 - offset)*radarsite.bmsep
		fradaz = b0 + (radarsite.maxbeam-1 - offset)*radarsite.bmsep
		radaz = b0 + (indgen(radarsite.maxbeam) - offset)*radarsite.bmsep
		if sradaz gt fradaz then begin
			tmp = fradaz
			fradaz = sradaz
			sradaz = tmp
		endif
		; check if the radar is close enough, and if the point is within its fov
		; then determine which beam is the closest to the point
		print, network[ir].code[0], dist, sradaz, fradaz, b0, az
		if dist ge 200. and dist le 3500.  and az  ge sradaz-1. and az le fradaz+1. then begin
			minaz = min(abs(az-radaz), min_ind)
			which_beam = [which_beam, min_ind]
			which_radar = [which_radar, network[ir].code[0]]
			which_dist = [which_dist, dist]
			nr = nr + 1
; 			print, which_radar[nr], which_beam[nr], dist
		endif
	endif
endfor

if nr ge 1 then begin
	which_radar = which_radar[1:*]
	which_beam = which_beam[1:*]
	which_dist = which_dist[1:*]
endif

end
