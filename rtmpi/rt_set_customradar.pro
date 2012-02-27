;+
; NAME:
; RT_SET_RADAR
;
; PURPOSE:
; This procedure creates a fake radar and includes it in the NETWORK structure
; in the place of the Test Radar (index 0 in the structure).
;
; CATEGORY:
; rt
;
; CALLING SEQUENCE:
; rt_set_customradar,
;
; INPUTS:
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
pro	rt_set_customradar, geolat, geolon, boresite, $
	alt=alt, bmsep=bmsep, vdir=vdir, atten=atten, tdiff=tdiff, phidiff=phidiff, $
	interfer=interfer, recrise=recrise, maxatten=maxatten, maxrange=maxrange, maxbeam=maxbeam

common radarinfo

network[0].operator = 'Virginia Tech - DaViT'

network[0].code[0] = 'custom'

network[0].site[0].geolat = geolat
network[0].site[0].geolon = geolon
network[0].site[0].boresite = boresite

if n_elements(bmsep) eq 0 then $
	network[0].site[0].bmsep = 3.24
	
if n_elements(maxbeam) eq 0 then $
	network[0].site[0].maxbeam = 24


end