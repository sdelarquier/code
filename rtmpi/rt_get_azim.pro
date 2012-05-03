;+ 
; NAME: 
; RT_GET_AZIM
;
; PURPOSE: 
; Returns beam azimuth for a given radar and beam
; 
; CATEGORY: 
; Input/Output
; 
; CALLING SEQUENCE:
; RT_GET_AZIM
;
; INPUTS:
; RADAR
; BEAM
;
; OPTIONAL INPUTS:
;
; KEYWORDS:
;
; COMMON BLOCKS:
; RT_DATA_BLK, RADARINFO
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
; MODIFICATION HISTORY:
; Written by Sebastien de Larquier, Oct.2010
;-
function	rt_get_azim, radar, beam, date

common	rt_data_blk
common	radarinfo

if (beam lt 0 OR beam gt 23) then begin
	prinfo, 'Beam must be a numerical value between 0 and 15.' 
	beam = 7 
	print, 'Default:', beam
endif

; Read radar location and boresight
parse_date, date, year, month, day
radID = where(network.code[0,*] eq radar)
tval = TimeYMDHMSToEpoch(year, month, day, 0, 0, 0)
for s=0,31 do begin
	if (network[radID].site[s].tval eq -1) then break
	if (network[radID].site[s].tval ge tval) then break
endfor
radarsite = network[radID].site[s]

; Middle beam
offset = radarsite.maxbeam/2. - .5

; radar real boresite
b0 = radarsite.boresite

; Azimuth of required beam
baz = b0 + (beam - offset)*radarsite.bmsep


return, baz

END
