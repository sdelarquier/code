;+ 
; NAME: 
; HISTC
;
; PURPOSE: 
; This function generates an histogram count
; 
; CATEGORY: 
; Input/Output
; 
; CALLING SEQUENCE:
; HISTC, y, x
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORDS:
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
; MODIFICATION HISTORY:
; Written by Sebastien de Larquier, Sept. 2010
;-
function	histc, y, x, weights=weights

if ~keyword_set(weights) then $
	weights = fltarr(n_elements(y)) + 1.

max_val = MAX(y)
min_val = MIN(y)

tmp = SORT(x)
x = x(tmp)

n = n_elements(x)-1
freq = fltarr(n)
FOR i=0, n-1 DO BEGIN
	inds = where(y LT x(i+1) AND y GE x(i),cc)
	if cc gt 0L then $
		freq[i] = TOTAL(weights[inds])
ENDFOR
freq(n-1) += TOTAL(y EQ x(n))


RETURN, freq

END
