PRO colortb

loadct, 33
tvlct, red, green, blue, /get
red[0] = 0
green[0] = 0
blue[0] = 0
red[1] = 80
green[1] = 80
blue[1] = 80
red[255] = 255
green[255] = 255
blue[255] = 255
if ~FILE_TEST('/tmp/colors2.tbl',/write) then begin
	spawn, 'cp /usr/local/itt/idl/resource/colors/colors1.tbl /tmp/colors2.tbl'
endif
modifyct, 0, 'red-green-blue modified', red, green, blue, file='/tmp/colors2.tbl'

fred = bytarr(256)
fgreen = bytarr(256)
fblue = bytarr(256)
loadct, 1
tvlct, red, green, blue, /get
fred[128:255] = red[128:*]
fgreen[128:255] = green[128:*]
fblue[128:255] = blue[128:*]
loadct, 3
tvlct, red, green, blue, /get
fred[0:127] = reverse(red[128:*])
fgreen[0:127] = reverse(green[128:*])
fblue[0:127] = reverse(blue[128:*])
fred[0] = 0
fgreen[0] = 0
fblue[0] = 0
fred[255] = 255
fgreen[255] = 255
fblue[255] = 255
if ~FILE_TEST('/tmp/colors2.tbl',/write) then begin
	spawn, 'cp /usr/local/itt/idl/resource/colors/colors1.tbl /tmp/colors2.tbl'
endif
modifyct, 1, 'red-white-blue', fred, fgreen, fblue, file='/tmp/colors2.tbl'

fred = bytarr(256)
fgreen = bytarr(256)
fblue = bytarr(256)
loadct, 1
tvlct, red, green, blue, /get
fred = reverse(interpolate(red[50:245], indgen(256)))
fgreen = reverse(interpolate(green[50:245], indgen(256)))
fblue = reverse(interpolate(blue[50:245], indgen(256)))
fred[0] = 0
fgreen[0] = 0
fblue[0] = 0
fred[255] = 255
fgreen[255] = 255
fblue[255] = 255
if ~FILE_TEST('/tmp/colors2.tbl',/write) then begin
	spawn, 'cp /usr/local/itt/idl/resource/colors/colors1.tbl /tmp/colors2.tbl'
endif
modifyct, 2, 'white-blue modified', fred, fgreen, fblue, file='/tmp/colors2.tbl'

fred = bytarr(256)
fgreen = bytarr(256)
fblue = bytarr(256)
loadct, 1
tvlct, red, green, blue, /get
fred = (interpolate(red[50:*], indgen(256)))
fgreen = (interpolate(green[50:*], indgen(256)))
fblue = (interpolate(blue[50:*], indgen(256)))
fred[0] = 0
fgreen[0] = 0
fblue[0] = 0
fred[255] = 255
fgreen[255] = 255
fblue[255] = 255
if ~FILE_TEST('/tmp/colors2.tbl',/write) then begin
	spawn, 'cp /usr/local/itt/idl/resource/colors/colors1.tbl /tmp/colors2.tbl'
endif
modifyct, 3, 'blue-white modified', fred, fgreen, fblue, file='/tmp/colors2.tbl'

fred = bytarr(256)
fgreen = bytarr(256)
fblue = bytarr(256)
loadct, 1
tvlct, red, green, blue, /get
fred[0:127] = reverse(red[128:*])
fgreen[0:127] = reverse(green[128:*])
fblue[0:127] = reverse(blue[128:*])
loadct, 3
tvlct, red, green, blue, /get
fred[128:255] = (red[128:*])
fgreen[128:255] = (green[128:*])
fblue[128:255] = (blue[128:*])
fred[0] = 0
fgreen[0] = 0
fblue[0] = 0
fred[255] = 255
fgreen[255] = 255
fblue[255] = 255
if ~FILE_TEST('/tmp/colors2.tbl',/write) then begin
	spawn, 'cp /usr/local/itt/idl/resource/colors/colors1.tbl /tmp/colors2.tbl'
endif
modifyct, 4, 'red-blue', fred, fgreen, fblue, file='/tmp/colors2.tbl'

fred = bytarr(256)
fgreen = bytarr(256)
fblue = bytarr(256)
loadct, 3
tvlct, red, green, blue, /get
fred = reverse(interpolate(red[50:*], indgen(256)))
fgreen = reverse(interpolate(green[50:*], indgen(256)))
fblue = reverse(interpolate(blue[50:*], indgen(256)))
fred[0] = 0
fgreen[0] = 0
fblue[0] = 0
fred[255] = 255
fgreen[255] = 255
fblue[255] = 255
if ~FILE_TEST('/tmp/colors2.tbl',/write) then begin
	spawn, 'cp /usr/local/itt/idl/resource/colors/colors1.tbl /tmp/colors2.tbl'
endif
modifyct, 5, 'Red-temperature inverted', fred, fgreen, fblue, file='/tmp/colors2.tbl'



; loadct, 0, file='/tmp/colors2.tbl'

END
