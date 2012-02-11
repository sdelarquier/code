pro colorb

window, xsize=1600, ysize=200

plot, [0,255], [0,1], /nodata, $
	xstyle=1, ystyle=1, ytickname=replicate(' ',60)

for nc=0,255 do begin
	
	polyfill, [nc, nc, nc+1, nc+1], [0,1,1,0], color=nc

endfor


end