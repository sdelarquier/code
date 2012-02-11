function getJD, day, month, year
	; print, day, month, year
	if month lt 2 then begin
		year -= 1
		month += 12
	endif
	A = floor(year/100.)
	B = 2.d - A + floor(A/4.d)
	JD = floor(365.25d*(year + 4716.d)) + floor(30.6001d*(month+1)) + day + B - 1524.5d
	return, JD
end