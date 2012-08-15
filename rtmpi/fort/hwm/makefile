
FOR95 = f95
FOR77 = gfortran
FFLAGS = -cpp

check:
	$(FOR95) $(FFLAGS) hwmOUT.f90 hwm07e.f90 dwm07b.f90 apexcord.f90 -o hwmOUT
	$(FOR77) igrfOUT.f90 igrf.f90 -o igrfOUT
# 	$(FOR95) $(FFLAGS) checkhwm07.f90 hwm07e.f90 dwm07b.f90 apexcord.f90 -o checkhwm

clean:
	rm *.mod hwmOUT igrfOUT
# 	rm *.mod checkhwm