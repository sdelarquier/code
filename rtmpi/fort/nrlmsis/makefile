# ***********************************************
# Compile NRLMSIS
FC := gfortran
FFLAGS := -w -O2 -ffixed-line-length-132

EXEC := msise

MSISOBJS := ../nrlmsis/nrlmsise00_sub.o

%.o: %.for
	$(FC) $(FFLAGS) -g -fno-automatic -c $< -o $@

%.o: %.f90
	$(FC) $(FFLAGS) -g -fno-automatic -c $< -o $@

all: $(EXEC)

msise: $(MSISOBJS) msise_run.o

$(EXEC):
	$(FC) -o $@ $^

.PHONY: all clean

clean:
	find . -name "*~" -o -name "*.o" | xargs rm -f $(EXEC)
	find ../nrlmsis -name "*~" -o -name "*.o" | xargs rm -f $(EXEC)