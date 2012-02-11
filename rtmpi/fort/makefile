# 
FC := mpif90
FFLAGS := -w -O2 -fbacktrace

EXEC := rtmpi/raydarn

IRIDIR := iri/
IRIOBJS := $(IRIDIR)irisub.o $(IRIDIR)irifun.o $(IRIDIR)iritec.o $(IRIDIR)iridreg.o $(IRIDIR)cira.o $(IRIDIR)iriflip.o
IGRFDIR := igrf/
IGRFOBJS := $(IGRFDIR)igrf.o
RTDIR := rtmpi/
RTOBJS := $(RTDIR)constants.o $(RTDIR)MPIutils.o

%.o: %.for
	$(FC) $(FFLAGS) -ffixed-line-length-132 -g -fno-automatic -c $< -o $@

%.o: %.f90
	$(FC) $(FFLAGS) -g -fno-automatic -c $< -o $@

all: 	$(EXEC)

rtmpi/raydarn: $(IRIOBJS) $(IGRFOBJS) $(RTOBJS) rtmpi/raytrace_mpi.o

$(EXEC):
	$(FC) -o $@ $^
	find . -maxdepth 1 -name "*.mod" | xargs -i cp {} -f rtmpi/
	find . -maxdepth 1 -name "*.mod" | xargs -i rm {} -f


.PHONY: all clean

clean:
	find rtmpi -name "*~" -o -name "*.o" -o -name "*.mod" | xargs rm -f $(EXEC)
	find iri -name "*~" -o -name "*.o" | xargs rm -f $(EXEC)
	find igrf -name "*~" -o -name "*.o" | xargs rm -f $(EXEC)
