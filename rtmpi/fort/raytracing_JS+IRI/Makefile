FC := gfortran
FFLAGS := -w -ffixed-line-length-132 -O2 -fbacktrace

ifdef RELEASE
	FFLAGS += -w -O2
endif

EXEC := raytrace grppath raypos kvect

IRIDIR := ./
IRIOBJS := $(IRIDIR)irisub.o $(IRIDIR)irifun.o $(IRIDIR)iritec.o $(IRIDIR)iridreg.o $(IRIDIR)cira.o
IGRFDIR := ./
IGRFOBJS := $(IGRFDIR)igrf.o 

%.o: %.for
	$(FC) $(FFLAGS) -g -fno-automatic -c $< -o $@

%.o: %.f
	$(FC) $(FFLAGS) -g -fno-automatic -c $< -o $@

all: $(EXEC)

raytrace: $(IRIOBJS) $(IGRFOBJS) rayjones.o
grppath: grppath.o
raypos: raypos.o
kvect: kvect.o

$(EXEC):
	$(FC) -o $@ $^ $(LDFLAGS)

.PHONY: all clean

clean:
	find . -name "*~" -o -name "*.o" | xargs rm -f $(EXEC)
