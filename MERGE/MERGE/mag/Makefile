# Makefile for the HF radar IGRF programs

I_PATH = $(SD_TOP)/include

.f.o:
	f77 -c +OP4 +O3 +es +z -I$(IPATH) -o $@ $<
.c.o:
	/bin/c89 -c +z +e -I$(IPATH) -D_HPUX_SOURCE -o $@ $<

igrftest: igfrtest.o
	f77 -v +OP4P +O3 -C +T -o igrftest igrftest.o -l mag -l gen

