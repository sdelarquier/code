# ~/radops/fitread/makefile
# Last Modification: 18-DEC-2001
# Author: Dieter Andre

SD_TOP= /home/darn
IPATH= $(SD_TOP)/include
LPATH= $(SD_TOP)/lib

CFLAGS=  -c -g -I$(IPATH)
FFLAGS=  -c -g -fugly-logint -fdollar-ok -ffixed-line-length-none -fno-underscoring
FLINK= -L/usr/lib/gcc-lib/i486-suse-linux/2.95.3 -lg2c -lgcc

.f.o:
	g77 $(FFLAGS) -I$(IPATH) -o $@ $<

.c.o:
	$(CC) $(CFLAGS) -o $@ $<



concat:		concat.o
	gcc -o $(SD_TOP)/bin/concat concat.o -L$(LPATH) -lgen


printpv:	printpv.f
	g77 -o printpv printpv.f -L$(LPATH) -lfit -laacgm  -lgen -lm







