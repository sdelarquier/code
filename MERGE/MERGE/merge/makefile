# makefile for rad_merge
# Last Modification: 03-OCT-2002
# Author: Dieter Andre

IPATH  = $(SD_TOP)/include
FIT = $(SD_TOP)/radops/fitread
MRG = $(SD_TOP)/merge/routines

CFLAGS=  -c -g -fPIC -I$(IPATH)
FFLAGS=  -c -g -W -f -N111
FLINK= -L/opt/absoft/lib -lU77 -lfio -lf77math


.f.o:
	f77 $(FFLAGS) -I$(IPATH) -o $@ $<

.c.o:
	$(CC) $(CFLAGS) -o $@ $<

MERGOBJS = $(MRG)/avrg_scan.o $(MRG)/read_init.o $(MRG)/corr_azim.o $(MRG)/div_free.o $(MRG)/div_calc.o $(MRG)/div_correct.o $(MRG)/div_extend.o $(MRG)/div_der.o $(MRG)/solve.o $(MRG)/c_contra.o $(MRG)/letter.o $(MRG)/look_table.o $(MRG)/make_table.o $(MRG)/merge_info.o $(MRG)/merge_vec.o $(MRG)/model.o $(MRG)/mrg_name.o $(MRG)/one_scan.o $(MRG)/pair.o $(MRG)/pair_name.o $(MRG)/quality_map.o $(MRG)/quality_pnt.o $(MRG)/radar_name.o $(MRG)/read_scan.o $(MRG)/station.o $(MRG)/virtual.o $(MRG)/hor_proj.o $(MRG)/dvcal.o $(MRG)/gaussj.o $(MRG)/tests.o $(MRG)/trigo.o $(MRG)/project_b.o $(MRG)/cp_run.o $(MRG)/pi_name.o $(FIT)/read_datrec.o

OBJS = merge.o

rad_merge:		$(OBJS)  $(MERGOBJS) $(IPATH)/info.inc
	f77 -g -W -f -o $(SD_TOP)/bin/rad_merge $(OBJS) $(MERGOBJS) -L/home/darn/lib -lfit $(FLINK) -lm -lc
	





