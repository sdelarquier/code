# my own version of the make file
# Last Modification: 15-JUL-2003
# use of the Absoft compiler
# for use under linux
# inlcude extra GO routines
# 20020228 : library for Tsyganenko models
# 20021003 : remove libmerge.so and link rad_merge statically
# Dieter Andre'

SD_TOP= /home/darn
AACGM = $(SD_TOP)/aacgm
FIT = $(SD_TOP)/radops/fitread
RAW = $(SD_TOP)/radops/rawread
ACF = $(SD_TOP)/radops/fitacf
GO = $(SD_TOP)/radops/go
GEN = $(SD_TOP)/gen
RB  = $(SD_TOP)/gen
MAG = $(SD_TOP)/mag
# MRG = $(SD_TOP)/merge/routines
READ= $(SD_TOP)/newprogs
ELLI= $(SD_TOP)/elli
COND= $(SD_TOP)/cond
IRI95= $(SD_TOP)/iri95
PER= $(SD_TOP)/newprogs
TSYG= $(SD_TOP)/tsyganenko

IPATH = $(SD_TOP)/include
LPATH = $(SD_TOP)/lib

#ok
# took out $(FIT)/block_read.o
# moved  read_datrec.o to MRG
FITOBJS = $(FIT)/find_fit_rec.o $(FIT)/fitropen.o  $(FIT)/fit_close.o  $(FIT)/fit_file_err.o   $(FIT)/read_fit.o $(FIT)/read_fit386_v110.o $(FIT)/read_fit386_v130.o $(FIT)/read_fit386_syowa.o  $(FIT)/read_fit_dg.o $(FIT)/swap_dg0.o $(FIT)/swap_rrn1.o $(FIT)/swap_v11_r0.o $(FIT)/swap_v13_r0.o $(FIT)/fitropen_idl.o $(FIT)/find_fit_rec_idl.o $(FIT)/read_fit_idl.o $(FIT)/fit_close_idl.o $(FIT)/bytjug.o

# these two still use rbpos_ !!!
# GOOBJS = $(GO)/beam_read.o $(GO)/scan_read.o $(GO)/son_of_scan_read.o
GOOBJS = $(GO)/son_of_scan_read.o

#ok
RAWOBJS = $(RAW)/find_raw_rec.o  $(RAW)/raw_close.o  $(RAW)/raw_read.o  $(RAW)/rawropen.o  $(RAW)/read_raw_dg.o  $(RAW)/swap_rawparms.o  $(RAW)/decommutate.o  $(RAW)/depack.o  $(RAW)/oldparms_newparms.o  $(RAW)/dg_pulse.o $(RAW)/raw_file_info.o $(RAW)/find_raw_rec_idl.o $(RAW)/raw_close_idl.o $(RAW)/rawropen_idl.o $(RAW)/read_raw_idl.o $(RAW)/raw_file_info_idl.o $(RAW)/identify.o

# ok
ACFOBJS = $(ACF)/badlags.o $(ACF)/rang_badlags.o $(ACF)/badlags_idl.o

# ok
AACGMOBJS = $(AACGM)/cnvcoord.o $(AACGM)/cnvcoord_idl.o $(AACGM)/sfc_convert_geo_coord.o $(AACGM)/rylm.o $(AACGM)/cgm_to_altitude.o $(AACGM)/altitude_to_cgm.o $(AACGM)/sfc_put_user_msg.o $(AACGM)/aacgm_init.o

# ok
MLTOBJS = $(AACGM)/mlt.o $(AACGM)/mlt1.o $(AACGM)/mlt_idl.o $(AACGM)/eqn_of_time.o $(AACGM)/solar_loc.o $(AACGM)/cnv_mlt_long.o $(AACGM)/cnv_mlt_long_idl.o $(GEN)/cnv_mdhms_sec.o $(GEN)/cnv_sec_mdhms.o 

# ok; missing invchrt,  gscat_pos_idl. fldpntn is not used
RBPOSOBJS =  $(RB)/cartosph.o $(RB)/fldpnt.o $(RB)/fldpnth.o $(RB)/geocnvrt.o $(RB)/geodtgc.o $(RB)/glbthor.o $(RB)/gsptcar.o $(RB)/hortmag.o $(RB)/hsptcar.o $(RB)/norm_vec.o $(RB)/radar_pos.o $(RB)/radar_pos_idl.o $(RB)/range_gate.o $(RB)/rbpos_gen.o $(RB)/slant_range.o $(RB)/sphtocar.o $(RB)/sphtocarvect.o $(RB)/rbpos_idl.o $(RB)/geochrt.o $(RB)/radcoor.o $(RB)/rdkelaz.o $(RB)/geochrt_idl.o $(RB)/gs_fldpnth.o

#ok still to add: $(GEN)/cnv_doy.o $(FIT)/bytjug.o $(GEN)/yes_no.o; missing locbg
GENOBJS = $(GEN)/cnv_mdhms_sec.o $(GEN)/cnv_sec_mdhms.o $(GEN)/quicksort.o $(GEN)/_makepath.o $(GEN)/_splitpath.o $(GEN)/smooth.o $(GEN)/trim.o $(GEN)/hardware.o $(GEN)/cnv_mdhms_sec_idl.o $(GEN)/cnv_sec_mdhms_idl.o $(GEN)/sd_swab.o $(GEN)/median_filter.o $(GEN)/median_filter_idl.o $(GEN)/block_cnv_sec_mdhms.o $(GEN)/strcrep.o $(GEN)/monday.o

#ok the old darn/mag has a file dgrf90.dat
MAGOBJS = $(MAG)/igrfcall.o $(MAG)/shval3.o $(MAG)/getshc.o $(MAG)/extrapshc.o $(MAG)/interpshc.o $(MAG)/magcmp.o $(MAG)/magcmp_idl.o  $(MAG)/igrf_idl.o

# ok
# all of this is now in ~/merge/makefile
# took out $(MRG)/lnblnk.o , part of absoft libU77
# MERGOBJS = $(MRG)/avrg_scan.o $(MRG)/read_init.o $(MRG)/corr_azim.o $(MRG)/div_free.o $(MRG)/div_calc.o $(MRG)/div_correct.o $(MRG)/div_extend.o $(MRG)/div_der.o $(MRG)/solve.o $(MRG)/c_contra.o $(MRG)/letter.o $(MRG)/look_table.o $(MRG)/make_table.o $(MRG)/merge_info.o $(MRG)/merge_vec.o $(MRG)/model.o $(MRG)/mrg_name.o $(MRG)/one_scan.o $(MRG)/pair.o $(MRG)/pair_name.o $(MRG)/quality_map.o $(MRG)/quality_pnt.o $(MRG)/radar_name.o $(MRG)/read_scan.o $(MRG)/station.o $(MRG)/virtual.o $(MRG)/hor_proj.o $(MRG)/dvcal.o $(MRG)/gaussj.o $(MRG)/tests.o $(MRG)/trigo.o $(MRG)/project_b.o $(MRG)/cp_run.o $(MRG)/pi_name.o $(FIT)/read_datrec.o

READOBJS= $(READ)/readlib.o $(READ)/read_mtl.o $(READ)/read_get_apl.o $(READ)/calc_gcd.o

ELLIOBJS= $(ELLI)/ellipfilt.o $(ELLI)/elli.o

CONDOBJS= $(COND)/hgrm.o $(COND)/pedhall.o $(COND)/solar.o $(COND)/solarpos.o

IRIOBJS = $(IRI95)/irif13.o $(IRI95)/iris13.o $(IRI95)/irit13.o $(IRI95)/cira86.o $(IRI95)/iri_web_idl.o

PEROBJS= $(PER)/period.o

TSYGOBJS= $(TSYG)/geopack.o $(TSYG)/locate.o $(TSYG)/t01_01.o $(TSYG)/t89c.o $(TSYG)/t96.o $(TSYG)/btrace.o $(TSYG)/gsetogsm.o $(TSYG)/modelt96.o $(TSYG)/tsyg_idl.o

#.f.o:
#	f90 -c -g +extend_source +z -I$(IPATH) -o $@ $<
#.c.o:
#
#	/bin/c89 -c -g +z +e -I$(IPATH) -D_HPUX_SOURCE -o $@ $<


CFLAGS=  -c -g -fPIC -I$(IPATH)
# FFLAGS=  -c -g -fugly-logint -fdollar-ok -ffixed-line-length-none -fno-underscoring $(STATIC)
# FLINK= -L/usr/lib/gcc-lib/i486-suse-linux/2.95.3 -lg2c -lgcc
FFLAGS=  -c -g -W -f -N111
FLINK= -L/opt/absoft/lib -lU77 -lfio -lf77math


# .f.o:
# 	g77 $(FFLAGS) -I$(IPATH) -o $@ $<

.f.o:
	f77 $(FFLAGS) -I$(IPATH) -o $@ $<

.c.o:
	$(CC) $(CFLAGS) -o $@ $<


libgen.so:	$(GENOBJS)
	ld -share -o libgen.so $(GENOBJS) $(FLINK) -lm -lc

libaacgm.so:	$(AACGMOBJS) $(MLTOBJS)
	ld -share -o libaacgm.so $(AACGMOBJS) $(MLTOBJS) $(FLINK) -lm -lc

libacf.so:	$(ACFOBJS)
	ld -share -L$(LPATH) -o libacf.so $(ACFOBJS) -lgen $(FLINK) -lm -lc

libraw.so:	$(RAWOBJS)
	ld -share -L$(LPATH) -o libraw.so $(RAWOBJS) -lacf -lgen $(FLINK) -lm -lc

libmag.so:	$(MAGOBJS)
	ld -share -L$(LPATH) -o libmag.so $(MAGOBJS) -lgen $(FLINK) -lm -lc

librbpos.so:	$(RBPOSOBJS)
	ld -share -L$(LPATH) -o librbpos.so $(RBPOSOBJS) -lmag -laacgm $(FLINK) -lm -lc

libfit.so:	$(FITOBJS) $(GOOBJS)
	ld -share -L$(LPATH) -o libfit.so $(FITOBJS) $(GOOBJS) -laacgm -lgen -lrbpos $(FLINK) -lm -lc

# libmerge.so:	$(MERGOBJS)
#	ld -share -L$(LPATH) -o libmerge.so $(MERGOBJS) -lfit -laacgm -lgen -lrbpos $(FLINK) -lm -lc

# read mrg and/or los files
libread.so:	$(READOBJS)
	ld -share -L$(LPATH) -o libread.so $(READOBJS) -laacgm $(FLINK) -lm -lc

# elliptic filter
libelli.so:	$(ELLIOBJS)
	ld -share -o libelli.so $(ELLIOBJS) -lm -lcl -lc

# conductivity model
libcond.so:	$(CONDOBJS)
	ld -share -L$(LPATH) -o libcond.so $(CONDOBJS) -lm -laacgm -lc

libiri.so:	$(IRIOBJS)
	ld -share -o libiri.so $(IRIOBJS) $(FLINK) -lm -lc

libper.so:	$(PEROBJS)
	ld -share -o libper.so $(PEROBJS) /home/andre/raytrace/nrutil.o -lm 

# set STATIC=-fno-automatic
libtsyg.so:	$(TSYGOBJS)
	ld -share -o libtsyg.so $(TSYGOBJS) $(FLINK) -lm -lc























