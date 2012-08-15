#	This make file works on an HP_UX unix system.
#	It will have to be modified to work on other
#	systems.
#
#	This is the make file for the 1995 epoch PACE Magnetic Coordinates
#
#
#	If you want to use the MLT routines, you will also need the
#	routine for converting yr, month, day, hour, minute, sec
#	to yr_sec.  This routine is called cnv$mdhms_sec.
#
#	The routine sfctest was provided by PHillips Lab. and creates
#	a table of magnetic coordinates.
#
#	The routine coordcnv is a little driver that asks the user to
#	input a latitude and longitude and will then return the converted
#	coordinates.  
#
#	NOTE:  in order to use the 1990 version of the coordinate
#	system you must have the two data files, aacgm_coeffs1990.asc
#	and aacgm_coeffs1995.asc.
#
# define the directory for header and include files
#
INC_DIR = /project/radar/include
#
# define the default flags for compilation
# C must specify position independent code (+z) and allow 
#    dollar signs in names (the fortran convert subroutine
#    is sfc$$convert_geo_coord.
#
# Fortran must specify VAX/VMS extensions (+e) 
#    extended source lines (+es) and position independent code (+z)
#
CFLAGS = -c  +z -D_HPUX_SOURCE -I$(INC_DIR) +e
FFLAGS = -c  +es +e +z -C -I$(INC_DIR) 

CC = /bin/c89

PGM_OBJS = cnvcoord.o sfc_convert_geo_coord.o rylm.o cgm_to_altitude.o  altitude_to_cgm.o sfc_put_user_msg.o aacgm_init.o

MLT_OBJS = mlt.o mlt1.o eqn_of_time.o solar_loc.o

.f.o:
	f77 $(FFLAGS) -o $@ $<

.c.o:	
	$(CC) $(CFLAGS) -o $@ $<

ALL: sfctest coordcnv coordcnv90 libpgm.sl

sfctest:	sfctest.o $(PGM_OBJS)
	f77 -g -o sfctest sfctest.o $(PGM_OBJS) -lm

coordcnv90:	coordcnv90.o $(PGM_OBJS)
	f77 -g -o coordcnv90 coordcnv90.o -lpgm -lm

coordcnv:	coordcnv.o $(PGM_OBJS)
	f77 -g -o coordcnv coordcnv.o -lpgm -lm

libpgm.sl:	$(PGM_OBJS) $(MLT_OBJS)
	ld -b -o libpgm.sl $(PGM_OBJS) $(MLT_OBJS)

