# ~/radops/fitread/makefile
# Last Modification: 29-NOV-2004
# Author: Dieter Andre

# RAW_HEADERS = /home/darn/include/radops.h /home/darn/include/rawfile.h

SD_TOP= /home/darn
IPATH= $(SD_TOP)/include
LPATH= $(SD_TOP)/lib

CFLAGS=  -c -g -I$(IPATH)

.c.o:
	$(CC) $(CFLAGS) -o $@ $<


print_raw: print_raw.o
	gcc -g -o print_raw print_raw.o -L $(LPATH) -lraw -lacf -lgen -lm

create_raw_index: create_raw_index.o
	gcc -g -o create_raw_index create_raw_index.o -L $(LPATH) -lraw -lgen -lm

print_raw_file_info: print_raw_file_info.o $(OBJS)
	gcc -g -o print_raw_file_info print_raw_file_info.o -L $(LPATH) -lraw -lgen -lm

