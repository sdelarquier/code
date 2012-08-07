/* This header file defines the structure used by the functions 
   _splitpath and _makepath.  It also defines the function prototypes */

#ifndef _SD_PATH_H_INCLUDED
#ifndef _MAX_PATH
#define _MAX_PATH 127
#define _MAX_NODE 24
#define _MAX_DIR 127
#define _MAX_FNAME 127
#define _MAX_EXT 127
#endif

struct PATH_STRUCT {
	char node[_MAX_NODE];
	char dir[_MAX_DIR];
	char fname[_MAX_FNAME];
	char ext[_MAX_EXT];
    };

extern int _splitpath(char *, char *, char *, char *, char *);
extern int _makepath (char *, char *, char *, char *, char *);

#define _SD_PATH_H_INCLUDED
#endif

