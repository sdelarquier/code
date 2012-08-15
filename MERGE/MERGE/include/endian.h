
#ifndef _qnx_sys_endian_h_included
#define    _qnx_sys_endian_h_included

#ifdef __cplusplus
extern "C" {
#endif

/*
    The data types 'be_32', 'be_24' and 'be_16' are for
    BIG ENDIAN 32 bit, 24 bit and 16 bit resp.
    The corresponding 'swabs' will assign with reversed byte ordering
    as appropriate.

 */

#if 0
typedef uchar be_16[2];
typedef    uchar be_24[3];
typedef    uchar be_32[4];
#else
typedef unsigned long be_32;
typedef unsigned short be_16;
typedef struct { unsigned char x[3];} be_24;
#endif

typedef	char * sd_cdt;

#define swab_32(x,y) ((x)[0] = (y)[3], (x)[1] = (y)[2], (x)[2] = (y)[1], (x)[3] = (y)[0])
#define swab_16(x,y) ((x)[0] = (y)[1],  (x)[1] = (y)[0])

#define swab_24(x,y) ((x)[0] = (y)[2],(x)[1] = (y)[1], (x)[2] = (y)[0])

#define swab_dword(x,y)  (swab_32((sd_cdt)(x),(sd_cdt)(y)))
#define swab_word(x,y)   (swab_16((sd_cdt)(x),(sd_cdt)(y)))


#define swap4to4(x, y)    swab_32(x,y)
#define swap3to3(x, y)    swab_24(x,y)
#define swap2to2(x, y)    swab_16(x,y)

/* the following macro must be called with a pointer to an int
	for example in your calling program you could use:

	int i, n;
	n = endian(&i);
	
	the result of n will be 0 if the machine is bigendian and 1 if it
	is little endian.  NOTE:  as a side effect, the value of the argument
	is either 256 (big) or 1 (little).  */
	
#define endian(x) ((*((sd_cdt)(x))=(char)1),(*(((sd_cdt)(x))+1)=0),((*((short *)(x))>1)? 0 : 1))



#ifdef __cplusplus
}
#endif

#endif
