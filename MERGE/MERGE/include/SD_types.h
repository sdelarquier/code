#ifndef _SD_types_
/* This defines the SuperDARN int16, int32, and int64 types */
#include <limits.h>
#ifdef WORD_BIT
#if WORD_BIT == 16
typedef int int16;
typedef long int32;
#endif
#if WORD_BIT == 32
typedef short int16;
typedef int int32;
#endif
#elif defined LONG_BIT
#if LONG_BIT == 32
typedef short int16;
typedef long int32;
#endif
#if LONG_BIT == 64
typedef short int16;
typedef int int32;
#endif
#else
typedef short int16;
typedef long int32;
#endif
#ifdef LONG_BIT
#if LONG_BIT == 64
typedef long int64;
#endif
#endif
#define _SD_types_
#endif
