/*
+---------------------------------------------------------------------+
|  Copyright(c) 1975-93 Regents of the University of California       |
|  All rights reserved.                                               |
|                                                                     |
|  Redistribution and use in source and binary forms are permitted    |
|  provided that the above copyright notice and this paragraph are    |
|  duplicated in all such forms and that any documentation,           |
|  advertising materials, and other materials related to such         |
|  distribution and use acknowledge that the software was developed   |
|  by the University of California, Los Angeles.  The name of the     |
|  University may not be used to endorse or promote products derived  |
|  from this software without specific prior written permission.      |
|  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR     |
|  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED     |
|  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.|
+---------------------------------------------------------------------+

*/

#include <ctype.h>
#include <math.h>
/*
************************************************************
NAME
  ebcdic2ascii - Convert EBCDIC characters to ASCII.
SYNOPSIS
  SUN C
    void ebcdic2ascii_(string,len)
    char *string;
    int len;
  HP9000 C
    void ebcdic2ascii(string,len)
    char *string;
    int len;
  FORTRAN
    call ebcdic2ascii(string)
    character*(*) string
DESCRIPTION
   Convert EBCDIC characters to ASCII, in place.  This
   uses the map contained in the VMS routine LIB$TRA_EBC_ASC.
   Any EBCDIC characters which have no ASCII equivalents
   are converted to '\'.  Other strange translations are:

   EBCDIC		Ascii
   IBM logical not	^
   cent sign		[
   !			]
   |			!

   The length of the string is passed.
RETURN VALUE
  no value, except value of argument is changed.
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics   Feb 89
UPDATES
  Used VMS translations instead of ones determined from charts. May 91
**************************************************************
*/
#ifdef sun
void ebcdic2ascii_ (string,len)
#else
void ebcdic2ascii (string,len)
#endif
unsigned char string[];
int len;
{
  static char table[]={						/* octal */
     '\000','\001','\002','\003',  '\\','\011',  '\\','\177',  /* 000 */
       '\\',  '\\',  '\\','\013','\014','\015','\016','\017',  /* 010 */
     '\020','\021','\022','\023',  '\\',  '\\','\010',  '\\',  /* 020 */
     '\030','\031',  '\\',  '\\','\034','\035','\036','\037',  /* 030 */
       '\\',  '\\',  '\\',  '\\',  '\\','\012','\027','\033',  /* 040 */
       '\\',  '\\',  '\\',  '\\',  '\\','\005','\006','\007',  /* 050 */
       '\\',  '\\','\026',  '\\',  '\\',  '\\',  '\\','\004',  /* 060 */
       '\\',  '\\',  '\\',  '\\','\024','\025',  '\\','\032',  /* 070 */
	' ',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 100 */
       '\\',  '\\',   '[',   '.',   '<',   '(',   '+',   '!',  /* 110 */
	'&',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 120 */
       '\\',  '\\',   ']',   '$',   '*',   ')',   ';',   '^',  /* 130 */
	'-',   '/',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 140 */
       '\\',  '\\',   '|',   ',',   '%',   '_',   '>',   '?',  /* 150 */
       '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 160 */
       '\\',   '`',   ':',   '#',   '@',  '\'',   '=',  '\"',  /* 170 */
       '\\',   'a',   'b',   'c',   'd',   'e',   'f',   'g',  /* 200 */
	'h',   'i',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 210 */
       '\\',   'j',   'k',   'l',   'm',   'n',   'o',   'p',  /* 220 */
	'q',   'r',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 230 */
       '\\',   '~',   's',   't',   'u',   'v',   'w',   'x',  /* 240 */
	'y',   'z',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 250 */
       '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 260 */
       '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 270 */
	'{',   'A',   'B',   'C',   'D',   'E',   'F',   'G',  /* 300 */
	'H',   'I',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 310 */
	'}',   'J',   'K',   'L',   'M',   'N',   'O',   'P',  /* 320 */
	'Q',   'R',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 330 */
       '\\',  '\\',   'S',   'T',   'U',   'V',   'W',   'X',  /* 340 */
	'Y',   'Z',  '\\',  '\\',  '\\',  '\\',  '\\',  '\\',  /* 350 */
	'0',   '1',   '2',   '3',   '4',   '5',   '6',   '7',  /* 360 */
	'8',   '9',  '\\',  '\\',  '\\',  '\\',  '\\','\377'}; /* 370 */

  int i;
  for (i=0; i < len; i++) 
    string[i] = table[string[i]];
  return;
}
/*
************************************************************
NAME
  ascii2ebcdic - Convert ASCII characters to EBCDIC.
SYNOPSIS
  SUN C
    void ascii2ebcdic_(string,len)
    char *string;
    int len;
  HP9000 C
    void ascii2ebcdic(string,len)
    char *string;
    int len;
  FORTRAN
    call ascii2ebcdic(string)
    character*(*) string
DESCRIPTION
   Convert ASCII characters to EBCDIC, in place.  This
   uses the map contained in the VMS routine LIB$TRA_ASC_EBC.
   Any ASCII characters which have no EBCDIC equivalents
   are converted to EBCDIC '?'.

   The length of the string is passed.
RETURN VALUE
  no value, except value of argument is changed.
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics   May 91
UPDATES
**************************************************************
*/
#ifdef sun
void ascii2ebcdic_ (string,len)
#else
void ascii2ebcdic (string,len)
#endif
unsigned char string[];
int len;
{
  static char table[]={						  /* octal */
       '\000','\001','\002','\003','\067','\055','\056','\057',  /* 000 */
       '\026','\005','\045','\013','\014','\015','\016','\017',  /* 010 */
       '\020','\021','\022','\023','\074','\075','\062','\046',  /* 020 */
       '\030','\031','\077','\047','\034','\035','\036','\037',  /* 030 */
       '\100','\117','\177','\173','\133','\154','\120','\175',  /* 040 */
       '\115','\135','\134','\116','\153','\140','\113','\141',  /* 050 */
       '\360','\361','\362','\363','\364','\365','\366','\367',  /* 060 */
       '\370','\371','\172','\136','\114','\176','\156','\157',  /* 070 */
       '\174','\301','\302','\303','\304','\305','\306','\307',  /* 100 */
       '\310','\311','\321','\322','\323','\324','\325','\326',  /* 110 */
       '\327','\330','\331','\342','\343','\344','\345','\346',  /* 120 */
       '\347','\350','\351','\112','\340','\132','\137','\155',  /* 130 */
       '\171','\201','\202','\203','\204','\205','\206','\207',  /* 140 */
       '\210','\211','\221','\222','\223','\224','\225','\226',  /* 150 */
       '\227','\230','\231','\242','\243','\244','\245','\246',  /* 160 */
       '\247','\250','\251','\300','\152','\320','\241','\007',  /* 170 */
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\077',
       '\077','\077','\077','\077','\077','\077','\077','\377'};
  int i;
  for (i=0; i < len; i++) 
    string[i] = table[string[i]];
  return;
}
/*
************************************************************
NAME
  casefold - Convert ASCII characters to upper case.
  uppercase - Convert ASCII characters to upper case.
  lowercase - Convert ASCII characters to lower  case.
SYNOPSIS
  SUN C
    void casefold_(string,len)
    void uppercase_(string,len)
    void lowercase_(string,len)
    char *string;
    int len;
  HP9000 C
    void casefold(string,len)
    void uppercase(string,len)
    void lowercase(string,len)
    char *string;
    int len;
  FORTRAN
    call casefold(string)
    call uppercase(string)
    call lowercase(string)
    character*(*) string
DESCRIPTION
  Convert character string to upper or lower, in place.
  In C, the length of the string is passed, so that the string
  does not need to be null terminated.
RETURN VALUE
  None.  Contents of string are changed.
ERRORS
CALLS
  ctype macros: islower(), toupper(), isupper(), tolower()
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics   March 89.
UPDATES
**************************************************************
*/
#ifdef sun
void casefold_ (string,len)
#else
void casefold (string,len)
#endif
char string[];
int len;
{
  int i;
  for (i=0; i < len; i++,string++) 
    if (islower(*string)) *string=toupper(*string);
}
#ifdef sun
void uppercase_ (string,len)
#else
void uppercase (string,len)
#endif
char string[];
int len;
{
  int i;
  for (i=0; i < len; i++,string++) 
    if (islower(*string)) *string=toupper(*string);
}
#ifdef sun
void lowercase_ (string,len)
#else
void lowercase (string,len)
#endif
char string[];
int len;
{
  int i;
  for (i=0; i < len; i++,string++) 
    if (isupper(*string)) *string=tolower(*string);
}
/*
************************************************************
NAME
  r4decieee - Convert DEC/VAX single precision floating point to
	      IEEE representation.
SYNOPSIS
  SUN C
    void r4decieee_(f)
    float *f;
  HP9000 C
    void r4decieee(f)
    float *f;
  FORTRAN
    call r4decieee(f)
    real f
DESCRIPTION
  This routine converts the datum in place: the contents of f are
  changed.  VAX reserved operands are converted to NaNs.
  Non-normalized IEEE values are supported.
RETURN VALUE
  None.  f is changed.
ERRORS
  Can return a NaN if DEC/VAX datum is a reserved operand.
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics, Feb 89
UPDATES
**************************************************************
*/
#ifdef sun
void r4decieee_(num)
#else
void r4decieee(num)
#endif
float *num;
{
  unsigned char *byteptr;
  union {
    unsigned char bytes[4];
    struct decfloat {
      unsigned int mant2:16;
      unsigned int sign:1;
      unsigned int exp:8;
      unsigned int mant1:7;
    } fields;
  } dec;
  union {
    float val;
    struct ieeefloat {
      unsigned int sign:1;
      unsigned int exp:8;
      unsigned int mant:23;
    } fields;
  } ieee;
  int exp;
  unsigned long int mant;

/* Swap bytes. */
  byteptr = (unsigned char *) num;
  dec.bytes[0] = *(byteptr+3);
  dec.bytes[1] = *(byteptr+2);
  dec.bytes[2] = *(byteptr+1);
  dec.bytes[3] = *(byteptr);
 
  mant = (dec.fields.mant1<<16) + dec.fields.mant2;
  if ((exp=dec.fields.exp) < 3) {
    if (exp==0) {
      if (!dec.fields.sign) {
	*num = 0.;
	return;
      }
/*
   According to Appendix C of "Programming in VAX Fortran"
   if the exponent is zero, and the sign bit is set the
   number is illegal.  Set this to NaN.
*/
      exp = 257;   /* NaN: Not a number */
      mant = 0x400000l;
    }
    else {
/* Correct for un-normalized numbers.  */
      mant = (0x800000l + mant) >> (3-exp);
      exp = 2;
    }
  }

  ieee.fields.mant = mant;
  
  ieee.fields.exp = exp-2;  /* 127 - 128 - 1 */
  ieee.fields.sign = dec.fields.sign;
  *num = ieee.val;
  return;
}
/*
************************************************************
NAME
  r8decieee - Convert DEC/VAX double precision floating point to
	      IEEE representation.
SYNOPSIS
  SUN C
    void r8decieee_(f)
    double *f;
  HP9000 C
    void r8decieee(f)
    double *f;
  FORTRAN
    call r8decieee(f)
    double precision f
DESCRIPTION
  This routine converts the datum in place: the contents of f are
  changed.  VAX reserved operands are converted to NaNs.
  Non-normalized IEEE values are possible supported.
RETURN VALUE
  None.  f is changed.
ERRORS
  Can return a NaN if DEC/VAX datum is a reserved operand.
CALLS  
  memcpy on HP9000, to avoid alignment problems.
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics, Feb 89
UPDATES
**************************************************************
*/
#ifdef sun
void r8decieee_(num)
double *num;
#else
void r8decieee(num)
char *num; 			/* Avoid alignment problems on HP9000 */
#endif
{
  unsigned char *byteptr;
  union {
    struct decfloat {
      unsigned int mant3:32;
      unsigned int mant2:16;
      unsigned int sign:1;
      unsigned int exp:8;
      unsigned int mant1:7;
    } fields;
    unsigned char bytes[8];
  } dec;
  union {
    struct ieeefloat {
      unsigned int sign:1;
      unsigned int exp:11;
      unsigned int mant1:20;
      unsigned int mant2:32;
    } fields;
    double val;
  } ieee;
  unsigned long int mant;
  int exp;
  double x;

/* Swap bytes. */
  byteptr = (unsigned char *) num;
  dec.bytes[0] = *(byteptr+5);
  dec.bytes[1] = *(byteptr+4);
  dec.bytes[2] = *(byteptr+7);
  dec.bytes[3] = *(byteptr+6);
  dec.bytes[4] = *(byteptr+3);
  dec.bytes[5] = *(byteptr+2);
  dec.bytes[6] = *(byteptr+1);
  dec.bytes[7] = *(byteptr);

  if ((exp=dec.fields.exp)==0) {
    if (!dec.fields.sign) {
      ieee.val = 0.;
      goto ret;
    }
/*
   According to Appendix C of "Programming in VAX Fortran"
   if the exponent is zero, and the sign bit is set the
   number is illegal.  Set this to NaN.
*/
    ieee.fields.exp=2047;
    ieee.fields.mant1 = 1;
    goto ret;
  }  
  mant = 0x800000l + (dec.fields.mant1<<16) + dec.fields.mant2;
  ieee.val = (double) mant;
/* 
  No problem with underflow/overflow with an 11 bit IEEE exponent.
*/
  ieee.fields.exp += - 24 + exp - 128;

  if (dec.fields.mant3 != 0) {
    x = ieee.val;
    ieee.val = (double) dec.fields.mant3;
    ieee.fields.exp += - 56 + exp - 128;
    ieee.val += x;
  }

  ieee.fields.sign = dec.fields.sign;
 ret:
#ifdef hpux
  memcpy (num,&ieee.val,8);
#else
  *num = ieee.val;
#endif
  return;
}
/*
************************************************************
NAME
  i4decieee - Convert DEC/VAX 32-bit integer to IEEE representation.
  i2decieee - Convert DEC/VAX 16-bit integer to IEEE representation.
SYNOPSIS
  SUN C
    void i4decieee_(i)
    long int *i;

    void i2decieee_(i)
    short int *i;
  HP9000 C
    void i4decieee(i)
    long int *i;

    void i2decieee(i)
    short int *i;
  FORTRAN
    call i4decieee(i)
    integer*4 i

    call i2decieee(i)
    integer*2 i
DESCRIPTION
  This routine flips the bytes in integers to convert from DEC/VAX
  representation to IEEE.
RETURN VALUE
  Converted number.
ERRORS
  None.
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics, Feb 89
UPDATES
**************************************************************
*/
#ifdef sun
void i4decieee_(num)
#else
void i4decieee(num)
#endif
unsigned char num[4];
{
  unsigned char i;

  i = num[0];
  num[0] = num[3];
  num[3] = i;
  i = num[1];
  num[1] = num[2];
  num[2] = i;
  return;
}
#ifdef sun
void i2decieee_(num)
#else
void i2decieee(num)
#endif
unsigned char num[2];
{
  unsigned char i;

  i = num[0];
  num[0] = num[1];
  num[1] = i;
  return;
}
/*
************************************************************
NAME
  r4ibmieee - Convert IBM 370 single precision floating point to
	      IEEE representation.
SYNOPSIS
  SUN C
    void r4ibmieee_(f)
    float *f;
  HP9000 C
    void r4ibmieee(f)
    float *f;
  FORTRAN
    call r4ibmieee(f)
    real f
DESCRIPTION
  This routine converts the datum in place: the contents of f are
  changed.  Non-normalized IEEE values are supported.
  Note:  Integer data does not need to be converted.
RETURN VALUE
  None.  f is changed.
ERRORS
  If IBM exponent is too large, f is returned as HUGE, which is defined
  in math.h.
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics, Feb 89
UPDATES
**************************************************************
*/
#ifdef sun
void r4ibmieee_(num)
#else
void r4ibmieee(num)
#endif
float *num;
{
  union {
    float val;
    struct ibmfloat {
      unsigned int sign:1;
      unsigned int exp:7;
      unsigned int mant:24;
    } fields;
    long int ival;
  } ibm;
  union {
    float val;
    struct ieeefloat {
      unsigned int sign:1;
      unsigned int exp:8;
      unsigned int mant:23;
    } fields;
  } ieee;
  int exp;

 
  ibm.val = *num;
  if (ibm.ival == 0) return;  /* zero is zero in both computers. */
  ieee.val = (float) ibm.fields.mant;

  /* ieee.fields.exp - 24 + 4*(ibm.fields.exp - 64)   */

  exp = ieee.fields.exp + 4 * ibm.fields.exp - 280;
/*
   Use IEEE un-normalized numbers if exponent is too small.
   Add hidden bit, shift right.        
*/
  if (exp < 0) {
    ieee.fields.mant = (ieee.fields.mant + 0x800000l)>>(-exp);
    ieee.fields.exp = 0;
  }
  else if (exp > 254) {
    ieee.val = HUGE;
  }
  else ieee.fields.exp = exp;
    
  ieee.fields.sign = ibm.fields.sign;
  *num = ieee.val;
  return;
}
/*
************************************************************
NAME
  r8ibmieee - Convert IBM 370 double precision floating point to
	      IEEE representation.
SYNOPSIS
  SUN C
    void r8ibmieee_(f)
    double *f;
  HP9000 C
    void r8ibmieee(f)
    double *f;
  FORTRAN
    call r8ibmieee(f)
    double precision f
DESCRIPTION
  This routine converts the datum in place: the contents of f are
  changed.
RETURN VALUE
  None.  f is changed.
ERRORS
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics, Feb 89
UPDATES
**************************************************************
*/
#ifdef sun
void r8ibmieee_(num)
#else
void r8ibmieee(num)
#endif
double *num;
{
  union {
    double val;
    struct ibmfloat {
      unsigned int sign:1;
      unsigned int exp:7;
      unsigned int mant1:24;
      unsigned int mant2:32;
    } fields;
  } ibm;
  union {
    struct ieeefloat {
      unsigned int sign:1;
      unsigned int exp:11;
      unsigned int mant1:20;
      unsigned int mant2:32;
    } fields;
    double val;
  } ieee;
  int exp;
  double x;

  ibm.val = *num;

  if (ibm.fields.mant1 == 0 && ibm.fields.mant2 == 0) {
    *num = 0.;
    return;
  }
  exp = 4 * (ibm.fields.exp-64);

  ieee.val = (double) ibm.fields.mant1;

  ieee.fields.exp += - 24 + exp;
/*
   No problem with normalization here with an 11 bit IEEE exponent.
*/
  if (ibm.fields.mant2 != 0) {
    x = ieee.val;

    ieee.val = (double) ibm.fields.mant2;

    ieee.fields.exp += -56 + exp;
    
    ieee.val += x;
  }

  ieee.fields.sign = ibm.fields.sign;
  *num = ieee.val;
  return;
}
/*
************************************************************
NAME
  r4hp1000ieee - Convert HP1000 single precision floating point to
	      IEEE representation.
SYNOPSIS
  SUN C
    void r4hp1000ieee_(f)
    float *f;
  HP9000 C
    void r4hp1000ieee(f)
    float *f;
  FORTRAN
    call r4hp1000ieee(f)
    real f
DESCRIPTION
  This routine converts the datum in place: the contents of f are
  changed.  Note:  Integer data does not need to be converted.
RETURN VALUE
  None.  f is changed.
ERRORS
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics, Feb 89
UPDATES
**************************************************************
*/
#ifdef sun
void r4hp1000ieee_(num)
#else
void r4hp1000ieee(num)
#endif
float *num;
{
  union {
    float val;
    long int ival;
    struct hpfloat {
               int mant:24;  /* must be signed for sign extension */
      unsigned int exp:7;
      unsigned int esign:1;
    } fields;
  } hp;
  union {
    float val;
    struct ieeefloat {
      unsigned int sign:1;
      unsigned int exp:8;
      unsigned int mant:23;
    } fields;
  } ieee;
  int sign;
  long int exp,mant;

  hp.val = *num;
  if (hp.ival == 0) return;  /* zero is zero in both computers. */
  exp = hp.fields.exp;
  if (hp.fields.esign) exp |= 0xffffff80;  /* sign extend */
/*
  The HP1000 mantissa is a 24 bit signed integer.
  The implied binary point is between bits 23 &22,
  numbered from 0 (i.e. to the right of leftmost bit).
*/

  mant = hp.fields.mant;
  if (mant < 0) {
    mant *= -1;
    sign = 1;
  }
  else sign = 0;

  ieee.val = (float) mant;
  ieee.fields.exp += (-23 + exp);
  ieee.fields.sign = sign;

  *num = ieee.val;
  return;
}
/*
************************************************************
NAME      
  r8hp1000ieee - Convert HP1000 double precision floating point to
	      IEEE representation.
SYNOPSIS
  SUN C
    void r8hp1000ieee_(f)
    double *f;
  HP9000 C
    void r8hp1000ieee(f)
    double *f;
  FORTRAN
    call r8hp1000ieee(f)
    double precision f
DESCRIPTION
  This routine converts the datum in place: the contents of f are
  changed.
RETURN VALUE
  None.  f is changed.
ERRORS
CALLS
  memcpy - On HP9000 to avoid alignment problems.
AUTHOR/DATE
  Gordon Maclean  UCLA Inst. of Geophysics, Feb 89
UPDATES
**************************************************************
*/
#ifdef hpux
void r8hp1000ieee(num)
char *num;
#else
void r8hp1000ieee_(num)
double *num;
#endif
{
  union {
    double val;
    struct hpfloat {
      unsigned int mant1:32;
      unsigned int mant2:24;
      unsigned int exp:7;
      unsigned int esign:1;
    } fields;
  } hp;
  union {
    struct ieeefloat {
      unsigned int sign:1;
      unsigned int exp:11;
      unsigned int mant1:20;
      unsigned int mant2:32;
    } fields;
    double val;
  } ieee;
  int mant1,mant2,exp,sign;
  double x;

#ifdef hpux
  memcpy (&hp.val,num,8);
#else
  hp.val = *num;
#endif

  mant1 = hp.fields.mant1;  /* no hidden bit */
  if (mant1 == 0) { ieee.val = 0.; goto ret; }
  mant2 = hp.fields.mant2;
  exp = hp.fields.exp;
  if (hp.fields.esign) exp |= 0xffffff80;  /* sign extend */
/*
   If mantissa is negative, then convert to positive
   by flipping bits (exclusive or) and adding 1.
   If bit 24 of mant2 is set, carry to mant1.
*/
  if (mant1 < 0) {
    mant1 ^= 0xffffffffl;
    mant2 ^= 0x00ffffffl;
    mant2 += 1;
/*
    if ((mant2 & 0x01000000l) != 0) mant1 += 1;
*/
    sign = 1;
  }
  else sign = 0;
  ieee.val = (double) mant1;
  ieee.fields.exp += (-31 + exp);
  if (mant2 != 0) {
    x = ieee.val;

/*
    ieee.val = (double) (mant2 & 0x00ffffffl);
*/
    ieee.val = (double) mant2;
    ieee.fields.exp += (-55 + exp);
    ieee.val += x;
  }
  ieee.fields.sign = sign;

ret:
#ifdef hpux
  memcpy (num,&ieee.val,8);
#else
  *num = ieee.val;
#endif
  return;
}


/***********************************************************************
NAME
  r4ieeehp1000 - Convert IEEE single precision floating point to
	         HP-1000 representation.
SYNOPSIS
  SUN C
    void r4ieeehp1000_(f)
    float *f;
  HP9000 C
    void r4ieeehp1000(f)
    float *f;
  FORTRAN
    call r4ieeehp1000(f)
    real f
DESCRIPTION
  This routine converts the datum in place: the contents of f are
  changed.  Note:  Integer data does not need to be converted.
RETURN VALUE
  None.  f is changed.
ERRORS
AUTHOR/DATE
  Qi Chen   UCLA, Inst. of Geophysics and Planetary Physics 01/18/90
UPDATES
***********************************************************************/

#ifdef hpux
void r4ieeehp1000(num)
float *num;
#else
void r4ieeehp1000_(num)
float *num;
#endif
{
   union {
      float val;
      struct hpfloat {
		     unsigned int snmant:1;
		     unsigned int mant:23;
		     unsigned int exp:7;
		     unsigned int snexp:1;
		     } fields;
      } hp;

   union {
      float val;
      long int ival;
      struct ieeefloat {
		     unsigned int snmant:1;
		     unsigned int exp:8;
		     unsigned int mant:23;
		     } fields;
      } ieee;
    
   long int exp=0, mant=0, intest=0;

   ieee.val = *num;

   if (ieee.ival == 0) return;

   exp = ieee.fields.exp;
/* if (ieee.fields.exp & 0x00000080) exp |= 0xffffff80; */
   intest = 126 - exp;
   if ( intest == 0)  {
      if (ieee.fields.snmant) {
	 if (ieee.fields.mant == 0){     
            intest = ~intest;
            hp.fields.snexp |= 0x00000001;  
	 }
         else hp.fields.snexp &= 0x00000000;
      }
      else hp.fields.snexp &= 0x00000000;  
      hp.fields.exp = intest;
   }
   else if (intest > 0) {
	   intest = ~intest + 1;
	   hp.fields.exp = intest;
	   hp.fields.snexp |= 0x00000001;
        }
        else {
     	     hp.fields.exp = exp - 126;
	     if ((ieee.fields.mant == 0) && ieee.fields.snmant) 
					   hp.fields.exp -= 1;
	     hp.fields.snexp &= 0x00000000;
        }

   mant = ieee.fields.mant;
   if (ieee.fields.snmant){
      if (mant==0)
	 hp.fields.mant =mant;
      else
         hp.fields.mant = ~((mant >> 1) | 0x00400000) + 1;
   }
   else 
      hp.fields.mant = (mant >> 1) | 0x00400000;
   hp.fields.snmant = ieee.fields.snmant;
	  
   *num = hp.val;
   return;
}



/***********************************************************************
NAME      
  r8ieeehp1000 - Convert IEEE double precision floating point to
	         HP-1000  representation.
SYNOPSIS
  SUN C
    void r8ieeehp1000_(f)
    double *f;
  HP9000 C
    void r8ieeehp1000(f)
    double *f;
  FORTRAN
    call r8ieeehp1000(f)
    double precision f
DESCRIPTION
  This routine converts the datum in place: the contents of f are
  changed.
RETURN VALUE
  None.  f is changed.
ERRORS
CALLS
AUTHOR/DATE
  Qi Chen   UCLA, Inst. of Geophysics and Planetary Physics, 01/18/90
UPDATES
***********************************************************************/

#ifdef sun
   void r8ieeehp1000_(num)
#else
   void r8ieeehp1000(num) 
#endif
double *num;
{
   struct common {
       unsigned int extra:1;
       } bus;

   union hp1000 {
      double val;
      struct hpfloat {
		     unsigned int mant1:32;
		     unsigned int mant2:20;
		     unsigned int extra1:1;
		     unsigned int extra2:1;
		     unsigned int filler:2;
		     unsigned int exp:7;
		     unsigned int snexp:1;
		     } fields;
      } hp;

   union ieeeux {
      double val;
      struct ieeeint{
                    long int ival1;
                    long int ival2;
		    } ifields;
      struct ieeefloat {
		     unsigned int snmant:1;
		     unsigned int exp:11;
		     unsigned int mant1:20;
                     unsigned int mant2:32;
		     } fields;
      } ieee;
    
   long int exp=0, intest=0 ;
   unsigned int onebyte=0;

   ieee.val = 0;
   hp.val = 0;
   ieee.val = *num;

   if (ieee.ifields.ival1 == 0  &&  ieee.ifields.ival2 == 0) return;

   exp = ieee.fields.exp;
   intest = 1022 - exp;    /* biased by +1023 */
   if ( intest == 0)  {
      if (ieee.fields.snmant) {
	 if (ieee.fields.mant1 == 0 && ieee.fields.mant2 == 0){     
            intest = ~intest;
            hp.fields.snexp = 0x1;  
	 }
         else hp.fields.snexp = 0x0;
      }
      else hp.fields.snexp = 0x0;  
      hp.fields.exp = intest;
   }
   else if (intest > 0) {
	   intest = ~intest + 1;
	   hp.fields.exp = intest;
	   hp.fields.snexp = 0x1;
        }
        else {
     	     hp.fields.exp = exp - 1022;
	     if ((ieee.fields.mant1==0 && ieee.fields.mant2==0 ) 
		  && ieee.fields.snmant) hp.fields.exp -= 1;
		  
	     hp.fields.snexp &= 0x0;
        }

   if (ieee.fields.snmant){
      if (ieee.fields.mant1==0 && ieee.fields.mant2==0) {
	 hp.fields.mant1 = hp.fields.mant2 = hp.fields.filler = 0;
	 hp.fields.extra1= hp.fields.extra2 = 0;
      }
      else {
	 bus.extra = ieee.fields.mant2 & 0x00000001;
	 ieee.fields.mant2 = ieee.fields.mant2 >> 1;
	 ieee.fields.mant2 |= ((ieee.fields.mant1 & 0x00000001) << 31);
	 ieee.fields.mant1 = (ieee.fields.mant1 >> 1) | 0x80000;
	 ieee.fields.mant1 = ~ieee.fields.mant1;
	 ieee.fields.mant2 = ~ieee.fields.mant2;
	 bus.extra = ~bus.extra;
	 if (bus.extra) {
	    if ( (ieee.fields.mant2 & 0xffffffff) 
	             == 0xffffffff) ieee.fields.mant1 += 1;
	    ieee.fields.mant2 += 1;
	 }
	 bus.extra += 1;
      }
   }
   else {
      bus.extra = ieee.fields.mant2 & 0x00000001;
      ieee.fields.mant2 = ieee.fields.mant2 >> 1;
      ieee.fields.mant2 |= ((ieee.fields.mant1 & 0x00000001) << 31);
      ieee.fields.mant1 = (ieee.fields.mant1 >> 1) | 0x80000;
   }

   hp.fields.extra2 = bus.extra;
   hp.fields.extra1 = ieee.fields.mant2 & 0x00000001;
   hp.fields.mant2 = (ieee.fields.mant2 & 0x000fffff);
   hp.fields.mant1 = (ieee.fields.mant1 & 0xfffff) << 12;
   hp.fields.mant1 |= (ieee.fields.mant2 >> 20);
   hp.fields.mant2 = hp.fields.mant2 >> 1;
   hp.fields.mant2 |= ((hp.fields.mant1 & 0x00000001 ) << 19);
   hp.fields.mant1 = hp.fields.mant1 >> 1;
   hp.fields.mant1 = (((onebyte & 0x00000000) | ieee.fields.snmant)<<31)
		   | hp.fields.mant1;

   *num = hp.val; 

   return; 
}


