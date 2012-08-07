/* File: Readme_First_Merge.txt */
/* Author: Dieter Andre         */
/* Last modification: 07-JUL-2010 */

This is the version of the SuperDARN merge program,
that I originally got from Catherine Senior and Jean-Claude Cerisier.
I made changes to it, but since I last used it in 2002,
I do not remember very well.
But, if you have questions, with some effort I should
be able to answer them.

=======================================================================

All the files have been combined in a a tarball [merge_tar.tar.gz],
which will uncompress and untar in a subdirectory merge_tar .

merge_tar has the following subdirectories:

include:
	Include files used by the Fortran programs [extension *.inc],
	and by the C programs [extension *.h].

merge:
	Main program [merge.f] and makefile.

merge/routines:
	Fortran subroutines used by the merge program.

mergedoc:
	Contains documentation, on how the merge program works.
	merge.ps is the actual documentation,
	the other one are plots of some fields of view,
	and some examples.

lib/makefile
	Makefile for making libraries
	merge only uses libfit.so, but libfit.so needs some other
	libraries, so I included whatever code may be used.

radops/fitread:
	C routines for reading fit files.

radops/fitacf
radops/go
radops/rawread
	Probably not needed.

aacgm
gen
mag
	Called by libfit.so

=======================================================================

makefile comments:

The program [rad_merge] has been compiled and linked under SuSE Linux 9.3 .
The C compiler is the standard Gnu compiler [gcc].
The Fortran compiler is an Absoft f77 .
Nowadays I would use the Gnu Fortran compiler [gfortran], but it will
require changes to the flags.

=======================================================================

The program only reads fit files and wokr for these radars
[defined in station.f]:

	'g','s','k','h','t','b','a','w','e','f','d','j'

=======================================================================
