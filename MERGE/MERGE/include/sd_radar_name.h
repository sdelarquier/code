/* this file defines radar id, data file code (i.e. g for goose, h for halley
   etc.) and radar site name
   it defines a structure for all the above things

   If SD_RADAR_NAME_EXT is defined then the definitions are defined as
	externals.  Otherwise, the definitions are global and memory is
	allocated to them.
*/
/*
	$Log: sd_radar_name.h,v $
 * Revision 1.1  1994/11/28  18:43:18  kile
 * Initial revision
 *
*/

#ifndef SD_RADAR_NAME_INCLUDED_H

#define SD_RADAR_NAME_INCLUDED_H

struct sd_radar_name_str {
   char l_code;
   char *name;
};

#ifdef SD_RADAR_NAME_EXT
/* only the external declaration is needed */
extern struct sd_radar_name_str sd_radar_name[];

#else
/* the actual global declaration is needed */
struct sd_radar_name_str sd_radar_name[] = {
   { 'u',   "unused" },				/* 0 */
   { 'g',   "goose bay" },			/* 1 */
   { 's',   "schefferville" },			/* 2 */
   { 'k',   "kapuskasing" },			/* 3 */
   { 'h',   "halley" },				/* 4 */
   { 't',   "saskatoon" },			/* 5 */
   { 'b',   "british columbia" },		/* 6 */
   { 'a',   "alaska" },				/* 7 */
   { 'w',   "stokkseyri (Iceland W)" },			/* 8 */
   { 'e',   "Pykkvibaer (Iceland E)" },			/* 9 */
   { 'f',   "Hankasalmi (Finland)" },			/* 10 */
   { 'd',   "sanae (Durban)" },				/* 11 */
   { 'j',   "syowa (Japan)" },				/* 12 */
   { 'n',   "syowa east (Nippon)"}                      /* 13 */
};

#define TOTAL_NUM_RADAR sizeof(sd_radar_name)/sizeof(struct sd_radar_name_str)

#endif

#endif

