/* this files define error codes for read_fit routines and find_fit_rec
   routine
*/

/* the following defines the error definitions for the read_fit routines
   all the error numbers have to be less than or equal to 0
*/

#define EOK			0	/* everything is ok */
#define RDFIT_INV_PARM_TIME	-4	/* invalid time in parameter record */
#define RDFIT_INV_PARM_RECORD	-2	/* unexpected parameter record type */
#define RDFIT_INV_RECORD	-10	/* invalid record (for record > 1) */
#define RDFIT_INV_TIME		-12	/* invalid time in record > 1 */
#define RDFIT_INV_RANGE		-8	/* invalid range */
#define RDFIT_INV_INX           -16     /* invalid index record */


/* the following defines the error codes for find_fit_rec routine
*/
#define F_FIT_REC_LT		-3	/* requested time < start time 
                                           fit_offset points to the start
                                        */
#define F_FIT_REC_INEXACT 	-5	/* inexact time but 
                                           requested time < the found time
                                        */
#define F_FIT_REC_GT		-14	/* requested time > end time */
  

