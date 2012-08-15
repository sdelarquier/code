#!/bin/bash
#  Creates diagnostics plots for all dates since Jan. 1st, 2010
#  It starts with the current year, and then works its way back, making sure no day is missing
#  Unless you provide a date, then it just does that date in YYYYMMDD
#
#  Created July 10, 2012

echo `date`

if [ -z $1 ]
then
	date=`date -d yesterday -u +%Y%m%d`
else
	date=$1
fi

echo $date

export DAVIT_NODISP=1
source /davit/env/env_davit
/davit/bin/davit <<EOF
  parse_date, ${date}, year, month, day
  juls = timegen(start=julday(01,01,2012),final=julday(month,day,year),units="Days",step=1)
  caldat, juls, month, day, year
  dates = year*10000L + month*100L + day
  rad_fit_check_all, dates, outdir="images/"
  exit
EOF
export DAVIT_NODISP=0

exit 1
