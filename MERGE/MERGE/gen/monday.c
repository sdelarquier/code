
/* give it year and day of year, it returns the month and day of month */

void monday(year,doy,imon,iday)
     int *year, *doy, *imon, *iday;
{
  int dmons[12]={31,28,31,30,31,30,31,31,30,31,30,31};
  int cdmons[12]={31,59,90,120,151,181,212,243,273,304,334,365};
  int i;

  if( !(*year%4) ){
    dmons[1] ++;
    for( i=1; i<12; i++ )cdmons[i] ++;
  }
  i=0;
  while((*iday=*doy-cdmons[i]) > 0 )
    i++;
  *iday = *iday + dmons[i];
  *imon = i+1;
}
