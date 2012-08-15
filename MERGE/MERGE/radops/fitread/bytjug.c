/*
   Routine for swapping bytes to convert from VMS
   to unix. Swaps shorts or longs. Swaps in place. 

   'data' is a pointer to the first element to be swapped. 
   'nval' is the number of values to be swapped. 
   'nbyt' is the length of the values; 2 for shorts, 4 for longs.

*/
void bytjugle(data,nval,nbyt)
short *nval,*nbyt;
char *data;
{
char hold[4];
short i,j;
if( *nbyt == 2 ){
  for( j=0; j<*nval; j++){
    for( i=0; i<2; i++ )hold[i] = data[i+j*2];
    data[j*2] = hold[1];
    data[j*2+1] = hold[0];
  }
}
if( *nbyt == 4 ){
  for( j=0; j<*nval; j++){
    for( i=0; i<4; i++ )hold[i] = data[i+j*4];
    data[j*4] = hold[3];
    data[j*4+1] = hold[2];
    data[j*4+2] = hold[1];
    data[j*4+3] = hold[0];
  }
}
}

