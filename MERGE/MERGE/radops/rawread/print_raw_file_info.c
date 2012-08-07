#include <stdio.h>
#include "rawfile.h"

extern RAW_FILE * rawropen();
extern void raw_file_info();

main() 
{
    RAW_FILE *file;
    char filename[80];
    
    printf("Enter file name: ");
    scanf("%s",filename);
    file = rawropen(filename);
    raw_file_info(file);
}
