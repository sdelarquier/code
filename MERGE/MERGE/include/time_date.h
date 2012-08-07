#ifndef TIME_DATE_H

#define TIME_DATE_H

static char SCCS_TIME_DATE[] = "  SCCS header     @(#)time_date.h	1.1	7/27/93";


struct time_date {
    int year;
    int month;
    int date;
    int hour;
    int min;
    int sec;
    int day_number;
    long year_secs;
};

#endif
