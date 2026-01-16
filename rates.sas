filename REFFILE disk '/aaim_shared_data/Health/temp/SW/Shearwater.csv';

proc import datafile=REFFILE dbms=csv out=import replace;
	getnames=yes;
	guessingrows=10000;
run;

/* calculate ascent/descent rate */
/* maybe only look at 60 second increments */
proc sort data=import(rename=('Dive.Number'n = Dive_Number 
            'Time.s'n = Time_s
            'First Stop Depth'n = First_Stop_Depth
            'First Stop Time'n = First_Stop_Time))
        out=sw1;
    by Dive_Number Time_s;
run;

/* calculate ascent/descent rate and ascent start */
data sw2;
    set sw1;
    by Dive_Number Time_s;
    length Phase $12.;
    retain temp;

    /* initialze retained variables */
    if first.Dive_Number then temp = 0;

    /* calculate the rate over the last minute */
    rate1 = (lag1(Depth) - Depth)*6;
    rate6 = (lag6(Depth) - Depth);

    /* flag Ascent */
    if rate6 > 10 or temp = 1 then Ascent = 1;
    else Ascent = 0;

    temp=Ascent;

    /* Flag deco stop */
    if Ascent and (
        ((First_Stop_Depth - 3) <= Depth <= (First_Stop_Depth + 3)) and
        First_Stop_Time > 0
        ) or
        Ascent and (First_Stop_Depth <=20) then Deco = 1;
    else Deco = 0;

    /* flag descent */
    if first.Dive_Number or rate6 <= -8 then Descent = 1;
    else Descent = 0;

    /* flag bottom time */
    if (Ascent + Descent) = 0 then Bottom = 1;
    else Bottom = 0;

    if Descent then Phase = "Descent";
    if Bottom then Phase = "Bottom";
    if Ascent then Phase = "Ascent";
    if Deco then Phase = "Deco Stop";

    keep Depth Time_s Dive_Number rate1 rate6 First_Stop_Depth First_Stop_Time Ascent Deco Descent Bottom Phase;
run;

proc means data=sw2;
    class Phase;

    var rate1 rate6;

run;

/* find the first stop depth for each dive */
proc summary data=sw2 noprint;
    by Dive_Number;

    var First_Stop_Depth;

    output out=fsd(drop=_TYPE_ _FREQ_)
        max() = max_fsd;
run;

/* look at rate to first stop depth */
data sw3;
    merge sw2
        fsd;
    by Dive_Number;

    if ascent = 1 and Depth > 0;

    if First_Stop_Depth = max_fsd and max_fsd > 0 then TYPE = 1;
    else TYPE = 2;

run;
proc means data=sw3;
    class TYPE;

    var rate1 rate6;

run;


