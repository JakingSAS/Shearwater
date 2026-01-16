cas mySession sessopts=(caslib=casuser timeout=1800 locale="en_US");

caslib _all_ assign;

filename REFFILE disk '/aaim_shared_data/Health/temp/SW/Shearwater.csv';

proc import datafile=REFFILE dbms=csv out=import replace;
	getnames=yes;
	guessingrows=10000;
run;
/*
proc casutil;
	droptable casdata="shearwater" incaslib="CASUSER(JAKING)" quiet;
quit;
 
data casuser.shearwater (promote=yes);
	set WORK.IMPORT;
run;
*/
/* create target */
proc sort data=import(rename=('Dive.Number'n = Dive_Number 
		'Time (ms)'n = Time_ms
		'Water Temp'n = Water_Temp
		'Average PPO2'n = PPO2
		'Exter.l O2 Sensor 1 (mV)'n = Sensor1
		'Exter.l O2 Sensor 2 (mV)'n = Sensor2
		'Exter.l O2 Sensor 3 (mV)'n = Sensor3));
        /*
		'External O2 Sensor 1 (mV)'n = Sensor1
		'External O2 Sensor 2 (mV)'n = Sensor2
		'External O2 Sensor 3 (mV)'n = Sensor3));
        */
	by Dive_Number Time_ms;
run;

proc sort data=import out=dates(keep=Dive_Number Date) nodupkey;
	by Dive_Number Date;
run;

proc reg data=import noprint outest=Sensor;
	by Dive_Number;
	model Sensor1 =  Water_Temp PPO2;
	model Sensor2 =  Water_Temp PPO2;
	model Sensor3 =  Water_Temp PPO2;
run;
data Sensor;
	merge Sensor(in=in1 where=(_RMSE_ > 0))
		dates(in=in2);
	by Dive_Number;
	if in1 and in2;

	if _MODEL_ = "MODEL1" then do;
		ID = "Cell 1 1";
		if Date >= '02SEP22'd then ID = "Cell 1 2";
		if Date >= '24FEB24'd then ID = "Cell 1 3";
		end;
	if _MODEL_ = "MODEL2" then do;
		ID = "Cell 2 1";
		if Date >= '05JUN21'd then ID = "Cell 2 2";
		if Date >= '02SEP22'd then ID = "Cell 2 3";
		if Date >= '24FEB24'd then ID = "Cell 2 4";
		if Date >= '28APR24'd then ID = "Cell 2 5";	
		end;
	if _MODEL_ = "MODEL3" then do;
		ID = "Cell 3 1";
		if Date >= '02SEP22'd then ID = "Cell 3 2";
		if Date >= '26JUL23'd then ID = "Cell 3 3";
		if Date >= '28APR24'd then ID = "Cell 3 4";	
		end;
run;
proc sort data=sensor;
	by ID Dive_Number;
run;
proc reg data=Sensor noprint outest=Sensor2;
	by ID;
	model Water_Temp = Dive_Number;
	model PPO2 = Dive_Number;
run;



data shearwater;
	set import;
	retain i j k;
	
	if _n_ = 1 then do;
		i = 1; j = 1; k = 1;
		end;
	if Date = '05JUN21'd then j = 2;
	if Date = '02SEP22'd then do;
		i = 2;
		j = 3;
		k = 2;
		end;
	if Date = '26JUL23'd then k = 3;
	if Date = '24FEB24'd then do;
		i = 3;
		j = 4;
		end;
	if Date = '25FEB24'd then k = 4;
	if Date = '28APR24'd then j = 5;	

	ID = catx(" ", "Cell 1", i);  Sensor_mV = Sensor1; output;
	ID = catx(" ", "Cell 2", j);  Sensor_mV = Sensor2; output;
	ID = catx(" ", "Cell 3", k);  Sensor_mV = Sensor3; output;

	keep Depth PPO2 Water_Temp 'Time.s'n Dive_Number Date
		ID Sensor_mV;
run;

/* fit a linear model by ID and day, then look for changes in slope */


/*
data shearwater;
	set import;
	by Dive_Number Time_ms;

	Cell_1_bad = 0;
	Cell_2_bad = 0;
	Cell_3_bad = 0;
	if last.Dive_Number and last.Time_ms then do;
		if 'Date'n in('28AUG22'd, '29OCT23'd) then Cell_1_bad = 1;
		if 'Date'n in('16MAY21'd, '28AUG22'd, '29OCT23'd, '25APR24'd) then Cell_2_bad = 1;
		if 'Date'n in('28AUG22'd, '24JUL23'd, '10OCT23'd) then Cell_3_bad = 1;
		end;
	Last_Dive_1 = 0;
	Last_Dive_2 = 0;
	Last_Dive_3 = 0;
	if Dive_Number = 668 then Last_Dive_2 = 1;
	if Dive_Number = 736 then do;
		Last_Dive_1 = 1;
		Last_Dive_2 = 1;
		Last_Dive_3 = 1;
		end;
	if Dive_Number = 778 then Last_Dive_3 = 1;
	if Dive_Number = 794 then do;
		Last_Dive_1 = 1;
		Last_Dive_2 = 1;
		Last_Dive_3 = 1;
		end;
	if Dive_Number = 800 then Last_Dive_2 = 1;
	
run;
*/




