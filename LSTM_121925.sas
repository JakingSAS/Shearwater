/*============================*
 * CONFIG: edit as needed     *
 *============================*/
%let caslib         = public;                         /* CAS libname */
%let in_table       = &caslib..shearwater_long;           /* input CAS table */
%let id_col         = Dive_Number;                       /* dive key */
%let time_col       = NewDt;                            /* datetime stamp */
%let feat_list      = Depth Water_Temp Sensor1 Sensor2 Sensor3;           /* features */
%let seq_len        = 60;                            /* timesteps per sequence (10 min) */
%let stride         = 1;                             /* how many rows to skip between windows */
%let valid_ratio    = 0.2;                           /* validation split by dive_id */

/* Start CAS & bind libname */
cas mySession sessopts=(caslib=casuser timeout=1800 locale="en_US");

caslib _all_ assign;

filename REFFILE disk '/aaim_shared_data/Health/temp/SW/Shearwater.csv';

proc import datafile=REFFILE dbms=csv out=import replace;
	getnames=yes;
	guessingrows=10000;
run;

/* load test data */
data shearwater;
	set import;

	where ('Exter.l O2 Sensor 1 (mV)'n > 0) &
        ('Time (ms)'n >= 0);

    NewDt = 'Start.Dttm'n + ('Time (ms)'n / 1000);
    format NewDT DATETIME.;

	rename 'Dive.Number'n = Dive_Number 
		'Time (ms)'n = Time_ms
		'Water Temp'n = Water_Temp
		'Average PPO2'n = PPO2
		'Exter.l O2 Sensor 1 (mV)'n = Sensor1
		'Exter.l O2 Sensor 2 (mV)'n = Sensor2
		'Exter.l O2 Sensor 3 (mV)'n = Sensor3
        'First Stop Depth'n =First_Stop_Depth
		'Time To Surface (min)'n = Time_to_Surface
		'Fraction He'n = Fraction_He
		'Fraction O2'n = Fraction_O2
		'First Stop Time'n = First_Stop_Time
		'Current NDL'n = Current_NDL
		'Battery Voltage'n = Battery_Voltage
		'Time.s'n = Time_s
        'Run.Time'n = Run_Dt;

	drop 'Current Circuit Mode'n 'Current CCR Mode'n /*'External PPO2'n*/ 
		'Exter.l PPO2'n 'Set Point Type'n 'Circuit Switch Type'n 
		'Tank 1 pressure (PSI)'n 'Tank 2 pressure (PSI)'n 
		'Tank 3 pressure (PSI)'n 'Tank 4 pressure (PSI)'n 
		'Gas Time Remaining'n 'SAC Rate (2 minute avg)'n CO2mbar 
		'file.me'n /*filename*/ 'Start.Dttm'n 'Gas Switch Needed'n 'Ascent Rate'n
		'Safe Ascent Depth'n;
run;
proc sort data=shearwater;
    by Dive_Number;
run;
proc casutil;
	droptable casdata="shearwater_long" incaslib="&caslib" quiet;
quit;
data &in_table. (promote=yes);
	set shearwater;
run;

/**********************/
/* Scale dynamically within each dive
/**********************/
/* Identify time-varying features */
%let dyn_features = Depth Water_Temp Sensor1 Sensor2 Sensor3 ;

/* Compute per-dive mean and std */
proc means data=shearwater noprint;
  by Dive_Number;
  var &dyn_features;
  output out=dive_stats
    mean=mean_Depth mean_Water_Temp mean_Sensor1 mean_Sensor2 mean_Sensor3
    std =std_Depth std_Water_Temp std_Sensor1 std_Sensor2 std_Sensor3;
run;

/* Join and scale within dive */
data dive_scaled;
  merge shearwater dive_stats;
  by Dive_Number;

  /* Z-score within dive */
  Depth_z = (Depth - mean_Depth) / std_Depth;
  Water_Temp_z = (Water_Temp - mean_Water_Temp) / std_Water_Temp;
  Sensor1_z = (Sensor1 - mean_Sensor1) / std_Sensor1;
  Sensor2_z = (Sensor2 - mean_Sensor2) / std_Sensor2;
  Sensor3_z = (Sensor3 - mean_Sensor3) / std_Sensor3;

  keep Dive_Number Time_s Depth_z Water_Temp_z Sensor1_z Sensor2_z Sensor3_z;
run;

/**********************/
/* Stack timesteps → wide vectors
/**********************/
/* fix timesteps */
%let T = 30;   /* max timesteps per dive */

/* Pivot to wide format*/
proc sort data=dive_scaled;
  by dive_Number Time_s;
run;
data dive_scaled;
    set dive_scaled;
    by Dive_Number;

    if first.Dive_Number then t=1;
    else t+1;
run;

data dive_wide;
  set dive_scaled;
  by Dive_Number;

  array f1[&T] f1_t1-f1_t&T;
  array f2[&T] f2_t1-f2_t&T;
  array f3[&T] f3_t1-f3_t&T;
  array f4[&T] f4_t1-f4_t&T;
  array f5[&T] f5_t1-f5_t&T;

  retain f1_t1-f1_t&T
         f2_t1-f2_t&T
         f3_t1-f3_t&T
         f4_t1-f4_t&T
         f5_t1-f5_t&T;

  if first.Dive_Number then call missing(of f1[*], of f2[*], of f3[*], of f4[*], of f5[*]);

  if t <= &T then do;
    f1[t] = Depth_z;
    f2[t] = Water_Temp_z;
    f3[t] = Sensor1_z;
    f4[t] = Sensor2_z;
    f5[t] = Sensor3_z;
  end;

  if last.Dive_Number then output;
run;

/**********************/
/* Add static dive features
/**********************/
proc means data=shearwater noprint;
    by Dive_Number;
    var Depth Time_S ppO2;
    output out=static(drop=_TYPE_ _FREQ_)
        max()= 
        / autoname;
run;
data dive_model;
    merge dive_wide static;
    by Dive_Number;
run;

/**********************/
/* Load CAS table for LSTM
/**********************/
proc casutil;
  load data=dive_model
    casout="dive_model"
    replace;
quit;

/**********************/
/* Build the temporal autoencoder
/**********************/
/* Create/replace model */
proc cas;
   deepLearn.buildModel /
      model = { name = "fc_ae", replace = TRUE };
quit;

/* Define INPUT (image-like strip) and FC stack */
proc cas;
   /* INPUT layer: give shape, not nominal/n */
   deepLearn.addLayer /
      modelTable = { name = "fc_ae" },
      name       = "input",
      layer      = { type = "INPUT", nChannels = 1, width = 153, height = 1 };

   /* Encoder */
   deepLearn.addLayer /
      modelTable = { name = "fc_ae" },
      name       = "enc1",
      layer      = { type = "FC", n = 100, act = "RELU" },
      srcLayers  = { "input" };

   /* Bottleneck (linear) */
   deepLearn.addLayer /
      modelTable = { name = "fc_ae" },
      name       = "bottleneck",
      layer      = { type = "FC", n = 32, act = "IDENTITY" },
      srcLayers  = { "enc1" };

   /* Decoder */
   deepLearn.addLayer /
      modelTable = { name = "fc_ae" },
      name       = "dec1",
      layer      = { type = "FC", n = 100, act = "RELU" },
      srcLayers  = { "bottleneck" };

   /* Output: reconstruct the original 158-dim vector */
   deepLearn.addLayer /
      modelTable = { name = "fc_ae" },
      name       = "output",
      layer      = { type = "FC", n = 153, act = "IDENTITY" },
      srcLayers  = { "dec1" };
quit;

/* Verify topology */
proc cas;
   deepLearn.modelInfo / modelTable = { name = "fc_ae" };
quit;


/**********************/
/* Train the autoencoder
/**********************/
%let inputs =
  f1_t1-f1_t&T
  f2_t1-f2_t&T
  f3_t1-f3_t&T
  f4_t1-f4_t&T
  f5_t1-f5_t&T
  max_depth run_time ppO2_setpoint;
  

proc contents data=dive_model(drop=Time_s Dive_Number Depth_z Water_Temp_z
    Sensor1_z Sensor2_z Sensor3_z t) out=varlist noprint;
run;
proc sql noprint;
    select distinct quote(trim(NAME),"'")
        into :varlist separated by ","
        from varlist;
quit; 
%put &varlist.;

/* Train: inputs = target for autoencoder */
proc cas;
   deepLearn.dlTrain /
      table      = { name = "dive_model", caslib = "CASUSER" },
      modelTable = { name = "fc_ae" },
      modelWeights={name="MODELWEIGHTS" replace=true},
      inputs     = { &varlist. },
      target     = { &varlist. },
      optimizer  = { algorithm = "ADAM", maxEpochs = 50,
        miniBatchSize = 256, regL2 = 0.001 };
quit;


/**********************/
/* Score and compute reconstruction error per timestep
/**********************/
proc cas;
deepLearn.score /
  modelTable={ name="fc_ae" },
  table={ name="dive_model" },
  casOut={ name="ae_scored", replace=TRUE };
quit;

data dive_anomaly;
  set ae_scored;

  array x[*] f1_t1-f1_t&T f2_t1-f2_t&T f3_t1-f3_t&T;
  array y[*] P_f1_t1-P_f1_t&T P_f2_t1-P_f2_t&T P_f3_t1-P_f3_t&T;

  mse = 0;
  do i = 1 to dim(x);
    mse + (x[i] - y[i])**2;
  end;
  mse = mse / dim(x);
run;

