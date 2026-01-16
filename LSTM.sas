/* LSTM test */
data shearwater;
	set import;

	year = year(Date);
	month = month(Date);
	day = day(Date);
	dow = weekday(Date);
	hour = hour('Run.Time'n);
	if hour < 7 then hour = hour + 12;
	minute = minute('Run.Time'n);
	second = second('Run.Time'n);

	rename 'First Stop Depth'n =First_Stop_Depth
		'Time To Surface (min)'n = Time_to_Surface
		'Fraction He'n = Fraction_He
		'Fraction O2'n = Fraction_O2
		'First Stop Time'n = First_Stop_Time
		'Current NDL'n = Current_NDL
		'Battery Voltage'n = Battery_Voltage
		'Time.s'n = Time_s;

	drop 'Current Circuit Mode'n 'Current CCR Mode'n 'External PPO2'n 
		'Set Point Type'n 'Circuit Switch Type'n 
		'Tank 1 pressure (PSI)'n 'Tank 2 pressure (PSI)'n 
		'Tank 3 pressure (PSI)'n 'Tank 4 pressure (PSI)'n 
		'Gas Time Remaining'n 'SAC Rate (2 minute avg)'n CO2mbar 
		filename 'Start.Dttm'n 'Gas Switch Needed'n 'Ascent Rate'n
		'Safe Ascent Depth'n;
run;
proc casutil;
	droptable casdata="shearwater_long" incaslib="public" quiet;
quit;
data public.shearwater_long (promote=yes);
	set shearwater;
	where sensor1 > 0;
run;

/* create Month Day Hour outdesign matrix using GLMSELECT */
proc glmselect data=shearwater outdesign=outdesign(drop=sensor1 intercept) noprint;
	class year month hour dow;
	model sensor1 = year month hour dow ;
run;


data shearwater_design_matrix;
   merge shearwater outdesign;
run;

/* add lags? */

/* data partition */
cas casauto;
caslib _all_ assign;

data casuser.training;
	set shearwater_design_matrix;
	where sensor1 > 0 and date < '01JAN2024'd;
run;

data casuser.validation;
	set shearwater_design_matrix;
	where sensor1 > 0 and date >= '01JAN2024'd;
run;

/* LSTM */
proc cas;
	session casauto;

	deepLearn.buildModel / modelTable={name="tsRnn1", replace=1} type="RNN";
run;

proc cas;
	deepLearn.addLayer / model='tsRnn1' name='data' 
		layer={type='input' std='std' }; 
	deepLearn.addLayer / model='tsRnn1' name='rnn1' 
		layer={type='recurrent' n=10 act='sigmoid' init='msra' rnnType='RNN' outputtype='samelength'}
		srcLayers={'data'}; 
	deepLearn.addLayer / model='tsRnn1' name='rnn2' 
		layer={type='recurrent' n=10 act='sigmoid' init='msra' rnnType='RNN' outputtype='encoding'}
		srcLayers={'rnn1'};  
	deepLearn.addLayer / model='tsRnn1' name='outlayer' 
		layer={type='output' act='identity' error='normal'}
		srcLayers={'rnn2'};
run;

proc cas;
	deepLearn.dlTrain / table='training' model='tsRnn1' 
		modelWeights={name='tsTrainedWeights1', replace=1} 
		bestweights={name='bestbaseweights1', replace=1}
		inputs=${Depth, First_Stop_Depth, Time_To_Surface, Fraction_He, First_Stop_Time, Current_NDL,
			Battery_Voltage, Time_s, Dive_Number, 
			year, month, day, dow, hour, PPO2, sensor1, sensor2, sensor3, Water_Temp}
		target='Fraction_O2'  
		sequenceopts={timestep=13}
		optimizer={minibatchsize=5, algorithm={method='ADAM', lrpolicy='step',  gamma=0.5,
			beta1=0.9, beta2=0.99, learningrate=.001 clipgradmin=-1000 clipgradmax=1000 }  
			maxepochs=30} 
		seed=54321;
run;
proc cas;
   deepLearn.dlExportModel / 
        casout={name="LSTM01_ExportedModel", replace=TRUE}
        initWeights={name="tsTrainedWeights1"}
        modelTable={name="tsRnn1"};
run;


proc cas;
	session casauto;

	deepLearn.buildModel / modelTable={name="tsRnn0", replace=1} type="RNN";
run;
proc cas;
	deepLearn.addLayer / model='tsRnn0' name='data' 
		layer={type='input' std='std' }; 
	deepLearn.addLayer / model='tsRnn0' name='rnn1' 
		layer={type='recurrent' n=10 act='sigmoid' init='msra' rnnType='LSTM' outputtype='samelength'}
		srcLayers={'data'}; 
	deepLearn.addLayer / model='tsRnn0' name='rnn2' 
		layer={type='recurrent' n=10 act='sigmoid' init='msra' rnnType='LSTM' outputtype='encoding'}
		srcLayers={'rnn1'};  
	deepLearn.addLayer / model='tsRnn0' name='outlayer' 
		layer={type='output' act='identity' error='normal'}
		srcLayers={'rnn2'};
run;

proc cas;
	deepLearn.dlTrain / table='training' model='tsRnn0' 
		modelWeights={name='tsTrainedWeights1', replace=1} 
		bestweights={name='bestbaseweights1', replace=1}
		inputs=${Depth, First_Stop_Depth, Time_To_Surface, Fraction_He, First_Stop_Time, Current_NDL,
			Battery_Voltage, Time_s, Dive_Number, 
			year, month, day, dow, hour, PPO2, sensor1, sensor2, sensor3, Water_Temp}
		target='Fraction_O2'  
		sequenceopts={timestep=13}
		optimizer={minibatchsize=5, algorithm={method='ADAM', lrpolicy='step',  gamma=0.5,
			beta1=0.9, beta2=0.99, learningrate=.001 clipgradmin=-1000 clipgradmax=1000 }  
			maxepochs=30} 
		seed=54321;
run;