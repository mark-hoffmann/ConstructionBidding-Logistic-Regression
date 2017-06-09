
libname bid “/path/to/project“;
filename f “/path/to/project/Construction Data.csv";

proc import datafile=f
	out=bid.data
	dbms=csv
	replace;
	getnames=yes
	GUESSINGROWS=10000
	 LRECL=32000;
run;
*split data into training and validation subsets;
data bid.data_train bid.data_validate;
	set bid.data;
	random = RAND('Uniform');
	if random <= 0.2 then output bid.data_validate;
		else output bid.data_train;
run;



proc contents data=bid.data_train;
run;
proc freq data=bid.data_train nlevels;
run;


*comparing everything to Win_Bid;
%let continuous = Bid_Price__Millions_  Cost_After_Engineering_Estimate Estimated_Cost__Millions_ Estimated_Years_to_Complete Number_of_Competitor_Bids /*Winning_Bid_Price__Millions_*/;
%let nominal = Region_of_Country Sector;
%let binary = Competitor_A Competitor_B Competitor_C  Competitor_D Competitor_E Competitor_F Competitor_G  Competitor_H Competitor_I Competitor_J;
%let interactions = Competitor_A*Competitor_B Competitor_A*Competitor_C  Competitor_A*Competitor_D  Competitor_A*Competitor_E Competitor_A*Competitor_F  Competitor_A*Competitor_G Competitor_A*Competitor_H Competitor_A*Competitor_I Competitor_A*Competitor_J
					Competitor_B*Competitor_C  Competitor_B*Competitor_D Competitor_B*Competitor_E Competitor_B*Competitor_F Competitor_B*Competitor_G Competitor_B*Competitor_H Competitor_B*Competitor_I Competitor_B*Competitor_J
					Competitor_C*Competitor_D Competitor_C*Competitor_E Competitor_C*Competitor_F Competitor_C*Competitor_G Competitor_C*Competitor_H Competitor_C*Competitor_I  Competitor_C*Competitor_J
					Competitor_D*Competitor_E Competitor_D*Competitor_F Competitor_D*Competitor_G Competitor_D*Competitor_H Competitor_D*Competitor_I Competitor_D*Competitor_J
					Competitor_E*Competitor_F Competitor_E*Competitor_G Competitor_E*Competitor_H Competitor_E*Competitor_I Competitor_E*Competitor_J
					Competitor_F*Competitor_G Competitor_F*Competitor_H  Competitor_F*Competitor_I Competitor_F*Competitor_J
					Competitor_G*Competitor_H Competitor_G*Competitor_I  Competitor_G*Competitor_J
					Competitor_H*Competitor_I  Competitor_H*Competitor_J
					Competitor_I*Competitor_J;

%let nominalInter = Competitor_A*Region_of_Country*Win_Bid Competitor_B*Region_of_Country*Win_Bid Competitor_C*Region_of_Country*Win_Bid  Competitor_D*Region_of_Country*Win_Bid Competitor_E*Region_of_Country*Win_Bid Competitor_F*Region_of_Country*Win_Bid Competitor_G*Region_of_Country*Win_Bid  Competitor_H*Region_of_Country*Win_Bid Competitor_I*Region_of_Country*Win_Bid Competitor_J*Region_of_Country*Win_Bid;



proc freq data=bid.data_train nlevels;
	tables &categorical &binary;
run;

proc univariate data=bid.data_train;
	var &continuous ;
	qqplot;
	histogram;
run;




*look at tests of significance and odds ratios / correlations on binary categorical variables;
proc freq data=bid.data;
    tables EstCostGroup*Win_Bid EstTimeGroup*Win_Bid Sector*Win_Bid Region_of_Country*Win_Bid
          / chisq relrisk; 
	ods output ChiSq= MH_B  RelativeRisks= OR;
run;
quit;

data MH_B;
	set MH_B;
	if Statistic ne "Mantel-Haenszel Chi-Square" then delete;
run;

proc sort data=MH_B;
	by Prob;
run;
proc print data=MH_B;
run;

data OR;
	set OR;
	if Statistic ne "Odds Ratio" then delete;
	if Value < 1 then OR = 1/Value; else OR = Value;
run;

proc sort data=OR;
 by descending OR;
run;

proc print data=OR;
run;

*updated binary variables based on an alpha of .003;
%let binary = Competitor_E Competitor_F Competitor_H;
*updated proc freq and code to get data desired into nice tables for binary variables;
proc freq data=bid.data_train;
    tables (&binary)*Win_Bid
          / chisq relrisk; 
	ods output ChiSq= MH_B  RelativeRisks= OR;
run;
quit;

data MH_B;
	set MH_B;
	if Statistic ne "Mantel-Haenszel Chi-Square" then delete;
	if Prob > .001 then delete;
run;

proc sort data=MH_B;
	by Prob;
run;
proc print data=MH_B;
run;

data OR;
	set OR;
	if Statistic ne "Odds Ratio" then delete;
	if Value < 1 then OR = 1/Value; else OR = Value;
	drop StudyType Statistic;
run;
*Do not have any non-binary ordinal categorical variables;
*looking for tests of significance for nominal categorical variables;
proc freq data=bid.data_train nlevels;
	tables (&nominal)*Win_Bid / chisq;
	ods output ChiSq=ChiSq;
run;
quit;
data ChiSq;
	set ChiSq;
	if Statistic ne "Chi-Square" then delete;
run;
proc sort data=ChiSq;
	by Prob;
run;
proc print data=ChiSq;
run;

*None of the nominal variables are significant, if there was one, we would then compute Cramer's V for them to get a mesaure of strength;

*NOW WE HAVE FINISHED ASSESING ALL CATEGORICAL VARIABLES FOR SIGNIFICANCE;
*ONLY THE 3 UPDATED BINARY VARIABLES ARE SIGNIFICANT;


*explore continous variables;
proc univariate data=bid.data_train;
	var &continuous;
	histogram ;
run;



*Below Encounters Quasi Complete Separation existing so have to fix before calculating assumptions on continuous variables;
data newData;
	set bid.data_train;
	LBid_Price__Millions_ = Bid_Price__Millions_*log(Bid_Price__Millions_ + .000001);
	LCost_After_Engineering_Estimate = Cost_After_Engineering_Estimate*log(Cost_After_Engineering_Estimate  + .000001);
	LEstimated_Cost__Millions_ = Estimated_Cost__Millions_*log(Estimated_Cost__Millions_ + .000001);
	LEstimated_Years_to_Complete = Estimated_Years_to_Complete*log(Estimated_Years_to_Complete + .000001);
	LNumber_of_Competitor_Bids = Number_of_Competitor_Bids*log(Number_of_Competitor_Bids  + .000001);
	*LWinning_Bid_Price__Millions_ = Winning_Bid_Price__Millions_*log(Winning_Bid_Price__Millions_ + .000001);
run;
%let slogs = LBid_Price__Millions_ LCost_After_Engineering_Estimate LEstimated_Cost__Millions_ LEstimated_Years_to_Complete LNumber_of_Competitor_Bids /*LWinning_Bid_Price__Millions_*/;

ods output parameterestimates=oldbetas;
/*data newData;*/
/*	set newData;*/
/*	Difference = Bid_Price__Millions_ - Winning_Bid_Price__Millions_;*/
/*run;*/
/**ADDED RECORD TO FIX QUASI COMPLETE SEPARATION;*/
/*data _addRec;*/
/*  set newData(firstobs=1); /* Get observation 1 */*/
/*    Win_Bid = 'No';  */
/*   output;      */
/*   stop;*/
/*run;*/
/**/
/*/* Add new obs to original data set */*/
/*proc append base=newData data=_addRec;*/
/*run;*/;
proc logistic data=newData
	plots(only)=(effect(clband showobs) oddsratio);
	class Competitor_E(param=ref ref='0')
		  Competitor_F(param=ref ref='0')
		  Competitor_H(param=ref ref='0');
	model Win_Bid(event='Yes') = &binary &continuous / clodds=pl ;
	title 'Old Logistic Model';
	title2 'Checking Assumptions Model 1';
run;


ods output parameterestimates=newbetas;
proc logistic data=newData
	plots(only)=(effect(clband showobs) oddsratio);
	class Competitor_E(param=ref ref='0')
		  Competitor_F(param=ref ref='0')
		  Competitor_H(param=ref ref='0');
	model Win_Bid(event='Yes') = &binary &continuous &slogs
	/ clodds=pl /*selection=backward slstay=0.001*/;
	title 'New Logistic Model';
	title2 'Checking Assumptions Model 2';
run;

*use tables oldbetas and newbetas to find gammas and apply necessary corrections here;
proc print data=oldBetas;
run;
proc print data=newBetas;
run;

data newData;
	set newData;

	if(Bid_Price__Millions_ < 60) then Bid_Price__Millions_Binned = 1;
	else if (Bid_Price__Millions_ >= 60 and Bid_Price__Millions_ < 100) then Bid_Price__Millions_Binned = 1;
	else if (Bid_Price__Millions_ >= 100 and Bid_Price__Millions_ < 140) then Bid_Price__Millions_Binned = 1;
	else if (Bid_Price__Millions_ >= 140 and Bid_Price__Millions_ < 180) then Bid_Price__Millions_Binned = 3;
	else if (Bid_Price__Millions_ >= 180 and Bid_Price__Millions_ < 220) then Bid_Price__Millions_Binned = 4;
	else if (Bid_Price__Millions_ >= 220 and Bid_Price__Millions_ < 260) then Bid_Price__Millions_Binned = 5;
	else if (Bid_Price__Millions_ >= 260) then Bid_Price__Millions_Binned = 6;

	if(Estimated_Cost__Millions_ < 80) then Estimated_Cost__Millions_Binned = 1;
	else if (Estimated_Cost__Millions_ >= 80 and Estimated_Cost__Millions_ < 120) then Estimated_Cost__Millions_Binned = 1;
	else if (Estimated_Cost__Millions_ >= 120 and Estimated_Cost__Millions_ < 160) then Estimated_Cost__Millions_Binned = 1;
	else if (Estimated_Cost__Millions_ >= 160 and Estimated_Cost__Millions_ < 200) then Estimated_Cost__Millions_Binned = 3;
	else if (Estimated_Cost__Millions_ >= 200 and Estimated_Cost__Millions_ < 280) then Estimated_Cost__Millions_Binned = 4;
	else if (Estimated_Cost__Millions_ >= 280) then Estimated_Cost__Millions_Binned = 5;
run;

proc freq data=newData;
	table (Bid_Price__Millions_Binned Estimated_Cost__Millions_Binned)*Win_Bid;
run;

*updated lists of variables;
%let continuous = Cost_After_Engineering_Estimate Estimated_Years_to_Complete Number_of_Competitor_Bids ;
%let ordinal = Bid_Price__Millions_Binned Estimated_Cost__Millions_Binned;
%let binary = Competitor_E Competitor_F Competitor_H;



*Update continuous variables macro if necessary (had to convert some to categorical)
 
*After calculating gammas on continuous variables;

*CANNOT USE SELECTION=ALL FOR MODELS WITH A CLASS STATEMENT SO WE RUN WITH FORWARD, BACKWARD, AND STEPWISE TO FIND DIFFERENT MODELS;
proc logistic data=newData
	plots(only)=(effect(clband showobs) oddsratio);
	class Competitor_E(param=ref ref='0')
		  Competitor_F(param=ref ref='0')
		  Competitor_H(param=ref ref='0')
		  Bid_Price__Millions_Binned(param=ref ref='1')
		  Estimated_Cost__Millions_Binned(param=ref ref='1');
	model Win_Bid(event='Yes') = &binary &continuous &ordinal &interactions
	/ clodds=pl selection=forward ;
	title 'Finding best moddel';
	title2;
run;
proc logistic data=newData
	plots(only)=(effect(clband showobs) oddsratio);
	class Competitor_E(param=ref ref='0')
		  Competitor_F(param=ref ref='0')
		  Competitor_H(param=ref ref='0')
		  Bid_Price__Millions_Binned(param=ref ref='1')
		  Estimated_Cost__Millions_Binned(param=ref ref='1');
	model Win_Bid(event='Yes') = &binary &continuous &ordinal &interactions
	/ clodds=pl selection=backward ;
	title 'Finding best moddel';
	title2;
run;
proc logistic data=newData
	plots(only)=(effect(clband showobs) oddsratio);
	class Competitor_E(param=ref ref='0')
		  Competitor_F(param=ref ref='0')
		  Competitor_H(param=ref ref='0')
		  Bid_Price__Millions_Binned(param=ref ref='1')
		  Estimated_Cost__Millions_Binned(param=ref ref='1');
	model Win_Bid(event='Yes') = &binary &continuous &ordinal
	/ clodds=pl  ;
	title 'Finding best moddel';
	title2;
run;

%let binaryFinal = Competitor_E Competitor_F;
%let continousFinal = Number_of_Competitor_Bids;

data newData;
	set newData;
	if Win_Bid = 'Yes' then Win_Bid = 'aYes';
	else if Win_Bid = 'No' then Win_Bid = 'bNo';
run;
proc freq data=newData;
    tables (&binaryFinal &continousFinal)*Win_Bid
          / chisq relrisk; 
	ods output ChiSq= MH_B  RelativeRisks= OR;
run;
quit;
data OR;
	set OR;
	if Statistic ne "Odds Ratio" then delete;
	if Value < 1 then OR = 1/Value; else OR = Value;
run;

proc sort data=OR;
 by descending OR;
run;

proc print data=OR;
run;

proc logistic data=newData
		plots(only)=(effect(clband showobs) oddsratio);
	class Competitor_E(param=ref ref='0')
		  Competitor_F(param=ref ref='0')
		  ;
	model Win_Bid(event='Yes') = &binaryFinal &continousFinal
	/ clodds=pl  ;
	title 'Final Model';
	
run;


*Take best model and now we score how well we did;

proc logistic data=bid.data outmodel=model
	plots(only label)= (phat);
	class ;
	Reduced: model Win_Bid(event='Yes') = &binaryFinal
		/ clodds=pl outroc=roc ;
	
run;


data Youden;
	set roc;
	J = _sensit_ - _1mspec_;
run;

proc sort data=Youden;
	by descending J ;
run;

proc print data=Youden (obs = 10);
run;
