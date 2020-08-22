libname f_data 'W:\00_FIRE\DATA\00_sasfile';
libname F_raw 'W:\00_FIRE\DATA\00_Raw';
libname F_final 'W:\00_FIRE\Analytic_Data';

proc sort data=f_raw.wave1; by aid; run;

proc sort data=f_raw.wave2; by aid; run;

proc sort data=f_raw.wave3; by aid; run;

proc sort data=f_raw.wave4; by aid; run;

proc sort data=f_raw.w1_context1; by aid; run;

*Merge all the datasets together;
data f_data.waves;
merge f_raw.wave1 (in=aa) f_raw.wave2 (in=bb) f_raw.wave3 (in=cc) f_raw.wave4 (in=dd) f_raw.w1_context1 (in=ee) ;
by aid;
if aa and bb and cc and dd and ee then output;
run;

PROC CONTENTS data=f_data.waves order=varnum;
run;

*Select variables;
data f_data.waves_selected;
set f_data.waves;
keep aid 
/*Demo*/
     /*Age*/ IMONTH IDAY IYEAR H1GI1Y H1GI1M H2GI1M H2GI1Y
     /*RACE*/ H1GI4 H1GI6A H1GI6B H1GI6C H1GI6D H1GI6E 
	 /*GENDER*/bio_sex bio_sex2 bio_sex3 bio_sex4
     /*CITIZEN*/ H1GI11 H1GI14 H4OD4 H4OD6M H4OD6Y
     /*PARENTAL*/ H1HR12 H1HR13 PA12 H1RF1 H1RM1 H1RM1 H1RF1
	 /*LGBTQI*/ H4SE31 SEXFLG2
/*Nhood*/
	 /*MINORITY*/ TST90006 TST90007 TST90008 TST90009 TST90011
	              BST90006 BST90007 BST90008 BST90009 BST90011
     /*POVERTY*/ TST90626 BST90626
     /*COLLEGE*/ TST90686 BST90686 
	 /*CRIME*/ CUC93982 CUC93983 CUC93984 CUC93985 CUC93986 CUC93987 CUC93988 CUC93990 CUC93991
               CUC93992 CUC93993 CUC93994 CUC93995 CUC93996 CUC93997 CUC93998 CUC93999 CUC93A01
     /*SES*/ TST90678 TST90679 BST90678 BST90679
	 /*HOUSING*/ TST90803 TST90811 BST90803 BST90811
	 /*POLITICAL*/ CCU92A27 CCU92A28 CCU92A29
/*TRAUMA*/
     /*POVERTY*/
	 /*WELFARE/
	 /*HOUSING*/ H3HR24
	 /*CRIME*/
	 /*FAMILY*/
	 /*PARENT IN JAIL*/ H4WP9 H4WP3 H4WP30 H4WP16
	 /*PARENTAL DEALTH*/
     /*MALTREATMENT*/ H3MA2 H3MA1 H3MA3 H3MA4 H3OD31 H4MA1
	 /*ACE*/ PA62
	 /*SUICIDE*/
/*Incarceration*/
     /*AGE OF FIRST INCARCERATION*/ H4CJ20
	 /*JUVENILE*/ H4CJ5 H4CJ24Y H4CJ24M
	 /*ADULT*/ H4CJ6 H4CJ25M H4CJ25Y
	 /*SCHOOL*/H1ED7 H1ED8 H1ED9
/*R Delinquency Involvement*/
    /*GUN TO SCHOOL*/ H1FV9 
    /*OTHER*/ H1FV7 H1FV8 H1FV3 H1FV4 H2FV3 H2FV4 
              H1DS3 H1DS4 H1JO11 H1DS5 H1DS2 H1JO26 H1DS6 H1DS1
    /*STEALING*/ H1DS13 H1DS9 H1DS4
    /*DRUGS*/ H1DS12 H1TO42 H1TO43 H1TO35 H1JO19 H1JO24
              H1JO21 H1JO23
    /*ALCOHOL*/ H1JO9
 /*R CRIME INVOLVEMENT*/
    /*POVERTY*/ H3DS8 H4DS8 H4DS1
	/*PHYSICAL FIGHT*/H3DS16 H4DS11
	/*ASSULT/STEAL*/H4CJ9I H4DS20 H4DS19 H4DS6 H4DS2
                    H3DS9 H4DS9
	/*SELL DRUGS*/H4DS5 H3DS5
	/*PROPERTY*/H4DS1 H3DS8 H4DS8 
    /*STOLE MORE THAN $50*/H4DS2 H3DS2 H4DS6 H3DS6
	/*ACE*/ H3MA1 H1PF23 H1PF1 H3MA3 H4MA1 H3MA4 H1SU6 PA62 PA10
        H4WP9 H4WP3 H4WP30 H4WP16  H3OD31 H1FV1
        H1FV2 H1FV3 H1FV4
;
     run;

PROC CONTENTS data=f_data.waves_selected order=varnum;
run;

data renamed;
set f_data.waves_selected;
keep aid 
/*DEMO*/
age age2 H2GI1M H2GI1Y bdate2 H1GI1Y H1GI1M 
HISPANIC NH_WHITE NH_BLACK NH_NATIVE NH_ASIAN OTHER race
sex1 sex2 sex3 sex4 female1
CITIZEN 
TWOPARENT PA_BAPLUS FIRSTGEN
LGBTQI

/*NEIGHBORHOOD*/  
BST90006 TST90006 BST90007 TST90007 BST90008 TST90008 BST90009 TST90009
BST90011 TST90011

BST90626 TST90626

BST90686 TST90686

CUC93982 CUC93983 CUC93984 CUC93985 CUC93986 CUC93987 CUC93988 CUC93990 CUC93991
CUC93992 CUC93993 CUC93994 CUC93995 CUC93996 CUC93997 CUC93998 CUC93999 CUC93A01

BST90678 TST90678 BST90679 TST90679

BST90803 TST90803 BST90811 TST90811

CCU92A27 CCU92A28 CCU92A29

/*TRAUMA*/
ACE_Supervisory_Neglect ACE_Emotional_Neglect ACE_Physical_Abuse ACE_Emotional_Abuse ACE_Sexual_Abuse
                    ACE_Suicide ACE_PArental_Alcohol_Misuse ACE_Divorce ACE_Parent_Jail ACE_Foster_Home
					ACE_Direct_Witness ACE_Victim Number_ACE ACE

/*INCARCERATION*/
FirstIncarcAge
JIncarceration JIncarceMonths
AIncarceration AIncarceMonths
EverSuspend SuspendGrade ESuspend MSuspend Hsuspend Expel

/*Delinquency Involvement*/
JDWeaponSchool 
JDKnifeGun JDShootStab JH1FV3 JH1FV4 JH2FV3 JH2FV4 
JDLying JDShoplift JDPhysicalFight JDSeriousPhysicalFight JDDamageProperty
JDWeaponFight JDHurtBadly JDGraffitiPaint

JDStealLess JDStealMore JDStealStore 

JDSellDrugs JDIllegalDrugUse JDIllegalDrugNeedle JDCocaineUse
JDDriveHigh JDAloneDrugUse JDFightonDrugs JDWeaponOnDrugs

JDDriveDrunk  

/*CRIME INVOLVEMENT*/
AStealProperty ADamageProperty
APhysicalFight
ASimpleAssault AShootStab AKnifeGun AstealLess AStealMore AStolenCard
ASellDrugs
ADamageProperty AstealProperty
AStealMore
AStealLess


;

*********************;
*** Demographics* ***;
*********************;

/*Age*/ 
idate=mdy(imonth,iday,iyear); bdate=mdy(H1GI1M,15,H1GI1Y); age=int((idate-bdate)/365.25);
                              bdate2=mdy(H2GI1M,15,H2GI1Y); age2=int((idate-bdate2)/365.25);

/*RACE*/
IF H1GI4=1 THEN Race="Hispanic";
ELSE IF H1GI4=0 AND H1GI6A=1 THEN Race="NH_White";
ELSE IF H1GI4=0 AND H1GI6B=1 THEN Race="NH_Black";
ELSE IF H1GI4=0 AND H1GI6C=1 THEN Race="NH_NATIVE";
ELSE IF H1GI4=0 AND H1GI6D=1 THEN Race="NH_Asian";
ELSE Race="Other";


if Race="Hispanic" then HISPANIC=1;
else HISPANIC=0;

if Race="NH_White" then NH_White=1;
else NH_White=0;

if Race="NH_Black" then NH_Black=1;
else NH_Black=0;

if Race="NH_NATIVE" then NH_NATIVE=1;
else NH_NATIVE=0;

if Race="NH_Asian" then NH_Asian=1;
else NH_Asian=0;

IF Race="Other" then OTHER=1;
ELSE OTHER=0;


/*BIO_SEX*/
if bio_sex=1 then sex1=0;
else if bio_sex=2 then sex1=1;
female1 = sex1;

if bio_sex2=1 then sex2=0;
else if bio_sex2=2 then sex2=1;

if bio_sex3=1 then sex3=0;
else if bio_sex3=2 then sex3=1;

if bio_sex4=1 then sex4=0;
else if bio_sex4=2 then sex4=1;

/*CITIZEN*/
ctzdate=mdy(H4OD6M,15,H4OD6Y); becomectz=int((ctzdate-bdate)/365.25);
if H1GI11=1 or H1GI14=1 or becomectz<18 then citizen=1;
else citizen=0;

/*PARENTAL*/ 
if H1HR12 in (1:97) and H1HR13 in (1:97) then twoparent=1;
else twoparent=0;

*college =8 or more=9;
if pa12 in (8,9) or H1RF1 in (8,9) or H1RM1 in (8,9) then pa_baplus=1;
else pa_baplus=0;

IF H1RM1 in (1,2,3,4,5,6,7,10) and H1RF1 in (1,2,3,4,5,6,7,10) and pa12 in (1,2,3,4,5,6,7,10) then firstgen=1;
else firstgen=0;


/*LGBTQI*/ 
IF H4SE31 IN (96, 98) then LGBTQI=.;
else LGBTQI=H4SE31;

*********************;
*** Neighborhood* ***;
*********************;

/*MINORITY*/ 
rename BST90006=B_WHITE;
rename BST90007=B_BLACK;
rename BST90008=B_ASIAN;
rename BST90009=B_OTHER;
rename BST90011=B_HISPANIC;
rename TST90006=T_WHITE;
rename TST90007=T_BLACK;
rename TST90008=T_ASIAN;
rename TST90009=T_OTHER;
rename TST90011=T_HISPANIC;

/*POVERTY*/
rename BST90626=B_POVERTY;
rename TST90626=T_POVERTY;

/*COLLEGE*/ 
rename BST90686=B_COLLEGE;
rename TST90686=T_COLLEGE;

/*CRIME*/ 
rename CUC93982=C_TOTALCRIMERATE;
rename CUC93983=C_VIOLENT;
rename CUC93984=C_ROBBERIES;
rename CUC93985=C_AGGRAVATED;
rename CUC93986=C_PROPERTY;
rename CUC93987=C_BURGLARIES;
rename CUC93988=C_LARCENY;
rename CUC93990=C_JUV_TOTAL;
rename CUC93991=C_JUV_VIOLENT;
rename CUC93992=C_JUV_ROBBERY;
rename CUC93993=C_JUV_ASSAULT;
rename CUC93994=C_JUV_PROPERTY;
rename CUC93995=C_JUV_BURGLARY;
rename CUC93996=C_JUV_THEFT;
rename CUC93997=C_JUV_MOTOR;
rename CUC93998=C_AD_TOTAL;
rename CUC93999=C_AD_VIOLENT;
rename CUC93A01=C_AD_ROBBERY;

/*SES*/
rename BST90678=B_PRE_PRIVATE;
rename BST90679=B_ELE_PRIVATE;
rename TST90678=T_PRE_PRIVATE;
rename TST90679=T_ELE_PRIVATE;

/*HOUSING*/
rename BST90803=B_OWNER;
rename BST90811=B_RENT;
rename TST90803=T_OWNER;
rename TST90811=T_RENT;

/*POLITICAL*/ 
rename CCU92A27=C_DEMO;
rename CCU92A28=C_REPUB;
rename CCU92A29=C_PEROT;

********************;
****** Trauma ******;
********************;

***********  TRAUMA / ACE;
*** Trauma/ACE Variables by Lee, Kim, and Terry - ACEs on Mental Disorders in young adulthood...;

*** "Supervisory_Neglect";
	 SELECT;
	 	WHEN (H3MA1 in(., 96, 98, 99)) ACE_Supervisory_Neglect = .;
	 	WHEN (H3MA1 in(1, 2, 3, 4, 5)) ACE_Supervisory_Neglect = 1;
	 	WHEN (H3MA1 = 6) ACE_Supervisory_Neglect = 0; END;

		
*** "Emotional Neglect";
	  SELECT;
	 	WHEN (H1PF23 in(6, 7, 8, 9)) EmotionalNeglect1 =.;
	 	WHEN (H1PF23 in(4, 5)) EmotionalNeglect1 = 0;
	 	OTHERWISE EmotionalNeglect1 = 1; END;
	 SELECT;
	 	WHEN (H1PF1 in(6, 7, 8)) EmotionalNeglect2 =.;
	 	WHEN (H1PF1 in(4, 5)) EmotionalNeglect2 = 0;
	 	OTHERWISE EmotionalNeglect2 = 1; END;
	 SELECT;
	 	WHEN (EmotionalNeglect1 =. & EmotionalNeglect2 =.) ACE_Emotional_Neglect =.;
	 	WHEN (EmotionalNeglect1 = 1 | EmotionalNeglect2 = 1) ACE_Emotional_Neglect = 1;
	 	OTHERWISE ACE_Emotional_Neglect = 0; END;
	 

*** "Physical Abuse";
	 SELECT;
	 	WHEN (H3MA3 in(., 96, 98, 99)) ACE_Physical_Abuse = .;
	 	WHEN (H3MA3 in(1, 2, 3, 4, 5)) ACE_Physical_Abuse = 1;
	 	WHEN (H3MA3 = 6) ACE_Physical_Abuse = 0; END;

*** "Emotional Abuse";
      SELECT;
	 	WHEN (H4MA1 in(., 96, 98)) ACE_Emotional_Abuse = .;
	 	WHEN (H4MA1 in(1, 2, 3, 4, 5)) ACE_Emotional_Abuse = 1;
	 	WHEN (H4MA1 = 6) ACE_Emotional_Abuse = 0; END;
      

*** "Sexual Abuse";
	 SELECT;
	 	WHEN (H3MA4 in(., 96, 98, 99, 0)) ACE_Sexual_Abuse = .;
	 	WHEN (H3MA4 in(1, 2, 3, 4, 5)) ACE_Sexual_Abuse = 1;
	 	WHEN (H3MA4 = 6) ACE_Sexual_Abuse = 0; END;

*** Suicidal Attempt of Household Adults;
	 SELECT;
	 	WHEN (H1SU6 in(6, 8, 9)) ACE_Suicide =.;
	 	WHEN (H1SU6 = 0) ACE_Suicide = 0;
	 	WHEN (H1SU6 = 1) ACE_Suicide = 1; END;
 
*** Parental Alcohol Misuse;	 
	 SELECT;
	 	WHEN (PA62 in(., 96)) ACE_Parental_Alcohol_Misuse = .;
	 	WHEN (PA62 in(2, 3, 4, 5, 6)) ACE_Parental_Alcohol_Misuse = 1;
	 	WHEN (PA62 = 1) ACE_Parental_Alcohol_Misuse = 0; END;

*** Parental Divorce;
     SELECT;
	 	WHEN (PA10 = .) ACE_Divorce = .;
	 	WHEN (PA10 = 96) ACE_Divorce = .;
	 	WHEN (PA10 in(1, 2, 3)) ACE_Divorce = 0;
	 	WHEN (PA10 in(4, 5)) ACE_Divorce = 1;
	 	OTHERWISE ACE_Divorce = .;
	 END;

*** "Parental Incarceration";
	 SELECT;
	 	WHEN (H4WP9 in(6, 8)) BioFatherJail = .;
	 	OTHERWISE BioFatherJail = H4WP9; END;
	 SELECT;
	 	WHEN (H4WP3 in(6, 8)) BioMotherJail = .;
	 	OTHERWISE BioMotherJail = H4WP3; END;
	 SELECT;
	 	WHEN (H4WP30 in(6, 7, 8)) FigFatherJail = .;
	 	OTHERWISE FigFatherJail = H4WP30; END;
	 SELECT;
	 	WHEN (H4WP16 in(7, 8)) FigMotherJail = .;
	 	OTHERWISE FigMotherJail = H4WP16; END;
	 SELECT;
	 	WHEN (BioMotherJail =. & BioFatherJail =. & FigMotherJail =. & FigFatherJail =.) ACE_Parent_Jail =.;
	 	WHEN (BioMotherJail = 1 | BioFatherJail = 1 | FigMotherJail = 1 | FigFatherJail = 1) ACE_Parent_Jail = 1;
	 	OTHERWISE ACE_Parent_Jail = 0; END;

*** Foster Home;
	SELECT;
	 	WHEN (H3OD31 in(., 6, 8, 9)) ACE_Foster_Home = .;
	 	OTHERWISE ACE_Foster_Home = H3OD31; END;

**** Direct Witness of Violence;
	SELECT;
	 	WHEN (H1FV1 in(., 6, 8, 9)) ACE_Direct_Witness = .;
		WHEN (H1FV1 in(1, 2)) ACE_Direct_Witness = 1;
	 	OTHERWISE ACE_Direct_Witness = 0; END;

*** Victim of Crime;
	SELECT;
	 	WHEN (H1FV2 =. & H1FV3 =. & H1FV4 =.) ACE_Victim =.;
	 	WHEN (H1FV2 = 1 | H1FV3 = 1 | H1FV4 = 1) ACE_Victim = 1;
	 	OTHERWISE ACE_Victim = 0; END;

DROP H3MA1 H1PF23 H1PF1 EmotionalNeglect1 EmotionalNeglect2 H3MA3 H4MA1 H3MA4 H1SU6 PA62 PA10
        H4WP9 H4WP3 H4WP30 H4WP16 BioMotherJail BioFatherJail FigFatherJail FigMotherJail H3OD31 H1FV1
        H1FV2 H1FV3 H1FV4;

****  Summary Variables;
Number_ACE = Sum(of ACE_Supervisory_Neglect ACE_Emotional_Neglect ACE_Physical_Abuse ACE_Emotional_Abuse ACE_Sexual_Abuse
                    ACE_Suicide ACE_PArental_Alcohol_Misuse ACE_Divorce ACE_Parent_Jail ACE_Foster_Home
					ACE_Direct_Witness ACE_Victim);
SELECT (Number_ACE);
   WHEN (.) ACE=.;
   WHEN (0) ACE=0;
   OTHERWISE ACE=1; END;


*********************;
*** iNCARCERATION ***;
*********************;

/*Age at first Incarceration*/
IF H4CJ20 in (96, 97, 98) THEN FirstIncarcAge=.; ELSE FirstIncarcAge=H4CJ20;

/*Juvenile Incarceration*/
/*Juvenile Incarceration    **** this is arrests;
IF H4CJ5 in (996, 998) then JIncarceration = .;
ELSE IF H4CJ5 = 997 THEN JIncarceration = 0;
ELSE IF H4CJ5 in (1:95) THEN JIncarceration = 1;
ELSE JIncarceration = .;  */

*Months Incarcerated as a Juvenile;
IF H4CJ24Y in (96, 98) THEN JIncarceMonths =.; ELSE IF H4CJ24Y = 97 THEN JIncarceMonths = 0;
   ELSE JIncarceMonths = H4CJ24Y;
IF H4CJ24M in (96, 98) THEN JIncarceMonths2 =.; ELSE IF H4CJ24M = 97 THEN JIncarceMonths2 = 0;
   ELSE JIncarceMonths2 = H4CJ24M;
JIncarceMonths = (12*JIncarceMonths);
JIncarceMonths = sum(JIncarceMonths, JIncarceMonths2);

IF JincarceMonths=0 then Jincarceration = 0;
  Else if JincarceMonths > 0 then Jincarceration =1;

/*Adult Incarceration*/

/*Adult Incarceration   -   ****   This is arrests;
IF H4CJ6 = . THEN AIncarceration = .;
ELSE IF H4CJ6 in (96, 98) THEN AIncarceration = .;
ELSE IF H4CJ6 = 97 THEN AIncarceration = 0;
ELSE IF H4CJ6 in (1:90) THEN AIncarceration = 1;
ELSE AIncarceration = .;*/

* Months Incarcerated as an Adult:;
IF H4CJ25Y in (96, 98) THEN AIncarceMonths =.; ELSE IF H4CJ25Y = 97 THEN AIncarceMonths = 0;
  ELSE AIncarceMonths = H4CJ25Y;
IF H4CJ25M in (96, 98) THEN AIncarceMonths2 =.; ELSE IF H4CJ25M = 97 THEN AIncarceMonths2 = 0;
  ELSE AIncarceMonths2 = H4CJ25M;
AIncarceMonths = (12*AIncarceMonths);
AIncarceMonths = sum(AIncarceMonths, AIncarceMonths2);

IF AincarceMonths=0 then Aincarceration = 0;
  Else if AincarceMonths > 0 then Aincarceration =1;

/*School Suspension*/

IF EverSuspend in(6, 8) THEN EverSuspend = .;
  ELSE EverSuspend = H1ED7;

IF SuspendGrade in (96, 97, 98, 99) THEN SuspendGrade = .;
  ELSE SuspendGrade = H1ED8;

IF SuspendGrade in (1:5) then ESuspend = 1;
  ELSE ESuspend = 0;

IF SuspendGrade in (6, 7, 8) then MSuspend = 1;
  else MSuspend = 0;

IF SuspendGrade in (9, 10, 11, 12) then HSuspend = 1;
  else HSuspend = 0;

IF Expel in(6, 8) THEN Expel = .;
  ELSE Expel = H1ED9;

  

****************************;
*** Juvenile Delinquency ***;
****************************;

/*Broight gun to school*/
IF H1FV9 in (6, 8 9) THEN JDWeaponSchool = .;
  ELSE JDWeaponSchool = H1FV9;


IF H1FV7 in (6, 8, 9) THEN JDKnifeGun = .;
  ELSE JDKnifeGun = H1FV7;

IF H1FV8 in(6, 8, 9) THEN JDShootStab = .;
  ELSE JDShootStab = H1FV8;

IF H1FV3 in (6, 8, 9) THEN JH1FV3 = .;
  ELSE IF H1FV3 in(1, 2) THEN JH1FV3 = 1;
  ELSE JH1FV3 = 0; 

IF H1FV4 in(6, 8, 9) THEN JH1FV4 = .;
  ELSE IF H1FV4 in(1, 2) THEN JH1FV4 = 1;
  ELSE JH1FV4 = 0;

IF H2FV3 in(6, 8) THEN JH2FV3 = .;
  ELSE IF H2FV3 in(1, 2) THEN JH2FV3 = 1;
  ELSE JH2FV3 = 0;

IF H2FV4 in (6, 8) THEN JH2FV4 = .;
  ELSE IF H2FV4 in (1, 2) THEN JH2FV4 = 1;
  ELSE JH2FV4 = 0;

IF H1DS3 in(6, 8, 9) THEN JDLying = .;
  ELSE JDLying = H1DS3;

IF H1DS4 in(6, 8, 9) THEN JDShoplift = .;
  ELSE JDShoplift = H1DS4;

IF H1JO11 in (6, 8) THEN JDPhysicalFight = .;
  ELSE IF H1JO11 in (0, 7) THEN JDPhysicalFight = 0;
  ELSE IF H1JO11 = 1 THEN JDPhysicalFight = 1;

IF H1DS5 in (6, 8, 9) THEN JDSeriousPhysicalFight = .;
  ELSE IF H1DS5 = 0 THEN JDSeriousPhysicalFight = 0;
  ELSE IF H1DS5 in (1, 2, 3) THEN JDSeriousPhysicalFight = 1;

IF H1DS2 in (6, 8, 9) THEN JDDamageProperty = .;
  ELSE IF H1DS2 = 0 THEN JDDamageProperty = 0;
  ELSE IF H1DS2 in (1, 2, 3) THEN JDDamageProperty = 1;

IF H1JO26 in (6, 8, 9) THEN JDWeaponFight = .;
  ELSE IF H1JO26 = 0 THEN JDWeaponFight = 0;
  ELSE IF H1JO26 = 1 THEN JDWeaponFight = 1;

IF H1DS6 in (6, 8, 9) THEN JDHurtBadly = .;
  ELSE IF H1DS6 = 0 THEN JDHurtBadly = 0;
  ELSE IF H1DS6 in (1, 2, 3) THEN JDHurtBadly = 1;

IF H1DS1 in (6, 8, 9) THEN JDGraffitiPaint = .;
  ELSE IF H1DS1 = 0 THEN JDGraffitiPaint = 0;
  ELSE IF H1DS1 in (1, 2, 3) THEN JDGraffitiPaint = 1;


*Stealing;
IF H1DS13 in(6, 8, 9) THEN JDStealLess = .;
  ELSE JDStealLess = H1DS13;

IF H1DS9 in(6, 8, 9) THEN JDStealMore = .;
  ELSE JDStealMore = H1DS9;

IF H1DS4 in (6, 8, 9) THEN JDStealStore = .;
  ELSE IF H1DS4 = 0 THEN JDStealStore = 0;
  ELSE IF H1DS4 in (1, 2, 3) THEN JDStealStore = 1;

*Drugs;
IF H1DS12 in (6, 8, 9) THEN JDSellDrugs = .;
ELSE IF H1DS12 = 0 THEN JDSellDrugs = 0;
ELSE IF H1DS12 in (1, 2, 3) THEN JDSellDrugs = 1;

IF H1TO42 in (996, 998, 999) THEN JDIllegalDrugUse = .;
ELSE IF H1TO42 in (0, 997) THEN JDIllegalDrugUse = 0;
ELSE IF H1TO42 in (1:900) THEN JDIllegalDrugUse = 1;
ELSE JDIllegalDrugUse = .;

IF H1TO43 in (6, 8, 9) THEN JDIllegalDrugNeedle = .;
ELSE IF H1TO43 in (0, 7) THEN JDIllegalDrugNeedle = 0;
ELSE IF H1TO43 = 1 THEN JDIllegalDrugNeedle = 1;
ELSE JDIllegalDrugNeedle = .;

IF H1TO35 in (996, 998, 999) THEN JDCocaineUse = .;
ELSE IF H1TO35 = 997 THEN JDCocaineUse = 0;
ELSE JDCocaineUse = 1;

IF H1JO19 in (6, 8) THEN JDDriveHigh = .;
ELSE IF H1JO19 in (0, 7) THEN JDDriveHigh = 0;
ELSE IF H1JO19 = 1 THEN JDDriveHigh = 1;

IF H1JO24 in (6, 8) THEN JDAloneDrugUse = .;
ELSE IF H1JO24 in (0, 7) THEN JDAloneDrugUse = 0;
ELSE IF H1JO24 = 1 THEN JDAloneDrugUse = 1;

IF H1JO21 in (6, 8) THEN JDFightOnDrugs = .;
ELSE IF H1JO21 in (0, 7) THEN JDFightOnDrugs = 0;
ELSE IF H1JO21 = 1 THEN JDFightOnDrugs = 1;

IF H1JO23 in (6, 8) THEN JDWeaponOnDrugs = .;
ELSE IF H1JO23 in (0, 7) THEN JDWeaponOnDrugs = 0;
ELSE IF H1JO23 = 1 THEN JDWeaponOnDrugs = 1;


*Alchol;
IF (H1JO9 in(6, 8)) THEN JDDriveDrunk = .;
	 	ELSE IF (H1JO9 in(0, 7)) THEN JDDriveDrunk = 0;
	 	ELSE IF (H1JO9 = 1)  THEN JDDriveDrunk = 1;
	 	ELSE JDDriveDrunk = .; 


********************;
*** Adult Crimes ***;
********************;

/*Property*/

*Steal Property_w3;
IF H3DS8 in (6, 8) THEN AStealProperty3 = .;
ELSE IF H3DS8 = 0 THEN AStealProperty3 = 0;
ELSE IF H3DS8 in (1, 2, 3) THEN AStealProperty3 = 1; 
ELSE AStealProperty3 = .;

*Steal Property_w4;
IF H4DS8 in (6, 8) THEN AStealProperty4 = .;
ELSE IF H4DS8 = 0 THEN AStealProperty4 = 0;
ELSE IF H4DS8 in (1, 2, 3) THEN AStealProperty4 = 1; 
ELSE AStealProperty4 = .;

*Steal Property_w3&w4;
IF AStealProperty3 = . AND AStealProperty4 = . THEN AStealProperty = .;
ELSE IF AStealProperty3 = 1 OR AStealProperty4 = 1 THEN AStealProperty = 1;
ELSE AStealProperty = 0; 

*DamageProperty;
IF H4DS1 in (6, 8) THEN ADamageProperty =.;
ELSE IF H4DS1 = 0 THEN ADamageProperty = 0;
ELSE IF H4DS1 in (1, 2, 3) THEN ADamageProperty = 1;

/*Physical Fight*/
*w3;
IF H3DS16 in (96, 98, 99) THEN AH3DS16 =.;
ELSE IF H3DS16 = 0 THEN AH3DS16 = 0;
ELSE IF H3DS16 in(1:56) THEN AH3DS16 = 1;
ELSE AH3DS16 =.;

*W4;
IF H4DS11 in (6, 8) THEN AH4DS11 = .;
ELSE IF H4DS11 = 0 THEN AH4DS11 = 0;
ELSE IF H4DS11 in (1, 2, 3) THEN AH4DS11 = 1; 
ELSE AH4DS11 =.;


*Physical fight_w3&w4;
IF AH3DS16 =. AND AH4DS11 =. THEN APhysicalFight =.;
ELSE IF AH3DS16 = 1 OR AH4DS11 = 1 THEN APhysicalFight = 1;
ELSE APhysicalFight =.;


/*ASSULT_STEAL*/
IF H4CJ9I in (6, 8) THEN ASimpleAssault =.;
ELSE IF H4CJ9I in (0, 7) THEN ASimpleAssault = 0;
ELSE IF H4CJ9I = 1 THEN ASimpleAssault = 1;

IF H4DS20 in (., 6, 8) THEN AShootStab = .;
ELSE AShootStab = H4DS20;

IF H4DS19 in (., 6, 8) THEN AKnifeGun = .;
ELSE AKnifeGun = H4DS19; 

IF H3DS6 in (6, 8) THEN AStealLess3 = .;
ELSE IF H3DS6 = 0 THEN AStealLess3 = 0;
ELSE IF H3DS6 in (1, 2, 3) THEN AStealLess3 = 1; 
ELSE AStealLess3 = .;

IF H4DS6 in (6, 8) THEN AStealLess4 = .;
ELSE IF H4DS6 = 0 THEN AStealLess4 = 0;
ELSE IF H4DS6 in (1, 2, 3) THEN AStealLess4 = 1; 
ELSE AStealLess4 = .;

IF AStealLess3=1 or AStealLess4=1 then AStealLess=1;
ELSE IF AStealLess3=0 AND AStealLess4=0 THEN AStealLess=0;
ELSE AStealLess=.;

IF H3DS2 in (6, 8) THEN AStealMore3 = .;
ELSE IF H3DS2 = 0 THEN AStealMore3 = 0;
ELSE IF H3DS2 in (1, 2, 3) THEN AStealMore3 = 1; 
ELSE AStealMore3 = .;

IF H4DS2 in (6, 8) THEN AStealMore4 = .;
ELSE IF H4DS2 = 0 THEN AStealMore4 = 0;
ELSE IF H4DS2 in (1, 2, 3) THEN AStealMore4 = 1; 
ELSE AStealMore4 = .;

IF AStealMore3=1 or AStealMore4=1 then AStealMore=1;
ELSE IF AStealMore3=0 AND AStealMore4=0 THEN AStealMore=0;
ELSE AStealMore=.;


*Stolen Card;
IF H3DS9 in (6, 8, 9) THEN AH3DS9 =.;
ELSE IF H3DS9 = 0 THEN AH3DS9 = 0;
ELSE IF H3DS9 in (1, 2, 3) THEN AH3DS9 = 1;
ELSE AH3DS9 =.;

IF H4DS9 in (6, 8) THEN AH4DS9 =.;
ELSE IF H4DS9 = 0 THEN  AH4DS9 = 0;
ELSE IF H4DS9 in (1, 2, 3) THEN AH4DS9 = 1;
ELSE AH4DS9 =.;

IF AH3DS9 =. AND AH4DS9 =. THEN AStolenCard =.;
ELSE IF AH3DS9 = 0 AND AH4DS9 = 0 THEN AStolenCard = 0;
ELSE IF AH3DS9 = 1 OR AH4DS9 = 1 THEN AStolenCard = 1;

/*Sell Drugs*/
IF H3DS5 in (6, 8, 9) THEN AH3DS5 =.;
ELSE IF H3DS5 = 0 THEN AH3DS5 = 0;
ELSE IF H3DS5 in (1, 2, 3) THEN AH3DS5 = 1;
ELSE AH3DS5 =.;

IF H4DS5 in (6, 8) THEN AH4DS5 =.;
ELSE IF H4DS5 = 0 THEN AH4DS5 = 0;
ELSE IF H4DS5 in (1, 2, 3) THEN AH4DS5 = 1;
ELSE AH4DS5 =.;

IF AH3DS5 =. AND AH4DS5 =. THEN ASellDrugs =.;
ELSE IF AH3DS5 = 0 AND AH4DS5 = 0 THEN ASellDrugs = 0;
ELSE IF AH3DS5 = 1 OR  AH4DS5 = 1 THEN ASellDrugs = 1;
ELSE ASellDrugs = .; 

IF AGE<18 THEN OUTPUT;

run;


/*
**NEED TO CHK LATER**
* AC = Aggressive Crimes;
* NAC = Non-aggressive Crimes;
 	WHEN (H4DS11 in(6, 8)) ACPhysicalAttack =.;
	 	OTHERWISE ACPhysicalAttack = H4DS11; END;
	 ACGun = AdultGun;
	 ACShootStab = AdultShootStab;
	 ACAttack = AdultAttack;
	 SELECT;
	 	WHEN (H4DS8 in(6, 8)) NACOtherProperty1 =.;
	 	OTHERWISE NACOtherProperty1 = H4DS8; END;
	 SELECT;
	 	WHEN (H3DS8 in(6, 8, 9)) NACOtherProperty2 =.;
	 	OTHERWISE NACOtherProperty2 = H3DS8; END;
	 NACOtherProperty = NACOtherProperty1 + NACOtherProperty2;
	 SELECT;
	 	WHEN (H4DS6 in(6, 8)) NACStealLess =.;
	 	OTHERWISE NACStealLess = H4DS6; END;
	 SELECT;
	 	WHEN (H4DS2 in(6, 8)) NACStealMore =.;
	 	OTHERWISE NACStealMore = H4DS2; END;

	 AggDelinq = ADPhysicalFight + ADKnifeGun + ADShootStab + ADWeaponSchool;
	 NonAggDelinq = NADShoplift + NADStealLess + NADStealMore;
	 AggCrime = ACPhysicalAttack + ACGun + ACShootStab + ACAttack;
	 NonAggCrime = NACOtherProperty + NACStealLess + NACStealMore;

*/





**********************
*  Combining School data onto participant data
**********************;

 %macro makedummies (k, numi, original, list);
    array dummy&k(&numi) &list;
    do i = 1 to &numi;  if &original ne  . and &original <= &numi then dummy&k(i)=0; end;
    if &original ne . and &original <= &numi then dummy&k(&original)=1;
 %mend makedummies;

*********************   SSCHLCDE = ASCHLCDE = SCID   **********************************************;

/*  Inschool has linkage between school and participant   */
Data inschool;
    set F_raw.inschool (keep=AID SSCHLCDE);
    rename sschlcde = scid;
run;


/*  School administrator questions  */
Data schadm1;
   set f_raw.schadm1 (keep = ASCHLCDE a2a a2b a2c a2d a2e a2d a2e a2f a2g a2h a2i a2j a2k a2o a3 a5 a6 a7 a8a a8b a9 a10 a11 a12 a13
                           a18a a18b a18c a18d a18e a18f a19a a19b a19c a19d a19e a19f
                           a20a a20b a20c a21 a31a a31c a31e a31g a31i a31k a31m a31o a31q a31s a31u a31w);

   %makedummies (1, 5, A3, sch_geoarea sch_geotransfer sch_balancerace sch_entrancetest sch_othassign);
   if A3 = 9 then do; sch_geoarea=1;  sch_geotransfer=1; end;

   %makedummies (2, 5, A5, sch_attend_95to100 sch_attend_90to94 sch_attend_85to89 sch_attend_80to84 sch_attend_75to79);

   array disc a31a a31c a31e a31g a31i a31k a31m  a31o a31q a31s a31u a31w 
              a18a a18b a18c a18d a18e a18f  a19a a19b a19c a19d a19e a19f a20a a20b a20c a21;

   do over disc; if disc in (97, 98, 99, 997) then disc=.; end;

   sch_discipline = mean(of a31a a31c a31e a31g a31i a31k a31m  a31o a31q a31s a31u a31w);

   rename aschlcde = scid
          a2a = sch_pubcomp
          a2b = sch_pubmagnet
          a2c = sch_public
          a2d = sch_yrround
          a2e = sch_vocational
          a2f = sch_othvocation
          a2g = sch_cathdioc
          a2h = sch_cathparish
          a2i = sch_cathrelig
          a2j = sch_privrelig
          a2k = sch_privnorelig
          a2o = sch_alternative
          a3  = sch_howstudentsassigned
          a5  = sch_avgattendance
          a6  = sch_FTEteachers
          a7  = sch_avgclass
          a8a = sch_teach_pctwhite
          a8b = sch_teach_pctblack
          a9  = sch_teach_pcthispanic
          a10 = sch_teach_pctwomen
          a11 = sch_teach_pct5yrplus
          a12 = sch_teach_pctnew
          a13 = sch_teach_masters
          a18a= sch_dropout7
          a18b= sch_dropout8
          a18c= sch_dropout9
          a18d= sch_dropout10
          a18e= sch_dropout11
          a18f= sch_dropout12
          a19a= sch_retain7
          a19b= sch_retain8
          a19c= sch_retain9
          a19d= sch_retain10
          a19e= sch_retain11
          a19f= sch_retain12
          a20a= sch_student_atgrade
          a20b= sch_student_belowgrade
          a20c= sch_student_abovegrade
          a21 = sch_student_collegebound;

    drop a31a a31c a31e a31g a31i a31k a31m  a31o a31q a31s a31u a31w;
run;

/* proc freq data=schadm1;
   run;  */

/*  School characteristics  */
Data Schinfo;
    set f_raw.schinfo   (keep= SCID SIZE SCHTYPE METRO REGION QPWHT INHOME N_ROSTER);
    if inhome=1;    /*  keep only data for those that had in_home questions */

    %makedummies (1, 3, schtype, sch_public sch_catholic sch_private);
    %makedummies (2, 4, size, sch_size_small sch_size_medium sch_size_large  sch_size_xlarge);
    %makedummies (3, 3, metro, sch_urban sch_suburban sch_rural);
    %makedummies (4, 4, region, sch_west sch_midwest sch_south sch_northeast);
    %makedummies (5, 4, qpwht, sch_pctwhite_0 sch_pctwhite_le66 sch_pctwhite_le93  sch_pctwhite_100);

    rename size = sch_size
           schtype = sch_type
           metro = sch_metro
           region = sch_region
           qpwht = sch_qpwht
           n_roster = sch_n_roster;
	drop i;
run;


*************  Link schoolinfo and schadm1 by SCID   ***********;

Proc sort data=schinfo; by scid;
Proc sort data=schadm1; by scid;
data merge_one;
   merge schinfo (in=in1) schadm1 (in=in2);
   by scid;
run;

*********** Now link merged file with student in-school survey by SSCHCDE;

Proc sort data=merge_one; by scid;
Proc sort data=inschool; by scid;
data schooldata;
   merge merge_one (in=in1) inschool (in=in2);
   by scid;
   if in2;
run;

****************  Now schooldata can be linked to the wave data by AID;

proc sort data=renamed; by aid;
proc sort data=schooldata; by aid;
data F_Final.Analytic;
   merge renamed (in=in1) schooldata (in=in2);
   if in1;
   if in2 then school_flag=1; else school_flag=0;
run;

PROC FREQ data=F_Final.Analytic;
   tables school_flag;
RUN;

proc EXPORT DATA=F_Final.Analytic
            OUTFILE= "W:\00_FIRE\Analytic_Data\Analytic.sav" 
            DBMS=sav REPLACE;
RUN;

