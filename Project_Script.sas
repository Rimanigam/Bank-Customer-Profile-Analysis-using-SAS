/*MACRO PROGRAM TO ASSIGN THE LIBRARY*/
%LET DIR =C:\Users\Veena Nigam\Desktop\SAS Documents\SAS Business Project ;
%PUT &DIR.;
%LET DSN = RIMA.PROJECT4;

LIBNAME RIMA "&DIR.";

/*Describing the properties of the project data*/

PROC CONTENTS DATA = &DSN. OUT =CONTENTS VARNUM SHORT;
RUN;

/*COLUMN NAMES*/
/*AcctAge DDA DDABal CashBk Checks DirDep NSF NSFAmt Phone Teller Sav SavBal ATM ATMAmt POS POSAmt CD CDBal IRA IRABal LOC LOCBal ILS ILSBal 
MM MMBal MMCred MTG MTGBal CC CCBal CCPurc SDB Income HMOwn LORes HMVal Age CRScore Moved InArea Ins Branch Res Dep DepAmt Inv InvBal*/


/*SPLIT INTO NUMERIC AND CATEGORICAL*/

PROC SQL;
SELECT NAME INTO : NUM_ONLY SEPARATED BY " "
FROM CONTENTS
WHERE TYPE EQ 1
;
SELECT NAME INTO : CHAR_ONLY SEPARATED BY " "
FROM CONTENTS
WHERE TYPE EQ 2
;
QUIT;


%PUT &NUM_ONLY;
%PUT &CHAR_ONLY;
%LET X = %SCAN(&NUM_ONLY.,1);
%PUT &X;
%LET X = %SCAN(&NUM_ONLY.,2);
%PUT &X;
%LET X = %SCAN(&NUM_ONLY.,3);
%PUT &X;

/*DATA PROFILING*/

%MACRO PROF_NUMERIC(DSN = , VAR= );

%LET N = %SYSFUNC(COUNTW(&VAR.));
%DO I = 1 %TO &N;
%LET X = %SCAN (&VAR, &I);
TITLE "DISTRIBUTION OF ALL NUMERIC VARIABLES:SUMMARY";
PROC MEANS DATA = RIMA.RISK N NMISS Q1 Q3 MEAN MEDIAN MAX;
VAR &X.;
RUN;

TITLE "DISTRIBUTION OF &X. : HISTOGRAM AND DENSITY CURVE";
PROC SGPLOT DATA = &DSN.;
HISTOGRAM &X.;
DENSITY &X./ TYPE=KERNEL;
KEYLEGEND/LOCATION=INSIDE POSITION = TOPRIGHT ACROSS=1 NOBORDER;
RUN;
QUIT;

TITLE "DISTRIBUTION OF &X. :VERTICLE BOX PLOT";
PROC SGPLOT DATA = &DSN.;
VBOX &X..;
YAXIS GRID;
XAXIS DISPLAY=(NOLABEL);
RUN;
QUIT;
%END;
%MEND;

ODS PDF FILE = "&DIR.\NUMERIC_SUMMARY_&SYSDATE9..PDF";
%PROF_NUMERIC (DSN = &DSN., VAR = &NUM_ONLY);
ODS PDF CLOSE;


/*CHARACTER VARIABLES*/

%LET X = %SCAN(&CHAR_ONLY.,1);
%PUT &X;
%LET X = %SCAN(&CHAR_ONLY.,2);
%PUT &X;

%MACRO PROF_CHAR(DSN = ,CVAR = , COLOR= );
TITLE "COUNT OF ALL CATEGORICAL VARIABLES:SUMMARY";
PROC FREQ DATA= &DSN. ORDER=FREQ; 
TABLE &CVAR./MISSING;
RUN;
%LET N = %SYSFUNC(COUNTW(&CVAR.));	
%DO I = 1 %TO &N;	
%LET X = %SCAN(&CVAR.,&I);

TITLE "COUNT BY %UPCASE(&X)";
PROC FREQ DATA = &DSN. ORDER=FREQ; 
TABLE &X./MISSING;
RUN;

TITLE "COUNT BY %UPCASE(&X.)";
PROC SGPLOT DATA = &DSN.; 
VBAR &X/categoryorder=respasc barwidth=0.6 fillattrs= &COLOR.; 
xaxis display=(nolabel);
RUN;
QUIT;

PROC TEMPLATE; 
DEFINE STATGRAPH PIE;  
BEGINGRAPH;    
ENTRYTITLE "COUNT BY %UPCASE(&X.)"; 
LAYOUT REGION;     
PIECHART CATEGORY=&X / DATALABELLOCATION=OUTSIDE DATASKIN = CRISP  DATALABELCONTENT = ALL CATEGORYDIRECTION = CLOCKWISE START = 180 NAME = 'PIE' ; 
DISCRETELEGEND 'PIE'; 
ENDLAYOUT;
ENDGRAPH;
END;
RUN;

PROC SGRENDER DATA = &DSN. TEMPLATE = PIE;
RUN;
%END;
%MEND;

%PROF_CHAR(DSN = &DSN.,CVAR = &CHAR_ONLY , COLOR = GRAPHDATA9 );


/*Columns names*/
*AcctAge DDA DDABal CashBk Checks DirDep NSF NSFAmt Phone Teller Sav SavBal ATM ATMAmt POS POSAmt CD CDBal IRA IRABal LOC LOCBal 
ILS ILSBal MM MMBal MMCred MTG MTGBal CC CCBal CCPurc SDB Income HMOwn LORes HMVal Age CRScore Moved InArea Ins Dep DepAmt Inv InvBal;

/*Select Y(DDA/Checking Account) and Xs variables (Using Proc Sql)*/

PROC SQL;
 CREATE TABLE RIMA.BANK_PROFILE AS
 SELECT DDA, Checks, DirDep, NSF,Phone, Teller, Sav,Income, ATM, CC, CCPurc, CRScore, Res, Branch, MTG,INS,AGE
 FROM RIMA.PROJECT4;
 QUIT;

 /*Selecting a new data based on variable identification process*/

%LET DSN_1 = RIMA.BANK_PROFILE;


 /*Missing Values Detection*/

/* create a format to group missing and nonmissing */

proc format;
 value $charmiss ' '='Missing' other='Not Missing';
 value  nummiss  . ='Missing' other='Not Missing';
run;
 
proc freq data= &DSN_1.; 
format _CHAR_ $charmiss.; 
tables _CHAR_ / missing missprint nocum nopercent;
format _NUMERIC_ nummiss.;
tables _NUMERIC_ / missing missprint nocum nopercent;
run;

/*Other way to find the missing values*/

PROC MEANS DATA = &DSN_1. N NMISS;
RUN;


/*UNIVARIATE ANALYSIS*/


/*Analysing Numerical Column*/

%MACRO UNIVARIATE_NUM(DSN= , VAR= );

%LET N = %SYSFUNC(COUNTW(&VAR.));
%DO I = 1 %TO &N;
%LET X = %SCAN (&VAR, &I);

PROC UNIVARIATE DATA = &DSN.;
TITLE "COMPREHENSIVE UNIVARIATE ANALYSIS OF %UPCASE(&X.) ";
 VAR &X.;
RUN;

TITLE "DISTRIBUTION OF &X. :BOX PLOT";
PROC SGPLOT DATA = &DSN.;
 VBOX &X.;
 YAXIS GRID;
 XAXIS DISPLAY=(NOLABEL);
 RUN;
 
TITLE "CORRELATION OF &X. WITH CHECKING ACCOUNT";
PROC CORR DATA=&DSN. PLOTS(MAXPOINTS=NONE) = MATRIX(HISTOGRAM);
 VAR &X.;
 WITH DDA;
 RUN;
%END;
%MEND;

ODS PDF FILE = "&DIR.\Images.PDF";
%UNIVARIATE_NUM(DSN = &DSN_1.,VAR = &NUM_ONLY.);
ODS PDF CLOSE;


/*AFTER LOOKING INTO THE BOX PLOTS- THESE ARE THE COLUMNS IN WHICH OUTLIERS WERE DETECTED --
TELLER, PHONE,NSF, MTG, DDA, CHECKS, CRSCORE, CCPURC, AGE

I will treate these outliers after handling the missing values*/

/*Analysing Categorical Variables*/

%MACRO UNIVARIATE_CAT(DSN = , CVAR = ,DEP = ,COLOR = );
TITLE "COUNT OF ALL CATEGORICAL VARIABLES:SUMMARY";
PROC FREQ DATA= &DSN. ORDER=FREQ; 
TABLE &CVAR./MISSING;
RUN;
%LET N = %SYSFUNC(COUNTW(&CVAR.));	
%DO I = 1 %TO &N;	
%LET X = %SCAN(&CVAR.,&I);

TITLE "COUNT BY %UPCASE(&X)";
PROC FREQ DATA = &DSN. ORDER=FREQ; 
TABLE &X./MISSING;
RUN;

TITLE "COUNT BY %UPCASE(&X.)";
PROC SGPLOT DATA = &DSN.; 
VBAR &X/categoryorder=respasc barwidth=0.6 fillattrs= &COLOR.; 
xaxis display=(nolabel);
RUN;
QUIT;

PROC ANOVA DATA = &DSN.;
TITLE "RELATIONSHIP BETWEEN DDA AND &X.";
CLASS &X.;
MODEL &DEP. = &X.;
/*MEANS &DEP./SCHEFFE;*/
RUN;
QUIT;

%END;
%MEND;

%UNIVARIATE_CAT(DSN = &DSN_1.,CVAR = &CHAR_ONLY ,DEP = DDA, COLOR = GRAPHDATA9 );

/*Only the Res column is more significant and shows a strong relationship with the DDA as its P value is less than 0.05 */


/*BIVARIATE ANALYSIS*/

%MACRO BIVARIATE (DSN = ,VAR1 = , VAR2 =  ,VAR3 = ,VAR4 = );

PROC SGPLOT DATA = &DSN_1.;
VBAR &VAR1. / GROUP = &VAR2. GROUPDISPLAY = CLUSTER;
TITLE 'DISTRIBUTION OF CUSTOMERS HAVING THE CHECKING ACCOUNT AS PER THE AREA';
RUN;

PROC SGPLOT DATA = &DSN_1.;
VBAR &VAR3. / GROUP = &VAR2. GROUPDISPLAY = CLUSTER;
TITLE 'DISTRIBUTION OF CUSTOMERS HAVING THE SAVING ACCOUNT AS PER THE AREA';
RUN;

PROC SGPLOT DATA = &DSN_1.;
TITLE'Customers having Insufficient Funds';
VBAR &VAR4./GROUP = &VAR1.;
RUN;
QUIT;
%MEND;

ODS PDF FILE = "&DIR.\Images.PDF";
%BIVARIATE(DSN = &DSN_1.,VAR1 = DDA, VAR2= RES, VAR3 = SAV, VAR4 = NSF);
ODS PDF CLOSE;


/*TELLER, PHONE,NSF, MTG, DDA, CHECKS, CRSCORE, CCPURC, AGE*/
/*As it is a normal distribution but having outliers so we can replace the missing values with median*/

/*Missing Values Treatment*/

%MACRO MISSING (DSN = , OUT = );
PROC MEANS DATA = &DSN. N NMISS;
RUN;

PROC STDIZE DATA = &DSN. OUT= &OUT. METHOD= MEDIAN REPONLY;
 VAR Age Income CRScore;
RUN;

/*Checking if the missing values are replaced by median value*/

PROC MEANS DATA = &OUT. MAXDEC=2 N NMISS;
RUN;

%MEND;

%MISSING(DSN= &DSN_1., OUT = RIMA.BANK_NEW);

/*Replacing the missing values of Binary Class Variable with the Word 'Missing'*/


/* Recode by using IF-THEN  */

%MACRO RECODE (DATA = , VAR = , OUTPUT = );
data &OUTPUT.;
set &DATA.;
/* use IF-THEN logic to recode gender */
length &VAR._N $8;
if      &VAR.= 0 then &VAR._N = "NO";
else if &VAR.=1 then &VAR._N = "YES";
else &VAR._N = "MISSING ";
DROP &VAR.;
run;
%MEND;


%RECODE (DATA = RIMA.BANK_NEW, VAR = CC, OUTPUT = RIMA.BNK_001);
%RECODE (DATA = RIMA.BNK_001, VAR = TELLER, OUTPUT = RIMA.BNK_02);
%RECODE (DATA = RIMA.BNK_02, VAR =CCPurc , OUTPUT = RIMA.BNK_03);
%RECODE (DATA = RIMA.BNK_03, VAR = Phone, OUTPUT = RIMA.BNK_04);

/*CHECKING IF ALL THE MISSING VALUES HAS BEEN TREATED*/

PROC MEANS DATA=RIMA.BNK_04 N NMISS;
RUN;


/*Other way to do the Missing Value Treatment of Binary class variable (with mode)*/

%MACRO REPLACE (INPUT= ,STATS= ,VARS= ,OUTPUT= );

* GENERATE ANALYSIS RESULTS ;
PROC UNIVARIATE DATA=&INPUT NOPRINT;
VAR &VARS;
OUTPUT OUT=DUMMY &STATS= &VARS;
RUN;

* CONVERT TO VERTICAL ;
PROC TRANSPOSE DATA=DUMMY OUT=DUMMY;
RUN;

* REPLACE MISSING WITH ANALYSIS RESULTS ;
DATA &OUTPUT;
SET &INPUT;
ARRAY VARS &VARS ;
DO I =1 TO DIM(VARS);
SET DUMMY(KEEP=COL1) POINT= I ;
VARS(I)=COALESCE(VARS(I),COL1);
DROP COL1 ;
END;
RUN;

%MEND;


/*Variables with outliers
TELLER, PHONE,NSF, MTG, DDA, CHECKS, CRSCORE, CCPURC, AGE*/

/*TREATING THE OUTLIERS*/

%MACRO OUTLIER (DATA= , VARNAME = , THRESHOLD= , OUTPUT = );
 PROC MEANS DATA = &DATA. MAXDEC=2 N P25 P75 QRANGE;
 VAR &VARNAME.;
 RUN;

 PROC MEANS DATA = &DATA. MAXDEC = 2 N P25 P75 QRANGE;
 VAR &VARNAME.;
 OUTPUT OUT = RIMA.DEL P25 = Q1 P75 = Q3 QRANGE=IQR;
 RUN;

 DATA RIMA.TEMP1;
  SET RIMA.DEL ;
 LOWER_LIMIT = Q1 - (&THRESHOLD.*IQR);
 UPPER_LIMIT = Q1 + (&THRESHOLD.*IQR);
 RUN;

/*CARTESIAN PRODUCT*/
  PROC SQL;
  CREATE TABLE RIMA.DATA_01 AS
  SELECT A.*,B.LOWER_LIMIT, B.UPPER_LIMIT
  FROM &DATA. AS A, RIMA.TEMP1 AS B
  ;
  QUIT;

  DATA RIMA.DATA_02;
  SET RIMA.DATA_01;
  IF &VARNAME. LE LOWER_LIMIT THEN RANGE = "BELOW LOWER LIMIT";
  ELSE IF &VARNAME. GE UPPER_LIMIT THEN RANGE = "ABOVE UPPER LIMIT";
  ELSE RANGE = "WITHIN RANGE";
  RUN;
  QUIT;

/*PRINTING WITHIN RANGE DATA*/

  PROC SQL;
  CREATE TABLE &OUTPUT.(DROP= LOWER_LIMIT UPPER_LIMIT RANGE) AS
  SELECT *
  FROM RIMA.DATA_02
  WHERE RANGE = "WITHIN RANGE";
  QUIT;

  PROC PRINT DATA=&OUTPUT.;
  RUN;

  /*DROP THE UNNECESSARY VARIABLES AND THE DATASETS */

/*  PROC DELETE DATA= &DATA.;*/
/*  RUN;*/

/*  DATA &OUTPUT.;*/
/*  DROP = LOWER_LIMIT UPPER_LIMIT RANGE);*/
/*  RUN;*/
  
/*  PROC DELETE DATA= RIMA.DEL; RUN;*/
/*  PROC DELETE DATA= RIMA.TEMP1; RUN;*/
/*  PROC DELETE DATA= RIMA.DATA_01; RUN;*/
/*  PROC DELETE DATA= RIMA.DATA_02; RUN;*/
 
  %MEND OUTLIER;


/*TELLER, PHONE,NSF, MTG, DDA, CHECKS, CRSCORE, CCPURC, AGE*/
%OUTLIER(DATA = RIMA.BNK_04, VARNAME = CRSCORE, THRESHOLD = 1.5, OUTPUT = RIMA.BANK_000);



PROC SQL;
CREATE TABLE RIMA.BANK_FINAL AS
SELECT *
FROM RIMA.BANK_000;
QUIT;


/*DATA TRANSFORMATION: CONTINUOUS TO CATEGORICAL VARIABLES*/

/*RECODING THE DDA and SAV VARIABLE AS ACTIVE AND INACTIVE CUSTOMERS*/

%MACRO RECODE_SAV (DATA = , VAR = , OUTPUT = );
data &OUTPUT.;
set &DATA.;
/* use IF-THEN logic to recode gender */
length SAVING_ACCOUNT $8;
if      &VAR.= 1 then SAVING_ACCOUNT = "ACTIVE";
else SAVING_ACCOUNT = "INACTIVE ";
DROP &VAR.;
run;

PROC PRINT DATA = &OUTPUT.;
RUN;
%MEND;

%RECODE_SAV(DATA = RIMA.BANK_FINAL , VAR = SAV , OUTPUT = RIMA.BANK_UPDATED_0);



%MACRO RECODE_DDA (DATA = , VAR = , OUTPUT = );
data &OUTPUT.;
set &DATA.;
/* use IF-THEN logic to recode gender */
length CHECKING_ACCOUNT $8;
if      &VAR.= 1 then CHECKING_ACCOUNT = "ACTIVE";
else CHECKING_ACCOUNT = "INACTIVE ";
DROP &VAR.;
run;

PROC PRINT DATA = &OUTPUT.;
RUN;
%MEND;

%RECODE_DDA (DATA = RIMA.BANK_UPDATED_0 , VAR = DDA , OUTPUT = RIMA.BANK_UPDATED);


/*GROUPING THE DATA USING HPBIN PROCEDURE*/

%MACRO BIN (DATA = , VAR1 = ,BINS = , OUT =);
PROC HPBIN DATA = &DATA. OUTPUT = &OUT. NUMBIN = &BINS.;
INPUT &VAR1.;
RUN;
%MEND;

%BIN (DATA = RIMA.BANK_UPDATED , VAR1 = AGE , BINS = 3 , OUT = RIMA.BANK1);

/*Creating the final dataset after treating the missing values and the outliers*/

PROC SQL;
CREATE TABLE RIMA.BANK_FINALISED AS
SELECT *
FROM RIMA.BANK_UPDATED;
QUIT;

%LET DSN_2 = RIMA.BANK_FINALISED;


/*The other way of grouping data using Proc Format if we know the range*/

/*Transforming Age column into groups*/

PROC MEANS DATA = &DSN_2. MAXDEC=1 N NMISS MIN MAX RANGE;
 VAR AGE;
RUN;

PROC FORMAT ; 
VALUE AGE_GROUP  LOW- 30  = "YOUNG"
		    	 31- 60   = " ADULT"
		 		 61- HIGH = "SENIOR";
RUN;


%LET DSN = &DSN_2.;
%LET VAR1 = AGE;
%LET VAR2 = CHECKING_ACCOUNT;

PROC FREQ DATA = &DSN_2.;
TITLE "RELATIONSHIP BETWEEN &VAR1. AND &VAR2.";
 TABLE &VAR1. * &VAR2. /CHISQ NOROW NOCOL ;
 FORMAT &VAR1. AGE_GROUP.;
RUN;

PROC SGPLOT DATA =  &DSN_2.;
TITLE "ACTIVE/INACTIVE CUSTOMERS AS PER THE AGE GROUP";
 VBAR &VAR1./ GROUP = &VAR2. ;
 FORMAT &VAR1. AGE_GROUP.;
RUN;
QUIT;


/*Transforming Credit Score column into Categories*/

PROC MEANS DATA = &DSN_2. MAXDEC=1 N NMISS MIN MAX RANGE;
 VAR CRSCORE;
RUN;

PROC FORMAT ; 
VALUE CRS  LOW- 600  = "GOOD"
		    	 601- 700 = " VERY GOOD"
		 		 701- HIGH  = "EXCELLENT";
RUN;


%LET DSN =  &DSN_2.;
%LET VAR1 = CRSCORE;
%LET VAR2 = CHECKING_ACCOUNT;

PROC FREQ DATA =  &DSN_2.;
TITLE "RELATIONSHIP BETWEEN &VAR1. AND &VAR2.";
 TABLE &VAR1. * &VAR2. /CHISQ NOROW NOCOL ;
 FORMAT &VAR1. CRS.;
RUN;

PROC SGPLOT DATA =  &DSN_2.;
TITLE "ACTIVE/INACTIVE CUSTOMERS AS PER THE CREDIT SCORE DISTRIBUTION";
 VBAR &VAR1./ GROUP = &VAR2. ;
 FORMAT &VAR1. CRS.;
RUN;
QUIT;



/*BUSINESS QUESTIONS*/

ODS PDF FILE = "C:\Users\Veena Nigam\Desktop\SAS Documents\SAS Business Project\Images.PDF";

/*Which type of Account customers are preferring to keep with the bank?*/

/*Distribution of Checking Account*/

PROC TEMPLATE; 
DEFINE STATGRAPH PIE;  
BEGINGRAPH;    
ENTRYTITLE "DISTRIBUTION OF CHECKING ACCOUNT"; 
LAYOUT REGION;     
PIECHART CATEGORY= CHECKING_ACCOUNT / DATALABELLOCATION=CALLOUT DATASKIN = SHEEN  DATALABELCONTENT = ALL CATEGORYDIRECTION = CLOCKWISE START = 180 NAME = 'PIE' ; 
DISCRETELEGEND 'PIE'; 
ENDLAYOUT;
ENDGRAPH;
END;
RUN;

PROC SGRENDER DATA = &DSN_2. TEMPLATE = PIE;
RUN;

/*Distribution of the Saving Account*/

goptions cback=black; 
PROC TEMPLATE; 
DEFINE STATGRAPH PIE;  
BEGINGRAPH;    
ENTRYTITLE "DISTRIBUTION OF SAVING ACCOUNT"; 
LAYOUT REGION;     
PIECHART CATEGORY= SAVING_ACCOUNT / DATALABELLOCATION=CALLOUT DATASKIN = SHEEN  DATALABELCONTENT = ALL CATEGORYDIRECTION = CLOCKWISE START = 180 NAME = 'PIE' ; 
DISCRETELEGEND 'PIE'; 
ENDLAYOUT;
ENDGRAPH;
END;
RUN;

PROC SGRENDER DATA = &DSN_2. TEMPLATE = PIE;
RUN;

/*What type of Account are liked by the customers as per the income*/

PROC MEANS DATA = RIMA.BANK_FINAL MAXDEC=1 N NMISS MIN MAX RANGE;
 VAR INCOME;
RUN;

PROC FORMAT;
VALUE INC  LOW- 50= "LOW"
		    	 51- 100 = "AVERAGE"
		 		 100- HIGH  = "EXCELLENT";
RUN;


/*Is there any effect of Banking habits on customers becoming active or inactive?*/

/*Creating a table of non-missing values*/

PROC SQL;
CREATE TABLE RIMA.HABIT AS
SELECT *
FROM &DSN_2.
WHERE PHONE_N NE "MISSING" AND TELLER_N NE "MISSING";
QUIT;

/*CUSTOMER SATISFACTION COUNT USING PHONE BANKING*/

%let graphs='C:\Users\Veena Nigam\Desktop\SAS Documents\SAS Business Project\IMAGES';

%let dpi=100;
%let w=8in;
%let h=4.5in;

title;
footnote;
ods html close;

/*--Macro Program to find the effect of banking habits on the customers*/
%macro RGBHex(rr,gg,bb);
  %sysfunc(compress(CX%sysfunc(putn(&rr,hex2.))
  %sysfunc(putn(&gg,hex2.))
  %sysfunc(putn(&bb,hex2.))))
%mend RGBHex;
proc template;
  define statgraph BarPie;
    dynamic _tsize _lsize;
    begingraph;
      entrytitle "Customers Satisfaction Count using Phone Banking as per the Area" / textattrs=(size=_tsize);

      /*--Define an attribute map to use specific colors by type--*/
      discreteattrmap name='PHONE_N' / ignorecase=true;
        value 'YES' / fillattrs=(color=%rgbhex(100, 150, 40));
        value 'NO' / fillattrs=(color=%rgbhex(180, 110, 50));
      enddiscreteattrmap;

       /*--Associate the Attribute Map to the TYPE variable--*/
      discreteattrvar attrvar=type var=type attrmap='PHONE_N';

      /*--Define a one row x two columns layout --*/
      layout lattice / columns=2 columnweights=(0.6 0.4) columngutter=20;

        /*--First cell has a Bar Chart--*/
        layout overlay / xaxisopts=(display=(tickvalues)) walldisplay=none
                         yaxisopts=(display=(tickvalues) linearopts=(tickvalueformat=percent.)
                         griddisplay=on offsetmax=0.2);
          barchart category=PHONE_N / name='Y' barlabel=true stat=proportion
                   dataskin=pressed group=RES groupdisplay=cluster barlabelformat=percent6.1
                   baselineattrs=(thickness=0) barlabelattrs=(size=_lsize) grouporder=descending
                   /*--FILLTYPE= requires SAS 9.4 (TS1M2) or later--*/
                   filltype=gradient
          ;
          discretelegend 'Y' / location=inside halign=center valign=top autoitemsize=true valueattrs=(size=_lsize);
        endlayout;

        /*--Second cell has a Pie Chart--*/
        layout region / pad=(bottom=30);
          piechart category=RES /  dataskin=sheen centerfirstslice=true start=270 stat=pct
                   datalabelattrs=(size=_lsize);;
        endlayout;
      endlayout;
    endgraph;
  end;
run;

/*--Render the graph--*/
ods listing style=listing gpath=&graphs image_dpi=&dpi;
ods graphics / reset width=&w height=&h imagename='BarPie';
proc sgrender data=RIMA.HABIT template=BarPie;
  dynamic _tsize=16 _lsize=13;
run;

/*CUSTOMER SATISFACTION COUNT USING TELLER SERVICES*/


%let graphs='C:\Users\Veena Nigam\Desktop\SAS Documents\SAS Business Project\IMAGES';

%let dpi=100;
%let w=8in;
%let h=4.5in;

title;
footnote;
ods html close;

/*--Macro Program to find the effect of banking habits on the customers*/
%macro RGBHex(rr,gg,bb);
  %sysfunc(compress(CX%sysfunc(putn(&rr,hex2.))
  %sysfunc(putn(&gg,hex2.))
  %sysfunc(putn(&bb,hex2.))))
%mend RGBHex;
proc template;
  define statgraph BarPie;
    dynamic _tsize _lsize;
    begingraph;
      entrytitle "Customers Satisfaction Count visiting Teller as per the Area" / textattrs=(size=_tsize);

      /*--Define an attribute map to use specific colors by type--*/
      discreteattrmap name='TELLER_N' / ignorecase=true;
        value 'YES' / fillattrs=(color=%rgbhex(100, 150, 40));
        value 'NO' / fillattrs=(color=%rgbhex(180, 110, 50));
      enddiscreteattrmap;

       /*--Associate the Attribute Map to the TYPE variable--*/
      discreteattrvar attrvar=type var=type attrmap='TELLER_N';

      /*--Define a one row x two columns layout --*/
      layout lattice / columns=2 columnweights=(0.6 0.4) columngutter=20;

        /*--First cell has a Bar Chart--*/
        layout overlay / xaxisopts=(display=(tickvalues)) walldisplay=none
                         yaxisopts=(display=(tickvalues) linearopts=(tickvalueformat=percent.)
                         griddisplay=on offsetmax=0.2);
          barchart category=TELLER_N / name='Y' barlabel=true stat=proportion
                   dataskin=pressed group=RES groupdisplay=cluster barlabelformat=percent6.1
                   baselineattrs=(thickness=0) barlabelattrs=(size=_lsize) grouporder=descending
                   /*--FILLTYPE= requires SAS 9.4 (TS1M2) or later--*/
                   filltype=gradient
          ;
          discretelegend 'Y' / location=inside halign=center valign=top autoitemsize=true valueattrs=(size=_lsize);
        endlayout;

/*        --Second cell has a Pie Chart--*/
        layout region / pad=(bottom=30);
          piechart category=RES /  dataskin=sheen centerfirstslice=true start=270 stat=pct
                   datalabelattrs=(size=_lsize);;
        endlayout;
      endlayout;
    endgraph;
  end;
run;

/*--Render the graph--*/
ods listing style=listing gpath=&graphs image_dpi=&dpi;
ods graphics / reset width=&w height=&h imagename='BarPie1';
proc sgrender data=RIMA.HABIT template=BarPie;
  dynamic _tsize=16 _lsize=13;
run;


/*What age group of customers are the most active customers?*/

data RIMA.AGE;
set &DSN_2.;
if CHECKING_ACCOUNT = "ACTIVE" then ACTIVE=1;
else INACTIVE=0;
/*ACTIVE=round(-2500*(1+ranuni(2))); INACTIVE=round(2400*(1+ranuni(2)));*/
CHECKING_ACCOUNT=0;
LENGTH AGEGROUP $12;
IF AGE LE 20 THEN AGEGROUP="TEEN";
IF 21<AGE<=30 THEN AGEGROUP="YOUNG ADULT";
IF 31<AGE<=50 THEN AGEGROUP="ADULT";
IF AGE GE 50 THEN AGEGROUP="SENIOR";

run;

data RIMA.AGE;
  length Age $12; 
  do Age='Teen', 'Young Adult', 'Adult', 'Senior';
    ACTIVE=round(-500*(1+ranuni(2))); INACTIVE=round(400*(1+ranuni(2))); CHECKING_ACCOUNT=0; output;
  end;

proc format;
  picture positive low-<0='0000' 0<-high='0000';

title 'ACTIVE/INACTIVE CUSTOMERS AS PER THE AGE';
proc sgplot data= RIMA.AGE noautolegend;
/*  format A B positive.;*/
  hbarparm category=age response=ACTIVE / dataskin=sheen name='m' 
    fillattrs=graphdata1 datalabel=ACTIVE datalabelattrs=(size=10) transparency=0.2;
  hbarparm category=age response=INACTIVE / dataskin=sheen name='f'
    fillattrs=graphdata2 datalabel=INACTIVE datalabelattrs=(size=10) transparency=0.2;
  scatter x=CHECKING_ACCOUNT y=age / markerchar=age markercharattrs=(size=11 weight=bold);
  keylegend 'm' 'f';
  xaxis values=(-1000 to 1000 by 200) display=(nolabel) grid offsetmin=0.05 offsetmax=0.05;
  yaxis display=(noticks novalues nolabel);
run;

/*Which branches are offering the direct deposit facilities*/

title"COMPLEMENTORY DIRECT DEPOSIT OFFERED BY VARIOUS BRANCHES";
proc sgplot data=&DSN_2. noautolegend;
  waterfall category=BRANCH response=DIRDEP/ colorgroup=checking_account dataskin=sheen datalabel name='a';
  keylegend 'a' / location=outside position=topright across=1;
  xaxis display=(nolabel);
  yaxis grid display=(nolabel) offsetmin=0;
run;


/*Which branch is having the maximum number of Inactive customers?*/
proc template;
  define statgraph barchart;
    begingraph / attrpriority=none;
      entrytitle "ACTIVE/INACTIVE CUSTOMERS AS PER THE BRANCHES";
      layout overlay;
        barchart x=CHECKING_ACCOUNT / name="BRANCHES"
          stat=pct display=all
          group=BRANCH groupdisplay=cluster group100=positive
          barlabel=true;
        discretelegend "BRANCHES";
      endlayout;
    endgraph;
  end;
run;

proc sgrender data=&dsn_2. template=barchart;
run;


/*Which is the oldest Branch as per the age of the Account?*/

proc template;
  define statgraph barchart;
    begingraph;
      entrytitle "Oldest Branch as per the Account Age";
      layout overlay;
        barchart category=branch response=acctage/ name="bar"
          stat=mean orient=horizontal
          colorbyfreq=true colorstat=pct;
        continuouslegend "bar" / 
          title="Percent of age of the Account";
      endlayout;
    endgraph;
  end;
run;

proc sgrender data=rima.project4 template=barchart;
run;

/*What type of Income people are the most active customers in the different branches of the bank ?*/

DATA RIMA.INCOME;
 SET &DSN_2.;
IF INCOME LE 50 THEN INCOME_1= 0;
ELSE IF 51< INCOME <=100 THEN INCOME_2 = 1;
ELSE INCOME_3 =2;
RUN; 


proc template;
define statgraph barchart;
    begingraph;
      entrytitle "INCOME GROUP AS PER THE AREA";
layout overlay / cycleattrs=true 
    xaxisopts=(display=(tickvalues))
    yaxisopts=(label="INCOME" offsetmax=0.2);
  barchart category=BRANCH response=INCOME_1 / stat=sum name="POOR INCOME"
    legendlabel="POOR INCOME" datatransparency=0.2
    discreteoffset=-0.2 barwidth=0.5 ;
  barchart category=BRANCH response=INCOME_2 / stat=sum name="AVERAGE INCOME"
    legendlabel="AVERAGE INCOME" datatransparency=0.2
    discreteoffset=0   barwidth=0.5 ;
  barchart category=BRANCH response= INCOME_3 / stat=sum name="HIGHER INCOME"
    legendlabel="HIGHER INCOME" datatransparency=0.2
    discreteoffset=+0.2 barwidth=0.5 ;

  discretelegend "POOR INCOME" "AVERAGE INCOME" "HIGHER INCOME" / title="INCOME:"
    location=inside halign=right valign=top;
endlayout;
endgraph;
  end;
run;

proc sgrender data= RIMA.INCOME template=barchart;
run;


/**/

data rima.atm;
 set &dsn_2.;
if atm = 1 then atm_n = "YES";
else if atm =0 then atm_n = "NO";
run;

proc freq data=rima.atm;
tables atm_n*checking_account / norow chisq plots=MOSAIC; /* alias for MOSAICPLOT */
run;

/*Does people also prefer to keep Money Market accounts instead of other type of accounts?*/
proc sgpanel data=rima.project4;
    panelby dda;
    vbar sav / response=MM group= Res groupdisplay=cluster stat=mean;
    title "Distribution of Accounts as per the Area";
run;
title;

/*Do the customers with mortgage are the most loyal customers?*/
proc sgpanel data=rima.project4;
    panelby dda;
    vbar MTG / response= INS group= DDA groupdisplay=cluster stat=mean;
    title "Customers keeping the loan account but not the checking account";
run;
title;
ODS PDF CLOSE;


/*Hypothesis Testing*/

/* Perform the t-test */

title 'Two Sample T-Test';

proc ttest data= &dsn_2.;

class checking_account; /* defines the grouping variable */

var age income; /* variable whose means will be compared */

run;

%MACRO RECODE_SAV (DATA = , VAR = ,NEW_VAR= , OUTPUT = );
data &OUTPUT.;
set &DATA.;
/* use IF-THEN logic to recode gender */
length NEW_VAR $8;
if      &VAR.= 1 then NEW_VAR = "YES";
else NEW_VAR = "NO";
DROP &VAR.;
run;

PROC PRINT DATA = &OUTPUT.;
RUN;
%MEND;

%RECODE_SAV(DATA = &DSN_2., VAR = ATM ,NEW_VAR = ATM_N, OUTPUT = RIMA.BANK_0);
%RECODE_SAV(DATA = RIMA.BANK_0, VAR = INS ,NEW_VAR = INS_N, OUTPUT = RIMA.BANK_A);
%RECODE_SAV(DATA = RIMA.BANK_A, VAR = MTG ,NEW_VAR = MTG_N, OUTPUT = RIMA.BANK_B);
%RECODE_SAV(DATA = RIMA.BANK_B, VAR = DIRDEP ,NEW_VAR = DIRDEP_N, OUTPUT = RIMA.BANK_C);


/*FINAL VARIABLE SELECTION*/

/*SAS MACRO : VARIABLE SELECTION BASED ON WALD CHI-SQUARE*/
/*Variable Selection based on Univariate Analysis (Wald Chi-Square and Standardized Coefficient)*/
/*PROC LOGISTIC is run on each of the variables and tracking p-value of wald chi-square and standardized coefficient*/

%macro perf(data=,targetvar=,vars=,output=);

%let n=%sysfunc(countw(&vars));
%do i=1 %to &n;
%let val = %scan(&vars,&i);

ods select none;
ods output ParameterEstimates=Estimate&i;
proc logistic data=&data;
model &targetvar(event='1')=&val / stb;
run;

data Estimate&i;
set Estimate&i;
length Sig $15;
where Variable NE 'Intercept';
if ProbChiSq < .05 then Sig ='Significant';
else if ProbChiSq >= .05 then Sig = 'Non-Significant';
help = abs(StandardizedEst);
run;

%end;

data &output;
set Estimate1 - Estimate&n;
run;

proc datasets library=work
nodetails nolist;
delete Estimate1 - Estimate&n;
run;
quit;

proc sort data = &output;
by descending help;
run;

data &output;
set &output(DROP=HELP);
run;

%mend perf;

options symbolgen mlogic;
%perf(data=rima.project4 ,targetvar= dda ,vars= ATM INS MTG CC CCPURC TELLER PHONE RES BRANCH DIRDEP INV INCOME MM SAV NSF ,output= rima.result1);

/*AcctAge DDABal CashBk Checks DirDep NSF NSFAmt Phone Teller Sav SavBal ATM ATMAmt POS POSAmt CD CDBal IRA IRABal LOC LOCBal ILS ILSBal 
MM MMBal MMCred MTG MTGBal CC CCBal CCPurc SDB Income HMOwn LORes HMVal Age CRScore Moved InArea Ins Branch Res Dep DepAmt Inv InvBal*/
/*ATM INS CC SAV CCPURC MTG RES*/


/*MODEL BUILDING*/

/*LOGISTIC REGRESSION MODEL*/

/*Y - DDA
 X- CRSCORE, DIRDEP, NSF, RES, ATM ,CC, MTG*/

%MACRO RECODE (DATA = , VAR = , NEW_VAR= ,OUTPUT = );
data &OUTPUT.;
set &DATA.;
/* use IF-THEN logic to recode gender */
length &NEW_VAR. $8;
if      &VAR.= "NO" then &NEW_VAR. = 0;
else if &VAR.= "YES" then &NEW_VAR. = 1;
else &NEW_VAR. = "MISSING ";
DROP &VAR.;
run;
%MEND;


%RECODE (DATA = &DSN_2., VAR = CC_N, NEW_VAR = CC, OUTPUT = RIMA.BANK_1);
%RECODE (DATA = RIMA.BANK_1, VAR = CCPURC_N, NEW_VAR = CCPURC, OUTPUT = RIMA.BANK_2);

PROC SQL;
CREATE TABLE RIMA.BANK_DATA AS
SELECT *
FROM RIMA.BANK_2
WHERE CC NE "MISSING" AND CCPURC NE "MISSING";
QUIT;

/*SPLITING THE DATA INTO TRAINING AND TESTING*/

PROC SURVEYSELECT DATA= RIMA.BANK_DATA OUT= RIMA.MODEL_DATA RATE=0.7 OUTALL; 
RUN;

PROC FREQ DATA = RIMA.MODEL_DATA;
TABLE SELECTED/MISSING;
RUN;


DATA TRAINING TESTING;
 SET RIMA.MODEL_DATA;
  IF SELECTED EQ 1 THEN OUTPUT TRAINING;
 ELSE OUTPUT TESTING;
RUN;

/*MODEL 1*/
PROC LOGISTIC DATA= TRAINING plots(only MAXPOINTS=NONE)=(roc ODDSRATIO);
class CC(param=ref ref="1") CCPURC(param=ref ref="1") mtg(param=ref ref="1") ins(param=ref ref="1") atm(param=ref ref="1");
model checking_account(event="active") = cc|ccpurc|mtg|ins|atm/CLODDS=PL;
oddsratio CC;
oddsratio CCPURC;
oddsratio mtg;
oddsratio ins;
oddsratio atm;
OUTPUT OUT = BANK_PROB P = PRED_PROB;
 RUN;
 QUIT;


/*INTERACTIONS*/

TITLE "RUNNING A LOGISTIC REGRESSION MODEL WITH INTERACTIONS";
 PROC LOGISTIC DATA = TRAINING plots(only MAXPOINTS=NONE)=(roc ODDSRATIO) ;
 class CC(param=ref ref="1") CCPURC(param=ref ref="1") mtg(param=ref ref="1") ins(param=ref ref="1") atm(param=ref ref="1");
model checking_account(event="active") = cc|ccpurc|mtg|ins|atm @2/CLODDS=PL SELECTION= BACKWARD SLSTAY=0.1;;
oddsratio CC;
oddsratio CCPURC;
oddsratio mtg;
oddsratio ins;
oddsratio atm;
OUTPUT OUT = BANK_PROB P = PRED_PROB;
 RUN;
QUIT;


/* Creating Logistic regression model using stepwise selection method */

Title"Logistic Regression Model using Stepwise Selection Method";
proc logistic data=training plots(only MAXPOINTS=NONE)=(roc ODDSRATIO);
class CC CCPURC atm mtg ins;
model checking_account(event="active") = cc|CCPURC|atm|mtg|ins/selection=stepwise expb stb lackfit;
oddsratio CC ;
oddsratio CCPURC;
oddsratio atm;
oddsratio mtg;
oddsratio ins;
output out = temp p=new;
store bank_logistic;
run;


*MODEL SCORING: TRAINING AND TESTING;
*1. ;
TITLE "MODEL SCORING : SCORING OPTION";
 PROC LOGISTIC DATA = TRAINING plots(only MAXPOINTS=NONE) ;
 class CC CCPURC atm mtg ins;
 model checking_account(event="active") = cc|CCPURC|atm|mtg|ins @2/CLODDS=PL SELECTION= BACKWARD SLSTAY=0.1 ;
 OUTPUT OUT= TEST P=PROB;
 SCORE DATA = TESTING OUT=TESTING_SCORE_FINAL;
RUN;
QUIT;







proc logistic data=training;
class cc atm INS;
model checking_account(event="active") = CC | ATM |INS;
oddsratio ATM / at(CC='1');
run;






