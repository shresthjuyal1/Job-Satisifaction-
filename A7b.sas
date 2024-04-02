proc format;
    value GenderFmt
        0 = 'Male'
        1 = 'Female';
    value VisMinorityFmt
        0 = 'No'
        1 = 'Yes';
    value MaritalFmt
        1 = 'Never Married'
        2 = 'Married'
        3 = 'Divorced or Separated'
        4 = 'Widowed';
    value BornFmt
        0 = 'Born in Canada'
        1 = 'Born Outside Canada';

proc import datafile="/home/u63745078/Assignment 7/DiversityExplore.xlsx"
    out=work.diversity_explore 
    dbms=xlsx 
    replace;
    getnames=yes;

data work.diversity_explore_final;
    set work.diversity_explore;
    Commitment = sum(of Com1-Com10);
    Relations_Colleagues = sum(of RelC1-RelC5);
    Relations_Management = sum(of RelM1-RelM12);
    Fair_Advancement = sum(of Fair1-Fair6);
    Job_Satisfaction = sum(of Sat1-Sat4);
    Diversity_Commitment = sum(of SM1-SM3);
    
    Numeric_Age = input(Age, best.);
    Numeric_EDUCLevel = input(EDUCLevel, best.);
    drop Age EDUCLevel;
    rename Numeric_Age = Age Numeric_EDUCLevel = EDUCLevel;
    
    Numeric_Gender = input(Gender, best.);
    Numeric_VisMinority = input(VisMinority, best.);
    Numeric_CAN_Foreign_Born = input(CAN_Foreign_Born, best.);

	if MaritalStatus = 2 then Married_Ind = 1;
    else if MaritalStatus in (1, 3, 4) then Married_Ind = 0;
    else Married_Ind = .;
    label Gender = 'Gender'
          VisMinority = 'Visible Minority Status'
          EDUCLevel = 'Education Level'
          MaritalStatus = 'Marital Status'
          CAN_Foreign_Born = 'Born Outside Canada'
          Married_Ind = 'Marital Indicator'
          Commitment = 'Commitment to Organization'
          Relations_Colleagues = 'Relations with Colleagues'
          Relations_Management = 'Relations with Management'
          Fair_Advancement = 'Fair Opportunities for Advancement'
          Job_Satisfaction = 'Job Satisfaction'
          Diversity_Commitment = 'Senior Management Commitment to Diversity'
          Age = 'Age';
    
    format Gender GenderFmt. VisMinority VisMinorityFmt. MaritalStatus MaritalFmt. CAN_Foreign_Born BornFmt.;

proc freq data=work.diversity_explore_final; /*fix labelling*/
	title '2e: frequency distributions of the categorical variables';
    tables Gender VisMinority EDUCLevel MaritalStatus CAN_Foreign_Born Married_Ind / missing;

proc means data=work.diversity_explore_final n mean min max missing;
	title '2e: basic statistics on the quantitative variables.';
    var Commitment Relations_Colleagues Relations_Management Fair_Advancement Job_Satisfaction Diversity_Commitment Age EDUCLevel;
    
proc freq data=work.diversity_explore_final;
	title '2f: test the association between sex and whether or not the person is married';
    tables Gender*Married_Ind / chisq;
    
proc corr data=work.diversity_explore_final nosimple plots=none;
	title '2g: correlation matrix of the quantitative variables,';
    var Commitment Relations_Colleagues Relations_Management Fair_Advancement Job_Satisfaction Diversity_Commitment Age EDUCLevel;
    
proc means data=work.diversity_explore_final mean;
	title '2h:  mean education for minority and non-minority respondents';
    class VisMinority;
    var EDUCLevel;

proc glm data=work.diversity_explore_final plots=None;
	title '2h:Test for significance';
    class VisMinority;
    model EDUCLevel = VisMinority;
    means VisMinority / tukey;
    
proc reg data=work.diversity_explore_final plots=none;
	title '2i';
    model Job_Satisfaction = Relations_Colleagues Relations_Management Fair_Advancement 
                            Diversity_Commitment Numeric_Gender Numeric_VisMinority EDUCLevel 
                            Married_Ind Age Numeric_CAN_Foreign_Born;
    Insignificant: test Diversity_Commitment = Numeric_Gender = EDUCLevel = Married_Ind = Numeric_CAN_Foreign_Born = Age = 0 ;


proc iml;
     title '2j ix:Proportion of remaining variation';
     n = 427; p = 6; s = 6;
     F = 1.12;  a = s*F/(n-p + s*F);
     b = 0.0154939;
     print a;
     print b;
     
proc reg data=work.diversity_explore_final plots=None;
	title '2k:  fit a model with just the four significant explanatory variables';
    model Job_Satisfaction = Relations_Colleagues Relations_Management Fair_Advancement Numeric_VisMinority;

proc import datafile="/home/u63745078/Assignment 7/DiversityReplic.xlsx"
    out=work.diversity_replic
    dbms=xlsx 
    replace;
    getnames=yes;
    
data work.diversity_explore_replic_final;
    set work.diversity_replic;
    Commitment = sum(of Com1-Com10);
    Relations_Colleagues = sum(of RelC1-RelC5);
    Relations_Management = sum(of RelM1-RelM12);
    Fair_Advancement = sum(of Fair1-Fair6);
    Job_Satisfaction = sum(of Sat1-Sat4);
    Diversity_Commitment = sum(of SM1-SM3);
    
    Numeric_Age = input(Age, best.);
    Numeric_EDUCLevel = input(EDUCLevel, best.);
    drop Age EDUCLevel;
    rename Numeric_Age = Age Numeric_EDUCLevel = EDUCLevel;
    
    Numeric_Gender = input(Gender, best.);
    Numeric_VisMinority = input(VisMinority, best.);
    Numeric_CAN_Foreign_Born = input(CAN_Foreign_Born, best.);

	if MaritalStatus = 2 then Married_Ind = 1;
    else if MaritalStatus in (1, 3, 4) then Married_Ind = 0;
    else Married_Ind = .;
    label Gender = 'Gender'
          VisMinority = 'Visible Minority Status'
          EDUCLevel = 'Education Level'
          MaritalStatus = 'Marital Status'
          CAN_Foreign_Born = 'Born Outside Canada'
          Married_Ind = 'Marital Indicator'
          Commitment = 'Commitment to Organization'
          Relations_Colleagues = 'Relations with Colleagues'
          Relations_Management = 'Relations with Management'
          Fair_Advancement = 'Fair Opportunities for Advancement'
          Job_Satisfaction = 'Job Satisfaction'
          Diversity_Commitment = 'Senior Management Commitment to Diversity'
          Age = 'Age';
          
proc reg data=work.diversity_explore_replic_final plots=none;
	title '2n: fit a model with just the four significant explanatory variables replic data.';
    model Job_Satisfaction = Relations_Colleagues Relations_Management Fair_Advancement Numeric_VisMinority; 

proc iml;
     title "Reproduce Bonferroni p-values from proc reg output";
     RC = 0.5123; RM = 0.0001; FA = 0.0001; M = 0.0059;
     print "Uncorrected" RC RM FA M; 
     BonRC = 4*RC; BonRM = 4*RM; BonFA = 4*FA; BonM = 4*M;
     print "Bonferroni " BonRC BonRM BonFA BonM;