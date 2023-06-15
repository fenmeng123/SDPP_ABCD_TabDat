# =============================================================================#
# SDPP Step 1: Preparing ABCD Demographic Variables
# R Packages Dependency: bruceR
# Step File Notes: 
# 1. Coding Scheme is referred to (Song et al., 2023) and ABCD Data Analytics and
# Informatics Resource Center (ABCD-DAIRC at UCSD).
# 2. Reference Link (Github Repository from ABCD-DAIRC):
# https://github.com/ABCD-STUDY/analysis-nda
# 3. Target File: pdem02.txt; acspsw03.txt; abcd_ehis01.txt; abcd_ant01.txt.
# Update Date: 2023.06.03 By Kunru Song
# =============================================================================#
# 1.1 Library Packages and Prepare Environment --------------------------------
library(bruceR)
set.wd()
source('SDPP_subfunctions.R')
# 1.2 SDPP Parameter Settings -------------------------------------------------
TabulatedDataDirectory = '../../Download_ABCDV4.0_skr220403/Package_1199282'
# Please replace the above string for your own downloaded data directory.
# Relative Path is required! (relative to the path of the current R script file)
ProjectDirectory = '../DataAnalysis/SMA_Trajectory'
Prefix = 'ABCD4.0'
AutoLogFileName = ''
# ==============================MAIN CODES=====================================#
# Create Project Folders and get the log-file directory
AutoLogFolder = SDPP.ABCD.TabDat.PrepareProject(ProjectDirectory)
# 2. Load pdem02.txt and perform re-coding --------------------------------
pdem_FileDir = fullfile(TabulatedDataDirectory,'pdem02.txt')
demo_base = readABCDdata(pdem_FileDir)
demo_base_recode = subset(demo_base,select = c(subjectkey,interview_date,interview_age,sex))

# 2.1 Youth's Grade -------------------------------------------------------
# demo_ed_v2, What grade is the child in? If it is summer, indicate grade starting in the fall.
# 0 = KINDERGARTEN ; 1 = 1ST GRADE  ; 2 = 2ND GRADE ; 3 = 3RD GRADE ; 4 = 4TH GRADE ; 5 = 5TH GRADE;
# 6 = 6TH GRADE ; 7 = 7TH GRADE ; 8 = 8TH GRADE; 9 = 9TH GRADE; 10 = 10TH GRADE ; 11 = 11TH GRADE;
# 12 = 12TH GRADE
demo_base_recode$EducationC = RECODE(demo_base$demo_ed_v2,
"0='KINDERGARTEN';1='1ST GRADE'; 2='2ND GRADE'; 3='3RD GRADE'; 4='4TH GRADE'; 5='5TH GRADE';
6='6TH GRADE'; 7='7TH GRADE'; 8='8TH GRADE'; 9='9TH GRADE';
10='10TH GRADE'; 11='11TH GRADE'; 12='12TH GRADE';else=NA")

# 2.2 Youth's Adoption Info -----------------------------------------------
# demo_adopt_agex_v2, How old was he/she at the time of adoption?
# demo_adopt_agex_v2_bl_dk, 999 = Dont know
demo_base_recode$AdoptionYear = as.numeric(
  paste(demo_base$demo_adopt_agex_v2,demo_base$demo_adopt_agex_v2_bl_dk,sep = ''))
demo_base_recode$AdoptionFlag = !is.na(demo_base_recode$AdoptionYear)
demo_base_recode$AdoptionYear[demo_base_recode$AdoptionYear==999]=NA

# 2.3 Youth's Sex assigned at birth ---------------------------------------
# demo_sex_v2, What sex was the child assigned at birth, on the original birth certificate? 
# 1 = Male ; 2 = Female ; 3 = Intersex-Male; 4 = Intersex-Female;
# 999 = Don't know; 777 = Refuse to answer
demo_base_recode$SexAssigned = RECODE(demo_base$demo_sex_v2,
                                      "1 = 'Male' ;
                                      2 = 'Female' ;
                                      3 = 'Intersex-Male';
                                      4 = 'Intersex-Female';
                                      else=NA")

# 2.4 Youth's Current Gender Identity -------------------------------------
# demo_gender_id_v2, What is the child's current gender identity?
# 1 = Male; 2 = Female ; 3 = Trans male; 4 = Trans female; 5 = Gender queer;
# 6 = Different; 777 = Refuse to answer; 999 = Don't know No lo sÃ©
demo_base_recode$GenderIdentity_PrntRep = RECODE(demo_base$demo_gender_id_v2,
                                                 "1 = 'Male';
                                                 2 = 'Female' ;
                                                 3 = 'Trans male';
                                                 4 = 'Trans female';
                                                 5 = 'Gender queer';
                                                 6 = 'Different';else=NA")

# 2.5 Youth's Race identified by their parents ----------------------------
# demo_race_a_p___10  What race do you consider the child to be? 
# White: 10.White;
# Black: 11, Black/African American
# AIAN: 12, American Indian, Native American India Americana; 13, Alaska Native;
# NHPI: 14, Native Hawaiian; 15, Guamanian; 16, Samoan; 17, Other Pacific Islander;
# Asian: 18, Asian Indian; 19, Chinese 20, Filipino; 21, Japanese; 22, Korean; 23, Vietnamese; 24, Other Asian;
# Other: 25, Other Race;
# 77, Refuse To Answer; 99, Don't Know
# Parent-reported race was coded according to https://github.com/ABCD-STUDY/analysis-nda/blob/master/notebooks/general/categorical_extension3.0.R
Race_PrntRep = subset(demo_base,select = c(demo_race_a_p___10,demo_race_a_p___11,
                                  demo_race_a_p___12,demo_race_a_p___13,
                                  demo_race_a_p___14,demo_race_a_p___15,
                                  demo_race_a_p___16,demo_race_a_p___17,
                                  demo_race_a_p___18,demo_race_a_p___19,
                                  demo_race_a_p___20,demo_race_a_p___21,
                                  demo_race_a_p___22,demo_race_a_p___23,
                                  demo_race_a_p___24,demo_race_a_p___25,
                                  demo_race_a_p___77,demo_race_a_p___99))
Race_PrntRep = as.data.table(Race_PrntRep)
# 2.5.1 White
# "White"
Race_PrntRep [ , white:=(demo_race_a_p___10=='1')*1]
# 2.5.2 Black
# "Black/African American Negra o afroamericana"
Race_PrntRep [ , black:=(demo_race_a_p___11=='1')*1]
# 2.5.3 Asian
# 'Asian Indian', 'Chinese', 'Filipino', 'Japanese', 'Korean', 'Vietnamese', 'Other Asian'
Race_PrntRep [,asian:=0]
Race_PrntRep [(
  demo_race_a_p___18 == "1" |
    demo_race_a_p___19 == "1" |
    demo_race_a_p___20 == "1" |
    demo_race_a_p___21 == "1" |
    demo_race_a_p___22 == "1" |
    demo_race_a_p___23 == "1" |
    demo_race_a_p___24=="1"), asian:= 1 ]
# 2.5.4 AIAN: American Indian and Alaska Native
Race_PrntRep [,AmericanIndian_AlaskaNative:=0]
Race_PrntRep [(demo_race_a_p___12 == "1" | demo_race_a_p___13 == "1" ), AmericanIndian_AlaskaNative:= 1 ]

# 2.5.5 NHPI: Native Hawaiian and Other Pacific
Race_PrntRep[, NativeHawaiian_OtherPacific:= 0]
Race_PrntRep[ (demo_race_a_p___14 == "1" | demo_race_a_p___15 == "1" | demo_race_a_p___16 == "1" |
       demo_race_a_p___17 == "1"), NativeHawaiian_OtherPacific:= 1 ]
# 2.5.6 Other
Race_PrntRep[, other:= 0 ]
Race_PrntRep[ demo_race_a_p___25 == "1", other:= 1 ]
# 2.5.7 Mixed Race
Race_PrntRep[, mixed:= (white + black + asian + AmericanIndian_AlaskaNative + NativeHawaiian_OtherPacific + other)]
Race_PrntRep[ mixed <= 1, mixed:= 0]
Race_PrntRep[ mixed > 1, mixed:= 1]

# Recode parent-reported child's race into 7 levels (White,Black,Asian,AIAN,NHPI,Other,Mixed)
Race_PrntRep[(white==1 ),Race_Level:='White']
Race_PrntRep[(black==1 ),Race_Level:='Black']
Race_PrntRep[(asian==1 ),Race_Level:='Asian']
Race_PrntRep[(AmericanIndian_AlaskaNative==1),Race_Level:='AIAN']
Race_PrntRep[(NativeHawaiian_OtherPacific==1 ),Race_Level:='NHPI']
Race_PrntRep[(other==1 ),Race_Level:='Other']
Race_PrntRep[(mixed==1),Race_Level:='Mixed']
demo_base_recode$Race_PrntRep = Race_PrntRep$Race_Level

# 2.6 Youth's Ethnicity identified by parents -----------------------------
# demo_ethn_v2 Do you consider the child Hispanic/Latino/Latina? 
# 1 = Yes; 2 = No; 777 = Refuse to answer; 999 = Don't know
demo_base_recode$Ethnicity_PrntRep = RECODE(demo_base$demo_ethn_v2,"1 = 'Hispanic/Latino/Latina'; 2 = 'No'; else=NA")


# 2.7 Youth's born country ------------------------------------------------
# demo_origin_v2, In which country was the child born? 
# 189 = USA
demo_base_recode$BirthCountry = RECODE(demo_base$demo_origin_v2,"189='USA';1:188='Other';190:198='Other';else=NA")

# 2.8 Years of youth lived in USA -----------------------------------------
# demo_years_us_v2, demo_years_us_v2_dk;  How many years has the child lived in the United States? 
# 0 :: 20
demo_base_recode$USALiveYears = as.numeric(paste(demo_base$demo_years_us_v2,demo_base$demo_years_us_v2_dk,sep = ''))


# 2.9 Youth's Religion identified by parents ------------------------------
# demo_relig_v2, What is the child's religious preference?
# 1 = Mainline Protestant; 2 = Evangelical Protestant ; 3 = Historically Black Church ; 4 = Roman Catholic; 5 = Jewish; 6 = Mormon; 
# 7 = Jehovahs Witness; 8 = Muslim; 9 = Buddhist ; 10 = Hindu ; 11 = Orthodox Christian; 12 = Unitarian; 13 = Other Christian; 14 = Atheist;
# 15 = Agnostic; 16 = Something else; 17 = Nothing in Particular; 777 = Refused to answer; 999 = Dont know
demo_base_recode$Religon_PrntRep = RECODE(demo_base$demo_relig_v2,"1 = 'Mainline Protestant'; 2 = 'Evangelical Protestant' ; 3 = 'Historically Black Church' ; 4 = 'Roman Catholic'; 5 = 'Jewish'; 6 = 'Mormon'; 
7 = 'Jehovahs Witness'; 8 = 'Muslim'; 9 = 'Buddhist' ; 10 = 'Hindu' ; 11 = 'Orthodox Christian'; 12 = 'Unitarian'; 13 = 'Other Christian'; 14 = 'Atheist';
15 = 'Agnostic'; 16 = 'Something else'; 17 = 'Nothing in Particular';else=NA")


# 2.10 Parent's Marital Status -----------------------------------------
# demo_prnt_marital_v2, Are you now married, widowed, divorced, separated, never married or living with a partner?
# 1 = Married ; 2 = Widowed; 3 = Divorced; 4 = Separated; 5 = Never married; 
# 6 = Living with partner; 777 = Refused to answer
demo_base_recode$ParentMaritalC = RECODE(demo_base$demo_prnt_marital_v2,"1 = 'Married' ; 2 = 'Widowed';
                                                          3 = 'Divorced'; 4 = 'Separated'; 5 = 'Never married'; 
                                                          6 = 'Living with partner';else=NA")


demo_base_recode$ParentMarital = RECODE(demo_base$demo_prnt_marital_v2,
                                        "c(1,6) = 'Married or living with partner';c(2,3,4,5)='Single';else=NA")

# 2.11 Parents' Education Level -------------------------------------------
# demo_prnt_ed_v2, What is the highest grade or level of school you have completed or the highest degree you have received?
# 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11 ; 12 ; 13 ; 14 ; 15 ; 16 ; 17 ; 18 ; 19 ; 20 ; 21 ; 777
# 0 = Never attended/Kindergarten only ; 1 = 1ST GRADE  ; 2 = 2ND GRADE ; 3 = 3RD GRADE ; 4 = 4TH GRADE ; 5 = 5TH GRADE;
# 6 = 6TH GRADE ; 7 = 7TH GRADE ; 8 = 8TH GRADE; 9 = 9TH GRADE; 10 = 10TH GRADE ; 11 = 11TH GRADE;12 = 12TH GRADE
# 13 = High school graduate;
# 14 = GED or equivalent Diploma General;
# 15 = Some college;16 = Associate degree: Occupational;17 = Associate degree: Academic Program;
# 18 = Bachelor's degree;19 = Master's degree; 
# 20 = Professional School degree (ex. MD); 21 = Doctoral degree (ex. PhD); 777 = Refused to answer
demo_base_recode$ParentHighEdu = RECODE(demo_base$demo_prnt_ed_v2,
                                        "0:14='high school or less';
                                        15:21='Colleage';else=NA;")

# The following questions are about your partner. 
# Your "partner" refers to any significant figure in your life that helps you in raising your child or has helped you for more than 2 years. 
# This person should be involved 40% or more of the daily activities your child does. For example, your partner could be your spouse. However, your partner could also be your boyfriend/girlfriend or relative.

# demo_prtnr_ed_v2, What is the highest grade or level of school your partner completed or highest degree they received?
demo_base_recode$PartnerHighEdu = RECODE(demo_base$demo_prtnr_ed_v2,
                                         "0:14='high school or less';15:21='Colleage or more';else=NA;")
CombPrntsHighEdu = cbind(as.numeric(demo_base$demo_prnt_ed_v2),
                         as.numeric(demo_base$demo_prtnr_ed_v2))
CombPrntsHighEdu = RECODE(CombPrntsHighEdu,'NA=-1;999=-1;777=-1;')
demo_base_recode$ParentsEdu = apply(CombPrntsHighEdu,1,max)
demo_base_recode$ParentsEdu = RECODE(demo_base_recode$ParentsEdu,
                                     "0:13='< HS Diploma';
                                     14:15='HS Diploma/GED';
                                     15:17='Some College';
                                     18='Bachelor';
                                     19:21='Post Graduate Degree';
                                     else=NA")

# 2.12 Parents' Employment Status ----------------------------------------------
# demo_prnt_empl_v2, Are you working now, looking for work, retired, stay at home parent, a student, or something else?
# 1 = Working now: FULL TIME/PART TIME; 2 = Temporarily Laid off ; 
# 3 = Looking for work; 4 = Retired ; 5 = Disabled: Permanently or Temporarily ;
# 6 = Stay at Home Parent; 7 = Student ; 8 = Other (Specify);
# 9 = Sick Leave; 10 = Maternity Leave; 11 = Unemployed not looking for work ;  777 = Refused to answer
demo_base_recode$ParentEmploy = RECODE(demo_base$demo_prnt_empl_v2,
                                       "1='Working';
                                       c(2,3,4,5,6,7,8,9,10,11)='Non-working';else=NA")
# demo_prtnr_empl_v2, We would like to know about what your partner does - are they working, looking for work, retired, stay at home parent, a student, or what?
demo_base_recode$PartnerEmploy = RECODE(demo_base$demo_prtnr_empl_v2,
                                        "1='Working';
                                        c(2,3,4,5,6,7,8,9,10,11)='Non-working';else=NA")
demo_base_recode$FamliyEmploy = cbind(
  RECODE(demo_base$demo_prnt_empl_v2,"1=1;c(2,3,4,5,6,7,8,9,10,11)=0;else=NA"),
  RECODE(demo_base$demo_prtnr_empl_v2,"1=1;c(2,3,4,5,6,7,8,9,10,11)=0;else=NA"))
demo_base_recode$NumInLF = apply(demo_base_recode$FamliyEmploy,1,sum)

demo_base_recode$ParentsMaritalEmploy[demo_base_recode$ParentMarital=='Married or living with partner' & 
                                        demo_base_recode$ParentEmploy=='Working' & 
                                        demo_base_recode$PartnerEmploy=='Working']='Married, 2 in LF'
demo_base_recode$ParentsMaritalEmploy[demo_base_recode$ParentMarital=='Married or living with partner' & 
                                        demo_base_recode$ParentEmploy=='Non-working' & 
                                        demo_base_recode$PartnerEmploy=='Working']='Married, 1 in LF'
demo_base_recode$ParentsMaritalEmploy[demo_base_recode$ParentMarital=='Married or living with partner' & 
                                        demo_base_recode$ParentEmploy=='Working' & 
                                        demo_base_recode$PartnerEmploy=='Non-Working']='Married, 1 in LF'
demo_base_recode$ParentsMaritalEmploy[demo_base_recode$ParentMarital=='Married or living with partner' & 
                                        demo_base_recode$ParentEmploy=='Non-working' & 
                                        demo_base_recode$PartnerEmploy=='Non-working']='Married, 0 in LF'
demo_base_recode$ParentsMaritalEmploy[demo_base_recode$ParentMarital=='Single' & 
                                        demo_base_recode$ParentEmploy=='Non-working']='Single, Not in LF'
demo_base_recode$ParentsMaritalEmploy[demo_base_recode$ParentMarital=='Single' & 
                                        demo_base_recode$ParentEmploy=='Working']='Single, in LF'

# 2.13 Family Income ------------------------------------------------------
# demo_comb_income_v2,What is your TOTAL COMBINED FAMILY INCOME for the past 12 months? This should include 
# income (before taxes and deductions) from all sources, wages, rent from properties, social security, disability 
# and/or veteran's benefits, unemployment benefits, workman's compensation, help from relative 
# (include child payments and alimony), and so on.
# 1= Less than $5,000; 2=$5,000 through $11,999; 3=$12,000 through $15,999; 4=$16,000 through $24,999; 
# 5=$25,000 through $34,999; 6=$35,000 through $49,999; 
# 7=$50,000 through $74,999; 
# 8= $75,000 through $99,999; 
# 9=$100,000 through $199,999; 
# 10=$200,000 and greater. 
# 999 = Don't know; 777 = Refuse to answer.
# If Separated/Divorced, please average the two household incomes.
demo_base_recode$FamilyIncome = RECODE(demo_base$demo_comb_income_v2,
                                       "c(1,2,3,4)='<$25k';c(5,6)='$25k-$49k';7='$50k-$74k';
                                       8='$75k-$99k';9='$100k-$199k';10='$200k+';else=NA")

# 2.14 Household Size -----------------------------------------------------
# demo_roster_v2,demo_roster_v2_refuse, How many people are living at your address?
demo_base_recode$HouseholdSize = RECODE(as.numeric(paste(demo_base$demo_roster_v2,demo_base$demo_roster_v2_refuse,sep = '')),
                                        "lo:3='2 to 3';4='4';5='5';6='6';7:100='7+';else=NA")

# 2.15 Household Structure ------------------------------------------------
# demo_child_time_v2, Is there another household in which the child spends a significant amount of time?
# 1 = Yes; 0 = No; 777 = Declined to answer
demo_base_recode$HouseholdStructure = RECODE(demo_base$demo_child_time_v2,
                                             "1='Another household';0='Single household';else=NA")


# 3. Load Propensity-based Weight Scores ---------------------------------------
# Ref: https://nda.nih.gov/data_structure.html?short_name=acspsw03
acspsw_FileDir = fullfile(TabulatedDataDirectory,'acspsw03.txt')
acs_weight = readABCDdata(acspsw_FileDir)
acs_weight = subset(acs_weight,
                    select = -c(acspsw03_id,src_subject_id))
acs_base_recode = select(acs_weight,c(subjectkey,interview_date,interview_age,sex))
# 3.1 Youth's Family ID ---------------------------------------------------
acs_base_recode$FamilyID = acs_weight$rel_family_id

# 3.2 Youth's Race&Ethnicity ----------------------------------------------
# 1 = White; 2 = Black; 3 = Hispanic; 4 = Asian; 5 = Other
acs_base_recode$RaceEthnicity = RECODE(acs_weight$race_ethnicity,
                                       "1='White';
                                       2='Black';
                                       3='Hispanic';
                                       4='Asian';
                                       5='Other';
                                       else=NA")

# 3.3 Youth's Family Relathionship ----------------------------------------
# 	0 = single; 1 = sibling; 2 = twin; 3 = triplet
acs_base_recode$Relationship = RECODE(acs_weight$rel_relationship,
                                      "0='single';
                                      1='sibling';
                                      2='twin';
                                      3='triplet';
                                      else=NA")

# 3.4 Same sex twin flag --------------------------------------------------
# Same sex twin 1 = Yes; 0 = No
acs_base_recode$SameSexTwinFlag = RECODE(acs_weight$rel_same_sex,"1='Yes';0='No';else=NA")

# 3.5 Youth's ACS-PS Weights ----------------------------------------------
# Imputed raked propensity weight. The raked propensity weight merges the ACS and ABCD data 
# (with missing data imputed), estimates the propensity model, computes and scales/trims the propensity 
# weights and finally rakes the scaled weights to final ACS control totals by age, sex and race/ethnicity.
acs_base_recode$ACS_weight = acs_weight$acs_raked_propensity_score


# 3.6 Genetics Info: Proportion of African ancestry -----------------------
acs_base_recode$GeneInfo_AncProp_African <- as.numeric(acs_weight$genetic_af_african)

# 3.7 Genetics Info: Proportion of European ancestry ----------------------
acs_base_recode$GeneInfo_AncProp_European <- as.numeric(acs_weight$genetic_af_european)

# 3.8 Genetics Info: Proportion of East Asian ancestry ----------------------
acs_base_recode$GeneInfo_AncProp_EastAsian <- as.numeric(acs_weight$genetic_af_east_asian)

# 3.9 Genetics Info: Proportion of American ancestry ----------------------
acs_base_recode$GeneInfo_AncProp_American <- as.numeric(acs_weight$genetic_af_american)


# 3.10 Probability of identity by descend between participant  ------------
# Descending order by SubID 1, 2, 3, 4
acs_base_recode$GeneInfo_IdentityProb_SubID_1 = as.numeric(acs_weight$genetic_pi_hat_1)
acs_base_recode$GeneInfo_IdentityProb_SubID_2 = as.numeric(acs_weight$genetic_pi_hat_2)
acs_base_recode$GeneInfo_IdentityProb_SubID_3 = as.numeric(acs_weight$genetic_pi_hat_3)
acs_base_recode$GeneInfo_IdentityProb_SubID_4 = as.numeric(acs_weight$genetic_pi_hat_4)

# 3.11 Genetically inferred zygosity status -------------------------------
# Genetically inferred zygosity status between participant and SubID 1, 2, 3, 4
# 1= monozygotic ; 2= dizygotic ; 3=siblings ;-1= not available (twins/sibs, genetic_pi_hat not calculated)
acs_base_recode$GeneInfo_Zygosity_SubID_1 = RECODE(acs_weight$genetic_zygosity_status_1,
                                                  "1 = 'Monozygotic';
                                                   2 = 'Dizygotic';
                                                   3 = 'Siblings';
                                                  else=NA")
acs_base_recode$GeneInfo_Zygosity_SubID_2 = RECODE(acs_weight$genetic_zygosity_status_2,
                                                   "1 = 'Monozygotic';
                                                   2 = 'Dizygotic';
                                                   3 = 'Siblings';
                                                  else=NA")
acs_base_recode$GeneInfo_Zygosity_SubID_3 = RECODE(acs_weight$genetic_zygosity_status_3,
                                                   "1 = 'Monozygotic';
                                                   2 = 'Dizygotic';
                                                   3 = 'Siblings';
                                                  else=NA")
acs_base_recode$GeneInfo_Zygosity_SubID_4 = RECODE(acs_weight$genetic_zygosity_status_4,
                                                   "1 = 'Monozygotic';
                                                   2 = 'Dizygotic';
                                                   3 = 'Siblings';
                                                  else=NA")

# 3.12 genetic related with participant based on Identity -----------------
acs_base_recode$GeneInfo_Paired_SubID_1 = (acs_weight$genetic_paired_subjectid_1)
acs_base_recode$GeneInfo_Paired_SubID_2 = (acs_weight$genetic_paired_subjectid_2)
acs_base_recode$GeneInfo_Paired_SubID_3 = (acs_weight$genetic_paired_subjectid_3)
acs_base_recode$GeneInfo_Paired_SubID_4 = (acs_weight$genetic_paired_subjectid_4)

acs_base_recode = select(acs_base_recode,
                         c(subjectkey,interview_date,interview_age,sex,
                           FamilyID,Relationship,ACS_weight,
                           GeneInfo_Paired_SubID_1,
                           GeneInfo_IdentityProb_SubID_1,
                           GeneInfo_Zygosity_SubID_1,
                           GeneInfo_Paired_SubID_2,
                           GeneInfo_IdentityProb_SubID_2,
                           GeneInfo_Zygosity_SubID_2,
                           GeneInfo_Paired_SubID_3,
                           GeneInfo_IdentityProb_SubID_3,
                           GeneInfo_Zygosity_SubID_3,
                           GeneInfo_Paired_SubID_4,
                           GeneInfo_IdentityProb_SubID_4,
                           GeneInfo_Zygosity_SubID_4,
                           everything(),
                           ))

# 4. Youth's Handedness ---------------------------------------------------
# ehi_y_ss_scoreb, 1=right handed; 2=left handed; 3=mixed handed//
# Calculation: if ((mean([ehi1b], [ehi2b], [ehi3b],[ehi4b]) < -60),2, 
# if ((mean([ehi1b], [ehi2b], [ehi3b],[ehi4b]) > 60),1,3)); 1. 
# Veale, J. F. (2014) Edinburgh Handedness Inventory - Short Form: a revised version based on confirmatory factor analysis. Laterality 19(2):164-77. 
# //If mixed handed, use the hand that the child writes with for NeuroCog and fMRI tasks.
ehis_FileDir = fullfile(TabulatedDataDirectory,'abcd_ehis01.txt')
EHIS = readABCDdata(ehis_FileDir)
EHIS$Handedness = RECODE(EHIS$ehi_y_ss_scoreb,
                         "1='Right';2='Left';3='Mixed';else=NA")
EHIS = subset(EHIS,select = -c(abcd_ehis01_id,src_subject_id,ehi1b,ehi2b,ehi3b,ehi4b,ehi_time,ehi_y_ss_scoreb))

# 5. Youth's Body Mass Index ----------------------------------------------
# The calculation of Body Mass Index was referred to
# https://github.com/ABCD-STUDY/analysis-nda/blob/master/notebooks/general/core_demographics3.0.R
ant_FileDir = fullfile(TabulatedDataDirectory,'abcd_ant01.txt')
BMI = readABCDdata(ant_FileDir)
# Average Measured Weight (lbs):If three measurements were obtained, the two closest measurements will be averaged. 
# Should the third measurement fall equally between the first two measurements, all three will be averaged.
# Calculation: 
BMI$anthroheightcalc = as.numeric(BMI$anthroheightcalc)
BMI$anthroweightcalc = as.numeric(BMI$anthroweightcalc)
BMI$anthroweight1lb = as.numeric(BMI$anthroweight1lb)
BMI$anthroweight2lb = as.numeric(BMI$anthroweight2lb)
BMI$anthroweight3lb = as.numeric(BMI$anthroweight3lb)
# find fall between and average all three measures
tmp = BMI[(!is.na(BMI$anthroweight3lb) & !is.na(BMI$anthroweight2lb) & !is.na(BMI$anthroweight1lb)),]
weight12_min = apply(data.table(tmp$anthroweight1lb,tmp$anthroweight2lb),1,min)
weight12_max = apply(data.table(tmp$anthroweight1lb,tmp$anthroweight2lb),1,max)
FallBetwIdx = (tmp$anthroweight3lb > weight12_min) &  (tmp$anthroweight3lb < weight12_max)
tmp$anthroweightcalc[FallBetwIdx] = MEAN(tmp[FallBetwIdx,],vars = c('anthroweight1lb','anthroweight2lb','anthroweight3lb'))
# locate the rest measures (not fall between) and find the closest two measures
weight123 = subset(tmp,select = c(anthroweight1lb,anthroweight2lb,anthroweight3lb))
weight123 = weight123[!FallBetwIdx,]
weight123$anthorweightcalc = NA
weight123_diff = data.table()
weight123_diff$diff12 =  abs(weight123$anthroweight1lb - weight123$anthroweight2lb)
weight123_diff$diff23 =  abs(weight123$anthroweight2lb - weight123$anthroweight3lb)
weight123_diff$diff13 =  abs(weight123$anthroweight1lb - weight123$anthroweight3lb)
cloestidx = (apply(weight123_diff, 1, which.min))
for (i in 1:length(cloestidx)){
  if (cloestidx[i]==1){
    weight123$anthorweightcalc[i] = mean(weight123$anthroweight1lb[i],weight123$anthroweight2lb[i])
  }else if (cloestidx[i]==2){
    weight123$anthorweightcalc[i] = mean(weight123$anthroweight2lb[i],weight123$anthroweight3lb[i])
  }else if (cloestidx[i]==3){
    weight123$anthorweightcalc[i] = mean(weight123$anthroweight1lb[i],weight123$anthroweight3lb[i])
  }
}
tmp$anthroweightcalc[!FallBetwIdx]  = weight123$anthorweightcalc
# attach calculated weight to BMI
BMI$anthroweightcalc[(!is.na(BMI$anthroweight3lb) & !is.na(BMI$anthroweight2lb) & !is.na(BMI$anthroweight1lb))] = tmp$anthroweightcalc
# calculate the rest who has only two measures, and then average them
BMI$anthroweightcalc[is.na(BMI$anthroweightcalc)] = MEAN(BMI[is.na(BMI$anthroweightcalc),],
                                                         vars = c('anthroweight1lb','anthroweight2lb'),na.rm = F)
# calculate BMI using the formula: weight/(height)^2 * 703, weight in lb, height in inch.
BMI$BMI_calc = BMI$anthroweightcalc / (BMI$anthroheightcalc)^2 * 703
BMI$BMI_calc[which(BMI$BMI_calc>36 | BMI$BMI_calc< 11)]=NA #reset unrealistic values

BMI = select(BMI,
             c(subjectkey,interview_date,interview_age,sex,eventname,BMI_calc))

# 6. Load Longitudinal Track Index ----------------------------------------
# Longitudinal Tracking Instrument (abcd_lt01): This instrument includes two
# variables that in combination provide information about the overall visit
# type for the session. The in-person, remote, and hybrid visits can be determined using:
# • If sched_delay = 7, then visit = In person
# • If sched_delay = 9 and sched_hybrid = 0, then visit = Remote
# • If sched_delay = 9 and sched_hybrid = 1, then visit = Hybrid
lt_FileDir = fullfile(TabulatedDataDirectory,'abcd_lt01.txt')
demo_anchor = readABCDdata(lt_FileDir)

demo_anchor = subset(demo_anchor,
                     eventname=='baseline_year_1_arm_1' |
                       eventname=='1_year_follow_up_y_arm_1'|
                       eventname=='2_year_follow_up_y_arm_1'|
                       eventname=='3_year_follow_up_y_arm_1',
                     select = -c(abcd_lt01_id,src_subject_id))
demo_anchor = as.data.table(demo_anchor)
demo_anchor [ (sched_delay=='7') , SessionType:='In-person']
demo_anchor [ (sched_delay=='9' & sched_hybrid=='0') , SessionType:='Remote']
demo_anchor [ (sched_delay=='9' & sched_hybrid=='1') , SessionType:='Hybrid']
demo_anchor [ (sched_delay=='9' & sched_hybrid=='') , SessionType:='Uncertain']
demo_anchor = select(demo_anchor,
                     -c(sched_delay,sched_hybrid))

# 7. Merge Data -----------------------------------------------------------
# Baseline Observation Carry Forward (BOCF)
demo_base_recode$eventname = 'baseline_year_1_arm_1'
demo_base_recode_y1 = select(demo_base_recode,-c(interview_date,interview_age))
demo_base_recode_y1$eventname = '1_year_follow_up_y_arm_1'
demo_base_recode_y2 = select(demo_base_recode,-c(interview_date,interview_age))
demo_base_recode_y2$eventname = '2_year_follow_up_y_arm_1'
demo_base_recode_y3 = select(demo_base_recode,-c(interview_date,interview_age))
demo_base_recode_y3$eventname = '3_year_follow_up_y_arm_1'
demo_base_recode_y0 = merge(demo_base_recode,
                            subset(demo_anchor,eventname=='baseline_year_1_arm_1'),
                            by=c('subjectkey','interview_date','interview_age','sex','eventname'),
                            all.y = T)
demo_base_recode_y1 = merge(demo_base_recode_y1,
                            subset(demo_anchor,eventname=='1_year_follow_up_y_arm_1'),
                            by=c('subjectkey','sex','eventname'),
                            all.y = T)
demo_base_recode_y2 = merge(demo_base_recode_y2,
                            subset(demo_anchor,eventname=='2_year_follow_up_y_arm_1'),
                            by=c('subjectkey','sex','eventname'),
                            all.y = T)
demo_base_recode_y3 = merge(demo_base_recode_y3,
                            subset(demo_anchor,eventname=='3_year_follow_up_y_arm_1'),
                            by=c('subjectkey','sex','eventname'),
                            all.y = T)
demo_base = rbind(demo_base_recode_y0,demo_base_recode_y1,
                  demo_base_recode_y2,demo_base_recode_y3)
# Generate Demographic: Merge ACS Propensity-based Weights with pdem
Demographic = merge(demo_base,
                    acs_base_recode,
                    by = intersect(colnames(demo_base_recode),
                                   colnames(acs_base_recode)),
                    all = T)
# Generate Demographic: Merge acspsw+pdem with EHIS (handedness)
Demographic = merge(Demographic,EHIS,
                    by = intersect(colnames(Demographic),colnames(EHIS)),
                    all = T)
# Generate Demographic: Merge acspsw+pdem+ehis with BMI
Demographic = merge(Demographic,BMI,
                    by = intersect(colnames(Demographic),colnames(BMI)),
                    all = T)
# BOCF for ACS-PSW and EHIS variables
BOCF.Variables(Demographic,'baseline_year_1_arm_1','FamilyID') %>%
  BOCF.Variables('baseline_year_1_arm_1','Relationship') %>%
  BOCF.Variables('baseline_year_1_arm_1','RaceEthnicity') %>%
  BOCF.Variables('baseline_year_1_arm_1','SameSexTwinFlag') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_AncProp_African') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_AncProp_European') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_AncProp_EastAsian') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_AncProp_American') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_Paired_SubID_1') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_Paired_SubID_2') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_Paired_SubID_3') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_Paired_SubID_4') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_IdentityProb_SubID_1') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_IdentityProb_SubID_2') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_IdentityProb_SubID_3') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_IdentityProb_SubID_4') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_Zygosity_SubID_1') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_Zygosity_SubID_2') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_Zygosity_SubID_3') %>%
  BOCF.Variables('baseline_year_1_arm_1','GeneInfo_Zygosity_SubID_4') %>%
  BOCF.Variables('baseline_year_1_arm_1','Handedness') -> Demographic

# 8. Set Columns Data Type and Re-order Columns ---------------------------
sapply(Demographic, typeof)
Demographic$AdoptionFlag <- as.numeric(Demographic$AdoptionFlag)
Demographic = select(Demographic,-FamliyEmploy)
Demographic = select(Demographic,
                     c(subjectkey,eventname,interview_age,SessionType,
                       sex,Race_PrntRep,Ethnicity_PrntRep,RaceEthnicity,
                       Handedness,BMI_calc,
                       ParentMaritalC,ParentMarital,ParentsEdu,
                       ParentEmploy,PartnerEmploy,
                       FamilyIncome,Relationship,SameSexTwinFlag,
                       HouseholdSize,HouseholdStructure,
                       BirthCountry,Religon_PrntRep,ParentsMaritalEmploy,NumInLF,
                       site_id_l,FamilyID,ACS_weight,
                       everything()))

# 9. Save Double- and Character-type Demographic Data ---------------------
OutputFileName = paste(Prefix,'Demographics','Raw.rds',sep = '_')
OutputFileDir = fullfile(ProjectDirectory,'Res_3_IntermediateData',OutputFileName)
cat(sprintf('Raw Demographics Data (Double and Character) will be saved into: %s\n',OutputFileDir))
saveRDS(Demographic,OutputFileDir)
cat(sprintf('Saving Data into RDS File: %s......\tFinished!\n',OutputFileName))
OutputFileName = paste(Prefix,'Demographics','Raw.csv',sep = '_')
OutputFileDir = fullfile(ProjectDirectory,'Res_3_IntermediateData',OutputFileName)
cat(sprintf('Raw Demographics Data (Double and Character) will be saved into: %s\n',OutputFileDir))
write.csv(Demographic,OutputFileDir,fileEncoding = 'UTF-8')