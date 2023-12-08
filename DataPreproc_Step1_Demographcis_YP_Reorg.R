# =============================================================================#
# SDPP Step 1: Preparing ABCD Demographic Variables
# R Packages Dependency: bruceR, readxl
# Step File Notes: 
# 1. Coding Scheme is referred to (Song et al., 2023) and ABCD Data Analytics and
# Informatics Resource Center (ABCD-DAIRC at UCSD).
# 2. Reference Link (Github Repository from ABCD-DAIRC):
# https://github.com/ABCD-STUDY/analysis-nda
# 3. Target File
# ABCD 4.0: pdem02.txt; acspsw03.txt; abcd_ehis01.txt; abcd_ant01.txt.
# ABCD 5.0: ce_p_acc.csv; abcd_p_demo.csv;
# 
# Update Date: 2023.12.08
# =============================================================================#
SDPP.Run.Step1 <- function(Prefix,
                           TabulatedDataDirectory,
                           ProjectDirectory,
                           AutoLogFolder,
                           ResultsOutputDir,
                           IntermediateDataDir,
                           SourceScriptName = s_get_script_name()){
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_1.txt'

SubfolderName = "abcd-general"

s_sink(fullfile(AutoLogFolder,AutoLogFileName))

# ==============================MAIN CODES=====================================#
# 2. Load abcd_p_demo.csv and perform re-coding --------------------------------
demo_base <- fullfile(TabulatedDataDirectory,
                        SubfolderName,
                        'abcd_p_demo.csv') %>%
  readABCDdata()
demo_base_recode <- demo_base %>%
  select(
    c(src_subject_id,eventname)
  )

# 2.0 Parent-reported Youth's Age -----------------------------------------
# demo_brthdat_v2, How old is the child?
# demo_brthdat_v2_l
demo_base_recode$YouthAge_Prnt = as.numeric(
  str_remove( paste(demo_base$demo_brthdat_v2,
                    demo_base$demo_brthdat_v2_l,sep = ''),'NA'))

# 2.1 Youth's Grade -------------------------------------------------------
# demo_ed_v2, What grade is the child in? If it is summer, indicate grade starting in the fall.
# 0 = KINDERGARTEN ; 1 = 1ST GRADE  ; 2 = 2ND GRADE ; 3 = 3RD GRADE ; 4 = 4TH GRADE ; 5 = 5TH GRADE;
# 6 = 6TH GRADE ; 7 = 7TH GRADE ; 8 = 8TH GRADE; 9 = 9TH GRADE; 10 = 10TH GRADE ; 11 = 11TH GRADE;
# 12 = 12TH GRADE
demo_base_recode$EducationC <- paste(demo_base$demo_ed_v2,
                                     demo_base$demo_ed_v2_l,
                                     sep = '') %>%
  str_remove_all(pattern = 'NA') %>%
  as.numeric() %>%
  RECODE("0='KINDERGARTEN';
          1='1ST GRADE';
          2='2ND GRADE';
          3='3RD GRADE';
          4='4TH GRADE';
          5='5TH GRADE';
          6='6TH GRADE';
          7='7TH GRADE';
          8='8TH GRADE';
          9='9TH GRADE';
          10='10TH GRADE';
          11='11TH GRADE';
          12='12TH GRADE';
          else=NA")

# 2.2 Youth's Adoption Info -----------------------------------------------
# demo_adopt_agex_v2, How old was he/she at the time of adoption?
# demo_adopt_agex_v2_bl_dk, 999 = Dont know
demo_base_recode$AdoptionAge <- tidyr::replace_na(demo_base$demo_adopt_agex_v2,
                                                  0L)+
                                tidyr::replace_na(demo_base$demo_adopt_agex_v2_bl_dk,
                                                  0L)
demo_base_recode$AdoptionAge <- RECODE(demo_base_recode$AdoptionAge,
                                      "0=NA;")
demo_base_recode$AdoptionFlag = !is.na(demo_base_recode$AdoptionAge) %>%
  as.numeric()
demo_base_recode$AdoptionAge = RECODE(demo_base_recode$AdoptionAge,
                                      "999=NA;")

# 2.3 Youth's Race identified by their parents ----------------------------
# demo_race_a_p___10  What race do you consider the child to be? 
# White: 10.White;
# Black: 11, Black/African American
# AIAN: 12, American Indian, Native American India Americana; 13, Alaska Native;
# NHPI: 14, Native Hawaiian; 15, Guamanian; 16, Samoan; 17, Other Pacific Islander;
# Asian: 18, Asian Indian; 19, Chinese 20, Filipino; 21, Japanese; 22, Korean; 23, Vietnamese; 24, Other Asian;
# Other: 25, Other Race;
# 77, Refuse To Answer; 99, Don't Know
# Parent-reported race was coded according to https://github.com/ABCD-STUDY/analysis-nda/blob/master/notebooks/general/categorical_extension3.0.R
Race_PrntRep = demo_base %>%
  select(
          c(demo_race_a_p___10,demo_race_a_p___11,
            demo_race_a_p___12,demo_race_a_p___13,
            demo_race_a_p___14,demo_race_a_p___15,
            demo_race_a_p___16,demo_race_a_p___17,
            demo_race_a_p___18,demo_race_a_p___19,
            demo_race_a_p___20,demo_race_a_p___21,
            demo_race_a_p___22,demo_race_a_p___23,
            demo_race_a_p___24,demo_race_a_p___25,
            demo_race_a_p___77,demo_race_a_p___99)
          ) %>%
  as.data.table()

# 2.3.1 White
# "White"
Race_PrntRep [ , white:=(demo_race_a_p___10==1)*1]
# 2.3.2 Black
# "Black/African American Negra o afroamericana"
Race_PrntRep [ , black:=(demo_race_a_p___11==1)*1]
# 2.3.3 Asian
# 'Asian Indian', 'Chinese', 'Filipino', 'Japanese', 'Korean', 'Vietnamese', 'Other Asian'
Race_PrntRep [,asian:=0]
Race_PrntRep [(
  demo_race_a_p___18 == 1 |
    demo_race_a_p___19 == 1 |
    demo_race_a_p___20 == 1 |
    demo_race_a_p___21 == 1 |
    demo_race_a_p___22 == 1 |
    demo_race_a_p___23 == 1 |
    demo_race_a_p___24==1), asian:= 1 ]
# 2.3.4 AIAN: American Indian and Alaska Native
Race_PrntRep [,AmericanIndian_AlaskaNative:=0]
Race_PrntRep [(demo_race_a_p___12 == 1 |
                 demo_race_a_p___13 == 1 ), AmericanIndian_AlaskaNative:= 1 ]

# 2.3.5 NHPI: Native Hawaiian and Other Pacific
Race_PrntRep[, NativeHawaiian_OtherPacific:= 0]
Race_PrntRep[ (demo_race_a_p___14 == 1 |
                 demo_race_a_p___15 == 1 |
                 demo_race_a_p___16 == 1 |
                 demo_race_a_p___17 == 1), NativeHawaiian_OtherPacific:= 1 ]
# 2.3.6 Other
Race_PrntRep[, other:= 0 ]
Race_PrntRep[ demo_race_a_p___25 == 1, other:= 1 ]
# 2.2.7 Mixed Race
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

# 2.4 Youth's Ethnicity identified by parents -----------------------------
# demo_ethn_v2 Do you consider the child Hispanic/Latino/Latina? 
# 1 = Yes; 2 = No; 777 = Refuse to answer; 999 = Don't know
demo_base_recode$Ethnicity_PrntRep = RECODE(demo_base$demo_ethn_v2,
                                            "1 = 'Hispanic/Latino/Latina';
                                            2 = 'No';
                                            else=NA")


# 2.5 Youth's born country ------------------------------------------------
# demo_origin_v2, In which country was the child born? 
# 189 = USA
demo_base_recode$BirthCountry = RECODE(demo_base$demo_origin_v2,
                                       "189='USA';
                                       1:188='Other';
                                       190:198='Other';
                                       else=NA")

# 2.6 Years of youth lived in USA -----------------------------------------
# demo_years_us_v2, demo_years_us_v2_dk;  How many years has the child lived in the United States? 
# 0 :: 20
demo_base_recode$USALiveYears = as.numeric(str_remove(paste(demo_base$demo_years_us_v2,
                                      demo_base$demo_years_us_v2_dk,sep = ''),'NA'))
demo_base_recode$USALiveYears = RECODE(demo_base_recode$USALiveYears,
                                       "999=NA;")

# 2.7 Parent-reported Youth's Religious Preference -------------------------
# demo_relig_v2, What is the child's religious preference?
# 1 = Mainline Protestant; 2 = Evangelical Protestant ; 3 = Historically Black Church ; 4 = Roman Catholic; 5 = Jewish; 6 = Mormon; 
# 7 = Jehovahs Witness; 8 = Muslim; 9 = Buddhist ; 10 = Hindu ; 11 = Orthodox Christian; 12 = Unitarian; 13 = Other Christian; 14 = Atheist;
# 15 = Agnostic; 16 = Something else; 17 = Nothing in Particular; 777 = Refused to answer; 999 = Dont know
demo_base_recode$Religon_PrntRep = RECODE(demo_base$demo_relig_v2,
                                          "1 = 'Mainline Protestant';
                                          2 = 'Evangelical Protestant' ; 
                                          3 = 'Historically Black Church' ;
                                          4 = 'Roman Catholic';
                                          5 = 'Jewish'; 6 = 'Mormon'; 
                                          7 = 'Jehovahs Witness'; 8 = 'Muslim'; 
                                          9 = 'Buddhist' ; 10 = 'Hindu' ;
                                          11 = 'Orthodox Christian';
                                          12 = 'Unitarian';
                                          13 = 'Other Christian';
                                          14 = 'Atheist';
                                          15 = 'Agnostic';
                                          16 = 'Something else';
                                          17 = 'Nothing in Particular';
                                          else=NA")

# 2.8 Parent Self-reported Age --------------------------------------------
# demo_prnt_age_v2, How old are you? Provide your age in years. If you reuse to 
# answer please choose "refuse to answer" in what follows below.
demo_base_recode$ParentAge = RECODE(replace_na(demo_base$demo_prnt_age_v2_l,0) + 
                              replace_na(demo_base$demo_prnt_age_v2_refuse_l,0),
                            "0=NA;777=NA;999=NA;lo:17=NA;100:hi=NA;")


# 2.9 Parent Self-reported Current Gender Identity ------------------------
demo_base_recode$GenderIdentity_PrntSelf = RECODE(demo_base$demo_prnt_gender_id_v2,
                                                  "1='Male';
                                                  2='Female';
                                                  3='Trans male';
                                                  4='Trans female';
                                                  5='Queer';
                                                  6='Different';
                                                  777=NA;
                                                  999=NA;")

# 2.10 Parent Self-reported Race ------------------------------------------
Race_PrntSelf = subset(demo_base,select = c(demo_prnt_race_a_v2___10,demo_prnt_race_a_v2___11,
                                           demo_prnt_race_a_v2___12,demo_prnt_race_a_v2___13,
                                           demo_prnt_race_a_v2___14,demo_prnt_race_a_v2___15,
                                           demo_prnt_race_a_v2___16,demo_prnt_race_a_v2___17,
                                           demo_prnt_race_a_v2___18,demo_prnt_race_a_v2___19,
                                           demo_prnt_race_a_v2___20,demo_prnt_race_a_v2___21,
                                           demo_prnt_race_a_v2___22,demo_prnt_race_a_v2___23,
                                           demo_prnt_race_a_v2___24,demo_prnt_race_a_v2___25,
                                           demo_prnt_race_a_v2___77,demo_prnt_race_a_v2___99))
Race_PrntSelf = as.data.table(Race_PrntSelf)
# "White"
Race_PrntSelf [ , white:=(demo_prnt_race_a_v2___10==1)*1]
# "Black/African American"
Race_PrntSelf [ , black:=(demo_prnt_race_a_v2___11==1)*1]
# 'Asian Indian', 'Chinese', 'Filipino', 'Japanese', 'Korean', 'Vietnamese', 'Other Asian'
Race_PrntSelf [,asian:=0]
Race_PrntSelf [(
  demo_prnt_race_a_v2___18 == 1 |
    demo_prnt_race_a_v2___19 == 1 |
    demo_prnt_race_a_v2___20 == 1 |
    demo_prnt_race_a_v2___21 == 1 |
    demo_prnt_race_a_v2___22 == 1 |
    demo_prnt_race_a_v2___23 == 1 |
    demo_prnt_race_a_v2___24==1), asian:= 1 ]
# AIAN: American Indian and Alaska Native
Race_PrntSelf [,AmericanIndian_AlaskaNative:=0]
Race_PrntSelf [(demo_prnt_race_a_v2___12 == 1 |
                  demo_prnt_race_a_v2___13 == 1 ), AmericanIndian_AlaskaNative:= 1 ]
# NHPI: Native Hawaiian and other Pacific Islanders
Race_PrntSelf[, NativeHawaiian_OtherPacific:= 0]
Race_PrntSelf[ (demo_prnt_race_a_v2___14 == 1 |
                  demo_prnt_race_a_v2___15 == 1 |
                  demo_prnt_race_a_v2___16 == 1 |
                  demo_prnt_race_a_v2___17 == 1), NativeHawaiian_OtherPacific:= 1 ]
# Other
Race_PrntSelf[, other:= 0 ]
Race_PrntSelf[ demo_prnt_race_a_v2___25 == 1, other:= 1 ]
# Mixed
Race_PrntSelf[, mixed:= (white + black + asian + AmericanIndian_AlaskaNative + NativeHawaiian_OtherPacific + other)]
Race_PrntSelf[ mixed <= 1, mixed:= 0]
Race_PrntSelf[ mixed > 1, mixed:= 1]
# Recoding Parent Self-reported Race into a 7-level variable
Race_PrntSelf[(white==1 ),Race_Level:='White']
Race_PrntSelf[(black==1 ),Race_Level:='Black']
Race_PrntSelf[(asian==1 ),Race_Level:='Asian']
Race_PrntSelf[(AmericanIndian_AlaskaNative==1),Race_Level:='AIAN']
Race_PrntSelf[(NativeHawaiian_OtherPacific==1 ),Race_Level:='NHPI']
Race_PrntSelf[(other==1 ),Race_Level:='Other']
Race_PrntSelf[(mixed==1),Race_Level:='Mixed']
demo_base_recode$Race_PrntSelf = Race_PrntSelf$Race_Level

# 2.11 Youth's Sex assigned at birth ---------------------------------------
# demo_sex_v2, What sex was the child assigned at birth, on the original birth certificate? 
# 1 = Male ; 2 = Female ; 3 = Intersex-Male; 4 = Intersex-Female;
# 999 = Don't know; 777 = Refuse to answer
demo_base_recode$SexAssigned = RECODE(demo_base$demo_sex_v2,
                                      "1 = 'Male' ;
                                      2 = 'Female' ;
                                      3 = 'Intersex-Male';
                                      4 = 'Intersex-Female';
                                      else=NA")

# 2.12 Youth's Current Gender Identity -------------------------------------
# demo_gender_id_v2, What is the child's current gender identity?
# 1 = Male; 2 = Female ; 3 = Trans male; 4 = Trans female; 5 = Gender queer;
# 6 = Different; 777 = Refuse to answer; 999 = Don't know
demo_base_recode$GenderIdentity_PrntRep = RECODE(demo_base$demo_gender_id_v2,
                                                 "1 = 'Male';
                                                 2 = 'Female' ;
                                                 3 = 'Trans male';
                                                 4 = 'Trans female';
                                                 5 = 'Gender queer';
                                                 6 = 'Different';
                                                 else=NA")

# 2.13 Parent's Marital Status -----------------------------------------
# demo_prnt_marital_v2, Are you now married, widowed, divorced, separated, never married or living with a partner?
# 1 = Married ; 2 = Widowed; 3 = Divorced; 4 = Separated; 5 = Never married; 
# 6 = Living with partner; 777 = Refused to answer
demo_base_recode$ParentsMarital = RECODE(demo_base$demo_prnt_marital_v2,
                                         "1 = 'Married' ; 2 = 'Widowed';
                                         3 = 'Divorced'; 4 = 'Separated';
                                         5 = 'Never married';
                                         6 = 'Living with partner';
                                         else=NA")

# 2.14 Parents' Education Level -------------------------------------------
# demo_prnt_ed_v2, What is the highest grade or level of school you have completed or the highest degree you have received?
# 0 = Never attended/Kindergarten only ; 1 = 1ST GRADE  ; 2 = 2ND GRADE ;
# 3 = 3RD GRADE ; 4 = 4TH GRADE ; 5 = 5TH GRADE;
# 6 = 6TH GRADE ; 7 = 7TH GRADE ; 8 = 8TH GRADE;
# 9 = 9TH GRADE; 10 = 10TH GRADE ; 11 = 11TH GRADE;
# 12 = 12TH GRADE
# 13 = High school graduate;
# 14 = GED or equivalent Diploma General;
# 15 = Some college;16 = Associate degree: Occupational;17 = Associate degree: Academic Program;
# 18 = Bachelor's degree;19 = Master's degree; 
# 20 = Professional School degree (ex. MD); 21 = Doctoral degree (ex. PhD); 777 = Refused to answer
demo_base_recode$ParentHighEdu = RECODE(demo_base$demo_prnt_ed_v2,
                                        "0:13='< HS Diploma';
                                     14:15='HS Diploma/GED';
                                     15:17='Some College';
                                     18='Bachelor';
                                     19:21='Post Graduate Degree';
                                     else=NA")

# demo_prtnr_ed_v2: The following questions are about your partner. 
# Your "partner" refers to any significant figure in your life that helps you in raising your child or has helped you for more than 2 years. 
# This person should be involved 40% or more of the daily activities your child does. For example, your partner could be your spouse. However, your partner could also be your boyfriend/girlfriend or relative.
# What is the highest grade or level of school your partner completed or highest degree they received?
demo_base_recode$PartnerHighEdu = RECODE(demo_base$demo_prtnr_ed_v2,
                                         "0:13='< HS Diploma';
                                     14:15='HS Diploma/GED';
                                     15:17='Some College';
                                     18='Bachelor';
                                     19:21='Post Graduate Degree';
                                     else=NA")
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

# 2.15 Parents' Employment Status ----------------------------------------------
# demo_prnt_empl_v2, Are you working now, looking for work, retired, stay at home parent, a student, or something else?
# 1 = Working now: FULL TIME/PART TIME; 2 = Temporarily Laid off ; 
# 3 = Looking for work; 4 = Retired ; 5 = Disabled: Permanently or Temporarily ;
# 6 = Stay at Home Parent; 7 = Student ; 8 = Other (Specify);
# 9 = Sick Leave; 10 = Maternity Leave; 11 = Unemployed not looking for work ;  777 = Refused to answer
demo_base_recode$ParentEmploy = RECODE(demo_base$demo_prnt_empl_v2,
                                       "1='Working';
                                       2:11='Non-working';else=NA")
# demo_prtnr_empl_v2, We would like to know about what your partner does - are they working, looking for work, retired, stay at home parent, a student, or what?
demo_base_recode$PartnerEmploy = RECODE(demo_base$demo_prtnr_empl_v2,
                                        "1='Working';
                                        2:11='Non-working';else=NA")
FamliyEmploy = cbind(
  RECODE(demo_base$demo_prnt_empl_v2,"1=1;2:11=0;else=NA"),
  RECODE(demo_base$demo_prtnr_empl_v2,"1=1;2:11=0;else=NA"))

demo_base_recode$NumInLF = apply(FamliyEmploy,1,sum)

demo_base_recode$ParentsMarital_2L = RECODE(demo_base_recode$ParentsMarital,
                                            "c('Married','Living with partner')='Married or living with partner';
                                            c('Widowed','Divorced','Separated','Never married')='Single';") 
demo_base_recode$ParentsMaritalXEmploy = ""
demo_base_recode$ParentsMaritalXEmploy[
  demo_base_recode$ParentsMarital_2L=='Married or living with partner' & 
    demo_base_recode$ParentEmploy=='Working' & 
    demo_base_recode$PartnerEmploy=='Working']='Married, 2 in LF'
demo_base_recode$ParentsMaritalXEmploy[
  demo_base_recode$ParentsMarital_2L=='Married or living with partner' & 
    demo_base_recode$ParentEmploy=='Non-working' & 
    demo_base_recode$PartnerEmploy=='Working']='Married, 1 in LF'
demo_base_recode$ParentsMaritalXEmploy[
  demo_base_recode$ParentsMarital_2L=='Married or living with partner' & 
    demo_base_recode$ParentEmploy=='Working' & 
    demo_base_recode$PartnerEmploy=='Non-Working']='Married, 1 in LF'
demo_base_recode$ParentsMaritalXEmploy[ 
  demo_base_recode$ParentsMarital_2L=='Married or living with partner' & 
    demo_base_recode$ParentEmploy=='Non-working' & 
    demo_base_recode$PartnerEmploy=='Non-working']='Married, 0 in LF'
demo_base_recode$ParentsMaritalXEmploy[
  demo_base_recode$ParentsMarital_2L=='Single' & 
    demo_base_recode$ParentEmploy=='Non-working']='Single, Not in LF'
demo_base_recode$ParentsMaritalXEmploy[
  demo_base_recode$ParentsMarital_2L=='Single' & 
    demo_base_recode$ParentEmploy=='Working']='Single, in LF'

# 2.16 Family Income ------------------------------------------------------
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
                                       "c(1,2,3,4)='<$25k';
                                       c(5,6)='$25k-$49k';
                                       7='$50k-$74k';
                                       8='$75k-$99k';
                                       9='$100k-$199k';
                                       10='$200k+';
                                       else=NA")

# 2.17 Household Size -----------------------------------------------------
# demo_roster_v2,demo_roster_v2_refuse, How many people are living at your address?
demo_base_recode$HouseholdSize = RECODE(demo_base$demo_roster_v2,
                                        "1:3='3 and below';
                                        4='4';
                                        5='5';
                                        6='6';
                                        7:hi='7 and more';
                                        else=NA")
demo_base_recode$HouseholdSize[demo_base$demo_roster_v2_refuse==777] = NA
demo_base_recode$HouseholdSize[demo_base$demo_roster_v2_refuse==999] = NA

# 2.18 Household Structure ------------------------------------------------
# demo_child_time_v2, Is there another household in which the child spends a significant amount of time?
# 1 = Yes; 0 = No; 777 = Declined to answer
demo_base_recode$HouseholdStructure = RECODE(demo_base$demo_child_time_v2,
                                             "1='Non-single household';
                                             0='Single household';
                                             else=NA")

# 2.19 Youth's Native Language ------------------------------------------------
# demo_nat_lang_l: What is your child's native language? In other words, what
# was the first language predominantly spoken to your child by their parent or
# guardian after birth? [you can only pick one]
# 58 = English; 1:57 = Other
demo_base_recode$YouthNativeLang = RECODE(demo_base$demo_nat_lang_l,
                                             "58='English';
                                             1:57='Other';
                                             else=NA")

# 3. Load Propensity-based Weight Scores ---------------------------------------
# Ref: https://nda.nih.gov/data_structure.html?short_name=acspsw03


# 3.0 ACS Imputed Raked Propensity Weight ---------------------------------
# Imputed raked propensity weight. The raked propensity weight merges the ACS and ABCD data 
# (with missing data imputed), estimates the propensity model, computes and scales/trims the propensity 
# weights and finally rakes the scaled weights to final ACS control totals by age, sex and race/ethnicity.
demo_base_recode$ACS_weight = demo_base$acs_raked_propensity_score
# 3.1 Youth's Race&Ethnicity ----------------------------------------------
# 1 = White; 2 = Black; 3 = Hispanic; 4 = Asian; 5 = Other
demo_base_recode$RaceEthnicity = RECODE(demo_base$race_ethnicity,
                                       "1='White';
                                       2='Black';
                                       3='Hispanic';
                                       4='Asian';
                                       5='Other';
                                       else=NA")
# 4. Longitudinal Anchor --------------------------------------------------
lt_FileDir = fullfile(TabulatedDataDirectory,'/abcd-general/abcd_y_lt.csv')
lt_recode = readABCDdata(lt_FileDir)
# 4.1 Youth's Site ID 
lt_recode = rename(lt_recode,SiteID = site_id_l)
# 4.2 Youth's Family ID
lt_recode = rename(lt_recode,FamilyID = rel_family_id)
# 4.3 Youth's Birth ID
lt_recode = rename(lt_recode,BirthID = rel_birth_id)
# 4.4 Youth's School ID
lt_recode = rename(lt_recode,SchoolID = school_id)
# 4.5 Youth's School District ID
lt_recode = rename(lt_recode,DistrictID = district_id)
# 4.6 Visit Type
lt_recode = rename(lt_recode,VisitType = visit_type)
lt_recode$VisitType = RECODE(lt_recode$VisitType,
                             "1 = 'on-site';
                             2 = 'remote';
                             3 = 'hybrid';")

# 5. Youth's Genetic Information -------------------------------------------
gen_FileDir = fullfile(TabulatedDataDirectory,'/genetics/gen_y_pihat.csv')
gen_recode = readABCDdata(gen_FileDir)
gen_recode = select(gen_recode,-c(rel_family_id,rel_birth_id)) # remove duplicate variables

# 5.1-5.3 Family Relationship, Same Sex Twin. and GroupID -----------------
# 5.1 Youth's Family Relationship
gen_recode = rename(gen_recode,Relationship=rel_relationship)
# 	0 = single; 1 = sibling; 2 = twin; 3 = triplet
gen_recode$Relationship = RECODE(gen_recode$Relationship,
                                      "0='Single';
                                      1='Sibling';
                                      2='Twin';
                                      3='Triplet';
                                      else=NA")
# 5.2 Same sex twin flag
gen_recode = rename(gen_recode,SameSexTwin=rel_same_sex)
# Same sex twin 1 = Yes; 0 = No
gen_recode$SameSexTwin = RECODE(gen_recode$SameSexTwin,
                                "1='Yes';
                                0='No';
                                else=NA")
# 5.3 Group ID and In-group Order ID
gen_recode = rename(gen_recode,GroupID = rel_group_id,
                    IgoID = rel_ingroup_order)
gen_recode$GroupID[gen_recode$Relationship == "Twin" |
                     gen_recode$Relationship == "Triplet"]=NA
gen_recode$GroupID[gen_recode$Relationship == "Single"]=0
gen_recode$IgoID[gen_recode$Relationship == "Single" |
                     gen_recode$Relationship == "Sibling"]=NA
gen_recode$GroupID = as.numeric(str_c(replace_na(as.character(gen_recode$GroupID),""),
                           replace_na(as.character(gen_recode$IgoID),"")))
gen_recode = select(gen_recode,-IgoID)
# 5.4 Probability of identity by descend between participant  ------------
# Descending order by SubID 1, 2, 3, 4
gen_recode = rename(gen_recode,
                    GI_PairProb_Sub_1 = genetic_pi_hat_1,
                    GI_PairProb_Sub_2 = genetic_pi_hat_2,
                    GI_PairProb_Sub_3 = genetic_pi_hat_3,
                    GI_PairProb_Sub_4 = genetic_pi_hat_4)
# 5.5 Genetically inferred zygosity status -------------------------------
# Genetically inferred zygosity status between participant and SubID 1, 2, 3, 4
# 1= monozygotic ; 2= dizygotic ; 3=siblings ;-1= not available (twins/sibs, genetic_pi_hat not calculated)
gen_recode = rename(gen_recode,
                    GI_Zygosity_Sub_1 = genetic_zygosity_status_1,
                    GI_Zygosity_Sub_2 = genetic_zygosity_status_2,
                    GI_Zygosity_Sub_3 = genetic_zygosity_status_3,
                    GI_Zygosity_Sub_4 = genetic_zygosity_status_4)
gen_recode$GI_Zygosity_Sub_1 = RECODE(gen_recode$GI_Zygosity_Sub_1,
                                                   "1 = 'Monozygotic';
                                                   2 = 'Dizygotic';
                                                   3 = 'Siblings';
                                                  else=NA")
gen_recode$GI_Zygosity_Sub_2 = RECODE(gen_recode$GI_Zygosity_Sub_2,
                                                   "1 = 'Monozygotic';
                                                   2 = 'Dizygotic';
                                                   3 = 'Siblings';
                                                  else=NA")
gen_recode$GI_Zygosity_Sub_3 = RECODE(gen_recode$GI_Zygosity_Sub_3,
                                                   "1 = 'Monozygotic';
                                                   2 = 'Dizygotic';
                                                   3 = 'Siblings';
                                                  else=NA")
gen_recode$GI_Zygosity_Sub_4 = RECODE(gen_recode$GI_Zygosity_Sub_4,
                                                   "1 = 'Monozygotic';
                                                   2 = 'Dizygotic';
                                                   3 = 'Siblings';
                                                  else=NA")
# 5.5 genetic related with participant based on Identity -----------------
gen_recode = rename(gen_recode,
                    GI_PairedSubID_Sub_1 = genetic_paired_subjectid_1,
                    GI_PairedSubID_Sub_2 = genetic_paired_subjectid_2,
                    GI_PairedSubID_Sub_3 = genetic_paired_subjectid_3,
                    GI_PairedSubID_Sub_4 = genetic_paired_subjectid_4)

# 5.6 Reordering and Filtering Variables in gen_recode -------------------
gen_recode = select(gen_recode,
                         c(src_subject_id,eventname,GroupID,Relationship,
                           SameSexTwin,
                           GI_PairedSubID_Sub_1,
                           GI_PairProb_Sub_1,
                           GI_Zygosity_Sub_1,
                           GI_PairedSubID_Sub_2,
                           GI_PairProb_Sub_2,
                           GI_Zygosity_Sub_2,
                           GI_PairedSubID_Sub_3,
                           GI_PairProb_Sub_3,
                           GI_Zygosity_Sub_3,
                           GI_PairedSubID_Sub_4,
                           GI_PairProb_Sub_4,
                           GI_Zygosity_Sub_4,
                           genetic_pc_1,
                           genetic_pc_2,
                           genetic_pc_3,
                           genetic_pc_4,
                           genetic_pc_5)
                    ) %>%
  rename(GI_AncestryPC_1 = genetic_pc_1,
        GI_AncestryPC_2 = genetic_pc_2,
        GI_AncestryPC_3 = genetic_pc_3,
        GI_AncestryPC_4 = genetic_pc_4,
        GI_AncestryPC_5 = genetic_pc_5)

# 6. Youth's Handedness ---------------------------------------------------
# ehi_y_ss_scoreb, 1=right handed; 2=left handed; 3=mixed handed//
# Calculation: if ((mean([ehi1b], [ehi2b], [ehi3b],[ehi4b]) < -60),2, 
# if ((mean([ehi1b], [ehi2b], [ehi3b],[ehi4b]) > 60),1,3)); 1. 
# Veale, J. F. (2014) Edinburgh Handedness Inventory - Short Form: a revised version based on confirmatory factor analysis. Laterality 19(2):164-77. 
# //If mixed handed, use the hand that the child writes with for NeuroCog and fMRI tasks.
ehis_FileDir = fullfile(TabulatedDataDirectory,'/neurocognition/nc_y_ehis.csv')
EHIS = readABCDdata(ehis_FileDir)
EHIS$Handedness = RECODE(EHIS$ehi_y_ss_scoreb,
                         "1='Right';2='Left';3='Mixed';else=NA")
EHIS = subset(EHIS,select = -c(ehi1b,ehi2b,ehi3b,ehi4b,ehi_time,ehi_y_ss_scoreb))
# 7. Youth's Body Mass Index ----------------------------------------------
# The calculation of Body Mass Index was referred to
# https://github.com/ABCD-STUDY/analysis-nda/blob/master/notebooks/general/core_demographics3.0.R
ant_FileDir = fullfile(TabulatedDataDirectory,'/physical-health/ph_y_anthro.csv')
BMI = readABCDdata(ant_FileDir)
if (file.exists('ABCD5.0_BMI_NotesTable.xlsx')){
  cat("ABCD_5.0_BMI_NotesTable.xlsx was found at working directory (i.e. the folder contains SDPP-ABCD-TabDat scripts)!\n")
  cat("Automated Replacement for errors in measures of heigth and weight will be executed.\n")
  CorrectiveTable = import(file = "./ABCD5.0_BMI_NotesTable.xlsx",
                               sheet = "ManualCorrectiveTable")
  for (i in 1:nrow(CorrectiveTable)){
    cat(sprintf("Replace values for subject:%s\t",CorrectiveTable$src_subject_id[i]))
    cat(sprintf("With eventname:%s\n",CorrectiveTable$eventname[i]))
    Flag = which((BMI$src_subject_id == CorrectiveTable$src_subject_id[i]) & 
      (BMI$eventname == CorrectiveTable$eventname[i]))
    cat(sprintf("Row Number:%d in ph_y_anthro.csv was located\n",Flag))
    cat('Uncorrected data:\n')
    print(BMI[Flag,])
    BMI$anthro_1_height_in[Flag] = CorrectiveTable$anthro_1_height_in[i]
    BMI$anthro2heightin[Flag] = CorrectiveTable$anthro2heightin[i]
    BMI$anthro3heightin[Flag] = CorrectiveTable$anthro3heightin[i]
    BMI$anthroheightcalc[Flag] = CorrectiveTable$anthroheightcalc[i]
    BMI$anthroweight1lb[Flag] = CorrectiveTable$anthroweight1lb[i]
    BMI$anthroweight2lb[Flag] = CorrectiveTable$anthroweight2lb[i]
    BMI$anthroweight3lb[Flag] = CorrectiveTable$anthroweight3lb[i]
    BMI$anthroweightcalc[Flag] = CorrectiveTable$anthroweightcalc[i]
    cat('Corrected data:\n')
    print(BMI[Flag,])
  }
  cat('Automated Replacement for measures in heigth and weight has been finished!\n')
  }else{
  cat('ABCD_5.0_BMI_NotesTable.xlsx Not Found!')
  warning("The automated replacement would not be executed!")
}
# calculate BMI using the formula: weight/(height)^2 * 703, weight in lb, height in inch.
BMI$BMI_calc = BMI$anthroweightcalc / (BMI$anthroheightcalc)^2 * 703
BMI$BMI_calc[which(BMI$BMI_calc>36 | BMI$BMI_calc< 11)]=NA #reset unrealistic values
BMI = select(BMI,
             c(src_subject_id,eventname,BMI_calc))
# 8. Merge Data -----------------------------------------------------------
# 8.0 Clear redundant variables in R environment
rm(CombPrntsHighEdu,CorrectiveTable,demo_base,FamliyEmploy,Race_PrntRep,Race_PrntSelf)
# 8.1 Generate Demographic: Merge acspsw+pdem with EHIS (handedness)
EHIS = subset(EHIS,eventname == "baseline_year_1_arm_1") #remove 4-year FU EHIS
Demographic = merge(demo_base_recode,EHIS,
                    by = intersect(colnames(demo_base_recode),colnames(EHIS)),
                    all = T)
# 8.2 Generate Demographic: Merge acspsw+pdem+ehis with genetic information
Demographic = merge(Demographic,gen_recode,
                    by = intersect(colnames(Demographic),colnames(gen_recode)),
                    all = T)
# 8.3 Generate Demographic: Merge acspsw+pdem+ehis+gen with BMI
Demographic = merge(Demographic,BMI,
                    by = intersect(colnames(Demographic),colnames(BMI)),
                    all = T)
# 8.4 Generate Demographic: Merge all with longitudinal tract index
Demographic = merge(Demographic,lt_recode,
                    by = intersect(colnames(Demographic),colnames(lt_recode)),
                    all = F)
# 9. Baseline Observation Carry Forward (BOCF)  ---------------------------

Demographic <- Demographic %>%
  BOCF.Variables('baseline_year_1_arm_1','AdoptionFlag') %>%
  BOCF.Variables('baseline_year_1_arm_1','AdoptionAge') %>%
  BOCF.Variables('baseline_year_1_arm_1','Race_PrntRep') %>%
  BOCF.Variables('baseline_year_1_arm_1','Ethnicity_PrntRep') %>%
  BOCF.Variables('baseline_year_1_arm_1','BirthCountry') %>%
  BOCF.Variables('baseline_year_1_arm_1','USALiveYears') %>%
  BOCF.Variables('baseline_year_1_arm_1','Religon_PrntRep') %>%
  BOCF.Variables('baseline_year_1_arm_1','GenderIdentity_PrntSelf') %>%
  BOCF.Variables('baseline_year_1_arm_1','Race_PrntSelf') %>%
  BOCF.Variables('baseline_year_1_arm_1','SexAssigned') %>%
  BOCF.Variables('baseline_year_1_arm_1','GenderIdentity_PrntRep') %>%
  BOCF.Variables('baseline_year_1_arm_1','ParentsMarital') %>%
  BOCF.Variables('baseline_year_1_arm_1','ParentHighEdu') %>%
  BOCF.Variables('baseline_year_1_arm_1','PartnerHighEdu') %>%
  BOCF.Variables('baseline_year_1_arm_1','ParentsEdu') %>%
  BOCF.Variables('baseline_year_1_arm_1','ParentEmploy') %>%
  BOCF.Variables('baseline_year_1_arm_1','PartnerEmploy') %>%
  BOCF.Variables('baseline_year_1_arm_1','NumInLF') %>%
  BOCF.Variables('baseline_year_1_arm_1','ParentsMarital_2L') %>%
  BOCF.Variables('baseline_year_1_arm_1','ParentsMaritalXEmploy') %>%
  BOCF.Variables('baseline_year_1_arm_1','FamilyIncome') %>%
  BOCF.Variables('baseline_year_1_arm_1','HouseholdSize') %>%
  BOCF.Variables('baseline_year_1_arm_1','HouseholdStructure') %>%
  BOCF.Variables('baseline_year_1_arm_1','RaceEthnicity') %>%
  BOCF.Variables('baseline_year_1_arm_1','GroupID') %>%
  BOCF.Variables('baseline_year_1_arm_1','Relationship') %>%
  BOCF.Variables('baseline_year_1_arm_1','SameSexTwin') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_PairedSubID_Sub_1') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_PairProb_Sub_1') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_Zygosity_Sub_1') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_PairedSubID_Sub_2') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_PairProb_Sub_2') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_Zygosity_Sub_2') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_PairedSubID_Sub_3') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_PairProb_Sub_3') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_Zygosity_Sub_3') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_PairedSubID_Sub_4') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_PairProb_Sub_4') %>%
  BOCF.Variables('baseline_year_1_arm_1','GI_Zygosity_Sub_4') %>%
  BOCF.Variables('baseline_year_1_arm_1','SiteID') %>%
  BOCF.Variables('baseline_year_1_arm_1','FamilyID') %>%
  BOCF.Variables('baseline_year_1_arm_1','BirthID') %>%
  BOCF.Variables('baseline_year_1_arm_1','Handedness') %>%
  BOCF.Variables('1_year_follow_up_y_arm_1','YouthNativeLang')

# 10. Set Columns Data Type and Re-order Columns ---------------------------
sapply(Demographic, typeof)
Demographic$AdoptionFlag <- as.numeric(Demographic$AdoptionFlag)
Demographic = select(Demographic,
                     c(src_subject_id,eventname,interview_age,interview_date,
                       SexAssigned,YouthNativeLang,
                       Race_PrntRep,Ethnicity_PrntRep,RaceEthnicity,
                       Handedness,BMI_calc,
                       ParentsMarital_2L,ParentsMaritalXEmploy,
                       ParentsEdu,
                       FamilyIncome,Relationship,
                       HouseholdSize,HouseholdStructure,
                       BirthCountry,Religon_PrntRep,
                       SiteID,FamilyID,GroupID,ACS_weight,
                       everything())) 

# 11. Save Double- and Character-type Demographic Data ---------------------
SDPP.StdOut.RDS.CSV.Files(NEW_data = Demographic,
                          FileLabel = "Demographics_Raw",
                          IntermediateDataDir = IntermediateDataDir,
                          Prefix = Prefix)

# End of script -------------------------------------------------------
s_close_sink(SourceScriptName = SourceScriptName)
}