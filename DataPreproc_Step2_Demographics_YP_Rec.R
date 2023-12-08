# =============================================================================#
# SDPP Step 2: Re-coding Individual Demographics into R factors data type.
# R Packages Dependency: bruceR, forcats
# Step File Notes: 
# 1. Coding Scheme is referred to (Song et al., 2023) and ABCD Data Analytics and
# Informatics Resource Center (ABCD-DAIRC at UCSD).
# 2. Reference Link (Github Repository from ABCD-DAIRC):
# https://github.com/ABCD-STUDY/analysis-nda
# 3. Target File
# ABCD 4.0: pdem02.txt; acspsw03.txt; abcd_ehis01.txt; abcd_ant01.txt.
# ABCD 5.0: ce_p_acc.csv; abcd_p_demo.csv;
# Intermediate Data File: ABCD5.0_Demographics_Recode.rds,
#                         ABCD5.0_Demographics_Recode.csv
# Refernce Link (Data Dictionary):
# https://nda.nih.gov/data_structure.html?short_name=pdem02
# https://nda.nih.gov/data_structure.html?short_name=acspsw03
# https://nda.nih.gov/data_structure.html?short_name=abcd_lt01
# https://data-dict.abcdstudy.org/
# 
# Update Date: 2023.12.09
# =============================================================================#
# 1 Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_2.txt'
s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(forcats)
# ==============================MAIN CODES=====================================#
# 2. Read RDS-file and print all columns data type ----------------------------
Demographic = readRDS(fullfile(ProjectDirectory,
                               'Res_3_IntermediateData',
                               str_c(Prefix,'_Demographics_Raw.rds')))
sapply(Demographic, typeof)
# 3. Re-coding Individual Key Demographics -----------------------------------
Demographic$SexAssigned = factor(Demographic$SexAssigned,
                         levels = c('Female','Male','Intersex-Male'))
Demographic$SexAssigned = droplevels(fct_collapse(Demographic$SexAssigned,
                                       Male = c('Male','Intersex-Male')))
print(table(Demographic$SexAssigned))
Demographic$EducationR = RECODE(Demographic$EducationC,
                                "c('KINDERGARTEN','1ST GRADE','2ND GRADE')='2ND GRADE or less';
                                c('10TH GRADE','11TH GRADE','12TH GRADE')='10TH GRADE or more'")
Demographic$EducationR = factor(Demographic$EducationR,
                                levels = c('2ND GRADE or less',
                                           '3RD GRADE','4TH GRADE','5TH GRADE',
                                           '6TH GRADE','7TH GRADE','8TH GRADE',
                                           '9TH GRADE',
                                           '10TH GRADE or more'),
                                ordered = T)
print(table(Demographic$EducationR))
Demographic$EducationC = factor(Demographic$EducationC,
                                levels = c('KINDERGARTEN','1ST GRADE',
                                           '2ND GRADE','3RD GRADE',
                                           '4TH GRADE','5TH GRADE',
                                           '6TH GRADE','7TH GRADE',
                                           '8TH GRADE','9TH GRADE',
                                           '10TH GRADE','11TH GRADE',
                                           '12TH GRADE'),
                                ordered = F)
print(table(Demographic$EducationC))
Demographic$GenderIdentity_PrntRep = factor(Demographic$GenderIdentity_PrntRep,
                                            levels = c('Female',
                                                       'Male',
                                                       'Trans female',
                                                       'Trans male',
                                                       'Gender queer',
                                                       'Different'),
                                            ordered = F)
print(table(Demographic$GenderIdentity_PrntRep))
Demographic$GenderIdentity_PrntSelf = factor(Demographic$GenderIdentity_PrntSelf,
                                            levels = c('Female',
                                                       'Male',
                                                       'Trans female',
                                                       'Trans male',
                                                       'Gender queer',
                                                       'Different'),
                                            ordered = F)
print(table(Demographic$GenderIdentity_PrntSelf))
Demographic$Race_PrntRep = factor(Demographic$Race_PrntRep,
                                  levels = c('White',
                                             'Black',
                                             'Asian',
                                             'AIAN',
                                             'NHPI',
                                             'Mixed',
                                             'Other'),
                                  ordered = F)
print(table(Demographic$Race_PrntRep))
Demographic$Race_4L = fct_collapse(Demographic$Race_PrntRep,
                                   `Mixed/Other` = c('AIAN',
                                                     'NHPI',
                                                     'Mixed',
                                                     'Other'))
print(table(Demographic$Race_4L))
Demographic$Race_6L = fct_collapse(Demographic$Race_PrntRep,
                                   `Mixed/Other` = c('Other',
                                                     'Mixed'))
print(table(Demographic$Race_6L))
Demographic$Race_PrntSelf = factor(Demographic$Race_PrntSelf,
                                   levels = c('White',
                                              'Black',
                                              'Asian',
                                              'AIAN',
                                              'NHPI',
                                              'Mixed',
                                              'Other'),
                                   ordered = F)
print(table(Demographic$Race_PrntSelf))
Demographic$Ethnicity_PrntRep = factor(Demographic$Ethnicity_PrntRep,
                                       levels = c('No',
                                                  'Hispanic/Latino/Latina'),
                                       labels = c('Non-hispanic',
                                                  'Hispanic'),
                                       ordered = F)
print(table(Demographic$Ethnicity_PrntRep))
Demographic$BirthCountry = factor(Demographic$BirthCountry,
                                  levels = c('Other',
                                             'USA'),
                                  ordered = F)
print(table(Demographic$BirthCountry))
Demographic$Religon_PrntRep = factor(Demographic$Religon_PrntRep,
                                levels = c('Nothing in Particular',
                                           'Atheist',
                                           'Agnostic',
                                           'Buddhist',
                                           'Evangelical Protestant',
                                           'Hindu',
                                           'Historically Black Church',
                                           'Jehovahs Witness',
                                           'Jewish',
                                           'Mainline Protestant',
                                           'Mormon',
                                           'Muslim',
                                           'Orthodox Christian',
                                           'Other Christian',
                                           'Roman Catholic',
                                           'Unitarian',
                                           'Something else'),
                                ordered = F)
print(table(Demographic$Religon_PrntRep))
Demographic$Religon_8L = fct_collapse(Demographic$Religon_PrntRep,
                                      `Eastern Asian` = c('Buddhist','Hindu'),
                                      Christianity = c('Evangelical Protestant',
                                                       'Historically Black Church',
                                                       'Jehovahs Witness',
                                                       'Mainline Protestant',
                                                       'Mormon',
                                                       'Orthodox Christian',
                                                       'Roman Catholic',
                                                       'Unitarian',
                                                       'Other Christian'))
print(table(Demographic$Religon_8L))
Demographic$Religon_2L = fct_collapse(Demographic$Religon_8L,
                                     Yes = c('Eastern Asian',
                                             'Christianity',
                                             'Jewish',
                                             'Muslim',
                                             'Something else'),
                                     No = c('Nothing in Particular',
                                            'Atheist',
                                            'Agnostic'))
print(table(Demographic$Religon_2L))
Demographic$ParentsMarital_6L = factor(Demographic$ParentsMarital,
                                    levels = c('Married',
                                               'Living with partner',
                                               'Divorced',
                                               'Separated',
                                               'Widowed',
                                               'Never married'),
                                    ordered = F)
print(table(Demographic$ParentsMarital))
Demographic$ParentsMarital_2L = factor(Demographic$ParentsMarital_2L,
                                       levels = c('Married or living with partner',
                                                  'Single'),
                                       ordered = F)
print(table(Demographic$ParentsMarital_2L))
Demographic$ParentsHighEdu_5L = factor(Demographic$ParentsEdu,
                                    levels = c('< HS Diploma',
                                               'HS Diploma/GED',
                                               'Some College',
                                               'Bachelor',
                                               'Post Graduate Degree'),
                                ordered = F)
print(table(Demographic$ParentsHighEdu_5L))
Demographic$ParentsHighEdu_2L = fct_collapse(Demographic$ParentsHighEdu_5L,
                                             `High school or less` = c('< HS Diploma',
                                                                         'HS Diploma/GED'),
                                             `College education` = c('Some College',
                                                                     'Bachelor',
                                                                     'Post Graduate Degree'),
                                             )
print(table(Demographic$ParentsHighEdu_2L))
Demographic$ParentsMaritalXEmploy = factor(Demographic$ParentsMaritalXEmploy,
                                          levels = c('Married, 2 in LF',
                                                     'Married, 1 in LF',
                                                     'Married, 0 in LF',
                                                     'Single, in LF',
                                                     'Single, Not in LF'),
                                          ordered = F)
print(table(Demographic$ParentsMaritalXEmploy))
Demographic$FamilyIncome = factor(Demographic$FamilyIncome,
                                  levels = c('<$25k',
                                             '$25k-$49k',
                                             '$50k-$74k',
                                             '$75k-$99k',
                                             '$100k-$199k',
                                             '$200k+'),
                                  ordered = T)
print(table(Demographic$FamilyIncome))
Demographic$HouseholdSize = factor(Demographic$HouseholdSize,
                                   levels = c('3 and below',
                                              '4',
                                              '5',
                                              '6',
                                              '7 and more'),
                                   ordered = T)
print(table(Demographic$HouseholdSize))
Demographic$HouseholdStructure = factor(Demographic$HouseholdStructure,
                                        levels = c('Single household',
                                                   'Non-single household'),
                                        ordered = F)
print(table(Demographic$HouseholdStructure))
Demographic$RaceEthnicity = factor(Demographic$RaceEthnicity,
                                   levels = c('White',
                                              'Black',
                                              'Asian',
                                              'Hispanic',
                                              'Other'),
                                   ordered = F)
print(table(Demographic$RaceEthnicity))
Demographic$Relationship = factor(Demographic$Relationship,
                                  levels = c('Single',
                                             'Sibling',
                                             'Twin',
                                             'Triplet'),
                                  ordered = F)
print(table(Demographic$Relationship))
Demographic$Relationship_3L = fct_collapse(Demographic$Relationship,
                                           `Twin/Triplet` = c('Twin',
                                                              'Triplet'))
print(table(Demographic$Relationship_3L))
Demographic$Handedness = factor(Demographic$Handedness,
                                levels = c('Right',
                                           'Left',
                                           'Mixed'),
                                ordered = F)
print(table(Demographic$Handedness))
Demographic$VisitType = factor(Demographic$VisitType,
                                levels = c('on-site',
                                           'remote',
                                           'hybrid'),
                               labels = c('On-site','Remote','Hybrid'),
                                ordered = F)
print(table(Demographic$VisitType))
Demographic$ParentEmploy = factor(Demographic$ParentEmploy,
                                  levels = c('Working',
                                             'Non-working'),
                                  ordered = F)
print(table(Demographic$ParentEmploy))
Demographic$PartnerEmploy = factor(Demographic$PartnerEmploy,
                                  levels = c('Working',
                                             'Non-working'),
                                  ordered = F)
print(table(Demographic$PartnerEmploy))
Demographic$ParentHighEdu = factor(Demographic$ParentHighEdu,
                                   levels = c('< HS Diploma',
                                              'HS Diploma/GED',
                                              'Some College',
                                              'Bachelor',
                                              'Post Graduate Degree'),
                                  ordered = F)
print(table(Demographic$ParentHighEdu))
Demographic$PartnerHighEdu = factor(Demographic$PartnerHighEdu,
                                    levels = c('< HS Diploma',
                                               'HS Diploma/GED',
                                               'Some College',
                                               'Bachelor',
                                               'Post Graduate Degree'),
                                   ordered = F)
print(table(Demographic$PartnerHighEdu))
Demographic$GI_Zygosity_Sub_1 = factor(Demographic$GI_Zygosity_Sub_1,
                                   levels = c('Monozygotic',
                                              'Dizygotic',
                                              'Sibling'),
                                   ordered = F)
print(table(Demographic$GI_Zygosity_Sub_1))
Demographic$GI_Zygosity_Sub_2 = factor(Demographic$GI_Zygosity_Sub_2,
                                               levels = c('Monozygotic',
                                                          'Dizygotic',
                                                          'Sibling'),
                                               ordered = F)
print(table(Demographic$GI_Zygosity_Sub_2))
Demographic$GI_Zygosity_Sub_3 = factor(Demographic$GI_Zygosity_Sub_3,
                                               levels = c('Monozygotic',
                                                          'Dizygotic',
                                                          'Sibling'),
                                               ordered = F)
print(table(Demographic$GI_Zygosity_Sub_3))
Demographic$GI_Zygosity_Sub_4 = factor(Demographic$GI_Zygosity_Sub_4,
                                               levels = c('Monozygotic',
                                                          'Dizygotic',
                                                          'Sibling'),
                                               ordered = F)
print(table(Demographic$GI_Zygosity_Sub_4))
Demographic$SameSexTwin = factor(Demographic$SameSexTwin,
                                               levels = c('No',
                                                          'Yes'),
                                               ordered = F)
print(table(Demographic$SameSexTwin))
Demographic$AdoptionFlag = factor(Demographic$AdoptionFlag,
                                     levels = c(0,
                                                1),
                                  labels = c('No',
                                             'Yes'),
                                     ordered = F)
print(table(Demographic$AdoptionFlag))
Demographic$YouthNativeLang = factor(Demographic$YouthNativeLang,
                                  levels = c('English',
                                             'Other'),
                                  ordered = F)
print(table(Demographic$YouthNativeLang))
# 4. Re-name and re-order variables -----------------------------------------
select(Demographic,-c(ParentsEdu,ParentsMarital)) %>%
  rename(ParentsMarital_X_Employ = ParentsMaritalXEmploy,
         BMI = BMI_calc,
         Education_R = EducationR,
         Education_13L = EducationC,
         FamilyNumInLF = NumInLF,
         AdoptionChild = AdoptionFlag,
         ) %>% 
  select(c(src_subject_id,interview_age,eventname,interview_date,SexAssigned,
           VisitType,Education_13L,
           SiteID,FamilyID,GroupID,SchoolID,DistrictID,ACS_weight,
           Race_6L,Ethnicity_PrntRep,Handedness,BMI,
           ParentsMarital_2L,ParentsHighEdu_2L,ParentsMarital_X_Employ,
           Relationship_3L,HouseholdStructure,HouseholdSize,FamilyIncome,
           Religon_2L,BirthCountry,AdoptionChild,GenderIdentity_PrntRep,
           everything())) -> Demographic
print(sapply(Demographic, typeof)) # print data type
print(sapply(Demographic, class))

# 5. Save Results ----------------------------------------------------
SDPP.StdOut.RDS.CSV.Files(NEW_data = Demographic,
                          FileLabel = "Demographics_Rec",
                          IntermediateDataDir = IntermediateDataDir,
                          Prefix = Prefix)

# End of script -----------------------------------------------------------
s_close_sink()

