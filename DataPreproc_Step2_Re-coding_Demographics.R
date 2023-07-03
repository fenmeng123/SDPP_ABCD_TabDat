# =============================================================================#
# SDPP Step 2: Re-coding Individual Demographics into R factors data type.
# R Packages Dependency: bruceR, forcats
# Step File Notes: 
# 1. Coding Scheme is referred to (Song et al., 2023) and ABCD Data Analytics and
# Informatics Resource Center (ABCD-DAIRC at UCSD).
# 2. Reference Link (Github Repository from ABCD-DAIRC):
# https://github.com/ABCD-STUDY/analysis-nda
# 3. Target File: pdem02.txt; acspsw03.txt; abcd_ehis01.txt; abcd_ant01.txt.
# Refernce Link (Data Dictionary):
# https://nda.nih.gov/data_structure.html?short_name=pdem02
# https://nda.nih.gov/data_structure.html?short_name=acspsw03
# https://nda.nih.gov/data_structure.html?short_name=abcd_lt01
# Update Date: 2023.06.15 By Kunru Song
# =============================================================================#
# 1.1 Library Packages and Prepare Environment --------------------------------
library(bruceR)
library(forcats)
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
# 2. Read RDS-file and print all columns data type ----------------------------
Demographic = readRDS(fullfile(ProjectDirectory,
                               'Res_3_IntermediateData',
                               'ABCD4.0_Demographics_Raw.rds'))
sapply(Demographic, typeof)
# 3. Re-coding Individual Key Demographics -----------------------------------
Demographic$sex = factor(Demographic$sex,
                         levels = c('F','M'),
                         labels = c('Female','Male'))
Demographic$EducationR = RECODE(Demographic$EducationC,
                                "c('KINDERGARTEN','1ST GRADE','2ND GRADE')='2ND GRADE or less';
                                c('6TH GRADE','7TH GRADE','10TH GRADE','12TH GRADE')='6TH GRADE or more'")
Demographic$EducationR = factor(Demographic$EducationR,
                                levels = c('2ND GRADE or less',
                                           '3RD GRADE','4TH GRADE','5TH GRADE',
                                           '6TH GRADE or more'),
                                ordered = T)
Demographic$EducationC = factor(Demographic$EducationC,
                                levels = c('KINDERGARTEN','1ST GRADE',
                                           '2ND GRADE','3RD GRADE',
                                           '4TH GRADE','5TH GRADE',
                                           '6TH GRADE','7TH GRADE',
                                           '10TH GRADE','11TH GRADE',
                                           '12TH GRADE'),
                                ordered = F)
Demographic$SexAssigned = droplevels(
                            factor(Demographic$SexAssigned,
                                 levels = c('Female',
                                            'Male',
                                            'Intersex-Male',
                                            'Intersex-Female'),
                                 ordered = F))
Demographic$GenderIdentity_PrntRep = factor(Demographic$GenderIdentity_PrntRep,
                                            levels = c('Female',
                                                       'Male',
                                                       'Trans female',
                                                       'Trans male',
                                                       'Gender queer',
                                                       'Different'),
                                            ordered = F)
Demographic$Race_PrntRep = factor(Demographic$Race_PrntRep,
                                  levels = c('White',
                                             'Black',
                                             'Asian',
                                             'AIAN',
                                             'NHPI',
                                             'Mixed',
                                             'Other'),
                                  ordered = F)
Demographic$Race_4L = fct_collapse(Demographic$Race_PrntRep,
                                   `Mixed/Other` = c('AIAN',
                                                     'NHPI',
                                                     'Mixed',
                                                     'Other'))
Demographic$Race_6L = fct_collapse(Demographic$Race_PrntRep,
                                   `Mixed/Other` = c('Other',
                                                     'Mixed'))
Demographic$Ethnicity_PrntRep = factor(Demographic$Ethnicity_PrntRep,
                                       levels = c('No',
                                                  'Hispanic/Latino/Latina'),
                                       labels = c('Non-hispanic',
                                                  'Hispanic'),
                                       ordered = F)
Demographic$BirthCountry = factor(Demographic$BirthCountry,
                                  levels = c('Other',
                                             'USA'),
                                  ordered = F)
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
Demographic$Religon_2L = fct_collapse(Demographic$Religon_8L,
                                     Yes = c('Eastern Asian',
                                             'Christianity',
                                             'Jewish',
                                             'Muslim',
                                             'Something else'),
                                     No = c('Nothing in Particular',
                                            'Atheist',
                                            'Agnostic'))
Demographic$ParentsMarital_6L = factor(Demographic$ParentMaritalC,
                                    levels = c('Married',
                                               'Living with partner',
                                               'Divorced',
                                               'Separated',
                                               'Widowed',
                                               'Never married'),
                                    ordered = F)
Demographic$ParentsMarital_2L = fct_collapse(Demographic$ParentsMarital_6L,
                                             `Married or living with partner` = c('Married',
                                                                                  'Living with partner'),
                                             Other = c('Divorced',
                                                       'Separated',
                                                       'Widowed',
                                                       'Never married'))
Demographic$ParentsHighEdu_5L = factor(Demographic$ParentsEdu,
                                    levels = c('< HS Diploma',
                                               'HS Diploma/GED',
                                               'Some College',
                                               'Bachelor',
                                               'Post Graduate Degree'),
                                ordered = F)
Demographic$ParentsHighEdu_2L = fct_collapse(Demographic$ParentsHighEdu_5L,
                                             `High school or less` = c('< HS Diploma',
                                                                         'HS Diploma/GED'),
                                             `College education` = c('Some College',
                                                                     'Bachelor',
                                                                     'Post Graduate Degree'),
                                             )
Demographic$ParentsMaritalEmploy = factor(Demographic$ParentsMaritalEmploy,
                                          levels = c('Married, 2 in LF',
                                                     'Married, 1 in LF',
                                                     'Married, 0 in LF',
                                                     'Single, in LF',
                                                     'Single, Not in LF'),
                                          ordered = F)
Demographic$FamilyIncome = factor(Demographic$FamilyIncome,
                                  levels = c('<$25k',
                                             '$25k-$49k',
                                             '$50k-$74k',
                                             '$75k-$99k',
                                             '$100k-$199k',
                                             '$200k+'),
                                  ordered = T)
Demographic$HouseholdSize = factor(Demographic$HouseholdSize,
                                   levels = c('2 to 3',
                                              '4',
                                              '5',
                                              '6',
                                              '7+'),
                                   ordered = T)
Demographic$HouseholdStructure = factor(Demographic$HouseholdStructure,
                                        levels = c('Single household',
                                                   'Another household'),
                                        ordered = F)
Demographic$RaceEthnicity = factor(Demographic$RaceEthnicity,
                                   levels = c('White',
                                              'Black',
                                              'Asian',
                                              'Hispanic',
                                              'Other'),
                                   ordered = F)
Demographic$Relationship = factor(Demographic$Relationship,
                                  levels = c('single',
                                             'sibling',
                                             'twin',
                                             'triplet'),
                                  ordered = F)
Demographic$Relationship_3L = fct_collapse(Demographic$Relationship,
                                           `twin or triplet` = c('twin',
                                                                 'triplet'))
Demographic$Handedness = factor(Demographic$Handedness,
                                levels = c('Right',
                                           'Left',
                                           'Mixed'),
                                ordered = F)
Demographic$SessionType = factor(Demographic$SessionType,
                                levels = c('In-person',
                                           'Remote',
                                           'Hybrid',
                                           'Uncertain'),
                                ordered = F)
Demographic$ParentEmploy = factor(Demographic$ParentEmploy,
                                  levels = c('Working',
                                             'Non-working'),
                                  ordered = F)
Demographic$PartnerEmploy = factor(Demographic$PartnerEmploy,
                                  levels = c('Working',
                                             'Non-working'),
                                  ordered = F)
Demographic$ParentHighEdu = factor(Demographic$ParentHighEdu,
                                  levels = c('high school or less',
                                             'Colleage'),
                                  labels = c('High school or less',
                                             'College education'),
                                  ordered = F)
Demographic$PartnerHighEdu = factor(Demographic$PartnerHighEdu,
                                   levels = c('high school or less',
                                              'Colleage or more'),
                                   labels = c('High school or less',
                                              'College Education'),
                                   ordered = F)
Demographic$GeneInfo_Zygosity_SubID_1 = factor(Demographic$GeneInfo_Zygosity_SubID_1,
                                   levels = c('Monozygotic',
                                              'Dizygotic',
                                              'Sibling'),
                                   ordered = F)
Demographic$GeneInfo_Zygosity_SubID_2 = factor(Demographic$GeneInfo_Zygosity_SubID_2,
                                               levels = c('Monozygotic',
                                                          'Dizygotic',
                                                          'Sibling'),
                                               ordered = F)
Demographic$GeneInfo_Zygosity_SubID_3 = factor(Demographic$GeneInfo_Zygosity_SubID_3,
                                               levels = c('Monozygotic',
                                                          'Dizygotic',
                                                          'Sibling'),
                                               ordered = F)
Demographic$GeneInfo_Zygosity_SubID_4 = factor(Demographic$GeneInfo_Zygosity_SubID_4,
                                               levels = c('Monozygotic',
                                                          'Dizygotic',
                                                          'Sibling'),
                                               ordered = F)
Demographic$SameSexTwinFlag = factor(Demographic$SameSexTwinFlag,
                                               levels = c('No',
                                                          'Yes'),
                                               ordered = F)
Demographic$AdoptionFlag = factor(as.numeric(Demographic$AdoptionFlag),
                                     levels = c(0,
                                                1),
                                  labels = c('No',
                                             'Yes'),
                                     ordered = F)
Demographic$Education_5L = factor(Demographic$EducationR,ordered = F)

# 4. Re-name and re-order variables -----------------------------------------
select(Demographic,-c(ParentMaritalC,ParentMarital,ParentsEdu)) %>%
  rename(ParentsMarita_X_Employ = ParentsMaritalEmploy,
         SiteID = site_id_l,
         Education_R = EducationR,
         Education_11L = EducationC) %>% 
  select(c(subjectkey,interview_age,eventname,interview_date,
           SessionType,sex,Education_5L,
           Race_6L,Ethnicity_PrntRep,Handedness,BMI_calc,
           ParentsMarital_2L,ParentsHighEdu_2L,ParentsMarita_X_Employ,
           Relationship_3L,HouseholdStructure,HouseholdSize,FamilyIncome,
           Religon_2L,BirthCountry,AdoptionFlag,GenderIdentity_PrntRep,SexAssigned,
           FamilyID,SiteID,ACS_weight,
           everything())) -> Demographic
Demographic$Education_R <- as.numeric(Demographic$Education_R)

sapply(Demographic, typeof) # print data type
sapply(Demographic, class)

# 5. Save Results ----------------------------------------------------
saveRDS(Demographic,fullfile(ProjectDirectory,'Res_3_IntermediateData','ABCD4.0_Demographics_Recode.rds'))
write.csv(Demographic,fullfile(ProjectDirectory,'Res_3_IntermediateData','ABCD4.0_Demographics_Recode.csv'),
          fileEncoding = 'UTF-8')


