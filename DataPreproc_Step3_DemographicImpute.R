# =============================================================================#
# SDPP Step 3: Imputing Demographics with Chained Equation Models.
# R Packages Dependency: bruceR, mice, forcats, naniar
# Step File Notes: 
# 1. The imputation about the core demographics was referred to ABCD-DAIRC
# 2. Ref Link:
# https://github.com/ABCD-STUDY/analysis-nda/blob/861cb4063dba93794dc8dd18feb2673c306675ea/notebooks/general/impute_demographics.md
# 3. Target File (Intermediate Data File): ABCD4.0_Demographics_Recode.rds
# Update Date: 2023.06.16 By Kunru Song
# =============================================================================#
# 1.1 Library Packages and Prepare Environment --------------------------------
library(bruceR)
library(mice)
library(forcats)
set.wd()
source('SDPP_subfunctions.R')

# 1.2 SDPP Parameter Settings -------------------------------------------------
TabulatedDataDirectory = '../../Download_ABCDV4.0_skr220403/Package_1199282'
# Please replace the above string for your own downloaded data directory.
# Relative Path is required! (relative to the path of the current R script file)
ProjectDirectory = '../DataAnalysis/SMA_Trajectory'
ResultsOutputDir = fullfile(ProjectDirectory,'Res_2_Results','Res_Preproc')
Prefix = 'ABCD4.0'
AutoLogFileName = ''
if (!dir.exists(ResultsOutputDir)){
  dir.create(ResultsOutputDir)
}
# ==============================MAIN CODES=====================================#
# 2. Load and prepare re-coded demographic data --------------------------
Demographic = readRDS(fullfile(ProjectDirectory,'Res_3_IntermediateData','ABCD4.0_Demographics_Recode.rds'))
baseline_demo = subset(Demographic,eventname=='baseline_year_1_arm_1')
naniar::miss_var_summary(baseline_demo) %>% 
  print_table(file = fullfile(ResultsOutputDir,'MissVarRep_Demographic_baseline.doc'))
sapply(baseline_demo, typeof)
baseline_demo = as.data.table(baseline_demo)

# convergently check Race (Parent-report), Ethnicity (Parent-report) and Race_ethnicity (from acspsw03.txt)
baseline_demo$Race_PrntRep = tidyr::replace_na(as.character(baseline_demo$Race_PrntRep),"")
baseline_demo$RaceEthnicity = tidyr::replace_na(as.character(baseline_demo$RaceEthnicity),"")
baseline_demo$Ethnicity_PrntRep = tidyr::replace_na(as.character(baseline_demo$Ethnicity_PrntRep),"")

tmp = baseline_demo[,c('Race_PrntRep','Ethnicity_PrntRep','RaceEthnicity')]
race_miss = tmp[(tmp$Race_PrntRep==""),]
ethn_miss = tmp[(tmp$Ethnicity_PrntRep==""),]
race_miss$Race_PrntRep[race_miss$RaceEthnicity!='Hispanic'] = race_miss$RaceEthnicity[race_miss$RaceEthnicity!='Hispanic']
ethn_miss$Ethnicity_PrntRep[ethn_miss$RaceEthnicity=='Hispanic'] = "Hispanic/Latino/Latina"
ethn_miss$Ethnicity_PrntRep[ethn_miss$RaceEthnicity!='Hispanic'] = "No"

baseline_demo$Race_PrntRep[baseline_demo$Race_PrntRep==""] = race_miss$Race_PrntRep
baseline_demo$Ethnicity_PrntRep[baseline_demo$Ethnicity_PrntRep==""] = ethn_miss$Ethnicity_PrntRep

baseline_demo[, table(Race_PrntRep,useNA = 'if')]
baseline_demo[, table(Ethnicity_PrntRep,useNA = 'if')]

baseline_demo$Race_PrntRep = factor(baseline_demo$Race_PrntRep,levels = c('White',
                                                                      'Black',
                                                                      'Asian',
                                                                      'Mixed',
                                                                      'Other'),
                                  ordered = F)
baseline_demo$Ethnicity_PrntRep = factor(baseline_demo$Ethnicity_PrntRep,levels = c('No',
                                                                                'Hispanic/Latino/Latina'),
                                       ordered = F)
baseline_demo$RaceEthnicity = factor(baseline_demo$RaceEthnicity,levels = c('White',
                                                                        'Black',
                                                                        'Asian',
                                                                        'Hispanic',
                                                                        'Other'),
                                   ordered = F)
Demographics_before_impute = rbind(baseline_demo,as.data.table(subset(Demographic,eventname!='baseline_year_1_arm_1')))
saveRDS(Demographics_before_impute,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Demographic_no_impute.rds")
# 
# Number of multiple imputed datasets & maximum number of iterations 
n.imp = 5
n.iter = 5

var.ls <- c("src_subject_id", "interview_age", "sex", "Race_PrntRep", "FamilyIncome", "ParentsEdu","ParentMarital")
dat0 <- baseline_demo[, var.ls, with = FALSE ]


ini <- mice( dat0, m = 1, maxit = 0 )
meth = ini$meth

meth["sex"]     <- "logreg"
meth["ParentMarital"] <- "logreg"
meth["Race_PrntRep"]   <- "polyreg"
meth["FamilyIncome"]      <- "polyreg"
meth["ParentsEdu"]  <- "polyreg"

pred = ini$pred

# Excluding variables from the imputation models
pred[, c("src_subject_id") ] <- 0
pred

# Specifying parameters for the imputation
post <- mice( dat0, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post

dat.imp <- mice( dat0, meth = meth, pred = pred, post = post,
                 seed = 1111,
                 m = n.imp, maxit = n.iter)
rm(dat0)

# get one imputed dataset out
completedData <- complete(dat.imp,1)
baseline_demo[, var.ls] = completedData

baseline_demo[, table(interview_age,useNA = 'if')]
baseline_demo[, table(sex,useNA = 'if')]
baseline_demo[, table(Race_PrntRep,useNA = 'if')]
baseline_demo[, table(RaceEthnicity,useNA = 'if')]
baseline_demo[, table(Ethnicity_PrntRep,useNA = 'if')]
baseline_demo[, table(FamilyIncome,useNA = 'if')]
baseline_demo[, table(ParentsEdu,useNA = 'if')]
baseline_demo[, table(ParentMarital,useNA = 'if')]

Demographics_after_impute = rbind(baseline_demo,as.data.table(subset(Demographic,eventname!='baseline_year_1_arm_1')))
saveRDS(Demographics_after_impute,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Demographic_imputed.rds")

after = subset(Demographics_after_impute,eventname=='baseline_year_1_arm_1',select = var.ls)
before = subset(Demographics_before_impute,eventname=='baseline_year_1_arm_1',select = var.ls)
after$Label = "AfterImpute"
before$Label = "BeforeImpute"
comp_baseline_demo = rbind(after,before)

compareGroups::compareGroups(Label~interview_age+sex+
                               Race_PrntRep+FamilyIncome+ParentsEdu+
                               ParentMarital,data = comp_baseline_demo) %>%
  compareGroups::createTable(show.n = T,show.ci = F,show.ratio = T) -> groupdiffTab

print(groupdiffTab)

compareGroups::export2xls(groupdiffTab,file = 'I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\Comp_Demo_Impute.xlsx')
