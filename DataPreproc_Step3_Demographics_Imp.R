# =============================================================================#
# SDPP Step 3: Imputing Demographics with Chained Equation Models.
# R Packages Dependency: bruceR, mice, forcats, naniar, comparegroups
# Step File Notes: 
# 1. The imputation about the core demographics was referred to ABCD-DAIRC
# 2. Ref Link:
# https://github.com/ABCD-STUDY/analysis-nda/blob/861cb4063dba93794dc8dd18feb2673c306675ea/notebooks/general/impute_demographics.md
# https://zhuanlan.zhihu.com/p/584937019
# 3. Target File (Intermediate Data File): 
#     ABCD5.0_Demographics_MICE.rds
#     ABCD5.0_Demographics_Non-impute.rds
#     ABCD5.0_Demographics_Non-impute.csv
#     ABCD5.0_Demographics_Imputed.rds
#     ABCD5.0_Demographics_Imputed.csv
# Update Date: 2023.06.16 By Kunru Song
# Update Date: 2023.07.05 By Kunru Song
# Update Date: 2023.07.07 By Kunru Song
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_3.txt'
s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(mice)
library(forcats)
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load and prepare re-coded demographic data --------------------------
Demographic <- addprefix(Prefix,'Demographics_Recode.rds') %>%
  fullfile(IntermediateDataDir,.) %>%
  import(verbose = T)

Demographic$FamilyID = as.character(Demographic$FamilyID)
baseline_demo = subset(Demographic,eventname=='baseline_year_1_arm_1')
baseline_demo %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_T0_Demo_Rec.doc'),
              row.names = F,
              digits = 1)
print(sapply(baseline_demo, typeof))
baseline_demo = as.data.table(baseline_demo)
fprintf("----------------------------------------------------------------------------\n")

# 3. Consistence Auto-check and auto-impute for Youth's Race and E --------
# Check the consistence among Race (Parent-report), Ethnicity (Parent-report) and Race_ethnicity (from acspsw03.txt)
baseline_demo$Race_PrntRep = tidyr::replace_na(as.character(baseline_demo$Race_PrntRep),"")
baseline_demo$RaceEthnicity = tidyr::replace_na(as.character(baseline_demo$RaceEthnicity),"")
baseline_demo$Ethnicity_PrntRep = tidyr::replace_na(as.character(baseline_demo$Ethnicity_PrntRep),"")
fprintf("Missing Value Counts before Consistence Check:\n")
dt.print.mva.counts('baseline_demo','Race_PrntRep')
dt.print.mva.counts('baseline_demo','Ethnicity_PrntRep')
dt.print.mva.counts('baseline_demo','RaceEthnicity')
fprintf("----------------------------------------------------------------------------\n")
fprintf("Beginning Race and Ethnithicity Consistence Auto-check and Auto-impute:\n")
tmp = baseline_demo[,c('Race_PrntRep','Ethnicity_PrntRep','RaceEthnicity')]
race_miss = tmp[(tmp$Race_PrntRep==""),]
ethn_miss = tmp[(tmp$Ethnicity_PrntRep==""),]
race_miss$Race_PrntRep[race_miss$RaceEthnicity!='Hispanic'] = race_miss$RaceEthnicity[race_miss$RaceEthnicity!='Hispanic']
ethn_miss$Ethnicity_PrntRep[ethn_miss$RaceEthnicity=='Hispanic'] = "Hispanic"
ethn_miss$Ethnicity_PrntRep[ethn_miss$RaceEthnicity!='Hispanic'] = "Non-hispanic"
baseline_demo$Race_PrntRep[baseline_demo$Race_PrntRep==""] = race_miss$Race_PrntRep
baseline_demo$Ethnicity_PrntRep[baseline_demo$Ethnicity_PrntRep==""] = ethn_miss$Ethnicity_PrntRep
fprintf("----------------------------------------------------------------------------\n")
fprintf("Consistence Auto-check and Auto-impute have been finished!\n")
dt.print.mva.counts('baseline_demo','Race_PrntRep')
dt.print.mva.counts('baseline_demo','Ethnicity_PrntRep')
fprintf("Re-coding Race and Ethniticy variables......\n")
baseline_demo$Race_PrntRep = factor(baseline_demo$Race_PrntRep,
                                  levels = c('White',
                                             'Black',
                                             'Asian',
                                             'AIAN',
                                             'NHPI',
                                             'Mixed',
                                             'Other'),
                                  ordered = F)
dt.print.mva.counts('baseline_demo','Race_PrntRep')
baseline_demo$Race_4L = fct_collapse(baseline_demo$Race_PrntRep,
                                   `Mixed/Other` = c('AIAN',
                                                     'NHPI',
                                                     'Mixed',
                                                     'Other'))
dt.print.mva.counts('baseline_demo','Race_4L')
baseline_demo$Race_6L = fct_collapse(baseline_demo$Race_PrntRep,
                                   `Mixed/Other` = c('Other',
                                                     'Mixed'))
dt.print.mva.counts('baseline_demo','Race_6L')
baseline_demo$Ethnicity_PrntRep = factor(baseline_demo$Ethnicity_PrntRep,
                                         levels = c('Non-hispanic',
                                                    'Hispanic'),
                                       ordered = F)
dt.print.mva.counts('baseline_demo','Ethnicity_PrntRep')
baseline_demo$RaceEthnicity = factor(baseline_demo$RaceEthnicity,levels = c('White',
                                                                        'Black',
                                                                        'Asian',
                                                                        'Hispanic',
                                                                        'Other'),
                                   ordered = F)
dt.print.mva.counts('baseline_demo','RaceEthnicity')
Demographics_before_impute = rbind(baseline_demo,
                                   as.data.table(subset(Demographic,eventname!='baseline_year_1_arm_1')))

# 4. Consistence Check for Youth's interview_age --------------------------
fprintf("No. %d rows were found in non-imputed data.table .",nrow(Demographics_before_impute))
Flag = which(is.na(Demographics_before_impute$interview_age))
fprintf("%d data points were found with missing interview_age, show as following:\n",length(Flag))
print(Demographics_before_impute[Flag,c("src_subject_id",'eventname','interview_age','YouthAge_Prnt')])
fprintf("Where the script found %d data points have non-empty YouthAge_Prnt.\n",
        sum(!is.na(Demographics_before_impute$YouthAge_Prnt[Flag])))
fprintf("These data points will be replace NA with YouthAge_Prnt*12.\n")
Demographics_before_impute$interview_age[Flag] = Demographics_before_impute$YouthAge_Prnt[Flag] * 12

# 5. Save Non-imputed Demographic Data ------------------------------------
SDPP.save.file(Demographics_before_impute,
              FileName = "Demographics_Non-impute.rds",
              Prefix = Prefix,
              ProjectDirectory = ProjectDirectory)
SDPP.save.file(Demographics_before_impute,
               FileName = "Demographics_Non-impute.csv",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)
Demographics_before_impute_baseline = subset(Demographics_before_impute,
                                             eventname == "baseline_year_1_arm_1")

Demographics_before_impute_baseline %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_T0_Demo_Rec_Consist.doc'),
              row.names = F,
              digits = 1)

# 6. Multiple Imputation for all key demographic variables--------------
var.ls <- c("src_subject_id", "interview_age",
            "SexAssigned","BMI",
            "BirthCountry", "YouthNativeLang",
            "Religon_2L",
            "Race_PrntRep", "Ethnicity_PrntRep",
            "ParentsHighEdu_5L","ParentsMarital_6L","ParentEmploy",
            "FamilyIncome", "HouseholdStructure","HouseholdSize",
            "Relationship_3L")
dat0 <- Demographics_before_impute_baseline[, var.ls, with = FALSE ]
# Draw a flux plot for original data (non-imputed data)
png(filename = fullfile(ResultsOutputDir,'MVA_MI_Fluxplot.png'),
    width = 1200,
    height = 800,
    units = "px")
fluxplot(dat0,eqscplot = T, font = 6)
dev.off()

fprintf("The following variables were included in Multilple Imputation:\n")
print(colnames(dat0)[2:ncol(dat0)])
fprintf("Missing Value Analysis (MVA):\n")
for (i in colnames(dat0)[2:ncol(dat0)]){
  dt.print.mva.counts('Demographics_before_impute_baseline',i)
}
print(miss_var_summary(dat0))
var.ls.imp = miss_var_summary(dat0)$variable[miss_var_summary(dat0)$n_miss != 0]

sapply(dat0, typeof)

dat0 %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_T0_Demo_KeyVars.doc'),
              row.names = F,
              digits = 1)

# Perform Little's MCAR Testing
fprintf("========================Little's MCAR Testing========================")
print(mcar_test(dat0[,-1]))
# Perform Multiple-imputation by mice package
ini <- mice( dat0, m = 1, maxit = 0 )
meth = ini$meth
pred = ini$pred

# Check the Imputation Method for each variable
fprintf('Imputation Methods:\n')
print(meth)

# Excluding Subject ID variables from the imputation models
pred[, c("src_subject_id") ] <- 0
pred[c("src_subject_id"), ] <- 0


fprintf("Predictors Matrix for Multiple Imputation Model:\n")
print(pred)

# Specifying parameters for the imputation
post <- mice( dat0, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post
# Perform Multiple Imputation for ABCD baseline wave key demographic data
fprintf("Performing Mutilple Imputation......\n")
dat.imp <- mice( dat0, meth = meth, pred = pred, post = post,
                 seed = 1111,
                 m = n.imp, maxit = n.iter)
rm(dat0)
fprintf("Mulitple Imputation Finished!\n")
SDPP.save.file(dat.imp,
               FileName = "Demographics_MICE_MILDS.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

# 7. Post-processing for Multiple Imputation ------------------------------
if (!exists('dat.imp')){
  dat.imp = SDPP.read.intdat('ABCD5.0_Demographics_MICE.rds',ProjectDirectory)
}
# get imputed dataset out
Imputed_Data = Comb.MICE(dat.imp,var.ls.imp)
Demographics_after_impute_baseline = Demographics_before_impute_baseline
Demographics_after_impute_baseline = select(Demographics_after_impute_baseline,
                                            -matches(var.ls.imp))
Demographics_after_impute_baseline = merge(Demographics_after_impute_baseline,
                                           Imputed_Data,
                                           by = 'src_subject_id')
Demographics_Imputed = rbind(Demographics_after_impute_baseline,
                            subset(Demographic,
                             eventname != "baseline_year_1_arm_1"))
# BOCF for imputed data
Demographics_Imputed = as.data.frame(Demographics_Imputed)
#     Remove BMI from BOCF variable list
var.ls.imp = grep('BMI',var.ls.imp,value = T,invert = T)
fprintf("The following variables will be processed by BOCF:\n")
print(var.ls.imp)
for (i in var.ls.imp){
  Demographics_Imputed = BOCF.Variables(Demographics_Imputed,'baseline_year_1_arm_1',i) 
}
# Comapre imputed and non-imputed data
Demographics_before_impute_baseline$Label = "Before MI"
Demographics_after_impute_baseline$Label ="After MI"
comp_baseline_demo = rbind(Demographics_after_impute_baseline,
                           Demographics_before_impute_baseline)
comp_baseline_demo$Label = factor(comp_baseline_demo$Label,
                                  levels = c('Before MI',
                                             'After MI'))
CompareFormula = str_c("Label ~ ",
                       paste(var.ls[! var.ls %in% c('src_subject_id','SiteID','FamilyID')],
                             collapse = "+"))
compareGroups::compareGroups(formula = as.formula(CompareFormula),
                             data = comp_baseline_demo) %>%
  compareGroups::createTable(show.n = T,show.ci = F,show.ratio = T) -> groupdiffTab

print(groupdiffTab)

compareGroups::export2xls(groupdiffTab,
                          file = fullfile(ResultsOutputDir,'MVA_MI_CompareTable.xlsx'))
# Recoding some varibales based on the imputed data
fprintf('Re-coding the imputed data......\n')

Demographics_Imputed$Race_6L = fct_collapse(Demographics_Imputed$Race_PrntRep,
                                            `Mixed/Other` = c('Other',
                                                              'Mixed'))
print(table(Demographic$Race_6L))

Demographics_Imputed$Race_4L = fct_collapse(Demographics_Imputed$Race_PrntRep,
                                   `Mixed/Other` = c('AIAN',
                                                     'NHPI',
                                                     'Mixed',
                                                     'Other'))
print(table(Demographic$Race_4L))

Demographics_Imputed$ParentsHighEdu_2L = fct_collapse(Demographics_Imputed$ParentsHighEdu_5L,
                                             `High school or less` = c('< HS Diploma',
                                                                       'HS Diploma/GED'),
                                             `College education` = c('Some College',
                                                                     'Bachelor',
                                                                     'Post Graduate Degree'),
)
print(table(Demographic$ParentsHighEdu_2L))

Demographics_Imputed$ParentsMarital_2L = fct_collapse(Demographics_Imputed$ParentsMarital_6L,
                                                     `Married or living with partner` = c('Married',
                                                                               'Living with partner'),
                                                     `Single` = c('Divorced',
                                                                  'Separated',
                                                                  'Widowed',
                                                                  'Never married'),
)
Demographics_Imputed$ParentsMarital_2L = factor(Demographics_Imputed$ParentsMarital_2L,
                                       levels = c('Married or living with partner',
                                                  'Single'),
                                       ordered = F)
print(table(Demographics_Imputed$ParentsMarital_2L,useNA = 'if'))

Demographics_Imputed$ParentsMarital_X_Employ[
  Demographics_Imputed$ParentsMarital_2L=='Married or living with partner' & 
    Demographics_Imputed$ParentEmploy=='Working' & 
    Demographics_Imputed$PartnerEmploy=='Working']='Married, 2 in LF'
Demographics_Imputed$ParentsMarital_X_Employ[
  Demographics_Imputed$ParentsMarital_2L=='Married or living with partner' & 
    Demographics_Imputed$ParentEmploy=='Non-working' & 
    Demographics_Imputed$PartnerEmploy=='Working']='Married, 1 in LF'
Demographics_Imputed$ParentsMarital_X_Employ[
  Demographics_Imputed$ParentsMarital_2L=='Married or living with partner' & 
    Demographics_Imputed$ParentEmploy=='Working' & 
    Demographics_Imputed$PartnerEmploy=='Non-Working']='Married, 1 in LF'
Demographics_Imputed$ParentsMarital_X_Employ[ 
  Demographics_Imputed$ParentsMarital_2L=='Married or living with partner' & 
    Demographics_Imputed$ParentEmploy=='Non-working' & 
    Demographics_Imputed$PartnerEmploy=='Non-working']='Married, 0 in LF'
Demographics_Imputed$ParentsMarital_X_Employ[
  Demographics_Imputed$ParentsMarital_2L=='Single' & 
    Demographics_Imputed$ParentEmploy=='Non-working']='Single, Not in LF'
Demographics_Imputed$ParentsMarital_X_Employ[
  Demographics_Imputed$ParentsMarital_2L=='Single' & 
    Demographics_Imputed$ParentEmploy=='Working']='Single, in LF'

Demographics_Imputed$ParentsMarital_X_Employ = factor(Demographics_Imputed$ParentsMarital_X_Employ,
                                           levels = c('Married, 2 in LF',
                                                      'Married, 1 in LF',
                                                      'Married, 0 in LF',
                                                      'Single, in LF',
                                                      'Single, Not in LF'),
                                           ordered = F)
print(table(Demographics_Imputed$ParentsMarital_X_Employ,useNA = 'if'))

# Save Imputed Demographics Data
SDPP.save.file(Demographics_Imputed,
               FileName = "Demographics_Imp.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)
SDPP.save.file(Demographics_Imputed,
               FileName = "Demographics_Imp.csv",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)
# MVA for Imputed Demographics Data
if (!exists('Demographics_Imputed')){
  Demographics_Imputed = SDPP.read.intdat(FileName = 'ABCD5.0_Demographics_Imp.rds',
                   ProjectDirectory = ProjectDirectory)
}
Demographics_Imputed %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_Demo_Rec_Imp.doc'),
              row.names = F,
              digits = 1)
# End of Script -----------------------------------------------------------
rm(ResultsOutputDir,n.imp,n.iter)
s_close_sink()


