# =============================================================================#
# SDPP Step 5: Perform KNN imputation for Screen Use-related Data
# R Packages Dependency: bruceR, naniar, simputation
# Step File Notes: 
# 1. Ref Link:
# https://github.com/fenmeng123/2022_JAACAP_ABCD_SMA_pattern
# https://wiki.abcdstudy.org/release-notes/non-imaging/novel-technologies.html#screen-time-questionnaire
# 2. Target File: ABCD5.0_SMA_Rec.rds, ABCD5.0_SMA_Rec_Imp.rds
# Update Date: 2023.7.7
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_5.txt'
s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(naniar)
library(R.matlab)
# ==============================MAIN CODES=====================================#
# 2. Load preprocessed SMA data from RDS file in the project directory
SMA = SDPP.read.intdat('ABCD5.0_SMA_Rec.rds',
                       ProjectDirectory = ProjectDirectory)
Matlab$startServer()
MATLAB_Client = Matlab()
isOpen <- open(MATLAB_Client)
if (!isOpen) {
  stop("Can not open a MATLAB server on local!")
}
print(MATLAB_Client)

# 3. Impute Screen Use Time -----------------------------------------------
impute_var_ls = c("weekday_TV","weekday_Video",
               "weekday_Game","weekday_Text",
               "weekday_SocialNet","weekday_VideoChat",
               "weekend_TV","weekend_Video",
               "weekend_Game","weekend_Text",
               "weekend_SocialNet","weekend_VideoChat")

SMA %>% subset(eventname == "baseline_year_1_arm_1"|
                 eventname == "1_year_follow_up_y_arm_1"|
                 eventname == "2_year_follow_up_y_arm_1")  %>%
  select(all_of(impute_var_ls),c(src_subject_id,eventname))-> SMA_TPD_STQ_T012

SMA_TPD_STQ_T012 %>% MVA.Report.CaseMiss.By.Wave() %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_STQ12_T012.doc'))

SMA_TPD_STQ_T012 <- MVA.KNNimpute(SMA_TPD_STQ_T012,impute_var_ls,MATLAB_Client)
  
SMA_TPD_STQ_T012 %>% MVA.Report.CaseMiss.By.Wave(verbose = F) %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_STQ12_T012_Imp.doc'))
  
SMA = Bind.imp.By.Wave('SMA',SMA_TPD_STQ_T012)

# 4. Impute MPIQ ----------------------------------------------------------
impute_var_ls = c("MPIQ_1","MPIQ_2",
                  "MPIQ_3","MPIQ_4",
                  "MPIQ_5","MPIQ_6",
                  "MPIQ_7","MPIQ_8")

SMA %>% subset(eventname == "2_year_follow_up_y_arm_1"|
                 eventname == "3_year_follow_up_y_arm_1"|
                 eventname == "4_year_follow_up_y_arm_1")  %>%
  select(all_of(impute_var_ls),c(src_subject_id,eventname))-> MPIQ_T234

MPIQ_T234 %>% MVA.Report.CaseMiss.By.Wave() %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_MPIQ_T234.doc'))

MPIQ_T234 <- MVA.KNNimpute(MPIQ_T234,impute_var_ls,MATLAB_Client)

MPIQ_T234 %>% MVA.Report.CaseMiss.By.Wave(verbose = F) %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_MPIQ_T234_Imp.doc'))

SMA = Bind.imp.By.Wave('SMA',MPIQ_T234)

# 5. Impute SMAS ----------------------------------------------------------
impute_var_ls = c("SMAS_1","SMAS_2",
                  "SMAS_3","SMAS_4",
                  "SMAS_5","SMAS_6")

SMA %>% subset(eventname == "2_year_follow_up_y_arm_1"|
                 eventname == "3_year_follow_up_y_arm_1"|
                 eventname == "4_year_follow_up_y_arm_1")  %>%
  select(all_of(impute_var_ls),c(src_subject_id,eventname))-> SMAS_T234

SMAS_T234 %>% MVA.Report.CaseMiss.By.Wave() %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_SMAS_T234.doc'))

SMAS_T234 <- MVA.KNNimpute(SMAS_T234,impute_var_ls,MATLAB_Client)

SMAS_T234 %>% MVA.Report.CaseMiss.By.Wave(verbose = F) %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_SMAS_T234_Imp.doc'))

SMA = Bind.imp.By.Wave('SMA',SMAS_T234)


# 6. Impute VGAS ----------------------------------------------------------
impute_var_ls = c("VGAS_1","VGAS_2",
                  "VGAS_3","VGAS_4",
                  "VGAS_5","VGAS_6")

SMA %>% subset(eventname == "2_year_follow_up_y_arm_1"|
                 eventname == "3_year_follow_up_y_arm_1"|
                 eventname == "4_year_follow_up_y_arm_1")  %>%
  select(all_of(impute_var_ls),c(src_subject_id,eventname))-> VGAS_T234

VGAS_T234 %>% MVA.Report.CaseMiss.By.Wave() %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_VGAS_T234.doc'))

VGAS_T234 <- MVA.KNNimpute(VGAS_T234,impute_var_ls,MATLAB_Client)

VGAS_T234 %>% MVA.Report.CaseMiss.By.Wave(verbose = F) %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_VGAS_T234_Imp.doc'))

SMA = Bind.imp.By.Wave('SMA',VGAS_T234)

# 7. Impute SUAB ----------------------------------------------------------
impute_var_ls = c("SUAB_1","SUAB_2",
                  "SUAB_3","SUAB_4",
                  "SUAB_5","SUAB_6",
                  "SUAB_7","SUAB_8","SUAB_9")

SMA %>% subset(eventname == "2_year_follow_up_y_arm_1"|
                 eventname == "3_year_follow_up_y_arm_1")  %>%
  select(all_of(impute_var_ls),c(src_subject_id,eventname))-> SUAB_T23

SUAB_T23 %>% MVA.Report.CaseMiss.By.Wave() %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_SUAB_T23.doc'))

SUAB_T23 <- MVA.KNNimpute(SUAB_T23,impute_var_ls,MATLAB_Client)

SUAB_T23 %>% MVA.Report.CaseMiss.By.Wave(verbose = F) %>%
  print_table(digits = 2,row.names = F,
              file = fullfile(ResultsOutputDir,'MVA_CaseReport_ALL_SMA_SUAB_T23_Imp.doc'))

SMA = Bind.imp.By.Wave('SMA',SUAB_T23)


# 8. Save the imputed SMA data --------------------------------------------
SMA %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_SMA_Rec_Imp.doc'),
              row.names = F,
              nsmalls = 1)

SDPP.save.file(SMA,
               FileName = "SMA_Rec_Imp.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

# End of Script -----------------------------------------------------------


close(MATLAB_Client)

print(MATLAB_Client)

s_close_sink()
