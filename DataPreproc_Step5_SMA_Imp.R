# =============================================================================#
# SDPP Step 5: Perform KNN imputation for Screen Use-related Data
# R Packages Dependency: bruceR, naniar, simputation
# Step File Notes: 
# 1. The imputation about the core demographics was referred to ABCD-DAIRC
# 2. Ref Link:
# https://github.com/fenmeng123/2022_JAACAP_ABCD_SMA_pattern
# https://wiki.abcdstudy.org/release-notes/non-imaging/novel-technologies.html#screen-time-questionnaire
# 3. Target File: ABCD4.0_Demographics_Recode.rds
# Update Date: 2023.7.7
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_5.txt'
AutoLogFilePath = fullfile(ProjectDirectory,'Res_1_Logs',AutoLogFileName)
sink(file = AutoLogFilePath)
ResultsOutputDir = S5_ResultsOutputDir
library(naniar)
library(R.matlab)
# ==============================MAIN CODES=====================================#
# 2. Load preprocessed SMA data from RDS file in the project directory
SMA = SDPP.read.intdat('ABCD5.0_SMA_Rec.rds',
                       ProjectDirectory = ProjectDirectory)
Matlab$startServer()
MATLAB_Client = Matlab()
isOpen <- open(MATLAB_Client)

# 3. Impute Screen Use Time -----------------------------------------------

impute_var_ls = c("weekday_TV","weekday_Video",
               "weekday_Game","weekday_Text",
               "weekday_SocialNet","weekday_VideoChat",
               "weekend_TV","weekend_Video",
               "weekend_Game","weekend_Text",
               "weekend_SocialNet","weekend_VideoChat")

imputed_var = 'weekday_TV'
impute_formula = paste(
  imputed_var,
  paste(impute_var,collapse = '+'),
  sep = '~')

SMA %>% subset(eventname == "baseline_year_1_arm_1"|
                 eventname == "1_year_follow_up_y_arm_1"|
                 eventname == "2_year_follow_up_y_arm_1")  %>%
  select(all_of(impute_var_ls))-> SMA_TPD_STQ_T012

setVariable(MATLAB_Client,data = SMA_TPD_STQ_T012)
evaluate(MATLAB_Client,"data = struct2table(data);")
# evaluate(MATLAB_Client,"help('knnimpute')")
evaluate(MATLAB_Client,"tmp = knnimpute(data.Variables);")
imputed_data <- getVariable(MATLAB_Client,"tmp")

close(MATLAB_Client)

print(MATLAB_Client)

MVA.Report.By.Wave(tmp)
