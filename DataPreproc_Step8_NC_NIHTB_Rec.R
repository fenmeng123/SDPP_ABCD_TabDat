# =============================================================================#
# SDPP Step 8: read and re-coding the NIH toolbox scores
# R Packages Dependency: bruceR, naniar, readxl
# Step File Notes: 
# 
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: /core/neurocognition/nc_y_nihtb.csv
# Update Date: 2023.7.18
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
# AutoLogFileName = 'Log_SDPP-ABCD-TabDat_8.txt'
# AutoLogFilePath = fullfile(ProjectDirectory,'Res_1_Logs',AutoLogFileName)
# sink(file = AutoLogFilePath)
ResultsOutputDir = S8_ResultsOutputDir 
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load NIHTB and its composite scores data ---------------------------------
nihtb_FileDir = fullfile(TabulatedDataDirectory,'/neurocognition/nc_y_nihtb.csv')
NIHTB = readABCDdata(nihtb_FileDir)
fprintf("NIHTB Data type:\n")
sapply(NIHTB,typeof)
NIHTB %>% MVA.Report.By.Wave() %>% 
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_NC_NIHTB_Raw.doc'),
              row.names = F,
              nsmalls = 1)