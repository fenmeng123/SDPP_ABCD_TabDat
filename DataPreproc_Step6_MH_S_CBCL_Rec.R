# =============================================================================#
# SDPP Step 6: Read and re-coding the mental health summary data
# R Packages Dependency: bruceR, naniar
# Step File Notes: 
# 
# Target File: 
# ABCD 4.0 : abcd_mhy02.txt
# ABCD 5.0: /core/mental-health/mh_y_upps.csv, mh_y_pps.csv, mh_y_peq.csv, 
#           mh_y_le.csv, mh_y_erq.csv, mh_y_bisbas.csv, mh_y_7up.csv
# Update Date: 2023.7.10
# =============================================================================#
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_6.txt'
AutoLogFilePath = fullfile(ProjectDirectory,'Res_1_Logs',AutoLogFileName)
sink(file = AutoLogFilePath)
ResultsOutputDir = S6_ResultsOutputDir
library(naniar)


file_ls = c("mh_y_upps.csv","mh_y_pps.csv","mh_y_peq.csv","mh_y_le.csv",
            "mh_y_erq.csv","mh_y_bisbas.csv","mh_y_7up.csv")
MH = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
              TableNames = file_ls,
              FolderName = 'mental-health')
MH %>% MVA.Report.By.Wave() %>% 
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_MH_S_Raw.doc'),
              row.names = F,
              nsmalls = 1)
