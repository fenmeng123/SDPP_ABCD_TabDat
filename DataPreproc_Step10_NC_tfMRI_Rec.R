# =============================================================================#
# SDPP Step 10: read and re-coding all behavioral tasks performance in task-based
# fMRI recording (i.e. Monetary Incentive Delay, Stop Signal, and Emotion-Nback Tasks)
# R Packages Dependency: bruceR, naniar, readxl
# Step File Notes: 
# https://wiki.abcdstudy.org/release-notes/imaging/task-fmri-behavior.html
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: /core/imaging/mri_y_tfmr_mid_beh.csv
#                         mri_y_tfmr_mid_qtn.csv
#                         mri_y_tfmr_sst_beh.csv
#                         mri_y_tfmr_nback_beh.csv
#                         mri_y_tfmr_nback_rec_beh.csv
# Update Date: 2023.8.7
# Update Date: 2023.11.2
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_10.txt'
DatTableNames = c("mri_y_tfmr_mid_beh.csv","mri_y_tfmr_mid_qtn.csv",
                  "mri_y_tfmr_sst_beh.csv","mri_y_tfmr_nback_beh.csv",
                  "mri_y_tfmr_nback_rec_beh.csv")
SubfolderName = "imaging"
AutoLogFilePath = fullfile(ProjectDirectory,'Res_1_Logs',AutoLogFileName)
s_sink(AutoLogFilePath)
ResultsOutputDir = S10_ResultsOutputDir
rm(S9_ResultsOutputDir)
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load NIHTB and its composite scores data ---------------------------------
data = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
                     TableNames = DatTableNames,
                     FolderName = SubfolderName)
NEW_data <- SDPP.select.cols.by.dict(data,
                                     TableNames = DatTableNames)




# 6. Save re-coded data into RDS file -------------------------------------

NEW_data[sort(colnames(NEW_data))] %>%
  select(c(src_subject_id,eventname,
           everything())) -> NEW_tfMRI_behav
SDPP.save.file(NEW_tfMRI_behav,
               FileName = "NC_tfMRI_behav.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

NEW_tfMRI_behav %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_NC_tfMRI_behav_Rec.doc'),
              row.names = F,
              nsmalls = 1)

select(NEW_tfMRI_behav,-c(src_subject_id,eventname)) %>% 
  psych::describeBy(group = Recode.Eventname(NEW_tfMRI_behav)$eventname,
                    mat = T,digits =2) %>%
  print_table(file = fullfile(ResultsOutputDir,'VSO_ALL_NC_tfMRI_behav.doc'),
              row.names = T,
              nsmalls = 1,
              digits = 2)
# End of Script -----------------------------------------------------------

s_close_sink()
