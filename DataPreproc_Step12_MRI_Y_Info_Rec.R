# =============================================================================#
# SDPP Step 12: read and re-coding some basic information and the ABCD Official
# 'Recommended Image Inclusion' for MRI Quality Control
# R Packages Dependency: bruceR, naniar
# Step File Notes: 
# https://wiki.abcdstudy.org/release-notes/imaging/overview.html
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: /core/imaging/mri_y_adm_info.csv
#                         mri_y_qc_incl.csv
#                         mri_y_qc_motion.csv
# 
# Update Date: 2023.11.27
# =============================================================================#

AutoLogFileName = 'Log_SDPP-ABCD-TabDat_12.txt'
DatTableNames = c("mri_y_adm_info.csv","mri_y_qc_incl.csv","mri_y_qc_motion.csv")
SubfolderName = "imaging"

s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load task performance data during fMRI scanning ----------------------
data = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
                     TableNames = DatTableNames,
                     FolderName = SubfolderName)
NEW_data <- SDPP.select.cols.by.dict(data,
                                     TableNames = DatTableNames)

# 3. Re-naming all selected variables -------------------------------------
NEW_data <- NEW_data %>%
  rename(MRI_BatchID = mri_info_deviceserialnumber,
         QC_RecInc_sMRI_T1w    = imgincl_t1w_include,
         QC_RecInc_sMRI_T2w    = imgincl_t2w_include,
         QC_RecInc_dMRI_DTIRSI = imgincl_dmri_include,
         QC_RecInc_rsfMRI      = imgincl_rsfmri_include,
         QC_RecInc_tfMRI_MID   = imgincl_mid_include,
         QC_RecInc_tfMRI_NB    = imgincl_nback_include,
         QC_RecInc_tfMRI_SST   = imgincl_sst_include,
         HM_tfMRI_NB_MeanFD         = tfmri_nback_all_meanmotion,
         HM_tfMRI_NB_MeanRotat      = tfmri_nback_all_meanrot,
         HM_tfMRI_NB_MeanTrans      = tfmri_nback_all_meantrans,
         HM_tfMRI_NB_MaxFD          = tfmri_nback_all_maxmotion,
         HM_tfMRI_NB_MaxRotat       = tfmri_nback_all_maxrot,
         HM_tfMRI_NB_MaxTrans       = tfmri_nback_all_maxtrans,
         HM_tfMRI_NB_NOF_ExclDummy  = tfmri_nback_all_nvols,
         HM_tfMRI_NB_NOF_SubThresh  = tfmri_nback_all_subthreshnvols,
         HM_tfMRI_SST_MeanFD        = tfmri_sst_all_meanmotion,
         HM_tfMRI_SST_MeanRotat     = tfmri_sst_all_meanrot,
         HM_tfMRI_SST_MeanTrans     = tfmri_sst_all_meantrans,
         HM_tfMRI_SST_MaxFD         = tfmri_sst_all_maxmotion,
         HM_tfMRI_SST_MaxRotat      = tfmri_sst_all_maxrot,
         HM_tfMRI_SST_MaxTrans      = tfmri_sst_all_maxtrans,
         HM_tfMRI_SST_NOF_ExclDummy = tfmri_sst_all_nvols,
         HM_tfMRI_SST_NOF_SubThresh = tfmri_sst_all_subthreshnvols,
         HM_tfMRI_MID_MeanFD        = tfmri_mid_all_meanmotion,
         HM_tfMRI_MID_MeanRotat     = tfmri_mid_all_meanrot,
         HM_tfMRI_MID_MeanTrans     = tfmri_mid_all_meantrans,
         HM_tfMRI_MID_MaxFD         = tfmri_mid_all_maxmotion,
         HM_tfMRI_MID_MaxRotat      = tfmri_mid_all_maxrot,
         HM_tfMRI_MID_MaxTrans      = tfmri_mid_all_maxtrans,
         HM_tfMRI_MID_NOF_ExclDummy = tfmri_mid_all_nvols,
         HM_tfMRI_MID_NOF_SubThresh = tfmri_mid_all_subthreshnvols,
         HM_dMRI_MeanFD             = dmri_meanmotion,
         HM_dMRI_MeanRotat          = dmri_meanrot,
         HM_dMRI_MeanTrans          = dmri_meantrans,
         HM_rsfMRI_MeanFD           = rsfmri_meanmotion,
         HM_rsfMRI_MeanRotat        = rsfmri_meanrot,
         HM_rsfMRI_MeanTrans        = rsfmri_meantrans,
         HM_rsfMRI_MaxFD            = rsfmri_maxmotion,
         HM_rsfMRI_MaxRotat         = rsfmri_maxrot,
         HM_rsfMRI_MaxTrans         = rsfmri_maxtrans,
         HM_rsfMRI_NOF_ExclDummy    = rsfmri_nvols,
         HM_rsfMRI_NOF_SubThresh    = rsfmri_subthreshnvols,
         HM_rsfMRI_NOF_SupraThre    = rsfmri_subthreshcontignvols,
  ) %>%
  select(
    c(src_subject_id,eventname,
      MRI_BatchID,
      starts_with('QC_RecInc'),
      everything())
  ) %>%
  mutate(
    across(starts_with('QC_RecInc'),
           function(x) replace_na(x,0))
  )
# 4. Save re-coded data ---------------------------------------------------

NEW_data[sort(colnames(NEW_data))] %>%
  select(c(src_subject_id,eventname,
           everything())) -> NEW_MRI_InfoQC
SDPP.save.file(NEW_MRI_InfoQC,
               FileName = "MRI_InfoQC.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

NEW_MRI_InfoQC %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_MRI_InfoQC.doc'),
              row.names = F)

select(NEW_MRI_InfoQC,-c(src_subject_id,eventname)) %>% 
  psych::describeBy(group = Recode.Eventname(NEW_MRI_InfoQC)$eventname,
                    mat = T,digits =2) %>%
  print_table(file = fullfile(ResultsOutputDir,'VSO_ALL_MRI_InfoQC.doc'),
              row.names = T,
              digits = 2)


# End of Script -----------------------------------------------------------

s_close_sink()