# =============================================================================#
# SDPP Step 13: read and select all sMRI-related variables in Desikan (aparc.annot)
# and Destrieux (aparc.a2009s.annot) atlas, also including subcortical segmentation
# (aseg)
# 
# R Packages Dependency: bruceR, naniar
# Step File Notes: 
# https://wiki.abcdstudy.org/release-notes/imaging/overview.html
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: 
# (Sub-cortical Segmentation)
#           /core/imaging/mri_y_smr_vol_aseg.csv
#                         mri_y_smr_t1_aseg.csv
#                         mri_y_smr_t2_aseg.csv
# 
# Update Date: 2023.12.7
# =============================================================================#
# 1.  ---------------------------------------------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_15.txt'
DK_FileNames = c(
  "mri_y_smr_vol_aseg.csv",
  "mri_y_smr_t1_aseg.csv",
  "mri_y_smr_t2_aseg.csv"
)

SubfolderName = "imaging"

# s_sink(fullfile(AutoLogFolder,AutoLogFileName))