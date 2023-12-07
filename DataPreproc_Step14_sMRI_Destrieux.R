# =============================================================================#
# SDPP Step 14: read and select all sMRI-related variables in Destrieux
# (aparc.a2009s.annot) atlas
# 
# R Packages Dependency: bruceR, naniar
# Step File Notes: 
# https://wiki.abcdstudy.org/release-notes/imaging/overview.html
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: 
# (Destrieux Atlas)
#           /core/imaging/mri_y_smr_thk_dst.csv
#                         mri_y_smr_sulc_dst.csv
#                         mri_y_smr_area_dst.csv
#                         mri_y_smr_vol_dst.csv
#                         mri_y_smr_t1_contr_dst.csv
#                         mri_y_smr_t2_contr_dst.csv
# 
# Update Date: 2023.12.7
# =============================================================================#
# 1.  ---------------------------------------------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_14.txt'
DK_FileNames = c(
  "mri_y_smr_thk_dst.csv",
  "mri_y_smr_sulc_dst.csv",
  "mri_y_smr_area_dst.csv",
  "mri_y_smr_vol_dst.csv",
  "mri_y_smr_t1_contr_dst.csv",
  "mri_y_smr_t2_contr_dst.csv"
)

SubfolderName = "imaging"

# s_sink(fullfile(AutoLogFolder,AutoLogFileName))