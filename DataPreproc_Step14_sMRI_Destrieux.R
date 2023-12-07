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
# 1. Destrieux Settings ------------------------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_14.txt'
DST_FileNames = c(
  "mri_y_smr_thk_dst.csv",
  "mri_y_smr_sulc_dst.csv",
  "mri_y_smr_area_dst.csv",
  "mri_y_smr_vol_dst.csv",
  "mri_y_smr_t1_contr_dst.csv",
  "mri_y_smr_t2_contr_dst.csv"
)

SubfolderName = "imaging"
AtlasName = "Destrieux"

# s_sink(fullfile(AutoLogFolder,AutoLogFileName))
# library(naniar)

# 2. Export labels of Destrieux Atlas from ggsegExtra ---------------------
if (requireNamespace("ggsegDesterieux", quietly = T)){
  ggsegDesterieux::desterieux %>%
    as.data.frame() %>%
    na.omit() %>%
    select(
      c(hemi,side,region,label)
    ) %>%
    export(file = './.github/SDPP_ggseg_AtlasLUT.xlsx',
           sheet = 'Destrieux',
           verbose = T)
}else{
  fprintf("ggsegDesterieux has not been installed! Using the default ggseg_AtlasLUT file from SDPP instead.\n")
}

# 3. Extract Destrieux Variables from data dictionary ---------------------

DST_TableName <- DST_FileNames %>%
  str_remove_all('\\.csv')

sMRI_DST_VarList <- SDPP.filter.data.dict(filter_col = 'table_name',
                                         filter_key = DST_TableName,
                                         search_col = c('var_name','var_label'))
# End of Script -----------------------------------------------------------

s_close_sink()
