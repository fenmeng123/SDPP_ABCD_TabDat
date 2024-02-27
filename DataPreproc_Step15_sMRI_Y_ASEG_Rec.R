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
SDPP.Run.Step15 <- function(Prefix,
                           TabulatedDataDirectory,
                           ProjectDirectory,
                           AutoLogFolder,
                           ResultsOutputDir,
                           IntermediateDataDir,
                           SourceScriptName = s_get_script_name(),
                           ...){
# 1. ASEG Settings -------------------------------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_15.txt'
ASEG_FileNames = c(
  "mri_y_smr_vol_aseg.csv",
  "mri_y_smr_t1_aseg.csv",
  "mri_y_smr_t2_aseg.csv"
)

SubfolderName = "imaging"
AtlasName = "ASEG"

library(naniar)
library(ggseg)
s_sink(fullfile(AutoLogFolder,AutoLogFileName))

# 2. Export labels of ASEG & ASEG Extra Atlas from ggsegExtra ------------

if (requireNamespace("ggsegDefaultExtra", quietly = T)) {
  ggsegDefaultExtra::hcpa_3d$ggseg_3d[[1]][,c('region','label','roi')] %>%
    as.data.frame() %>%
    na.omit() %>%
    select(
      c(region,label)
    ) %>%
    export(file = './.github/SDPP_ggseg_AtlasLUT.xlsx',
           sheet = 'ASEG_Extra',
           verbose = T)
}else{
  fprintf("ggsegDefaultExtra has not been installed! Using the default ggseg_AtlasLUT file from SDPP instead.\n")
}
if (requireNamespace("ggseg", quietly = T)) {
  ggseg::aseg$data %>%
    as.data.frame() %>%
    na.omit() %>%
    select(
      c(hemi,side,region,label)
    ) %>%
    export(file = './.github/SDPP_ggseg_AtlasLUT.xlsx',
           sheet = 'ASEG',
           verbose = T)
}else{
  fprintf("ggseg has not been installed! Using the default ggseg_AtlasLUT file from SDPP instead.\n")
}

# 3. Load ASEG data -------------------------------------------------------

ASEG_TableName <- ASEG_FileNames %>%
  str_remove_all('\\.csv')

sMRI_ASEG_VarList <- SDPP.filter.data.dict(filter_col = 'table_name',
                                          filter_key = ASEG_TableName,
                                          search_col = c('var_name','var_label'))

# End of Script -----------------------------------------------------------

s_close_sink()
}