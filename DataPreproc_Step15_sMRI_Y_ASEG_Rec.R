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
if (requireNamespace("ggseg", quietly = T)) {
  ggseg3d::aseg_3d$ggseg_3d[[1]] %>%
    as.data.frame() %>%
    na.omit() %>%
    select(c(region,label)) %>%
    export(file = './.github/SDPP_ggseg_AtlasLUT.xlsx',
           sheet = 'ASEG_3D',
           verbose = T)
} else{
  fprintf("ggseg3d has not been installed! Using the default ggseg_AtlasLUT file from SDPP instead.\n")
}



# 3. Load ASEG data -------------------------------------------------------

sMRI_ASEG_VarList <- SDPP.filter.data.dict(filter_col = 'table_name',
                                          filter_key = str_remove_all(ASEG_FileNames,pattern = '\\.csv'),
                                          search_col = c('var_name','var_label'))

sMRI_ASEG_VarList$rawvarlabel <- sMRI_ASEG_VarList$var_label %>%
  str_replace_all(
    pattern = c("Average intensity of the normalized T2-weighted image within ASEG ROI " = 'T2Gray-',
                "Average intensity of the normalized T1-weighted image within ASEG ROI" = 'T1Gray-',
                "Volume in mm\\^3 of ASEG ROI " = 'VOL-',
                "CC-" = 'CC_',
                "Mid-" = 'Mid_',
                "-Ventricle" = '_Ventricle',
                "WM-" = 'WM_',
                "Brain-Stem" = 'BrainStem',
                "(- )|(--)|(-ASEG ROI )" = "-")) 

sMRI_ASEG_VarList <- sMRI_ASEG_VarList$rawvarlabel %>%
  str_split_fixed(pattern = "-",n = 3) %>%
  as.data.frame() %>%
  rename(Type = V1,
         Side = V2,
         ROIname = V3) %>%
  cbind(.,sMRI_ASEG_VarList) %>%
  rename(rawvarname = var_name) %>%
  select(-var_label)

# Add annotations for whole brain measures
tmpRowFlag <- which(sMRI_ASEG_VarList$ROIname == "")
sMRI_ASEG_VarList$ROIname[tmpRowFlag] <- sMRI_ASEG_VarList$Side[tmpRowFlag]
sMRI_ASEG_VarList$Side[tmpRowFlag] <- "whole brain"


# 4. Load ASEG LUT and re-naming columns ---------------------------
LUT <- import(
  file = "./.github/SDPP_MRI_ROI_LookUpTable.xlsx",
  sheet = AtlasName
) %>%
  rename(key = `ABCD ROI name`)

sMRI_ASEG_VarList <- sMRI_ASEG_VarList %>%
  mutate(newvarname = NA,
         key = str_remove_all(rawvarname,"(smri_t1w_)|(smri_t2w_)|(smri_vol_)"))

# Using the ASEG LUT to re-name all columns in tabulated data
sMRI_ASEG_VarList <- merge(sMRI_ASEG_VarList,LUT,
                           by = 'key',
                           all.x = T) %>%
  mutate(newvarname = paste0(Type,'_',`SDPP ROI abbr`))


# 5. Load sMRI-derivatives data of ASEG Atlas and perform renaming ----

data = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
                     TableNames = ASEG_FileNames,
                     FolderName = SubfolderName)
NEW_data <- SDPP.select.cols.by.dict(data,
                                     TableNames = ASEG_FileNames)
Vec_ColNames <- colnames(NEW_data)

for (iIndex in 1:nrow(sMRI_ASEG_VarList)){
  tmpColFlag <- which(Vec_ColNames == sMRI_ASEG_VarList$rawvarname[iIndex])
  fprintf("\t|Column No. %d| Replace %s \twith [%s]\n",
          tmpColFlag,Vec_ColNames[tmpColFlag],sMRI_ASEG_VarList$newvarname[iIndex])
  Vec_ColNames[tmpColFlag] <- sMRI_ASEG_VarList$newvarname[iIndex]
}
colnames(NEW_data) <- Vec_ColNames
fprintf("Finished Variable Re-naming for sMRI Measures from ASEG Atlas!\n")
# 6. Save re-named data ---------------------------------------------------

SDPP.StdOut.MVA.VSO.Files(NEW_data,
                          FileLabel = 'sMRI_ASEG',
                          Prefix = Prefix,
                          ProjectDirectory = ProjectDirectory,
                          ResultsOutputDir = ResultsOutputDir)

# End of Script -----------------------------------------------------------

s_close_sink()
}