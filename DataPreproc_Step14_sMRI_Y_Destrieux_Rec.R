# =============================================================================#
# SDPP Step 14: read and select all sMRI-related variables in Destrieux
# (aparc.a2009s.annot) atlas
# 
# R Packages Dependency: bruceR, naniar
# Step File Notes: 
# https://wiki.abcdstudy.org/release-notes/imaging/overview.html
# Target File: 
# ABCD 4.0: 
# ABCD 5.0: 
# (Destrieux Atlas)
#           /core/imaging/mri_y_smr_thk_dst.csv
#                         mri_y_smr_sulc_dst.csv
#                         mri_y_smr_area_dst.csv
#                         mri_y_smr_vol_dst.csv
#                         mri_y_smr_t1_contr_dst.csv
#                         mri_y_smr_t1_gray_dst.csv
#                         mri_y_smr_t1_white_dst.csv
#                         mri_y_smr_t2_contr_dst.csv
#                         mri_y_smr_t2_gray_dst.csv
#                         mri_y_smr_t2_white_dst.csv
# 
# Update Date: 2023.12.8
# =============================================================================#
# 1. Destrieux Settings ------------------------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_14.txt'
DST_FileNames = c(
  "mri_y_smr_thk_dst.csv",
  "mri_y_smr_sulc_dst.csv",
  "mri_y_smr_area_dst.csv",
  "mri_y_smr_vol_dst.csv",
  "mri_y_smr_t1_contr_dst.csv",
  "mri_y_smr_t1_gray_dst.csv",
  "mri_y_smr_t1_white_dst.csv",
  "mri_y_smr_t2_contr_dst.csv",
  "mri_y_smr_t2_gray_dst.csv",
  "mri_y_smr_t2_white_dst.csv"
)

SubfolderName = "imaging"
AtlasName = "Destrieux"

s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(naniar)

# 2. Export labels of Destrieux Atlas from ggsegExtra ---------------------
if (requireNamespace("ggsegDesterieux", quietly = T)){
  ggsegDesterieux::desterieux$data[,c('hemi','side','region','label','roi')] %>%
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

sMRI_DST_VarList$rawvarlabel <- sMRI_DST_VarList$var_label %>%
  str_replace_all(
    pattern = "( weighted image for )|( weighted imaged for )|( cortical Destrieux ROI )",
    replacement = '-') %>%
  str_replace_all(
    pattern = "(Average T2 intensity of white matter voxels 0.2 mm from the white matter surface for )",
    replacement = "T2White-"
  ) %>%
  str_replace_all(
    pattern = "(Average T1 intensity of white matter voxels 0.2 mm from the white matter surface for )",
    replacement = "T1White-"
  )%>%
  str_replace_all(
    pattern = "(Average T2 intensity of gray matter voxels 0.2 mm from the white matter surface for )",
    replacement = "T2Gray-"
  ) %>%
  str_replace_all(
    pattern = "(Average T1 intensity of gray matter voxels 0.2 mm from the white matter surface for )",
    replacement = "T1Gray-"
  ) %>%
  str_replace_all(
    pattern = "(\\(CortContrast-White - CortContrast-Gray\\) \\/ \\(\\(CortContrast-White plus CortContrast-Gray\\)\\/2\\) of T2)",
    replacement = "T2Contr"
  ) %>%
  str_replace_all(
    pattern = "(\\(CortContrast-White - CortContrast-Gray\\) \\/ \\(\\(CortContrast-White plus CortContrast-Gray\\)\\/2\\) of T1)",
    replacement = "T1Contr"
  ) %>%
  str_replace_all(
    pattern = "Average T2 intensity of white matter voxels 0.2 mm from the white matter surface for",
    replacement = 'T2White-'
  ) %>%
  str_replace_all(
    pattern = "Average T2 intensity of gray matter voxels 0.2 mm from the white matter surface for",
    replacement = 'T2Gray-'
  ) %>%
  str_replace_all(
    pattern = "Average T1 intensity of gray matter voxels 0.2 mm from the white matter surface for",
    replacement = 'T1Gray-'
  ) %>%
  str_replace_all(
    pattern = "Average T1 intensity of white matter voxels 0.2 mm from the white matter surface for",
    replacement = 'T1White-'
  ) %>%
  str_replace_all(
    pattern = "(Cortical thickness in mm for)",
    replacement = "THICK-"
  ) %>%
  str_replace_all(
    pattern = "(Cortical volume in mm\\^3 for)",
    replacement = 'VOL-'
  ) %>%
  str_replace_all(
    pattern = "(Cortical area in mm\\^2 for)",
    replacement = "AREA-"
  ) %>%
  str_replace_all(
    pattern = "(Sulcal depth in mm for)",
    replacement = "SULC-"
  ) %>%
  str_replace_all(
    pattern = "(- )|(--)|(-cortical Destrieux ROI )",
    replacement = "-"
  )
sMRI_DST_VarList <- sMRI_DST_VarList$rawvarlabel %>%
  str_split_fixed(pattern = "-",n = 3) %>%
  as.data.frame() %>%
  rename(Type = V1,
         Side = V2,
         ROIname = V3) %>%
  cbind(.,sMRI_DST_VarList) %>%
  rename(rawvarname = var_name) %>%
  select(-var_label)
# Add annatotations for whole brain measures
tmpRowFlag <- which(sMRI_DST_VarList$ROIname == "")
sMRI_DST_VarList$ROIname[tmpRowFlag] <- sMRI_DST_VarList$Side[tmpRowFlag]
sMRI_DST_VarList$Side[tmpRowFlag] <- "whole brain"

# 4. Update Destrieux LUT and re-naming columns ---------------------------
# Because the Destrieux-derivatives were named in form of "mrisdp_[number]".
# We need to re-name all columns to have a more user-friendly nomenclature.
LUT <- import(
  file = "./.github/SDPP_MRI_ROI_LookUpTable.xlsx",
  sheet = AtlasName
)
# Update look up table
if (any(is.na(LUT$`SDPP ROI abbr`))){
  LUT$`SDPP ROI abbr` <- LUT$`ABCD ROI Name` %>%
    str_remove_all(
      pattern = "cort.destrieux_"
    ) %>%
    str_replace_all(
      pattern = "\\.",
      replacement = "_"
    ) %>%
    str_replace_all(
      pattern = "(mean_)",
      replacement = "mean"
    ) %>%
    str_replace_all(
      pattern = "total_",
      replacement = "total"
    )
  export(LUT,
         file = "./.github/SDPP_MRI_ROI_LookUpTable.xlsx",
         sheet = AtlasName,
         verbose = T
  )
}else{
  fprintf("For [%s] sheet in LUT file, all cells in the 'SDPP ROI abbr' already have values.\n",
          AtlasName)
  fprintf("See the top five rows in below:\n")
  head(LUT) %>%
    knitr::kable(format = 'simple') %>%
    print()
}
sMRI_DST_VarList$newvarname <- NA
# Using the updated LUT to re-name all columns in tabulated data
LUT_ROIname <- LUT$Description %>%
  str_remove_all(
    pattern = " \\(.*\\)"
    )
for (irow in 1:nrow(sMRI_DST_VarList)){
  tmpRowFlag = which(LUT_ROIname == sMRI_DST_VarList$ROIname[irow])
  fprintf("|#Iter %d| ",irow)
  if (length(tmpRowFlag) > 2){
    fprintf("Detected multiple matched-ROI description in LUT, label is [%s]\n",
            sMRI_DST_VarList$rawvarlabel[irow])
    fprintf("\tUsing string from |ROIname| [%s] and |Side| [%s]\n",
            sMRI_DST_VarList$ROIname[irow],sMRI_DST_VarList$Side[irow])
    sMRI_DST_VarList$newvarname[irow] <- str_c(
      sMRI_DST_VarList$Type[irow],
      sMRI_DST_VarList$ROIname[irow],
      sep = '_'
    ) %>%
      str_c(
        .,
        str_replace_all(sMRI_DST_VarList$Side[irow],
                    pattern = c("left hemisphere" = 'lh',
                                "right hemisphere" = 'rh',
                                "whole brain" = ''))
      )
    fprintf("\tRe-name column [%s] to [%s]\n",
            sMRI_DST_VarList$rawvarname[irow],sMRI_DST_VarList$newvarname[irow])
  }else if(length(tmpRowFlag) == 2){
    tmp_LUT_ROIname <- unique(LUT_ROIname[tmpRowFlag])
    tmp_SideAbbr <- str_replace_all(sMRI_DST_VarList$Side[irow],
                                    pattern = c("left hemisphere" = 'lh',
                                                "right hemisphere" = 'rh'))
    if (length(tmp_LUT_ROIname) == 1){
      fprintf("Detected Unique matched-ROI description in LUT: [%s]\n",tmp_LUT_ROIname)
      fprintf("\t |Side| At [%s], abbr is [%s].\n",
              sMRI_DST_VarList$Side[irow],tmp_SideAbbr)
      tmpnewvarname <- str_match(LUT$`SDPP ROI abbr`[tmpRowFlag],
                  pattern = sprintf(".*\\_%s$",tmp_SideAbbr)) %>%
        na.omit() %>%
        as.character()
                      
      sMRI_DST_VarList$newvarname[irow] <- str_c(
        sMRI_DST_VarList$Type[irow],
        tmpnewvarname,
        sep = '_'
      )
      fprintf("\tRe-name column [%s](label: %s)\n\t\t\tto [%s]\n",
              sMRI_DST_VarList$rawvarname[irow],
              sMRI_DST_VarList$rawvarlabel[irow],
              sMRI_DST_VarList$newvarname[irow])
    }else{
      fprintf("|Something wrong| with [%s]. Skip re-naming step!",tmp_LUT_ROIname)
      warnings("Mutiple ROInames were detected after accounting for bilteral!")
    }
  }
}
export(sMRI_DST_VarList,
       file = './.github/SDPP_sMRI_VarNameMapping.xlsx',
       sheet = AtlasName,
       verbose = T)

# 5. Load sMRI-derivatives data of Destrieux Atlas and perform renaming----

data = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
                     TableNames = DST_FileNames,
                     FolderName = SubfolderName)
NEW_data <- SDPP.select.cols.by.dict(data,
                                     TableNames = DST_FileNames)

Vec_ColNames <- colnames(NEW_data)
for (iIndex in 1:nrow(sMRI_DST_VarList)){
  tmpColFlag <- which(Vec_ColNames == sMRI_DST_VarList$rawvarname[iIndex])
  fprintf("\t|Column No. %d| Replace %s \twith [%s]\n",
          tmpColFlag,Vec_ColNames[tmpColFlag],sMRI_DST_VarList$newvarname[iIndex])
  Vec_ColNames[tmpColFlag] <- sMRI_DST_VarList$newvarname[iIndex]
}
colnames(NEW_data) <- Vec_ColNames
fprintf("Finished Variable Re-naming for sMRI Measures from Destrieux Atlas!\n")

# 6. Save re-named data ---------------------------------------------------

SDPP.StdOut.MVA.VSO.Files(NEW_data,
                          FileLabel = 'sMRI_Destrieux',
                          Prefix = Prefix,
                          ProjectDirectory = ProjectDirectory,
                          ResultsOutputDir = ResultsOutputDir)

# End of Script -----------------------------------------------------------

s_close_sink()
