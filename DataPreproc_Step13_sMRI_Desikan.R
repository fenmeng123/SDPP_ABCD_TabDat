# =============================================================================#
# SDPP Step 13: read and select all sMRI-related variables in Desikan (aparc.annot)
# 
# R Packages Dependency: bruceR, naniar
# Step File Notes: 
# https://wiki.abcdstudy.org/release-notes/imaging/overview.html
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: 
# (Desikan-Killiany Atlas)
#           /core/imaging/mri_y_smr_thk_dsk.csv
#                         mri_y_smr_sulc_dsk.csv
#                         mri_y_smr_area_dsk.csv
#                         mri_y_smr_vol_dsk.csv
#                         mri_y_smr_t1_contr_dsk.csv
#                         mri_y_smr_t2_contr_dsk.csv
#                         mri_y_smr_t1_gray_dsk.csv
#                         mri_y_smr_t1_white_dsk.csv
#                         mri_y_smr_t2_gray_dsk.csv
#                         mri_y_smr_t2_white_dsk.csv
# 
# Update Date: 2023.12.7
# =============================================================================#
# 1.  ---------------------------------------------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_13.txt'
DK_FileNames = c(
  "mri_y_smr_thk_dsk.csv",
  "mri_y_smr_sulc_dsk.csv",
  "mri_y_smr_area_dsk.csv",
  "mri_y_smr_vol_dsk.csv",
  "mri_y_smr_t1_contr_dsk.csv",
  "mri_y_smr_t2_contr_dsk.csv",
  "mri_y_smr_t1_gray_dsk.csv",
  "mri_y_smr_t1_white_dsk.csv",
  "mri_y_smr_t2_gray_dsk.csv",
  "mri_y_smr_t2_white_dsk.csv"
  )

SubfolderName = "imaging"

s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(naniar)

# ==============================MAIN CODES=====================================#
# 2. Extract and Update Desikan-Killiany Atlas Variable Names -------------

# Export the data frame of Desikan-Killiany Atlas from ggseg mapping
ggseg::dk$data %>%
  as.data.frame() %>%
  na.omit() %>%
  select(
    c(hemi,side,region,label)
  ) %>%
  export(file = './.github/SDPP_ggseg_AtlasLUT.xlsx',
         sheet = 'Desikan',
         verbose = T)
# Load Desikan-Killinay Nomenclature from dictionary
DK_TableName <- DK_FileNames %>%
  str_remove_all('\\.csv')

sMRI_DK_VarList <- SDPP.filter.data.dict(filter_col = 'table_name',
                      filter_key = DK_TableName,
                      search_col = c('var_name','var_label'))

sMRI_DK_VarName <- sMRI_DK_VarList$var_name %>%
  str_split_fixed(pattern = '_',n = Inf) %>%
  as.data.frame() %>%
  cbind(.,sMRI_DK_VarList) %>%
  cbind(.,str_extract_all(sMRI_DK_VarList$var_label,
          pattern = '(APARC ROI .*)|(left hemisphere)|(right hemisphere)|(whole brain)',
          simplify = T) %>%
          str_remove_all('APARC ROI ') %>%
          str_squish()
        )

colnames(sMRI_DK_VarName) <- c('rawvarname_p1',
                                'rawvarname_p2',
                                'rawvarname_p3',
                                'rawvarname_p4',
                                'rawvarname',
                                'rawlabel',
                                'ROIname')

# Standardize the naming of ROI-level variables
sMRI_DK_VarName$ROIname <- sMRI_DK_VarName$ROIname %>%
  RECODE("'lh-Banks of Superior Temporal Sulcus' = 'lh-bankssts';
          'rh-Banks of Superior Temporal Sulcus' = 'rh-bankssts';") %>%
  str_replace_all(pattern = '-',replacement = '_')
# Call the user-defined function to perform standardizing
sMRI_DK_VarName <- Standardize.MRI.Atlas.Name(sMRI_DK_VarName)
# Standardize the naming of sMRI-level variables
sMRI_DK_VarName$VarType <- sMRI_DK_VarName$rawvarname_p2 %>%
  RECODE("'thick' = 'THICK';
            'sulc' = 'SULC';
            'area' = 'AREA';
            'vol' = 'VOL';
            't1wcnt' = 'T1Contr';
            't1wgray02' = 'T1Gray'; 
            't1ww02' = 'T1White';
            't2wcnt' = 'T2Contr';
            't2wg02' = 'T2Gray';
            't2ww02' = 'T2White';")
# Create new variable names in form of SDPP sMRI nomenclature
sMRI_DK_VarName$newvarname <- str_c(sMRI_DK_VarName$VarType,
                                    sMRI_DK_VarName$SDPP_ROIname,
                                    sep = '_')
# Add the SDPP nomenclature to MRI look-up table 
LUT <- import(file = './.github/SDPP_MRI_ROI_LookUpTable.xlsx',
              sheet = 'Desikan',
              verbose = T)

# Update look up table
LUT <- Update.LUT.Content(LUT)
LUT %>%
  export(file = './.github/SDPP_MRI_ROI_LookUpTable.xlsx',
         sheet = 'Desikan',
         verbose = T)

# 3. Load and re-naming all Desikan-Killiany Variables --------------------

data = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
                     TableNames = DK_FileNames,
                     FolderName = SubfolderName)
NEW_data <- SDPP.select.cols.by.dict(data,
                                     TableNames = DK_FileNames)

Vec_ColNames <- colnames(NEW_data)
for (iIndex in 1:nrow(sMRI_DK_VarName)){
 tmpColFlag <- which(Vec_ColNames == sMRI_DK_VarName$rawvarname[iIndex])
 fprintf("\t|Column No. %d| Replace %s \twith [%s]\n",
         tmpColFlag,Vec_ColNames[tmpColFlag],sMRI_DK_VarName$newvarname[iIndex])
 Vec_ColNames[tmpColFlag] <- sMRI_DK_VarName$newvarname[iIndex]
}
colnames(NEW_data) <- Vec_ColNames
fprintf("Finished Variable Re-naming for sMRI Measures from Desikan-Killiany Atlas!\n")
# 4. Save re-coded data ---------------------------------------------------

SDPP.StdOut.IntermediateData.Files(NEW_data,
                                   FileLabel = 'sMRI_Desikan',
                                   Prefix = Prefix,
                                   ProjectDirectory = ProjectDirectory,
                                   ResultsOutputDir = ResultsOutputDir)
# End of Script -----------------------------------------------------------

s_close_sink()


