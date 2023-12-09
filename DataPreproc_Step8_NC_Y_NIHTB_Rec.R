# =============================================================================#
# SDPP Step 8: read and re-coding the NIH toolbox scores
# R Packages Dependency: bruceR, naniar, readxl
# Step File Notes: 
# 
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: /core/neurocognition/nc_y_nihtb.csv
# Update Date: 2023.7.18
# =============================================================================#
SDPP.Run.Step8 <- function(Prefix,
                           TabulatedDataDirectory,
                           ProjectDirectory,
                           AutoLogFolder,
                           ResultsOutputDir,
                           IntermediateDataDir,
                           SourceScriptName = s_get_script_name(),
                           ...){
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_8.txt'
s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load NIHTB and its composite scores data ---------------------------------
nihtb_FileDir = fullfile(TabulatedDataDirectory,'/neurocognition/nc_y_nihtb.csv')
NIHTB = readABCDdata(nihtb_FileDir)
fprintf("NIHTB Data type:\n")
sapply(NIHTB,typeof)
NIHTB %>% MVA.Report.By.Wave() %>% 
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_NC_NIHTB_Raw.doc'),
              row.names = F,
              digits = 1)

# 3. Filter columns and re-name it ----------------------------------------
# Only Uncorrected Standard Score will be retained.
fprintf("Only Uncorrected Standard NIHTB Score will be retained in preprocessed data, all variable names are showed as follows:\n")
NIHTB %>% select(c(src_subject_id,
                   eventname,
                   nihtbx_picvocab_uncorrected,
                   nihtbx_flanker_uncorrected,
                   nihtbx_list_uncorrected,
                   nihtbx_cardsort_uncorrected,
                   nihtbx_pattern_uncorrected,
                   nihtbx_picture_uncorrected,
                   nihtbx_reading_uncorrected,
                   nihtbx_fluidcomp_uncorrected,
                   nihtbx_cryst_uncorrected,
                   nihtbx_totalcomp_uncorrected)) %>%
  rename(NIHTB_PVT = nihtbx_picvocab_uncorrected,
         NIHTB_FICAT = nihtbx_flanker_uncorrected,
         NIHTB_PSMT = nihtbx_picture_uncorrected,
         NIHTB_DCCST = nihtbx_cardsort_uncorrected,
         NIHTB_PCST = nihtbx_pattern_uncorrected,
         NIHTB_ORRT = nihtbx_reading_uncorrected,
         NIHTB_LSWMT = nihtbx_list_uncorrected,
         NIHTB_ICS_Cryst = nihtbx_cryst_uncorrected,
         NIHTB_ICS_Fluid = nihtbx_fluidcomp_uncorrected,
         NIHTB_ICS_Total = nihtbx_totalcomp_uncorrected,
         ) -> NEW_NIHTB
print(colnames(NEW_NIHTB))
fprintf("Here, SDPP-ABCD-TabDat doesn't indicate ;Uncorrected Standard Score; in variable names, which should be noted for users.\n")
fprintf("For abbreviation reference table, please see the section 'The NIH Cognition Toolbox' in user manual.\n")

fprintf('NIHTB Sum Scores - Brief Overview:\n')
select(NEW_NIHTB,-c(src_subject_id,eventname)) %>% 
  psych::describeBy(group = Recode.Eventname(NEW_NIHTB)$eventname,
                    mat = T,digits =2) %>%
  print_table(file = fullfile(ResultsOutputDir,'VSO_ALL_NC_NIHTB.doc'),
              row.names = T,
              digits = 1,
              digits = 2)

# 5. Save Re-coded NIHTB Sum Scores data ----------------------------------------------

SDPP.save.file(NEW_NIHTB,
               FileName = "NC_NIHTB.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

NEW_NIHTB %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_NC_NIHTB_Rec.doc'),
              row.names = F,
              digits = 1)

# End of Script -----------------------------------------------------------

s_close_sink()
}