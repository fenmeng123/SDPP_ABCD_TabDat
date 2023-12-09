# =============================================================================#
# SDPP Step 11: read and re-coding all youth-reported Culture and Environment
# variables
# R Packages Dependency: bruceR, naniar
# Step File Notes: 
# https://wiki.abcdstudy.org/release-notes/non-imaging/culture-environment.html
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: /core/culture-environment/ce_y_acc.csv
#                                     ce_y_macv.csv
#                                     ce_y_meim.csv
#                                     ce_y_via.csv
#                                     ce_y_nsc.csv
#                                     ce_y_dm.csv
#                                     ce_y_crpbi.csv
#                                     ce_y_mnbs.csv
#                                     ce_y_pm.csv
#                                     ce_y_fes.csv
#                                     ce_y_pet.csv
#                                     ce_y_pbp.csv
#                                     ce_y_pnh.csv
#                                     ce_y_rpi.csv
#                                     ce_y_sag.csv
#                                     ce_y_srpf.csv
#                                     ce_y_psb.csv
#                                     ce_y_wps.csv
# Update Date: 2023.11.25
# =============================================================================#
SDPP.Run.Step11 <- function(Prefix,
                           TabulatedDataDirectory,
                           ProjectDirectory,
                           AutoLogFolder,
                           ResultsOutputDir,
                           IntermediateDataDir,
                           SourceScriptName = s_get_script_name(),
                           ...){
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_11.txt'
DatTableNames = c("ce_y_acc.csv","ce_y_macv.csv",
                  "ce_y_meim.csv","ce_y_via.csv",
                  "ce_y_nsc.csv","ce_y_dm.csv",
                  "ce_y_crpbi.csv","ce_y_mnbs.csv",
                  "ce_y_pm.csv","ce_y_fes.csv",
                  "ce_y_pet.csv","ce_y_pbp.csv",
                  "ce_y_pnh.csv","ce_y_rpi.csv",
                  "ce_y_sag.csv","ce_y_srpf.csv",
                  "ce_y_psb.csv","ce_y_wps.csv")
SubfolderName = "culture-environment"

s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load task performance data during fMRI scanning ----------------------
data = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
                     TableNames = DatTableNames,
                     FolderName = SubfolderName)
NEW_data <- SDPP.select.cols.by.dict(data,
                                     TableNames = DatTableNames)

# 3. Re-coding and re-naming all selected variables -----------------------
NEW_data <- NEW_data %>%
  rename(
    # ACCULT
    ACCULT_ENG_LEVEL = accult_q1_y,
    ACCULT_OTHLANG_YN = accult_q2_y,
    ACCULT_COMLANG_Friend = accult_q4_y,
    ACCULT_COMLANG_Family = accult_q5_y,
    ACCULT_OTHLANG_LEVEL = accult_q3b,
    # MACV
    MACV_FS_Sum = macv_y_ss_fs,
    MACV_FO_Sum = macv_y_ss_fo,
    MACV_FR_Sum = macv_y_ss_fr,
    MACV_ISR_Sum = macv_y_ss_isr,
    MACV_R_Sum = macv_y_ss_r,
    MACV_Vald_Comprehend = macv_comprehend,
    # MEIM
    MEIM_Ethic_ID = meim_ethnic_id,
    MEIM_EXP_Sum = meim_ss_exp,
    MEIM_COM_Sum = meim_ss_com,
    MEIM_TOT_Sum = meim_ss_total,
    # VIA
    VIA_HeritCU_ID = vancouver_q1_dd,
    VIA_HeritCU_Sum = via_ss_hc,
    VIA_AmeriMS_Sum = via_ss_amer,
    # NSC
    NSC_Crime = neighborhood_crime_y,
    # DM
    DM_Sum = dim_y_ss_mean,
    # PBI
    PBI_Prima_Caregiver_ID = crpbi_studycaregiver_id,
    PBI_Secon_Caregiver_ID = crpbi_caregiver2_y,
    PBI_ACCPT_Parent_Sum = crpbi_y_ss_parent,
    PBI_ACCPT_Caregiver_Sum = crpbi_y_ss_caregiver,
    # MNBS
    MNBS_MoSup_Sum = mnbs_ss_monitor_supervision,
    MNBS_Total_Sum = mnbs_ss_mean_all,
    MNBS_EduSu_Sum = mnbs_ss_ed_support,
    # PM
    PM_Sum = pmq_y_ss_mean,
    # FES
    FES_Conflict_Sum = fes_y_ss_fc,
    FES_Conflict_Pro = fes_y_ss_fc_pr,
    # PET
    PET_OS = pet_identify___0,
    PET_1 = pet_identify___1,
    PET_2 = pet_identify___2,
    PET_3 = pet_identify___3,
    PET_4 = pet_identify___4,
    PET_5 = pet_identify___5,
    PET_6 = pet_identify___6,
    # PBP
    PBP_Prosocial_Sum = pbp_ss_prosocial_peers,
    PBP_RuleBreak_Sum = pbp_ss_rule_break,
    # PNH
    PNH_Protective_Sum = pnh_ss_protective_scale,
    # RPI
    RPI_Sum = peerinfluence_ss_mean,
    # SAG
    SAG_Skips = sag_days_skip_school,
    SAG_Grade = sag_grades_last_yr,
    # SRPF
    SRPF_Environment_Sum = srpf_y_ss_ses,
    SRPF_Involvement_Sum = srpf_y_ss_iiss,
    SRPF_Disengagement_Sum = srpf_y_ss_dfs,
    # PSB
    PSB_Sum = psb_y_ss_mean,
    # WPS
    WPS_Sum = wps_ss_sum
  ) %>%
  unite('PET_ID',matches('PET_\\d'),na.rm = T,sep = '')
# Re-code pet ownership
NEW_data$PET_ID <- NEW_data$PET_ID %>%
  RECODE("'100000' = 'Dog';
          '010000' = 'Cat';
          '001000' = 'Horse';
          '000100' = 'Fish';
          '000010' = 'Other Small Animal';
          c('000001','000000') = 'Other';
          '' = NA;
          else = 'Multiple';")
NEW_data$PET_ID[NEW_data$PET_OS == 0] <- 'Do not have pets'


# 4. Save re-coded data ---------------------------------------------------

NEW_data[sort(colnames(NEW_data))] %>%
  select(c(src_subject_id,eventname,
           everything())) -> NEW_CE_Y
SDPP.save.file(NEW_CE_Y,
               FileName = "CE_Y_Rec.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

NEW_CE_Y %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_CE_Y_Rec.doc'),
              row.names = F)

select(NEW_CE_Y,-c(src_subject_id,eventname)) %>% 
  psych::describeBy(group = Recode.Eventname(NEW_CE_Y)$eventname,
                    mat = T,digits =2) %>%
  print_table(file = fullfile(ResultsOutputDir,'VSO_ALL_CE_Y.doc'),
              row.names = T,
              digits = 2)

# End of Script -----------------------------------------------------------

s_close_sink()


}