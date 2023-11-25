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
    MEIN_TOT_Sum = meim_ss_total,
    # VIA
    VIA_HeritCU_ID = vancouver_q1_dd,
    VIA_HeritCU_Sum = via_ss_hc,
    VIA_AmeriMS_Sum = via_ss_amer,
    # NSC
    NSC_Crime = neighborhood_crime_y,
    # DM
    DM_Sum = dim_y_ss_mean,
    
  )





# End of Script -----------------------------------------------------------

s_close_sink()


