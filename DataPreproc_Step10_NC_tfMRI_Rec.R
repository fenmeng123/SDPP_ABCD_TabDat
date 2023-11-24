# =============================================================================#
# SDPP Step 10: read and re-coding all behavioral tasks performance in task-based
# fMRI recording (i.e. Monetary Incentive Delay, Stop Signal, and Emotion-Nback Tasks)
# R Packages Dependency: bruceR, naniar, readxl
# Step File Notes: 
# https://wiki.abcdstudy.org/release-notes/imaging/task-fmri-behavior.html
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: /core/imaging/mri_y_tfmr_mid_beh.csv
#                         mri_y_tfmr_mid_qtn.csv
#                         mri_y_tfmr_sst_beh.csv
#                         mri_y_tfmr_nback_beh.csv
#                         mri_y_tfmr_nback_rec_beh.csv
# Update Date: 2023.8.7
# Update Date: 2023.11.24
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_10.txt'
DatTableNames = c("mri_y_tfmr_mid_beh.csv","mri_y_tfmr_mid_qtn.csv",
                  "mri_y_tfmr_sst_beh.csv","mri_y_tfmr_nback_beh.csv",
                  "mri_y_tfmr_nback_rec_beh.csv")
SubfolderName = "imaging"

s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load task performance data during fMRI scanning ----------------------
data = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
                     TableNames = DatTableNames,
                     FolderName = SubfolderName)
NEW_data <- SDPP.select.cols.by.dict(data,
                                     TableNames = DatTableNames)
# 3. Process MID-PQ data, especially for re-scan items --------------------
# Combine the re-scan and initial scan MIDPQ data
NEW_data <- NEW_data %>%
  unite('MIDPQ_EXC_LRW',
        c(midq_exc_1a_rescan_y,midq_exc_1a_y),
        na.rm = T) %>%
  unite('MIDPQ_EXC_SRW',
        c(midq_exc_1b_rescan_y,midq_exc_1b_y),
        na.rm = T) %>%
  unite('MIDPQ_EXC_LLO',
        c(midq_exc_1c_rescan_y,midq_exc_1c_y),
        na.rm = T) %>%
  unite('MIDPQ_EXC_SLO',
        c(midq_exc_1d_rescan_y,midq_exc_1d_y),
        na.rm = T) %>%
  unite('MIDPQ_EXC_NEU',
        c(midq_exc_1e_rescan_y,midq_exc_1e_y),
        na.rm = T) %>%
  unite('MIDPQ_NERV_LRW',
        c(midq_nerv_1a_rescan_y,midq_nerv_1a_y),
        na.rm = T) %>%
  unite('MIDPQ_NERV_SRW',
        c(midq_nerv_1b_rescan_y,midq_nerv_1b_y),
        na.rm = T) %>%
  unite('MIDPQ_NERV_LLO',
        c(midq_nerv_1c_rescan_y,midq_nerv_1c_y),
        na.rm = T) %>%
  unite('MIDPQ_NERV_SLO',
        c(midq_nerv_1d_rescan_y,midq_nerv_1d_y),
        na.rm = T) %>%
  unite('MIDPQ_NERV_NEU',
        c(midq_nerv_1e_rescan_y,midq_nerv_1e_y),
        na.rm = T)  %>%
  unite('MIDPQ_TRY_LRW',
        c(midq_try_1a_rescan_y,midq_try_1a_y),
        na.rm = T) %>%
  unite('MIDPQ_TRY_SRW',
        c(midq_try_1b_rescan_y,midq_try_1b_y),
        na.rm = T) %>%
  unite('MIDPQ_TRY_LLO',
        c(midq_try_1c_rescan_y,midq_try_1c_y),
        na.rm = T) %>%
  unite('MIDPQ_TRY_SLO',
        c(midq_try_1d_rescan_y,midq_try_1d_y),
        na.rm = T) %>%
  unite('MIDPQ_TRY_NEU',
        c(midq_try_1e_rescan_y,midq_try_1e_y),
        na.rm = T) %>%
  unite('MIDPQ_THINK_YN',
        c(midq_2_rescan_y,midq_2),
        na.rm = T)
# select all MIDPQ columns
tmp_MIDPQ <- NEW_data %>%
  select(starts_with('MIDPQ_'))
# find duplicated data, individuals have two values in MIDPQ because re-scan was
# actually administrated
Duplicate_Index <- tmp_MIDPQ %>%
  sapply(function(x) str_detect(x,'_')) %>%
  which(arr.ind = T) %>%
  as.data.frame()
# Only use the re-scan MIDPQ data, which are the first element in separated string
for (i in 1:nrow(Duplicate_Index)){
  tmp_MIDPQ[
    Duplicate_Index$row[i],
    Duplicate_Index$col[i]
    ] <- tmp_MIDPQ %>%
    .[Duplicate_Index$row[i],Duplicate_Index$col[i]] %>%
    str_split(pattern = '_', simplify = T) %>%
    .[1]
}
# Convert MIDPQ string to numeric data type
tmp_MIDPQ <- tmp_MIDPQ %>%
  sapply(as.numeric)
# replace MIDPQ-related columns in NEW_data
NEW_data <- NEW_data %>%
  select(-starts_with('MIDPQ_')) %>%
  cbind(tmp_MIDPQ)
rm(tmp_MIDPQ)
# 4. re-name all columns  ------------------------------------------
NEW_data <- NEW_data %>% 
  rename(# MID
    MID_Vald_Comp = tfmri_mid_all_beh_t_nt,
    MID_Vald_Perf = tfmri_mid_beh_performflag,
    MID_Vald_Feed = tfmri_mid_beh_feedbackflag,
    MID_Vald_NR = tfmri_mid_beh_nruns,
    MID_Vald_R1_NT = tfmri_mid_r1_beh_t_nt,
    MID_Vald_R2_NT = tfmri_mid_r2_beh_t_nt,
    MID_SRW_Mean_RT = tfmri_mid_all_beh_srw_mrt,
    MID_SRW_Std_RT = tfmri_mid_all_beh_srw_stdrt,
    MID_SRWPFB_ACC = tfmri_mid_all_beh_srwpfb_rate,
    MID_SRWPFB_Mean_RT = tfmri_mid_all_beh_srwpfb_mrt,
    MID_SRWPFB_Std_RT = tfmri_mid_all_beh_srwpfb_stdrt,
    MID_SRWNFB_ACC = tfmri_mid_all_beh_srwnfb_rate,
    MID_SRWNFB_Mean_RT = tfmri_mid_all_beh_srwnfb_mrt,
    MID_SRWNFB_Std_RT = tfmri_mid_all_beh_srwnfb_stdrt,
    MID_LRW_Mean_RT = tfmri_mid_all_beh_lrw_mrt,
    MID_LRW_Std_RT = tfmri_mid_all_beh_lrw_stdrt,
    MID_LRWPFB_ACC = tfmri_mid_all_beh_lrwpfb_rate,
    MID_LRWPFB_Mean_RT = tfmri_mid_all_beh_lrwpfb_mrt,
    MID_LRWPFB_Std_RT = tfmri_mid_all_beh_lrwpfb_stdrt,
    MID_LRWNFB_ACC = tfmri_mid_all_beh_lrwnfb_rate,
    MID_LRWNFB_Mean_RT = tfmri_mid_all_beh_lrwnfb_mrt,
    MID_LRWNFB_Std_RT = tfmri_mid_all_beh_lrwnfb_stdrt,
    MID_SLO_Mean_RT = tfmri_mid_all_beh_sl_mrt,
    MID_SLO_Std_RT = tfmri_mid_all_beh_sl_stdrt,
    MID_SLOPFB_ACC = tfmri_mid_all_beh_slpfb_rate,
    MID_SLOPFB_Mean_RT = tfmri_mid_all_beh_slpfb_mrt,
    MID_SLOPFB_Std_RT = tfmri_mid_all_beh_slpfb_stdrt,
    MID_SLONFB_ACC = tfmri_mid_all_beh_slnfb_rate,
    MID_SLONFB_Mean_RT = tfmri_mid_all_beh_slnfb_mrt,
    MID_SLONFB_Std_RT = tfmri_mid_all_beh_slnfb_stdrt,
    MID_LLO_Mean_RT = tfmri_mid_all_beh_ll_mrt,
    MID_LLO_Std_RT = tfmri_mid_all_beh_ll_stdrt,
    MID_LLOPFB_ACC = tfmri_mid_all_beh_llpfb_rate,
    MID_LLOPFB_Mean_RT = tfmri_mid_all_beh_llpfb_mrt,
    MID_LLOPFB_Std_RT = tfmri_mid_all_beh_llpfb_stdrt,
    MID_LLONFB_ACC = tfmri_mid_all_beh_llnfb_rate,
    MID_LLONFB_Mean_RT = tfmri_mid_all_beh_llnfb_mrt,
    MID_LLONFB_Std_RT = tfmri_mid_all_beh_llnfb_stdrt,
    MID_NEU_Mean_RT = tfmri_mid_all_beh_nt_mrt,
    MID_NEU_Std_RT = tfmri_mid_all_beh_nt_stdrt,
    MID_NEUPFB_ACC = tfmri_mid_all_beh_ntpfb_rate,
    MID_NEUPFB_Mean_RT = tfmri_mid_all_beh_ntpfb_mrt,
    MID_NEUPFB_Std_RT = tfmri_mid_all_beh_ntpfb_stdrt,
    MID_NEUNFB_ACC = tfmri_mid_all_beh_ntnfb_rate,
    MID_NEUNFB_Mean_RT = tfmri_mid_all_beh_ntnfb_mrt,
    MID_NEUNFB_Std_RT = tfmri_mid_all_beh_ntnfb_stdrt,
    MID_HRW_Mean_RT = tfmri_mid_all_beh_hrw_mrt,
    MID_HRW_Std_RT = tfmri_mid_all_beh_hrw_stdrt,
    MID_HRWPFB_ACC = tfmri_mid_all_beh_hrwpfb_rate,
    MID_HRWPFB_Mean_RT = tfmri_mid_all_beh_hrwpfb_mrt,
    MID_HRWPFB_Std_RT = tfmri_mid_all_beh_hrwpfb_stdrt,
    MID_HRWNFB_ACC = tfmri_mid_all_beh_hrwnfb_rate,
    MID_HRWNFB_Mean_RT = tfmri_mid_all_beh_hrwnfb_mrt,
    MID_HRWNFB_Std_RT = tfmri_mid_all_beh_hrwnfb_stdrt,
    MID_HLO_Mean_RT = tfmri_mid_all_beh_hl_mrt,
    MID_HLO_Std_RT = tfmri_mid_all_beh_hl_stdrt,
    MID_HLOPFB_ACC = tfmri_mid_all_beh_hlpfb_rate,
    MID_HLOPFB_Mean_RT = tfmri_mid_all_beh_hlpfb_mrt,
    MID_HLOPFB_Std_RT = tfmri_mid_all_beh_hlpfb_stdrt,
    MID_HLONFB_ACC = tfmri_mid_all_beh_hlnfb_rate,
    MID_HLONFB_Mean_RT = tfmri_mid_all_beh_hlnfb_mrt,
    MID_HLONFB_Std_RT = tfmri_mid_all_beh_hlnfb_stdrt,
    MID_Score = tfmri_mid_all_beh_t_earnings,
    # MID-PQ has already been re-named
    # SST
    SST_Vald_Perf = tfmri_sst_beh_performflag,
    SST_Vald_Race = tfmri_sst_beh_violatorflag,
    SST_Vald_Glit = tfmri_sst_beh_glitchflag,
    SST_Vald_0SSD = tfmri_sst_beh_0ssdcount,
    SST_SSRT_Mean = tfmri_sst_all_beh_total_mssrt,
    SST_SSRT_Inte = tfmri_sst_all_beh_total_issrt,
    # ENB
    ENB_0BK_CALL_Mean_RT = tfmri_nb_all_beh_c0b_mrt,
    ENB_0BK_CALL_ACC = tfmri_nb_all_beh_c0b_rate,
    ENB_0BK_CALL_Std_RT = tfmri_nb_all_beh_c0b_stdrt,
    ENB_0BK_CNGF_Mean_RT = tfmri_nb_all_beh_c0bnf_mrt,
    ENB_0BK_CNGF_ACC = tfmri_nb_all_beh_c0bnf_rate,
    ENB_0BK_CNEF_Std_RT = tfmri_nb_all_beh_c0bnf_stdrt,
    ENB_0BK_CPLA_Mean_RT = tfmri_nb_all_beh_c0bp_mrt,
    ENB_0BK_CPLA_ACC = tfmri_nb_all_beh_c0bp_rate,
    ENB_0BK_CPLA_Std_RT = tfmri_nb_all_beh_c0bp_stdrt,
    ENB_0BK_CPSF_Mean_RT = tfmri_nb_all_beh_c0bpf_mrt,
    ENB_0BK_CPSF_ACC = tfmri_nb_all_beh_c0bpf_rate,
    ENB_0BK_CPSF_Std_RT = tfmri_nb_all_beh_c0bpf_stdrt,
    ENB_2BK_CALL_Mean_RT = tfmri_nb_all_beh_c2b_mrt,
    ENB_2BK_CALL_ACC = tfmri_nb_all_beh_c2b_rate,
    ENB_2BK_CALL_Std_RT = tfmri_nb_all_beh_c2b_stdrt,
    ENB_2BK_CNEF_Mean_RT = tfmri_nb_all_beh_c2bnf_mrt,
    ENB_2BK_CNEF_ACC = tfmri_nb_all_beh_c2bnf_rate,
    ENB_2BK_CNEF_Std_RT = tfmri_nb_all_beh_c2bnf_stdrt,
    ENB_2BK_CPLA_Mean_RT = tfmri_nb_all_beh_c2bp_mrt,
    ENB_2BK_CPLA_ACC = tfmri_nb_all_beh_c2bp_rate,
    ENB_2BK_CPLA_Std_RT = tfmri_nb_all_beh_c2bp_stdrt,
    ENB_2BK_CPSF_Mean_RT = tfmri_nb_all_beh_c2bpf_mrt,
    ENB_2BK_CPSF_ACC = tfmri_nb_all_beh_c2bpf_rate,
    ENB_2BK_CPSF_Std_RT = tfmri_nb_all_beh_c2bpf_stdrt,
    ENB_TOT_CNGF_Mean_RT = tfmri_nb_all_beh_cngf_mrt,
    ENB_TOT_CNGF_ACC = tfmri_nb_all_beh_cngf_rate,
    ENB_TOT_CNGF_Std_RT = tfmri_nb_all_beh_cngf_stdrt,
    ENB_TOT_CNUF_Mean_RT = tfmri_nb_all_beh_cnf_mrt,
    ENB_TOT_CNUF_ACC = tfmri_nb_all_beh_cnf_rate,
    ENB_TOT_CNUF_Std_RT = tfmri_nb_all_beh_cnf_stdrt,
    ENB_TOT_CPLA_Mean_RT = tfmri_nb_all_beh_cplace_mrt,
    ENB_TOT_CPLA_ACC = tfmri_nb_all_beh_cplace_rate,
    ENB_TOT_CPLA_Std_RT = tfmri_nb_all_beh_cplace_stdrt,
    ENB_TOT_CPSF_Mean_RT = tfmri_nb_all_beh_cpf_mrt,
    ENB_TOT_CPSF_ACC = tfmri_nb_all_beh_cpf_rate,
    ENB_TOT_CPSF_Std_RT = tfmri_nb_all_beh_cpf_stdrt,
    ENB_TOT_CALL_Mean_RT = tfmri_nb_all_beh_ctotal_mrt,
    ENB_TOT_CALL_ACC = tfmri_nb_all_beh_ctotal_rate,
    ENB_TOT_CALL_Std_RT = tfmri_nb_all_beh_ctotal_stdrt,
    # RECMEM
    RECMEM_NGF_BR = tfmri_rec_all_beh_negface_br,
    RECMEM_NGF_DPR = tfmri_rec_all_beh_negf_dp,
    RECMEM_NGF_PR = tfmri_rec_all_beh_negface_pr,
    RECMEM_NUF_BR = tfmri_rec_all_beh_neutface_br,
    RECMEM_NUF_DPR = tfmri_rec_all_beh_neutf_dp,
    RECMEM_NUF_PR = tfmri_rec_all_beh_neutface_pr,
    RECMEM_PLA_BR = tfmri_rec_all_beh_place_br,
    RECMEM_PLA_DPR = tfmri_rec_all_beh_place_dp,
    RECMEM_PLA_PR = tfmri_rec_all_beh_place_pr,
    RECMEM_PSF_BR = tfmri_rec_all_beh_posface_br,
    RECMEM_PSF_DPR = tfmri_rec_all_beh_posf_dpr,
    RECMEM_PSF_PR = tfmri_rec_all_beh_posface_pr
  )

NEW_data$MID_Vald_Comp <- (NEW_data$MID_Vald_Comp == 100) %>%
  as.numeric()


# 6. Save re-coded data into RDS file -------------------------------------

NEW_data[sort(colnames(NEW_data))] %>%
  select(c(src_subject_id,eventname,
           everything())) -> NEW_tfMRI_behav
SDPP.save.file(NEW_tfMRI_behav,
               FileName = "NC_tfMRI_behav.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

NEW_tfMRI_behav %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_NC_tfMRI_behav_Rec.doc'),
              row.names = F)

select(NEW_tfMRI_behav,-c(src_subject_id,eventname)) %>% 
  psych::describeBy(group = Recode.Eventname(NEW_tfMRI_behav)$eventname,
                    mat = T,digits =2) %>%
  print_table(file = fullfile(ResultsOutputDir,'VSO_ALL_NC_tfMRI_behav.doc'),
              row.names = T,
              digits = 2)
# End of Script -----------------------------------------------------------

s_close_sink()
