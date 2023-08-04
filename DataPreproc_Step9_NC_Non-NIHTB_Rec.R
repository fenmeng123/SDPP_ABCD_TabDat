# =============================================================================#
# SDPP Step 9: read and re-coding all behavioral task (exclude NIHTB and tfMRI)
# R Packages Dependency: bruceR, naniar, readxl
# Step File Notes: 
# 
# Target File: 
# ABCD 4.0 : 
# ABCD 5.0: /core/neurocognition/nc_y_cct.csv
#                               /nc_y_lmt.csv
#                               /nc_y_ravlt.csv
#                               /nc_y_wisc.csv
#                               /nc_y_ddis.csv
#                               /nc_y_est.csv
#                               /nc_y_gdt.csv
#                               /nc_y_sit.csv
#                               /nc_y_smarte.csv
#                               /nc_y_bird.csv
#                               /nc_y_flkr.csv
#                               /nc_y_adm.csv
#                               /nc_y_svs.csv
#                               /nc_p_bdef.csv
# Update Date: 2023.7.20
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_9.txt'
AutoLogFilePath = fullfile(ProjectDirectory,'Res_1_Logs',AutoLogFileName)
sink(file = AutoLogFilePath)
ResultsOutputDir = S9_ResultsOutputDir
rm(S9_ResultsOutputDir)
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load NIHTB and its composite scores data ---------------------------------
data = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
                    TableNames = c("nc_y_adm.csv","nc_y_svs.csv",
                                   "nc_y_cct.csv","nc_y_lmt.csv",
                                   "nc_y_ravlt.csv","nc_y_wisc.csv",
                                   "nc_y_ddis.csv","nc_y_est.csv",
                                   "nc_y_gdt.csv","nc_y_sit.csv",
                                   "nc_y_smarte.csv","nc_p_bdef.csv",
                                   "nc_y_flkr.csv","nc_y_bird.csv"),
                    FolderName = "neurocognition")
NEW_data = select(data,c(src_subject_id,eventname))


data %>% MVA.Report.By.Wave() %>% 
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_NC_Non-NIHTB_Raw.doc'),
              row.names = F,
              nsmalls = 1)

# 3. Filter columns and re-name it ----------------------------------------
data %>% select(c(snellen_va_y,
                  cash_choice_task,
                  lmt_scr_perc_correct,lmt_scr_rt_correct,lmt_scr_efficiency,
                  pea_ravlt_sd_trial_i_tc,pea_ravlt_sd_trial_ii_tc,pea_ravlt_sd_trial_iii_tc,
                  pea_ravlt_sd_trial_iv_tc,pea_ravlt_sd_trial_v_tc,pea_ravlt_sd_listb_tc,
                  pea_ravlt_sd_trial_vi_tc,pea_ravlt_ld_trial_vii_tc,
                  pea_wiscv_trs,pea_wiscv_tss,
                  ddis_scr_values_completed,ddis_scr_val_immedcho,
                  ddis_scr_val_cons_per_jbcrit1,ddis_scr_val_jbpass1_num_violations,ddis_scr_val_cons_per_jbcrit2,
                  ddis_scr_val_indif_point_6h,ddis_scr_val_indif_pnt_1da,
                  ddis_scr_val_indif_pnt_1week,ddis_scr_val_indif_pnt_1mth,
                  ddis_scr_val_indif_pnt_3mth,ddis_scr_val_indif_pnt_1yr,
                  ddis_scr_val_indif_pnt_5yr,
                  ddis_scr_expr_mnrt_immcho,ddis_scr_expr_medrt_immedcho,ddis_scr_expr_mnrt_delaycho,
                  ddis_scr_expr_medrt_delaycho,
                  strp_scr_values_completed,strp_scr_acc_all,strp_scr_acc_congr,
                  strp_scr_acc_incongr,strp_scr_mnrt_all,strp_scr_mnrt_congr,
                  strp_scr_mnrt_incongr,
                  gdt_scr_values_account_balance,
                  gdt_scr_values_single,gdt_scr_values_double,
                  gdt_scr_values_triple,gdt_scr_values_quadruple,
                  gdt_scr_values_safe,
                  gdt_scr_values_risky,
                  gdt_scr_expressions_net_score,gdt_scr_values_wins,
                  gdt_scr_values_losses,
                  gdt_scr_values_wins,
                  sit_scr_expr_minitialrat,sit_scr_expr_mrt_initialrat,sit_scr_expr_mfinalrat,
                  sit_scr_expr_mrt_finalrat,sit_scr_expr_mratdiff1,sit_scr_expr_mratdiff2,
                  sit_scr_expr_mratdiff3,sit_scr_expr_mratdiff4,sit_scr_values_count1,
                  sit_scr_values_count2,sit_scr_values_count3,sit_scr_values_count4,
                  sit_scr_values_countflips,sit_scr_values_countnr_initial,sit_scr_values_countnr_final,
                  smarte_ss_sdf_total_corr,
                  smarte_ss_sdf_rule_rt,
                  smarte_ss_sdf_rule_corr,
                  smarte_ss_sdf_rule_acc,
                  smarte_ss_sdf_norule_rt,
                  smarte_ss_sdf_norule_corr,
                  smarte_ss_sdf_norule_acc,
                  smarte_ss_mdf_total_corr,
                  smarte_ss_mdf_regroup_rt,
                  smarte_ss_mdf_regroup_corr,
                  smarte_ss_mdf_regroup_acc,
                  smarte_ss_mdf_noregroup_rt,
                  smarte_ss_mdf_noregroup_corr,
                  smarte_ss_mdf_noregroup_acc,
                  smarte_ss_mdf_challenge_rt,
                  smarte_ss_mdf_challenge_corr,
                  smarte_ss_mdf_challenge_acc,
                  smarte_ss_dot_total_corr,
                  smarte_ss_dot_6to9nogroup_rt,
                  smarte_ss_dot_6to9nogroup_corr,
                  smarte_ss_dot_6to9nogroup_acc,
                  smarte_ss_dot_6to9group_rt,
                  smarte_ss_dot_6to9group_corr,
                  smarte_ss_dot_6to9group_acc,
                  smarte_ss_dot_1to4subit_rt,
                  smarte_ss_dot_1to4subit_corr,
                  smarte_ss_dot_1to4subit_acc,
                  smarte_ss_completedenumeration,
                  smarte_ss_completed_recall,
                  smarte_ss_completed_fluency,
                  smarte_ss_all_total_corr,
                  bird_scr_score,
                  bird_scr_meandotlatency,
                  bird_scr_challenge_latency,
                  bird_scr_quit,
                  bird_scr_lvl3_duration,
                  bird_scr_pre_anxiety,
                  bird_scr_pre_frustration,
                  bird_scr_pre_irritability,
                  bird_scr_pre_happiness,
                  bird_scr_post_anxiety,
                  bird_scr_post_frustration,
                  bird_scr_post_irritability,
                  bird_scr_post_happiness,
                  flkr_scr_completed,
                  flkr_scr_propcorrect,
                  flkr_scr_meanrt,
                  flkr_scr_medrt,
                  flkr_scr_propcorrect_congruent,
                  flkr_scr_meanrt_congruent,
                  flkr_scr_medrt_congruent,
                  flkr_scr_propcorrect_incongruent,
                  flkr_scr_meanrt_incongruent,
                  flkr_scr_medrt_incongruent,
                  bdefs_ss_sum,
                  bdefs_ss_sympt,
                  )) %>% 
  rename(# SVS
         SVS_Score = snellen_va_y,
         # CCT
         CCT_Score = cash_choice_task,
         # LMT
         LMT_ACC = lmt_scr_perc_correct,
         LMT_RT = lmt_scr_rt_correct,
         LMT_EFC = lmt_scr_efficiency,
         # RAVLT
         RAVLT_I_Score = pea_ravlt_sd_trial_i_tc,
         RAVLT_II_Score = pea_ravlt_sd_trial_ii_tc,
         RAVLT_III_Score = pea_ravlt_sd_trial_iii_tc,
         RAVLT_IV_Score = pea_ravlt_sd_trial_iv_tc,
         RAVLT_V_Score = pea_ravlt_sd_trial_v_tc,
         RAVLT_B_Score = pea_ravlt_sd_listb_tc,
         RAVLT_VI_Score = pea_ravlt_sd_trial_vi_tc,
         RAVLT_VII_Score = pea_ravlt_ld_trial_vii_tc,
         # WISC-V
         WISC_Raw_Score = pea_wiscv_trs,
         WISC_Scaled_Score = pea_wiscv_tss,
         # DDT
         DDT_Vald_Comp = ddis_scr_values_completed,
         DDT_Vald_ImdCho = ddis_scr_val_immedcho,
         DDT_Vald_JBP1 = ddis_scr_val_cons_per_jbcrit1,
         DDT_Vald_JBP1NV = ddis_scr_val_jbpass1_num_violations,
         DDT_Vald_JBP2 = ddis_scr_val_cons_per_jbcrit2,
         DDT_6h_IDP_Score = ddis_scr_val_indif_point_6h,
         DDT_1d_IDP_Score = ddis_scr_val_indif_pnt_1da,
         DDT_1w_IDP_Score = ddis_scr_val_indif_pnt_1week,
         DDT_1m_IDP_Score = ddis_scr_val_indif_pnt_1mth,
         DDT_3m_IDP_Score = ddis_scr_val_indif_pnt_3mth,
         DDT_1y_IDP_Score = ddis_scr_val_indif_pnt_1yr,
         DDT_5y_IDP_Score = ddis_scr_val_indif_pnt_5yr,
         DDT_ImdCho_Mean_RT = ddis_scr_expr_mnrt_immcho,
         DDT_ImdCho_Median_RT = ddis_scr_expr_medrt_immedcho,
         DDT_DeyCho_Mean_RT = ddis_scr_expr_mnrt_delaycho,
         DDT_DeyCho_Median_RT = ddis_scr_expr_medrt_delaycho,
         # EST
         EST_Vald_Comp = strp_scr_values_completed,
         EST_Cong_ACC = strp_scr_acc_congr,
         EST_InCong_ACC = strp_scr_acc_incongr,
         EST_All_ACC = strp_scr_acc_all,
         EST_Cong_RT = strp_scr_mnrt_congr,
         EST_InCong_RT = strp_scr_mnrt_incongr,
         EST_All_RT = strp_scr_mnrt_all,
         # GDT
         GDT_Total_Score = gdt_scr_values_account_balance,
         GDT_Single_Score = gdt_scr_values_single,
         GDT_Double_Score = gdt_scr_values_double,
         GDT_Triple_Score = gdt_scr_values_triple,
         GDT_Quadru_Score = gdt_scr_values_quadruple,
         GDT_Safe_Score = gdt_scr_values_safe,
         GDT_Risky_Score = gdt_scr_values_risky,
         GDT_Net_Score = gdt_scr_expressions_net_score,
         GDT_Win_Score = gdt_scr_values_wins,
         GDT_Loss_Score = gdt_scr_values_losses,
         # SIT
         SIT_Initial_Mean_Score = sit_scr_expr_minitialrat,
         SIT_Initial_RT = sit_scr_expr_mrt_initialrat,
         SIT_Final_Mean_Score = sit_scr_expr_mfinalrat,
         SIT_Final_RT = sit_scr_expr_mrt_finalrat,
         SIT_Minus_4_Score = sit_scr_expr_mratdiff1,
         SIT_Minus_2_Score = sit_scr_expr_mratdiff2,
         SIT_Plus_2_Score = sit_scr_expr_mratdiff3,
         SIT_Plus_4_Score = sit_scr_expr_mratdiff4,
         SIT_Flips_Score = sit_scr_values_countflips,
         SIT_Minus_4_Counts = sit_scr_values_count1,
         SIT_Minus_2_Counts = sit_scr_values_count2,
         SIT_Plus_2_Counts = sit_scr_values_count3,
         SIT_Plus_4_Counts = sit_scr_values_count4,
         SIT_Initial_NR_Counts = sit_scr_values_countnr_initial,
         SIT_Final_NR_Counts = sit_scr_values_countnr_final,
         # SMARTE
         SMARTE_Enumeration_Vald_Comp = smarte_ss_completedenumeration,
         SMARTE_Fluency_Vald_Comp = smarte_ss_completed_fluency,
         SMARTE_Recall_Vald_Comp = smarte_ss_completed_recall,
         SMARTE_Total_Score = smarte_ss_all_total_corr,
         SMARTE_Enumeration_Score = smarte_ss_dot_total_corr,
         SMARTE_Rule_ACC = smarte_ss_sdf_rule_acc,
         SMARTE_Rule_Score = smarte_ss_sdf_rule_corr,
         SMARTE_Rule_RT = smarte_ss_sdf_rule_rt,
         SMARTE_NoRule_ACC = smarte_ss_sdf_norule_acc,
         SMARTE_NoRule_Score = smarte_ss_sdf_norule_corr,
         SMARTE_NoRule_RT = smarte_ss_sdf_norule_rt,
         SMARTE_Regroup_ACC = smarte_ss_mdf_regroup_acc,
         SMARTE_Regroup_Score = smarte_ss_mdf_regroup_corr,
         SMARTE_Regroup_RT = smarte_ss_mdf_regroup_rt,
         SMARTE_NoRegroup_ACC = smarte_ss_mdf_noregroup_acc,
         SMARTE_NoRegroup_Score = smarte_ss_mdf_noregroup_corr,
         SMARTE_NoRegroup_RT = smarte_ss_mdf_noregroup_rt,
         SMARTE_Challenge_ACC = smarte_ss_mdf_challenge_acc,
         SMARTE_Challenge_Score = smarte_ss_mdf_challenge_corr,
         SMARTE_Challenge_RT = smarte_ss_mdf_challenge_rt,
         SMARTE_MDA_Score = smarte_ss_mdf_total_corr,
         SMARTE_SDA_Score = smarte_ss_sdf_total_corr,
         SMARTE_1to4Subit_ACC = smarte_ss_dot_1to4subit_acc,
         SMARTE_1to4Subit_Score = smarte_ss_dot_1to4subit_corr,
         SMARTE_1to4Subit_RT = smarte_ss_dot_1to4subit_rt,
         SMARTE_6to9Nogroup_ACC = smarte_ss_dot_6to9nogroup_acc,
         SMARTE_6to9Nogroup_Score = smarte_ss_dot_6to9nogroup_corr,
         SMARTE_6to9Nogroup_RT = smarte_ss_dot_6to9nogroup_rt,
         SMARTE_6to9Group_ACC = smarte_ss_dot_6to9group_acc,
         SMARTE_6to9Group_Score = smarte_ss_dot_6to9group_corr,
         SMARTE_6to9Group_RT = smarte_ss_dot_6to9group_rt,
         # BIRD
         BIRD_Score = bird_scr_score,
         BIRD_Dot_RT = bird_scr_meandotlatency,
         BIRD_Challenge_RT = bird_scr_challenge_latency,
         BIRD_Quit_Score = bird_scr_quit,
         BIRD_Persistence_Score = bird_scr_lvl3_duration,
         BIRD_Pre_Anxiety_Score = bird_scr_pre_anxiety,
         BIRD_Pre_Frustra_Score = bird_scr_pre_frustration,
         BIRD_Pre_Irritab_Score = bird_scr_pre_irritability,
         BIRD_Pre_Happine_Score = bird_scr_pre_happiness,
         BIRD_Post_Anxiety_Score = bird_scr_post_anxiety,
         BIRD_Post_Frustra_Score = bird_scr_post_frustration,
         BIRD_Post_Irritab_Score = bird_scr_post_irritability,
         BIRD_Post_Happine_Score = bird_scr_post_happiness,
         # FLKR
         FLKR_Vald_Comp = flkr_scr_completed,
         FLKR_Total_ACC = flkr_scr_propcorrect,
         FLKR_Total_Mean_RT = flkr_scr_meanrt,
         FLKR_Total_Median_RT = flkr_scr_medrt,
         FLKR_Cong_ACC = flkr_scr_propcorrect_congruent,
         FLKR_Cong_Mean_RT = flkr_scr_meanrt_congruent,
         FLKR_Cong_Median_RT = flkr_scr_medrt_congruent,
         FLKR_InCong_ACC = flkr_scr_propcorrect_incongruent,
         FLKR_InCong_Mean_RT = flkr_scr_meanrt_incongruent,
         FLKR_InCong_Median_RT = flkr_scr_medrt_incongruent,
         # BDEF
         BDEFS_Score = bdefs_ss_sum,
         BDEFS_Symptom_Counts = bdefs_ss_sympt,
         ) %>% cbind(NEW_data) -> NEW_data

# NEW_data %>% MVA.Report.By.Wave() %>% 
#   print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_NC_Non-NIHTB_Rec.doc'),
#               row.names = F,
#               nsmalls = 1)

# 4. Check and Re-formate Variable Type ---------------------------------------
sapply(NEW_data, typeof) %>% sort() -> DataType
fprintf("Neurocognition Non-NIHTB variables' data type (before re-formating)\n")
print(DataType)
RECODE(NEW_data[['DDT_Vald_JBP1']],
       "'yes'=1;'no'=0;'no not evaluated yet'=0;") %>% as.numeric() %>%
  Recode.ABCD.YN(VarName = 'DDT_Vald_JBP1') -> NEW_data[['DDT_Vald_JBP1']]
RECODE(NEW_data[['DDT_Vald_JBP2']],
       "'yes'=1;'no'=0;'no not evaluated yet'=0;") %>% as.numeric() %>%
  Recode.ABCD.YN(VarName = 'DDT_Vald_JBP1') -> NEW_data[['DDT_Vald_JBP2']]  
NEW_data %>% select(-c(DDT_Vald_JBP1,DDT_Vald_JBP2,DDT_Vald_JBP1NV)) %>%
  select(contains('Vald')) %>% lapply(as.numeric) %>% as.data.frame() -> Vald_data
for (i in colnames(Vald_data)){
  Vald_data[[i]] <- Recode.ABCD.YN(Vald_data[[i]],VarName = i)
}
NEW_data[,colnames(Vald_data)] <- Vald_data

NEW_data[,grep('Score$',names(DataType)[DataType  %in% 'character'],value = T)] %>%
  sapply(as.numeric) %>% as.data.frame() %>% 
  sapply(function(x) round(x,digits = 4)) -> NEW_data[,grep('Score$',names(DataType)[DataType  %in% 'character'],value = T)]

NEW_data[,grep('ACC$',names(DataType)[DataType  %in% 'character'],value = T)] %>%
  sapply(as.numeric) %>% as.data.frame() %>% 
  sapply(function(x) round(x,digits = 4)) -> NEW_data[,grep('ACC$',names(DataType)[DataType  %in% 'character'],value = T)]

NEW_data[,grep('RT$',names(DataType)[DataType  %in% 'character'],value = T)] %>%
  sapply(as.numeric) %>% as.data.frame() %>% 
  sapply(function(x) round(x,digits = 2)) -> NEW_data[,grep('RT$',names(DataType)[DataType  %in% 'character'],value = T)]

NEW_data[,grep('EFC$',names(DataType)[DataType  %in% 'character'],value = T)] %>%
  sapply(as.numeric) %>% as.data.frame() %>% 
  sapply(function(x) round(x,digits = 6)) -> NEW_data[,grep('EFC$',names(DataType)[DataType  %in% 'character'],value = T)]

NEW_data[,grep('Counts$',names(DataType)[DataType  %in% 'character'],value = T)] %>%
  sapply(as.integer) %>% as.data.frame() -> NEW_data[,grep('Counts$',names(DataType)[DataType  %in% 'character'],value = T)]

NEW_data[,grep('(EFC$)|(ACC$)|(RT$)|(Score$)',colnames(NEW_data),value = T)] %>%
  sapply(as.double) %>% as.data.frame() -> NEW_data[,grep('(EFC$)|(ACC$)|(RT$)|(Score$)',colnames(NEW_data),value = T)]

NEW_data[,grep('(Counts$)',colnames(NEW_data),value = T)] %>% 
  sapply(as.integer) %>% as.data.frame() -> NEW_data[,grep('(Counts$)',colnames(NEW_data),value = T)]
fprintf("Neurocognition Non-NIHTB variables' data type (after re-formating)\n")
sapply(NEW_data,typeof) %>% sort %>% print()
  
# 5. Perform Re-coding for all Non-NIHTB Tasks ---------------------------------

# CCT
RECODE(NEW_data$CCT_Score,
       "1 = 'Immediate Reward';
        2 = 'Delayed Reward';
        3 = 'Cannot Decide';") %>%
  factor(levels = c("Immediate Reward",
                    "Delayed Reward",
                    "Cannot Decide")) -> NEW_data$CCT_Choice
# LMT
NEW_data$LMT_ACC[replace_na((NEW_data$LMT_ACC > 1),F)] = NEW_data$LMT_ACC[replace_na((NEW_data$LMT_ACC > 1),F)]/100

# RAVLT
NEW_data$RAVLT_Immediate_Score = NEW_data$RAVLT_I_Score + 
  NEW_data$RAVLT_II_Score + NEW_data$RAVLT_III_Score +
  NEW_data$RAVLT_IV_Score + NEW_data$RAVLT_V_Score 
NEW_data$RAVLT_Learning_Score = NEW_data$RAVLT_V_Score - NEW_data$RAVLT_I_Score
NEW_data$RAVLT_Forgetting_Score = NEW_data$RAVLT_V_Score - NEW_data$RAVLT_VII_Score
NEW_data$RAVLT_Perc_Forgetting_Score = NEW_data$RAVLT_Forgetting_Score / NEW_data$RAVLT_V_Score

# DDT
DDT <- select(NEW_data,c(src_subject_id,eventname,
                         DDT_6h_IDP_Score,DDT_1d_IDP_Score,DDT_1w_IDP_Score,
                         DDT_1m_IDP_Score,DDT_3m_IDP_Score,DDT_1y_IDP_Score,
                         DDT_5y_IDP_Score))

DDT$DDT_DiscountRate_Score <- NA
DDT$DDT_Vald_NLSConv <- NA

for (i in 1:nrow(DDT)){
  SingleSubData <- DDT[i,]
  if (sum(is.na(SingleSubData))>=7){
    next
  }else{
    tmp <- select(SingleSubData,c(
      DDT_6h_IDP_Score,DDT_1d_IDP_Score,DDT_1w_IDP_Score,
      DDT_1m_IDP_Score,DDT_3m_IDP_Score,DDT_1y_IDP_Score,
      DDT_5y_IDP_Score))
    tmp <- t(tmp)
    tmp <- as.data.frame(tmp)
    colnames(tmp) <- 'indifference'
    DDT$DDT_JBCriteria_1[i] <- (sum(diff(tmp$indifference)<20)==6)
    DDT$DDT_JBCriteria_2[i] <- tail(tmp$indifference,1) - tmp$indifference[1] < -10
    tmp$delay <- c(6/24/30,1/30,0.25,1,3,12,60)
    res <- nls(indifference ~ 100 / (1 + k*delay), data = tmp,
               start = list(k = 1),control = list(maxiter=50000))
    DDT$DDT_DiscountRate_Score[i] <- coef(res)
    DDT$DDT_Vald_NLSConv[i] <- res$convInfo$isConv
  }
  fprintf("# Iter-%d SubID:%s Wave:%s Estimated K-value=%.4f\n",i,DDT$src_subject_id[i],DDT$eventname[i],DDT$DDT_DiscountRate_Score[i])
}
NEW_data = merge(NEW_data,
                 select(DDT,c(src_subject_id,
                              eventname,
                              DDT_DiscountRate_Score,
                              DDT_Vald_NLSConv)),
                 by = c("src_subject_id","eventname"),
                 all = T)
NEW_data$DDT_Vald_FlagBasic = (NEW_data$DDT_Vald_Comp == 'Yes') &
  (NEW_data$DDT_Vald_JBP1NV<2) & 
  NEW_data$DDT_Vald_NLSConv
NEW_data$DDT_Vald_FlagRestricedABCD = NEW_data$DDT_Vald_FlagBasic &
  (NEW_data$DDT_Vald_ImdCho == 'Yes') &
  (NEW_data$DDT_Vald_JBP1 == 'Yes') & 
  (NEW_data$DDT_Vald_JBP2== 'Yes')
fprintf("DDT: The number of valid data points - Basic QC Threshold:%d\n",sum(NEW_data$DDT_Vald_FlagBasic,na.rm = T))
print(table(NEW_data$DDT_Vald_FlagBasic,useNA = 'if'))
fprintf("DDT: The number of valid data points - Restricted ABCD Sample:%d\n",sum(NEW_data$DDT_Vald_FlagRestricedABCD,na.rm = T))
print(table(NEW_data$DDT_Vald_FlagRestricedABCD,useNA = 'if'))

DDT$DDT_LogTransedK_Score = log(DDT$DDT_DiscountRate_Score)

# EST 
NEW_data$EST_Vald_Flag = (NEW_data$EST_Vald_Comp == 'Yes') &
  ( (NEW_data$EST_All_RT > 200) &
    (NEW_data$EST_All_RT < 2000) )
fprintf("EST: The number of valid data points - Basic QC Threshold:%d\n",sum(NEW_data$EST_Vald_Flag,na.rm = T))
print(table(NEW_data$EST_Vald_Flag,useNA = 'if'))

NEW_data$EST_RT_Score = NEW_data$EST_InCong_RT - NEW_data$EST_Cong_RT
NEW_data$EST_ACC_Score = NEW_data$EST_InCong_ACC - NEW_data$EST_Cong_ACC

# SIT

NEW_data$SIT_Rating_Score = NEW_data$SIT_Final_Mean_Score - NEW_data$SIT_Initial_Mean_Score
NEW_data$SIT_RT_Score = NEW_data$SIT_Final_RT - NEW_data$SIT_Initial_RT

# SMARTE

NEW_data$SMARTE_Enumeration_Score = SUM(NEW_data,
                                        vars = c("SMARTE_1to4Subit_Score",
                                                 "SMARTE_6to9Group_Score",
                                                 "SMARTE_6to9Nogroup_Score"),
                                        na.rm = F)
NEW_data$SMARTE_Fluency_Score = SUM(NEW_data,
                                        vars = c("SMARTE_Rule_Score",
                                                 "SMARTE_NoRule_Score",
                                                 "SMARTE_Regroup_Score",
                                                 "SMARTE_NoRegroup_Score"),
                                    na.rm = F)
NEW_data$SMARTE_Recall_Score = SUM(NEW_data,
                                    vars = c("SMARTE_MDA_Score",
                                             "SMARTE_Challenge_Score"),
                                    na.rm = T)
NEW_data$SMARTE_Enumeration_ACC = MEAN(NEW_data,
                                        vars = c("SMARTE_1to4Subit_ACC",
                                                 "SMARTE_6to9Group_ACC",
                                                 "SMARTE_6to9Nogroup_ACC"),
                                        na.rm = F)
NEW_data$SMARTE_Fluency_ACC = MEAN(NEW_data,
                                    vars = c("SMARTE_Rule_ACC",
                                             "SMARTE_NoRule_ACC",
                                             "SMARTE_Regroup_ACC",
                                             "SMARTE_NoRegroup_ACC"),
                                    na.rm = F)
NEW_data$SMARTE_Recall_ACC = MEAN(NEW_data,
                                   vars = c("SMARTE_Regroup_ACC",
                                            "SMARTE_NoRegroup_ACC",
                                            "SMARTE_Challenge_ACC"),
                                   na.rm = F)
NEW_data$SMARTE_Enumeration_RT = MEAN(NEW_data,
                                       vars = c("SMARTE_1to4Subit_RT",
                                                "SMARTE_6to9Group_RT",
                                                "SMARTE_6to9Nogroup_RT"),
                                       na.rm = F)
NEW_data$SMARTE_Fluency_RT = MEAN(NEW_data,
                                   vars = c("SMARTE_Rule_RT",
                                            "SMARTE_NoRule_RT",
                                            "SMARTE_Regroup_RT",
                                            "SMARTE_NoRegroup_RT"),
                                   na.rm = F)
NEW_data$SMARTE_Recall_RT = MEAN(NEW_data,
                                  vars = c("SMARTE_Regroup_RT",
                                           "SMARTE_NoRegroup_RT",
                                           "SMARTE_Challenge_RT"),
                                  na.rm = F)
NEW_data$SMARTE_Score = SUM(NEW_data,
                            vars = c("SMARTE_Enumeration_Score",
                                     "SMARTE_Fluency_Score",
                                     "SMARTE_Recall_Score"))
NEW_data$SMARTE_EFC = NEW_data$SMARTE_Score/(NEW_data$SMARTE_Enumeration_RT+
                                               NEW_data$SMARTE_Fluency_RT+
                                               NEW_data$SMARTE_Recall_RT)
NEW_data$SMARTE_Vald_Flag = (NEW_data$SMARTE_Enumeration_Vald_Comp == 'Yes') &
  (NEW_data$SMARTE_Fluency_Vald_Comp == 'Yes') &
  (NEW_data$SMARTE_Recall_Vald_Comp == 'Yes')
# FLKR
NEW_data$FLKR_RT_Score = NEW_data$FLKR_InCong_Mean_RT - NEW_data$FLKR_Cong_Mean_RT
NEW_data$FLKR_ACC_Score = NEW_data$FLKR_InCong_ACC - NEW_data$FLKR_Cong_ACC



# 6. Save re-coded data into RDS file -------------------------------------


NEW_data[sort(colnames(NEW_data))] %>% select(c(src_subject_id,eventname,
                                                everything())) -> NEW_NonNIHTB
SDPP.save.file(NEW_NonNIHTB,
               FileName = "NC_Non-NIHTB.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

NEW_NonNIHTB %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_NC_Non-NIHTB_Rec.doc'),
              row.names = F,
              nsmalls = 1)

select(NEW_NonNIHTB,-c(src_subject_id,eventname)) %>% 
  psych::describeBy(group = Recode.Eventname(NEW_NonNIHTB)$eventname,
                    mat = T,digits =2) %>%
  print_table(file = fullfile(ResultsOutputDir,'VSO_ALL_NC_Non-NIHTB.doc'),
              row.names = T,
              nsmalls = 1,
              digits = 2)
# End of Script -----------------------------------------------------------

fprintf("SDPP-ABCD-TabDat Step 9 finished! Finish Time:%s\n",Sys.time())

sink()
