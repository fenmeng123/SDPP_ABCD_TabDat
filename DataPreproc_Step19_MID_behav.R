library(bruceR)

setwd('I:\\ABCDStudyNDA\\Download_ABCDV4.0_skr220403\\Package_1206116')

readABCDdata<-function(filename){
  data = read.table(filename,header = TRUE,sep = '\t')
  # remove some specific columns which are same across all .txt files
  data <- subset(data, select = -c(collection_id, dataset_id,collection_title))
  # get variable descriptions
  var.descrip <- data[1,]
  # remove the first row
  data<-data[-1,]
  # add comments to all columns
  # for (i in 1:length(var.descrip)){
  #   comment(data[,i])<-var.descrip[1,i]
  # }
  return(data)
}
# File_list <- list.files('./abcd_mid_tlb01/',full.names = T)
# data <- data.frame()
# for (i in 1:length(File_list)){
#   # Read single subject's MID Task Behavioral Performance Data from a zipped file
#   File_name = unzip(File_list[i],list = T)
#   File_connect <- unz(File_list[i],File_name$Name)
#   Sub_df = read.csv(File_connect)
#   # processing the loaded data and calculate corresponding task indices
#   # 1. Overall Accuracy across all trials
#   Acc_all = mean(Sub_df$mid_acc)
#   # 2. Averaged Accuracy for Anticipation Condition: loss (include small loss and large loss)
#   Acc_loss = mean(filter(Sub_df,(mid_anticipationtype=='LL')|(mid_anticipationtype=='SL'))$mid_acc)
#   # 3. Averaged Accuracy for Anticipation Condition: reward (include small reward and large reward)
#   Acc_reward = mean(filter(Sub_df,(mid_anticipationtype=='LR')|(mid_anticipationtype=='SR'))$mid_acc)
#   # 4. Averaged Accuracy for Anticipation Condition: Small Loss
#   Acc_SL = mean(filter(Sub_df,(mid_anticipationtype=='SL'))$mid_acc)
#   # 5. Averaged Accuracy for Anticipation Condition: Larger Loss
#   Acc_LL = mean(filter(Sub_df,(mid_anticipationtype=='LL'))$mid_acc)
#   # 6. Averaged Accuracy for Anticipation Condition: Small Reward
#   Acc_SR = mean(filter(Sub_df,(mid_anticipationtype=='SR'))$mid_acc)
#   # 7. Averaged Accuracy for Anticipation Condition: Large Reward
#   Acc_LR = mean(filter(Sub_df,(mid_anticipationtype=='LR'))$mid_acc)
#   # 8. Overall Reaction Time across all trials
#   RT_all = mean(Sub_df$mid_rt,na.rm = T)
#   # 9. Averaged Reaction Time for loss
#   RT_loss = mean(filter(Sub_df,(mid_anticipationtype=='LL')|(mid_anticipationtype=='SL'))$mid_rt,na.rm = T)
#   # 10. Averaged Reaction Time for Reward
#   RT_reward = mean(filter(Sub_df,(mid_anticipationtype=='LR')|(mid_anticipationtype=='SR'))$mid_rt,na.rm = T)
#   # 11. Averaged Reaction Time for Small Loss
#   RT_SL = mean(filter(Sub_df,(mid_anticipationtype=='SL'))$mid_rt,na.rm = T)
#   # 12. Averaged Reaction Time for Large Loss
#   RT_LL = mean(filter(Sub_df,(mid_anticipationtype=='LL'))$mid_rt,na.rm = T)
#   # 13. Averaged Reaction Time for Small Reward
#   RT_SR = mean(filter(Sub_df,(mid_anticipationtype=='SR'))$mid_rt,na.rm = T)
#   # 14. Averaged Reaction Time for Large Reward
#   RT_LR = mean(filter(Sub_df,(mid_anticipationtype=='LR'))$mid_rt,na.rm = T)
#   # Extract the Subject ID and eventname (time stamp)
#   src_subject_id = Sub_df$subject[1]
#   eventname = Sub_df$eventname[1]
#   single_sub_df = data.frame(src_subject_id,eventname,Acc_all,Acc_loss,Acc_reward,Acc_SL,Acc_SR,Acc_LL,Acc_LR,
#                              RT_all,RT_loss,RT_reward,RT_SL,RT_SR,RT_LL,RT_LR)
#   data <- rbind(data,single_sub_df)
# }
# saveRDS(data,'I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataPreprocessing/ABCD4.0_MID_behav_individual.rds')

MID_behav <- readABCDdata('I:\\ABCDStudyNDA\\Download_ABCDV4.0_skr220403\\Package_1199282\\abcd_mid02.txt')
# 1. tfmri_mid_beh_performflag: Whether the participant had an acceptable performance
# in the task; must have at least 20 responses per run; 1= acceptable; 0= not acceptable
# 2. tfmri_mid_all_beh_t_nt: Total number of trials; MID task fMRI require that
# 100 total trials were acquired: tfmri_mid_all_beh_t_nt = 100.
# 3. tfmri_mid_beh_feedbackflag: Whether the participant had acceptable performance
# for feedback analyses; all trial types must yield 4 or more events for both
# positive and negative feedback; 1= acceptable; 0= not acceptable
# 4. tfmri_mid_r1_beh_srw_mrt: Average reaction time for small reward trials for run 1
# 5. tfmri_mid_r2_beh_srw_mrt: Average reaction time for small reward trials for run 2
# 6. tfmri_mid_r1_beh_lrw_mrt: Average reaction time for large reward trials for run 1	
# 7. tfmri_mid_r2_beh_lrw_mrt: Average reaction time for large reward trials for run 2
# 8. tfmri_mid_r1_beh_sl_mrt: Average reaction time for small loss trials for run 1
# 9. tfmri_mid_r2_beh_sl_mrt: Average reaction time for small loss trials for run 2
# 10. tfmri_mid_r1_beh_ll_mrt: Average reaction time for large loss trials for run 1	
# 11. tfmri_mid_r2_beh_ll_mrt: Average reaction time for large loss trials for run 2
# 12. tfmri_mid_all_beh_nt_mrt: Average reaction time for neutral trials for run 1 and run 2
# 13. tfmri_mid_r1_beh_nt_mrt: Average reaction time for neutral trials for run 1
# 14. tfmri_mid_r2_beh_nt_mrt: Average reaction time for neutral trials for run 2
# 15. tfmri_mid_all_beh_hrw_mrt: Average reaction time for large and small reward trials for run 1 and run 2	
# 16. tfmri_mid_r1_beh_hrw_mrt: Average reaction time for large and small reward trials for run 1	
# 17. tfmri_mid_r2_beh_hrw_mrt: Average reaction time for large and small reward trials for run 2	
# 18. tfmri_mid_all_beh_hl_mrt: Average reaction time for large and small loss conditions for run 1 and run 2
# 19. tfmri_mid_r1_beh_hl_mrt: Average reaction time for large and small loss trials for run 1
# 20. tfmri_mid_r2_beh_hl_mrt: Average reaction time for large and small loss trials for run 2
# 21. tfmri_mid_all_beh_ntpfb_rate: Number of neutral trials that yielded 
#                                   positive feedback in run 1 and run 2
#                                   combined divided by the total number of neutral trials
# 22. tfmri_mid_all_beh_srwpfb_rate: Number of small reward trials that yielded positive feedback in run 1 and run 2 combined divided by the total number of small reward trials
# 23. tfmri_mid_all_beh_lrwpfb_rate
# 24. tfmri_mid_all_beh_slpfb_rate
# 25. tfmri_mid_all_beh_llpfb_rate
# 26. tfmri_mid_all_beh_hrwpfb_rate
# 27. tfmri_mid_all_beh_hlpfb_rate
MID_behav <- select(MID_behav,c(src_subject_id,interview_age,sex,eventname,
                                tfmri_mid_beh_performflag,
                                tfmri_mid_all_beh_t_nt,
                                tfmri_mid_beh_feedbackflag,
                                tfmri_mid_all_beh_srwpfb_nt,#Total number of small reward trials answered correctly for run 1 and run 2	
                                tfmri_mid_all_beh_lrwpfb_nt,#Total number of large reward trials answered correctly for run 1 and run 2
                                tfmri_mid_all_beh_slpfb_nt,#Total number of small loss trials answered correctly for run 1 and run 2
                                tfmri_mid_all_beh_llpfb_nt,#Total number of large loss trials answered correctly for run 1 and run 2	
                                tfmri_mid_all_beh_srwnfb_nt,#Total number of small reward trials answered incorrectly for run 1 and run 2	
                                tfmri_mid_all_beh_lrwnfb_nt,#Total number of large reward trials answered incorrectly for run 1 and run 2	
                                tfmri_mid_all_beh_slnfb_nt,#Total number of small loss trials answered incorrectly for run 1 and run 2	
                                tfmri_mid_all_beh_llnfb_nt,#Total number of large loss trials answered incorrectly for run 1 and run 2	
                                tfmri_mid_r1_beh_srw_mrt,
                                tfmri_mid_r2_beh_srw_mrt,
                                tfmri_mid_r1_beh_lrw_mrt,
                                tfmri_mid_r2_beh_lrw_mrt,
                                tfmri_mid_r1_beh_sl_mrt,
                                tfmri_mid_r2_beh_sl_mrt,
                                tfmri_mid_r1_beh_ll_mrt,
                                tfmri_mid_r2_beh_ll_mrt,
                                tfmri_mid_all_beh_nt_mrt,
                                tfmri_mid_r1_beh_nt_mrt,
                                tfmri_mid_r2_beh_nt_mrt,
                                tfmri_mid_all_beh_hrw_mrt,
                                tfmri_mid_r1_beh_hrw_mrt,
                                tfmri_mid_r2_beh_hrw_mrt,
                                tfmri_mid_all_beh_hl_mrt,
                                tfmri_mid_r1_beh_hl_mrt,
                                tfmri_mid_r2_beh_hl_mrt,
                                tfmri_mid_all_beh_ntpfb_rate,
                                tfmri_mid_all_beh_srwpfb_rate,
                                tfmri_mid_all_beh_lrwpfb_rate,
                                tfmri_mid_all_beh_slpfb_rate,
                                tfmri_mid_all_beh_llpfb_rate,
                                tfmri_mid_all_beh_hrwpfb_rate,
                                tfmri_mid_all_beh_hlpfb_rate))
sapply(MID_behav, typeof)
MID_behav = mutate(MID_behav,across(.cols=5:ncol(MID_behav), .fns=as.numeric))
sapply(MID_behav, typeof)
colnames(MID_behav) <- c('src_subject_id','interview_age','sex','eventname',
                         'PerformFlag',
                         'NumTrials',
                         'FeedbackFlag',
                         'Num_Correct_SR',
                         'Num_Correct_LR',
                         'Num_Correct_SL',
                         'Num_Correct_LL',
                         'Num_Incorr_SR',
                         'Num_Incorr_LR',
                         'Num_Incorr_SL',
                         'Num_Incorr_LL',
                         'RT_r1_SR',
                         'RT_r2_SR',
                         'RT_r1_LR',
                         'RT_r2_LR',
                         'RT_r1_SL',
                         'RT_r2_SL',
                         'RT_r1_LL',
                         'RT_r2_LL',
                         'RT_Neutral',
                         'RT_r1_Neutral',
                         'RT_r2_Neutral',
                         'RT_Reward',
                         'RT_r1_Reward',
                         'RT_r2_Reward',
                         'RT_Loss',
                         'RT_r1_Loss',
                         'RT_r2_Loss',
                         'ACC_Neutral',
                         'ACC_SR_byAllReward',
                         'ACC_LR_byAllReward',
                         'ACC_SL_byAllLoss',
                         'ACC_LL_byAllLoss',
                         'ACC_Reward_byAllReward',
                         'ACC_Loss_byAllLoss')
# Notes: Kunru Song 2023.3.15
# Find an data error in MID_behav, the raw data from tabulated data file is wrong,
# RT_Reward = RT_Neutral, which can not happen in theory.
# After insepction, I found the RT_Reward is the wrong column, the Reaction Time 
# for Reward condition is re-calulated by (RT_r1_Reward+RT_r2_Reward)/2
MID_behav$RT_Reward = (MID_behav$RT_r1_Reward + MID_behav$RT_r2_Reward)/2

MID_behav$FeedbackFlag = MID_behav$Num_Correct_SR >= 4 & 
  MID_behav$Num_Correct_LR >= 4 & 
  MID_behav$Num_Correct_SL >=4 & MID_behav$Num_Correct_LL >= 4 &
  MID_behav$Num_Incorr_SR >= 4 & MID_behav$Num_Incorr_LR >= 4 &
  MID_behav$Num_Incorr_SL >= 4 & MID_behav$Num_Incorr_LL >= 4
MID_behav$FeedbackFlag = replace_na(MID_behav$FeedbackFlag,FALSE)
MID_behav$PerformFlag = replace_na(MID_behav$PerformFlag,0)
MID_behav$TrialFlag = (MID_behav$NumTrials == 100)
MID_behav$TrialFlag = replace_na(MID_behav$TrialFlag,0)
MID_behav <- select(MID_behav,c(src_subject_id,interview_age,sex,eventname,
                                TrialFlag,PerformFlag,FeedbackFlag,
                                RT_Reward,RT_Loss,RT_Neutral,
                                ACC_Reward_byAllReward,
                                ACC_Loss_byAllLoss,
                                ACC_Neutral))
colnames(MID_behav) <- c('src_subject_id','interview_age','sex','eventname',
                         'MID_TrialFlag',
                         'MID_PerformFlag',
                         'MID_FeedbackFlag',
                         'MID_RT_Reward',
                         'MID_RT_Loss',
                         'MID_RT_Neutral',
                         'MID_ACC_Reward_byAllReward',
                         'MID_ACC_Loss_byAllLoss',
                         'MID_ACC_Neutral')
saveRDS(MID_behav,'I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataPreprocessing/ABCD4.0_MID_behavWithQC.rds')
# data <- readRDS('I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataPreprocessing/ABCD4.0_MID_behav_individual.rds')
