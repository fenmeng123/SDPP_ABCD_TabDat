library(bruceR)

setwd('I:\\ABCDStudyNDA\\Download_ABCDV4.0_skr220403\\Package_1199282')

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

# Extract Trial-level MID task performance data ---------------------------
# File_list <- list.files('./abcd_sst_tlb01/',full.names = T)
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
# sapply(data, typeof)

# Load Stop Signal Task - individual level data ---------------------------

sst <- readABCDdata('abcd_sst02.txt')
# URL: https://nda.nih.gov/data_structure.html?short_name=abcd_sst02
# 1. tfmri_sst_beh_performflag: 	Whether the participant had an acceptable 
# performance in the task; all trial types must yield more than 3 events 
# for both positive and negative feedback
# 2. tfmri_sst_beh_glitchflag: a coding error wherein a fast response (<50msec)
# made when the Stop Signal Delay (SSD) was 50 msec results in all subsequent
# Stop trials recording this same fast response. This error disrupts the 
# functioning of the SSD tracking algorithm. Although useful data (before the
# occurrence of the coding error) may be retrievable for many of these
# participants, we identify for possible exclusion all participants with this
# coding error (variable tfmri_sst_beh_glitchflag).
# 3. tfmri_sst_beh_0SSDcount: A second concern relates to the Go stimuli not
# being presented on trials in which the SSD was 0 msec. On these trials,
# participants saw only the Stop signal (up arrow) which may have created
# confusion for participants. Across the full sample of participants, the
# impact of these occasional trials appears to be minimal for both the Stop
# Signal Reaction Time calculation and brain activation. However, we now provide
# a count of 0ms SSD trials per participant (variable tfmri_sst_beh_0SSDcount)
# which investigators can use to set a threshold for excluding participants, if desired.
# 4. tfmri_sst_beh_violatorflag: Finally, in accordance with race model assumptions
# and best practices (Verbruggen et al., eLife 2019) the Stop Signal Reaction Time
# should not be estimated for participants whose Stop Fail RT > Go RT. These
# participants are identified by the variable tfmri_sst_beh_violatorflag.
# However, there is no compelling reason to exclude these participants from
# brain activation analyses unless researchers believe that doing so is
# warranted given the neuroimaging measurements they are investigating or
# given their specific research question.
# 5. tfmri_sst_all_beh_total_mssrt: Stop Signal Reaction Time, mean estimation
# 6. tfmri_sst_all_beh_total_issrt: Stop Signal Reaction Time, integration estimation
# We provide two estimates of the Stop Signal Reaction Time
# (Logan et al., Psychological Review, 1984), a derived measure of the
# response inhibition process.
# Note for 5: The mean method (tfmri_sst_all_beh_total_mssrt) subtracts each participant’s
# mean SSD from their mean Go RT. (In previous data releases this variable was
# labelled tfmri_sst_all_beh_total_meanrt).
# The second estimate uses the integration method (tfmri_sst_all_beh_total_issrt)
# in which the mean SSD is subtracted from the nth Go RT with n being the
# participant’s overall successful inhibition rate. For the integration method
# calculation, any Go RT omissions were replaced with the longest go RT for
# that participant. In addition, premature responses on Stop trials
# (i.e., choice responses made before the Stop signal was presented) were 
# included when calculating the participant’s probability of successful
# stopping and the SSD’s on these trials were included in calculating the average SSD.
# 27. tfmri_mid_all_beh_hlpfb_rate
sst <- select(sst,c(src_subject_id,interview_age,sex,eventname,
                    tfmri_sst_beh_performflag,
                    tfmri_sst_beh_glitchflag,
                    tfmri_sst_beh_0ssdcount,
                    tfmri_sst_all_beh_total_mssrt,
                    tfmri_sst_all_beh_total_issrt))
sapply(sst, typeof)
sst = mutate(sst,across(.cols=5:ncol(sst), .fns=as.numeric))
sapply(sst, typeof)
colnames(sst) <- c('src_subject_id','interview_age','sex','eventname',
                         'SST_PerformFlag',
                         'SST_GlitchFlag',
                         'SST_ZeroSSDCountsFlag',
                         'SST_SSRT_MeanEst',
                         'SST_SSRT_IntegEst')
sst$PerformFlag = replace_na(sst$PerformFlag,0)
sst$GlitchFlag = replace_na(sst$GlitchFlag,0)
saveRDS(sst,'I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataPreprocessing/ABCD4.0_SST_behavWithQC.rds')

