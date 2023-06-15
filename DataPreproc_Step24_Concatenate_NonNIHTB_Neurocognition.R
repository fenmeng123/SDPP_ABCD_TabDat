library(bruceR)

setwd("I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing")

mid <- readRDS('ABCD4.0_MID_behavWithQC.rds')
sst <- readRDS('ABCD4.0_SST_behavWithQC.rds')
enback <- readRDS('ABCD4.0_EnBack_behavWithQC.rds')

nonNIHTB <- readRDS('ABCD4.0_Merged_NeurocognitionTask.rds')

# Concatenate all data frame
data = merge(mid,sst,by = intersect(colnames(mid),colnames(sst)),all = T)
data = merge(data,enback,by = intersect(colnames(data),colnames(enback)),all = T)
data = merge(data,nonNIHTB,by = intersect(colnames(data),colnames(nonNIHTB)),all = T)

data$MID_QC_Flag = data$MID_TrialFlag & data$MID_PerformFlag & data$MID_FeedbackFlag

data$MID_RT_RewardDiff <- data$MID_RT_Neutral - data$MID_RT_Reward
data$MID_RT_LossDiff <- data$MID_RT_Neutral - data$MID_RT_Loss

data$SST_QC_Flag = data$SST_PerformFlag & !data$SST_GlitchFlag & (data$SST_ZeroSSDCountsFlag<30)

DDT <- select(data,c(src_subject_id,eventname,
                     DDT_CompletedFlag,DDT_Validity,
                     DDT_IndifPnt_6h,DDT_IndifPnt_1day,DDT_IndifPnt_1week,
                     DDT_IndifPnt_1mth,DDT_IndifPnt_3mth,DDT_IndifPnt_1yr,
                     DDT_IndifPnt_5yr))
DDT <- subset(DDT,(eventname!='baseline_year_1_arm_1'))

DDT$DDT_k <- NA
DDT$DDT_JBCriteria_1 <- NA
DDT$DDT_JBCriteria_2 <- NA
DDT$ConvergenceFlag <- NA
for (i in 1:nrow(DDT)){
  SingleSubData <- DDT[i,]
  if (sum(is.na(SingleSubData))>=7){
    next
  }else{
    tmp <- select(SingleSubData,c(DDT_IndifPnt_6h,
                                  DDT_IndifPnt_1day,
                                  DDT_IndifPnt_1week,
                                  DDT_IndifPnt_1mth,
                                  DDT_IndifPnt_3mth,
                                  DDT_IndifPnt_1yr,
                                  DDT_IndifPnt_5yr))
    tmp <- t(tmp)
    tmp <- as.data.frame(tmp)
    colnames(tmp) <- 'indifference'
    DDT$DDT_JBCriteria_1[i] <- (sum(diff(tmp$indifference)<20)==6)
    DDT$DDT_JBCriteria_2[i] <- tail(tmp$indifference,1) - tmp$indifference[1] < -10
    tmp$delay <- c(6/24/30,1/30,0.25,1,3,12,60)
    res <- nls(indifference ~ 100 / (1 + k*delay), data = tmp,
               start = list(k = 1),control = list(maxiter=50000))
    DDT$DDT_k[i] <- coef(res)
    DDT$ConvergenceFlag[i] <- res$convInfo$isConv
    print(coef(res))
  }
}
DDT <- select(DDT,-c(DDT_CompletedFlag,DDT_Validity,
                     DDT_IndifPnt_6h,
                     DDT_IndifPnt_1day,
                     DDT_IndifPnt_1week,
                     DDT_IndifPnt_1mth,
                     DDT_IndifPnt_3mth,
                     DDT_IndifPnt_1yr,
                     DDT_IndifPnt_5yr))
data <- merge(data,DDT,by = c('src_subject_id','eventname'),all = T)
data$DDT_QC_Flag <- (data$ConvergenceFlag & data$DDT_JBCriteria_1 & data$DDT_JBCriteria_2)



data$lmt_scr_perc_correct[data$lmt_scr_perc_correct>1] <- NA
data$lmt_scr_perc_correct[data$lmt_scr_perc_correct<0] <- NA
data$lmt_scr_efficiency <- data$lmt_scr_perc_correct/data$lmt_scr_rt_correct




data <- select(data,c(src_subject_id,eventname,
                      interview_age,sex,
                      MID_QC_Flag,MID_RT_RewardDiff,MID_RT_LossDiff,
                      MID_ACC_Reward_byAllReward,MID_ACC_Loss_byAllLoss,
                      SST_QC_Flag,SST_SSRT_IntegEst,
                      ENback_PerformFlag,ENback_Acc_all,
                      REC_Mean_CorrAcc,
                      WISCV_ScaledScore,
                      RAVLT_Immediate,RAVLT_Learning,RAVLT_Forgetting,RAVLT_PercForget,
                      DDT_QC_Flag,DDT_k,
                      EmST_Acc_Diff,EmST_MeanRT_Diff,
                      GDT_NetScore,
                      SIT_Counts_Flips,
                      BEDFS_Sum))

saveRDS(data,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_NonNIHTB_Neurocognition.rds")


# data %>% 
#   group_by(eventname) %>%
#   summarise(count_na = sum(is.na(MID_RT_Reward))) -> counts
