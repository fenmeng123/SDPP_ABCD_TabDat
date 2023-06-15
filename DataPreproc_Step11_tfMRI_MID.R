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
readABCDdata_varDescrip<-function(filename){
  data = read.table(filename,header = TRUE,sep = '\t')
  data <- subset(data, select = -c(collection_id, dataset_id,collection_title))
  var.descrip <- data[1,]
  return(var.descrip)
}

mid_p1 <- readABCDdata('midaparc03.txt')
mid_p2 <- readABCDdata('midaparcp203.txt')
mid_p1$tfMRI_MID_meanFD <- mid_p1$tfmri_mid_all_b_meanmotion
mid_p1 <- select(mid_p1,-c(midaparc03_id,tfmri_mid_all_b_visitid,subjectkey,
                           tfmri_mid_all_b_tr,tfmri_mid_all_b_numtrs,
                           tfmri_mid_all_b_dof,tfmri_mid_all_b_nvols,
                           tfmri_mid_all_b_subthreshnvols,
                           tfmri_mid_all_b_meanmotion,tfmri_mid_all_b_maxmotion,
                           tfmri_mid_all_b_meantrans,tfmri_mid_all_b_maxtrans,
                           tfmri_mid_all_b_meanrot,tfmri_mid_all_b_maxrot))
mid_p2 <- select(mid_p2,-c(midaparcp203_id,subjectkey))
# tfmri-MID variable naming reference:
# example: tfmri_ma_acdn_b_scs_cbwmlh |scs indicates ASEG ROI (subcortical structure)
# a        tfmri_ma_arvn_b_cds_bkslh  |cds indicates APARC ROI (cortical DK atlas)
# formate: tfmri_ma_***(contrast name)_b_scs_***(ROI name)
# Contrast abbreviations:
################################################# 
#                midaparc03.txt                 #
################################################# 
# 1 acdn/arvn - all anticipation of reward versus neutral contrast
# 2 acvn - all anticipation of loss versus neutral contrast
# 3 rpvnfb - all reward positive versus negative feedback contrast
# 4 lpvnfb/lvnfb - all loss positive versus negative feedback contrast
# 5 alrvn - all anticipation of large reward versus neutral contrast
# 6 asrvn - all anticipation of small reward versus neutral contrast
# 7 alvcr/alvsr - all anticipation of large reward versus small reward contrast
# 8 aclvn/allvn - all anticipation of large loss versus neutral contrast
# 9 acmvn/asvn - all anticipation of small loss versus neutral contrast
# 10 acgml/alvsl - all anticipation of large versus small loss contrast
# 10 alvsl - all anticipation of large versus small loss contrast
################################################# 
#               midaparcp203.txt                #
#################################################
# 1 arvn - all anticipation of reward versus neutral contrast
# 2 acvn - all anticipation of loss versus neutral contrast
# 3 rpvnfb - all reward positive versus negative feedback contrast
# 4 lvnfb - all loss positive versus negative feedback contrast
# 5 alrvn
# 6 asrvn
# 7 alvsr - all anticipation of large reward versus small reward contrast
# 8 allvn - all anticipation of large loss versus neutral contrast
# 9 asvn - all anticipation of small loss versus neutral contrast
# 10 alvsl - all anticipation of large versus small loss contrast
##################################################
mid <- merge(mid_p1,mid_p2,by = c('src_subject_id','interview_date',
                                  'interview_age','sex','eventname'),all = TRUE)
mid <- select(mid,src_subject_id,interview_date,interview_age,sex,eventname,tfMRI_MID_meanFD,everything())
rm(mid_p1,mid_p2)
gc()
mid_vardesc_p1 <- readABCDdata_varDescrip('midaparc03.txt')
mid_vardesc_p2 <- readABCDdata_varDescrip('midaparcp203.txt')
mid_vardesc_p1$tfMRI_MID_meanFD <- mid_vardesc_p1$tfmri_mid_all_b_meanmotion
mid_vardesc_p1 <- select(mid_vardesc_p1,-c(midaparc03_id,tfmri_mid_all_b_visitid,subjectkey,
                           tfmri_mid_all_b_tr,tfmri_mid_all_b_numtrs,
                           tfmri_mid_all_b_dof,tfmri_mid_all_b_nvols,
                           tfmri_mid_all_b_subthreshnvols,
                           tfmri_mid_all_b_meanmotion,tfmri_mid_all_b_maxmotion,
                           tfmri_mid_all_b_meantrans,tfmri_mid_all_b_maxtrans,
                           tfmri_mid_all_b_meanrot,tfmri_mid_all_b_maxrot))
mid_vardesc_p2 <- select(mid_vardesc_p2,-c(midaparcp203_id,subjectkey))
mid_vardesc <- merge(mid_vardesc_p1,mid_vardesc_p2,
                     by = c('src_subject_id','interview_date',
                            'interview_age','sex','eventname'),all = TRUE)
rm(mid_vardesc_p1,mid_vardesc_p2)
gc()
# rename mid beta weight variables
# 1 acdn/arvn - all anticipation of reward versus neutral contrast
# 2 acvn - all anticipation of loss versus neutral contrast
# 3 rpvnfb - all reward positive versus negative feedback contrast
# 4 lpvnfb/lvnfb - all loss positive versus negative feedback contrast
# 5 alrvn - all anticipation of large reward versus neutral contrast
# 6 asrvn - all anticipation of small reward versus neutral contrast
# 7 alvcr/alvsr - all anticipation of large reward versus small reward contrast
# 8 aclvn/allvn - all anticipation of large loss versus neutral contrast
# 9 acmvn/asvn - all anticipation of small loss versus neutral contrast
# 10 acgml/alvsl - all anticipation of large versus small loss contrast
Contrast_RegPattern <- c('(.*_acdn_.*)|(.*_arvn_.*)','(.*_acvn_.*)',
                        '(.*_rpvnfb_.*)','(.*_lpvnfb_.*)|(.*_lvnfb_.*)',
                        '(.*_alrvn_*)','(.*_asrvn_*)',
                        '(.*_alvcr_.*)|(.*_alvsr_.*)','(.*_aclvn_.*)|(.*_allvn_.*)',
                        '(.*_acmvn_.*)|(.*_asvn_.*)','(.*_acgml_.*)|(.*_alvsl_.*)')
Contrast_newname <- c('MID_Anticipat_Reward_VS_Neutral','MID_Anticipat_Loss_VS_Neutral',
                      'MID_Feedback_Reward_Pos_VS_Neg','MID_Feedback_Loss_Pos_VS_Neg',
                      'MID_Anticipat_LargeReward_VS_Neutral','MID_Anticipat_SmallReward_VS_Neutral',
                      'MID_Anticipat_Reward_Large_VS_Small','MID_Anticipat_LargeLoss_VS_Neutral',
                      'MID_Anticipat_SmallLoss_VS_Neutral','MID_Anticipat_Loss_Large_VS_Small')
for (i in 1:length(Contrast_RegPattern)){
  index <- grepl(Contrast_RegPattern[i],colnames(mid))
  cat(sprintf('%d variables were found in contrast: %s\n', sum(index), Contrast_newname[i]))
  splited.varname <- strsplit(colnames(mid)[index],'_')
  newname = list()
  for (j in 1:sum(index)){
    newname[j] <- paste(Contrast_newname[i],splited.varname[[j]] %>% tail(1),sep = '_')
  }
  colnames(mid)[index] <- newname
}
# colnames(mid)
saveRDS(mid,'I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_MID_BetaWeight.rds')

for (i in 1:length(Contrast_RegPattern)){
  index <- grepl(Contrast_RegPattern[i],colnames(mid_vardesc))
  cat(sprintf('%d variables were found in contrast: %s\n', sum(index), Contrast_newname[i]))
  splited.varname <- strsplit(colnames(mid_vardesc)[index],'_')
  newname = list()
  for (j in 1:sum(index)){
    newname[j] <- paste(Contrast_newname[i],splited.varname[[j]] %>% tail(1),sep = '_')
  }
  colnames(mid_vardesc)[index] <- newname
}
# colnames(mid_vardesc)

saveRDS(mid_vardesc,'I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_MID_ROIname.rds')
