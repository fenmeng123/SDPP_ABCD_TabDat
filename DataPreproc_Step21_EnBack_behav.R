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
nback <- readABCDdata('abcd_mrinback02.txt')
# URL: https://nda.nih.gov/data_structure.html?short_name=abcd_mrinback02
# fMRI Emotional nBack Working Memory Task
# The fMRI Emotional nBack task assesses memory, emotion and face and place
# perceptual processes. The task is a block design working memory task using
# four categories of stimuli (places, positive faces, negative faces and
# neutral faces) and two memory load conditions (0 and 2 back). 
# The task includes 80 trials for each of the two memory load conditions,
# 20 trials for each stimulus type in each of the two memory load conditions
# and 40 trials of each stimulus type.
# Accuracy and reaction time (RT) are provided for all conditions.
# When possible, participants who were unable to complete the task in the
# scanner, completed the task behaviorally outside the scanner.
# 1. tfmri_nback_beh_performflag: Poor performance in the nBack task is
# indicated if the overall response accuracy for the 0-back or 2-back blocks
# is less than 60%
# 2. tfmri_nb_all_beh_ctotal_rate: The rate of all correct responses
# 3. tfmri_nb_all_beh_ctotal_mrt: Mean reaction time for all correct responses	
nback <- select(nback,c(src_subject_id,interview_age,sex,eventname,
                        tfmri_nback_beh_performflag	,
                        tfmri_nb_all_beh_ctotal_rate,
                        tfmri_nb_all_beh_ctotal_mrt))
sapply(nback, typeof)
nback = mutate(nback,across(.cols=5:ncol(nback), .fns=as.numeric))
sapply(nback, typeof)
nback$PerformFlag = replace_na(nback$PerformFlag,0)
colnames(nback) <- c('src_subject_id','interview_age','sex','eventname',
                   'ENback_PerformFlag',
                   'ENback_Acc_all',
                   'ENback_RT_allcorrect')

rec <- readABCDdata('mribrec02.txt')
# URL: https://nda.nih.gov/data_structure.html?short_name=mribrec02
# 1. tfmri_rec_all_beh_posface_pr: Corrected accuracy for positive faces
# 2. tfmri_rec_all_beh_posface_br: Response bias for positive faces
# 3. tfmri_rec_all_beh_posf_dpr: D-prime for positive faces
# 4. tfmri_rec_all_beh_neutface_pr: Corrected accuracy for neutral faces
# 5. tfmri_rec_all_beh_neutface_br: Response bias for neutral faces
# 6. tfmri_rec_all_beh_neutf_dp: D-prime for neutral faces
# 7. tfmri_rec_all_beh_negface_pr: Corrected accuracy for negative faces
# 8. tfmri_rec_all_beh_negface_br: Response bias for negative faces
# 9. tfmri_rec_all_beh_negf_dp: D-prime for negative faces
# 10. tfmri_rec_all_beh_place_pr: Corrected accuracy for places
# 11. tfmri_rec_all_beh_place_br: Response bias for places
# 12. tfmri_rec_all_beh_place_dp: D-prime for places
rec <- select(rec,c(src_subject_id,interview_age,sex,eventname,
                        tfmri_rec_all_beh_posface_pr,
                        tfmri_rec_all_beh_posface_br,
                        tfmri_rec_all_beh_posf_dpr,
                        tfmri_rec_all_beh_neutface_pr,
                        tfmri_rec_all_beh_neutface_br,
                        tfmri_rec_all_beh_neutf_dp,
                        tfmri_rec_all_beh_negface_pr,
                        tfmri_rec_all_beh_negface_br,
                        tfmri_rec_all_beh_negf_dp,
                        tfmri_rec_all_beh_place_pr,
                        tfmri_rec_all_beh_place_br,
                        tfmri_rec_all_beh_place_dp))
sapply(rec, typeof)
rec = mutate(rec,across(.cols=5:ncol(rec), .fns=as.numeric))
sapply(rec, typeof)
colnames(rec) <- c('src_subject_id','interview_age','sex','eventname',
                     'REC_CorrAcc_PosFace',
                     'REC_RespBias_PosFace',
                     'REC_Dprime_PosFace',
                     'REC_CorrAcc_NeuFace',
                     'REC_RespBias_NeuFace',
                     'REC_Dprime_NeuFace',
                     'REC_CorrAcc_NegFace',
                     'REC_RespBias_NegFace',
                     'REC_Dprime_NegFace',
                     'REC_CorrAcc_Place',
                     'REC_RespBias_Place',
                     'REC_Dprime_Place')
rec$REC_Mean_CorrAcc = ( rec$REC_CorrAcc_PosFace + rec$REC_CorrAcc_NeuFace +
  rec$REC_CorrAcc_NegFace + rec$REC_CorrAcc_Place )/4

data = merge(nback,rec,by = intersect(colnames(nback),colnames(rec)),all = T)

data$ENback_PerformFlag = replace_na(data$ENback_PerformFlag,0)
saveRDS(data,'I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataPreprocessing/ABCD4.0_EnBack_behavWithQC.rds')








