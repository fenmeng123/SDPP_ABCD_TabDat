library(bruceR)

setwd('I:\\ABCDStudyNDA\\Download_ABCDV4.0_skr220403\\Package_1199282')

readABCDdata<-function(filename){
  data = read.table(filename,header = TRUE,sep = '\t')
  # remove some specific columns which are same across all .txt files
  data <- subset(data, select = -c(collection_id, dataset_id,collection_title))
  # get variable descriptions
  # var.descrip <- data[1,]
  # remove the first row
  data<-data[-1,]
  # add comments to all columns
  # for (i in 1:length(var.descrip)){
  #   comment(data[,i])<-var.descrip[1,i]
  # }
  return(data)
}

replace_nm <-function(data,nm.var.name,var.name){
  MissIndex <- which(data[nm.var.name] !=0)
  cat("|----------------------------------------------------------------------------------------|\n")
  cat(sprintf("|Var:%s; number of missing answers:%d                       |\n",nm.var.name,length(MissIndex)))
  cat(sprintf("|target var:%s; values with any missing answers have been replaced by NA|\n",var.name))
  cat("| target var raw values (as indicated by the number of missing above 0)|\n")
  options(max.print=25) 
  print(data[MissIndex,var.name])
  options(max.print=1000) 
  cat("|----------------------------------------------------------------------------------------|\n")

  data[MissIndex,var.name] <- NA
  return(data)
}


# Extract Mental Health Sum Score
# includes: Youth Life Events (PLE), Prodominal Psychosis Symptom (PPS), UPPS, BIS/BAS, 
# 
# 

mhy_sum = readABCDdata('abcd_mhy02.txt')

mhy_sum = subset(mhy_sum,select = -c(abcd_mhy02_id,interview_age))
mhy_sum[,6:ncol(mhy_sum)] = apply(mhy_sum[,6:ncol(mhy_sum)],2,as.numeric)

mhy_anchor = subset(mhy_sum,select = c(subjectkey,src_subject_id,interview_date,sex,eventname))

# Notes about PPS
# pps_y_ss_number: Prodromal Psychosis Scale: Number of Yes Responses
# pps_y_ss_severity_score: Prodromal Psychosis: Severity Score 
# Sum: (prodromal_1b_y, prodromal_2b_y, prodromal_3b_y, prodromal_4b_y,
# prodromal_5b_y, prodromal_6b_y, prodromal_7b_y, prodromal_8b_y,
# prodromal_9b_y, prodromal_10b_y, prodromal_11b_y, prodromal_12b_y,
# prodromal_13b_y, prodromal_14b_y, [prodromal_15b_y, prodromal_16b_y,
# prodromal_17b_y, prodromal_18b_y, prodromal_19b_y, prodromal_20b_y, prodromal_21b_y) + (pps_y_ss_bother_n_1),
# If this score = ", then score = pps_y_ss_number; 
# In the Prodromal Psychosis survey, "How much did it bother you?" is typically scored 2 - 6. 
# In the version used by ABCD,  this item was scored  1 - 5. 
# Because the "Did it bother you" sum score is equal to the sum of the number of 
# severity scores used in the severity score calculation, 
# we were able to compensate for this scoring difference by adding the "Did it bother you" sum score
# to the severity score.  After applying this change, the severity scores reported here are comparable
# to those calculated using the  "How much did it bother you"  2 - 6  scoring range.

# Notes about UPPS
# upps_y_ss_negative_urgency: Negative Urgency: upps7_y + upps11_y + upps17_y + upps20_y;
# upps_y_ss_lack_of_planning: Lack of Planning: upps6_y + upps16_y + upps23_y + upps28_y;
# upps_y_ss_sensation_seeking: Sensation Seeking: upps12_y + upps18_y + upps21_y + upps27_y;
# upps_y_ss_positive_urgency: Positive Urgency: upps35_y + upps36_y + upps37_y + upps39_y;
# upps_y_ss_lack_of_perseverance: Lack of Perseverance (GSSF) upps15_y plus upps19_y plus upps22_y plus upps24_y

# Notes about BISBAS
# bis_y_ss_bis_sum: BIS Sum bisbas1_y + bisbas2_y + bisbas3_y + bisbas4_y + bisbas5_y + bisbas6_y+ bisbas7_y;
# bis_y_ss_bas_rr:  BAS Reward Responsiveness: bisbas8_y + bisbas9_y + bisbas10_y + bisbas11_y + bisbas12_y
# bis_y_ss_bas_drive: BAS drive: bisbas13_y + bisbas14_y + bisbas15_y + bisbas16_y;
# bis_y_ss_bas_fs: BAS Fun Seeking: bisbas17_y + bisbas18_y + bisbas19_y + bisbas20_y;
# bis_y_ss_bism_sum: BIS Sum (modified): bisbas2_y + bisbas3_y + bisbas4_y + bisbas6_y; 
# bis_y_ss_basm_rr: BAS Reward Responsiveness (modified): bisbas8_y + bisbas9_y + bisbas11_y + bisbas12_y;
# bis_y_ss_basm_drive: BAS drive (modified): bisbas13_y + bisbas14_y + bisbas15_y + bisbas16_y;

# Notes about GISH
# gish_y_ss_m_sum: Gish Male sum(gish_m1_y:gish_m4_y)
# gish_y_ss_f_sum: GISH Female (gish_f_y:gish_f4_y)

# Notes about Peer Experiences
# peq_ss_relational_victim: Peer Experiences: Relational Victimization Summary Score; 
#                           sum(peq_left_out_vic,peq_invite_vic, peq_exclude_vic);
# peq_ss_reputation_victim: Peer Experiences: Reputational Victimization Summary Score;
#                           sum(peq_rumor_vic, peq_gossip_vic, peq_loser_vic);
# peq_ss_overt_victim: Peer Experiences: Overt Victimization Summary Score;
#                     sum(peq_chase_vic, peq_threat_vic, peq_hit_vic);
# peq_ss_relational_aggs: Peer Experiences: Relational Aggression Summary Score; 
#                         sum(peq_left_out_perp, peq_invite_perp, peq_exclude_perp);
# peq_ss_reputation_aggs: Peer Experiences: Reputational Aggression Summary Score; 
#                         sum(peq_rumor_perp, peq_gossip_perp, peq_loser_perp);
# peq_ss_overt_aggression: Overt Aggression Summary Score; 
#                           sum (peq_chase_perp, peq_threat_perp, peq_hit_perp);

# Notes about 7-up Mania
# sup_y_ss_sum: 7UP sum ([sup_1_y]:[sup_7_y]) ;

# check missing answers ---------------------------------------------------

mhy_sum <- replace_nm(mhy_sum,"ple_y_ss_total_number_nm","ple_y_ss_total_good")
mhy_sum <- replace_nm(mhy_sum,"ple_y_ss_total_number_nm","ple_y_ss_total_bad")
mhy_sum <- replace_nm(mhy_sum,"ple_y_ss_total_number_nm","ple_y_ss_affected_good_sum")
mhy_sum <- replace_nm(mhy_sum,"ple_y_ss_total_number_nm","ple_y_ss_affected_good_mean")
mhy_sum <- replace_nm(mhy_sum,"ple_y_ss_total_number_nm","ple_y_ss_affected_bad_sum")
mhy_sum <- replace_nm(mhy_sum,"ple_y_ss_total_number_nm","ple_y_ss_affected_bad_mean")

mhy_sum <- replace_nm(mhy_sum,"pps_y_ss_number_nm","pps_y_ss_number")
# |The number of missing answers for PPS severity is very strange. It indicated|
# |all rows have missing values, which is impossible. Thus, I didn't apply the |
# |*_nm for the following two variables and keep the raw values from text files|
# |By Kunru Song 2022.11.14|
# mhy_sum <- replace_nm(mhy_sum,"pps_y_ss_severity_score_nm","pps_y_ss_severity_score")
# mhy_sum <- replace_nm(mhy_sum,"pps_y_ss_severity_score_nm","pps_ss_mean_severity")

mhy_sum <- replace_nm(mhy_sum,"upps_y_ss_negative_urgency_nm","upps_y_ss_negative_urgency")
mhy_sum <- replace_nm(mhy_sum,"upps_y_ss_lack_of_planning_nm","upps_y_ss_lack_of_planning")
mhy_sum <- replace_nm(mhy_sum,"upps_y_ss_sensation_seeking_nm","upps_y_ss_sensation_seeking")
mhy_sum <- replace_nm(mhy_sum,"upps_y_ss_positive_urgency_nm","upps_y_ss_positive_urgency")
mhy_sum <- replace_nm(mhy_sum,"upps_y_ss_lack_of_pers_nm","upps_y_ss_lack_of_perseverance")

mhy_sum <- replace_nm(mhy_sum,"bis_y_ss_bism_sum_nm","bis_y_ss_bism_sum")
mhy_sum <- replace_nm(mhy_sum,"bis_y_ss_basm_rr_nm","bis_y_ss_basm_rr")
mhy_sum <- replace_nm(mhy_sum,"bis_y_ss_basm_drive_nm","bis_y_ss_basm_drive")
mhy_sum <- replace_nm(mhy_sum,"bis_y_ss_bas_fs_nm","bis_y_ss_bas_fs")

mhy_sum <- replace_nm(mhy_sum,"gish_y_ss_m_sum_nm","gish_y_ss_m_sum")
mhy_sum <- replace_nm(mhy_sum,"gish_y_ss_f_sum_nm","gish_y_ss_f_sum")

mhy_sum <- replace_nm(mhy_sum,"peq_ss_relational_victim_nm","peq_ss_relational_victim")
mhy_sum <- replace_nm(mhy_sum,"peq_ss_reputation_victim_nm","peq_ss_reputation_victim")
mhy_sum <- replace_nm(mhy_sum,"peq_ss_overt_victim_nm","peq_ss_overt_victim")
mhy_sum <- replace_nm(mhy_sum,"peq_ss_relational_aggs_nm","peq_ss_relational_aggs")
mhy_sum <- replace_nm(mhy_sum,"peq_ss_reputation_aggs_nm","peq_ss_reputation_aggs")
mhy_sum <- replace_nm(mhy_sum,"peq_ss_overt_aggression_nm","peq_ss_overt_aggression")

mhy_sum <- replace_nm(mhy_sum,"sup_y_ss_sum_nm","sup_y_ss_sum")


# rewrite column names ----------------------------------------------------

mhy_score = subset(mhy_sum,select = c(ple_y_ss_total_good,ple_y_ss_affected_good_sum,ple_y_ss_affected_good_mean,
                                      ple_y_ss_total_bad,ple_y_ss_affected_bad_sum,ple_y_ss_affected_bad_mean,
                                      pps_y_ss_number,pps_y_ss_severity_score,pps_ss_mean_severity,
                                      upps_y_ss_negative_urgency,upps_y_ss_lack_of_planning,
                                      upps_y_ss_sensation_seeking,upps_y_ss_positive_urgency,
                                      upps_y_ss_lack_of_perseverance,
                                      bis_y_ss_bism_sum,
                                      bis_y_ss_basm_rr,bis_y_ss_basm_drive,bis_y_ss_bas_fs,
                                      gish_y_ss_m_sum,gish_y_ss_f_sum,
                                      peq_ss_relational_victim,peq_ss_reputation_victim,peq_ss_overt_victim,
                                      peq_ss_relational_aggs,peq_ss_reputation_aggs,peq_ss_overt_aggression,
                                      sup_y_ss_sum))

colnames(mhy_score) = c('PLE_GoodEvent_Sum','PLE_Affected_GoodEvent_Sum','PLE_Affected_GoodEvent_Mean',
                        'PLE_BadEvent_Sum','PLE_Affected_BadEvent_Sum','PLE_Affected_BadEvent_Mean',
                        'PPS_Number_Sum','PPS_Severity_Sum','PPS_Severity_Mean',
                        'UPPS_NegUrge','UPPS_LackPlan',
                        'UPPS_SenSeek','UPPS_PosUrge',
                        'UPPS_LackPersev',
                        'BIS_Sum',
                        'BAS_RR','BAS_Drive','BAS_FS',
                        'GISH_Male_Sum','GISH_Female_Sum',
                        'PEQ_Rela_Victim','PEQ_Repu_Victim','PEQ_Overt_Victim',
                        'PEQ_Rela_Aggs','PEQ_Repu_Aggs','PEQ_Overt_Aggs',
                        'Mania_7UP_Sum')
mhy_score = cbind(mhy_anchor,mhy_score)

cbcl_sum = readABCDdata('abcd_cbcls01.txt')

cbcl_anchor = subset(cbcl_sum,
                     select = c(subjectkey,src_subject_id,interview_date,sex,eventname))

cbcl_tscore = subset(cbcl_sum,select = grep('.*_t$',colnames(cbcl_sum),value = T))
cbcl_tscore = cbind(cbcl_anchor,cbcl_tscore)
colnames(cbcl_tscore) = stringr::str_replace(colnames(cbcl_tscore),'cbcl_','CBCL_')
colnames(cbcl_tscore) = stringr::str_replace(colnames(cbcl_tscore),'scr_','')


rm(cbcl_anchor,cbcl_dat,cbcl_desp,cbcl_sum,mhy_anchor,mhy_dat,mhy_desp,mhy_sum)
MentalHealth = merge(cbcl_tscore,mhy_score,by = intersect(colnames(cbcl_tscore),colnames(mhy_score)),all = T)
saveRDS(MentalHealth,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_MentalHealth_CBCL.rds") 
