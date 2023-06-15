library(bruceR)
readABCDdata<-function(filename){
  data = read.table(filename,header = TRUE,sep = '\t')
  # remove some specific columns which are same across all .txt files
  data <- subset(data, select = -c(collection_id, dataset_id,collection_title))
  # get variable descriptions
  var.descrip <- data[1,]
  # remove the first row
  data<-data[-1,]
  # add comments to all columns
  for (i in 1:length(var.descrip)){
    comment(data[,i])<-var.descrip[1,i]
  }
  return(data)
}

setwd("I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing")

# Part A: Concatenate all behavioral measures -----------------------------
STQ = readRDS('ABCD4.0_STQ_RECODE_Comb_7level.rds')
MH = readRDS('ABCD4.0_MentalHealth_CBCL.rds')
NIHTB = readRDS('ABCD4.0_NIHTB.rds')
NonNIHTB = readRDS('ABCD4.0_Merged_NonNIHTB_Neurocognition.rds')
SLEEP = readRDS('ABCD4.0_SleepDistub.rds')
CulEnv = readRDS('ABCD4.0_Merged_CulEnv.rds')

STQ <- select(STQ,-c(subjectkey,
                     open_weekday_TV,open_weekday_Video,
                     open_weekday_SingleGame,open_weekday_MultiGame,
                     open_weekday_Text,open_weekday_SocialApps,
                     open_weekday_EditPhoto,open_weekday_VideoChat,
                     open_weekday_Surf,open_weekend_TV,
                     open_weekend_Video,open_weekend_SingleGame,
                     open_weekend_MultiGame,open_weekend_Text,
                     open_weekend_SocialApps,open_weekend_EditPhoto,
                     open_weekend_VideoChat,open_weekend_Surf,
                     NumAccount_facebook,NumAccount_Instagram,
                     NumAccount_snapchat,NumAccount_twitter,
                     NumAccount_youtube,NumAccount_pinterest,
                     NumAccount_tumblr,NumAccout_reddit,
                     NumAccout_mg_chat,NumAccount_tiktok,
                     NumAccount_other,SMS_Own_facebook,
                     SMS_Own_instagrm,SMS_Own_snapchat,
                     SMS_Own_twitter,SMS_Own_youtube,
                     SMS_Own_pinterest,SMS_Own_tumblr,
                     SMS_Own_reddit,SMS_Own_mg_chat,
                     SMS_Own_other,SMS_Own_tiktok,
                     Ownership_PubSM_Account,
                     Ownership_SMFollowers,
                     Ownership_SMFollowing,
                     Ownership_SecretAccount))
STQ <- naniar::replace_with_na(STQ,list(VGAS_1 = 777,
                                        VGAS_2 = 777,
                                        VGAS_3 = 777,
                                        VGAS_4 = 777,
                                        VGAS_5 = 777,
                                        VGAS_6 = 777,
                                        SMAS_1 = 777,
                                        SMAS_2 = 777,
                                        SMAS_3 = 777,
                                        SMAS_4 = 777,
                                        SMAS_5 = 777,
                                        SMAS_6 = 777,
                                        MPIQ_1 = 777,
                                        MPIQ_2 = 777,
                                        MPIQ_3 = 777,
                                        MPIQ_4 = 777,
                                        MPIQ_5 = 777,
                                        MPIQ_6 = 777,
                                        MPIQ_7 = 777,
                                        MPIQ_8 = 777))
STQ$VGAS_Sum = STQ$VGAS_1 + STQ$VGAS_2 + STQ$VGAS_3 + STQ$VGAS_4 + 
  STQ$VGAS_5 + STQ$VGAS_6
STQ$SMAS_Sum = STQ$SMAS_1 + STQ$SMAS_2 + STQ$SMAS_3 + STQ$SMAS_4 + 
  STQ$SMAS_5 + STQ$SMAS_6
STQ$MPIQ_Sum = STQ$MPIQ_1 + STQ$MPIQ_2 + STQ$MPIQ_3 + STQ$MPIQ_4 + 
  STQ$MPIQ_5 + STQ$MPIQ_6 + STQ$MPIQ_7 + STQ$MPIQ_8
STQ <- select(STQ,-c(SMAS_1,SMAS_2,
                     SMAS_3,SMAS_4,SMAS_5,SMAS_6,
                     VGAS_1,VGAS_2,VGAS_3,VGAS_4,
                     VGAS_5,VGAS_6,MPIQ_1,
                     MPIQ_2,MPIQ_3,MPIQ_4,MPIQ_5,
                     MPIQ_6,MPIQ_7,MPIQ_8))
STQ$SABT_Sum <- STQ$SABT_TV + STQ$SABT_Game + STQ$SABT_Music + STQ$SABT_Text + 
  STQ$SABT_SocialNet + STQ$SABT_ChatRoom + STQ$SABT_Surf +STQ$SABT_Study + STQ$SABT_Read
STQ <- select(STQ,-c(SABT_TV,SABT_Game,
                     SABT_Music,SABT_Text,SABT_SocialNet,SABT_ChatRoom,
                     SABT_Surf,SABT_Study,SABT_Read,SABT_PhoneAction))
STQ$Sleep_PhoneAction = RECODE(STQ$Sleep_PhoneAction,
                               "-1='Not Applicable' ; 1='Turn off' ; 2='Silent' ; 3='Ringer On' ; 4='Outside';else=NA")
STQ$Sleep_WakeBefore = RECODE(STQ$Sleep_WakeBefore,
                              "-1='Not Applicable' ; 1='Turn off' ; 2='Silent' ; 3='Ringer On' ; 4='Outside';else=NA")
STQ$Sleep_WakeDuring = RECODE(STQ$Sleep_WakeDuring,
                              "-1='Not Applicable' ; 1='Turn off' ; 2='Silent' ; 3='Ringer On' ; 4='Outside';else=NA")
stq = readABCDdata('I:\\ABCDStudyNDA\\Download_ABCDV4.0_skr220403\\Package_1199282\\abcd_stq01.txt')
stq = subset(stq,select = c(src_subject_id,interview_date,sex,eventname,screen13_y,screen14_y))
colnames(stq)[5:6] <- c("MatureRated_VideoGame","R_Rated_Movies")
STQ = merge(STQ,stq,by=c('src_subject_id','interview_date','sex','eventname'))

MH$UPPS_Sum = MH$UPPS_NegUrge + MH$UPPS_PosUrge + MH$UPPS_SenSeek + 
  MH$UPPS_LackPlan + MH$UPPS_LackPersev

MH$BAS_Sum = MH$BAS_Drive + MH$BAS_RR + MH$BAS_FS

MH <- select(MH,-c(subjectkey))
NIHTB <- select(NIHTB,-c(subjectkey))

SLEEP <- select(SLEEP,c(src_subject_id,interview_age,sex,eventname,SleepDistub_sum))

data_behav = merge(STQ,MH,by = intersect(colnames(STQ),colnames(MH)),all = T)
data_behav = merge(data_behav,SLEEP,by = intersect(colnames(data_behav),colnames(SLEEP)),all = T)
data_behav = merge(data_behav,NIHTB,by = intersect(colnames(data_behav),colnames(NIHTB)),all = T)
data_behav = merge(data_behav,NonNIHTB,by = intersect(colnames(data_behav),colnames(NonNIHTB)),all = T)
data_behav = merge(data_behav,CulEnv,by = intersect(colnames(data_behav),colnames(CulEnv)),all = T)

# Part B: Neuroimaging-derived Measures -----------------------------------
SMRI <- readRDS('ABCD4.0_SMRI.rds')
RSFNC <- readRDS('ABCD4.0_RSFNC.rds')
DTI = readRDS('ABCD4.0_DTI.rds')
Imgincl = readRDS('ABCD4.0_ImgIncl.rds')

SMRI <- select(SMRI,-c(subjectkey))
RSFNC <- select(RSFNC,-c(subjectkey,interview_age))
DTI <- select(DTI,-c(interview_age))
Imgincl <- select(Imgincl,-c(interview_age,visit,
                             imgincl_t2w_include,imgincl_mid_include,
                             imgincl_nback_include,imgincl_sst_include))

data_brain = merge(SMRI,RSFNC,by = c('src_subject_id','interview_date','sex','eventname'),all = T)
data_brain = merge(data_brain,DTI,by = c('src_subject_id','interview_date','sex','eventname'),all = T)
data_brain = merge(data_brain,Imgincl,by = c('src_subject_id','interview_date','sex','eventname'),all = T)


# Final Part: Concatenate Behavioral and Brain Measures -------------------
# Concatenate behavioral measures with Demographic variables
Demographic = readRDS('ABCD4.0_Demographic_imputed.rds')
Demographic <- select(Demographic,-subjectkey)
data_behav$interview_age <- as.numeric(data_behav$interview_age)
data_behav = merge(Demographic,data_behav,by = intersect(colnames(Demographic),colnames(data_behav)),all = T)
data_behav$FamilyIncomeR <- as.numeric(data_behav$FamilyIncome)
data_behav$HouseholdSizeR <- as.numeric(data_behav$HouseholdSize)
# Re-order columns 
data_behav <- select(data_behav,c(src_subject_id,interview_age,interview_date,
                                  eventname,sex,
                      EducationC,EducationR,
                      SexAssigned,GenderIdentity_PrntRep,
                      Race_PrntRep,Ethnicity_PrntRep,RaceEthnicity,
                      ParentMarital,ParentsEdu,
                      ParentsMaritalEmploy,ParentMaritalC,
                      FamilyIncomeR,FamilyIncome,
                      HouseholdStructure,HouseholdSizeR,HouseholdSize,
                      BirthCountry,Religon_PrntRep,
                      FamilyID,Relationship,Handedness,
                      ACS_weight,site_id_l,BMI_calc,
                      weekday_TV,weekday_Video,weekday_Game,
                      weekday_Text,weekday_SocialNet,weekday_VideoChat,
                      weekend_TV,weekend_Video,weekend_Game,
                      weekend_Text,weekend_SocialNet,weekend_VideoChat,
                      MatureRated_VideoGame,R_Rated_Movies,
                      VGAS_Sum,SMAS_Sum,MPIQ_Sum,SABT_Sum,
                      Ownership_SMAccount,
                      Ownership_MobilePhone,
                      Ownership_DeviceInBedroom,
                      Ownership_Past_DatingApps,
                      Ownership_Current_DatingApps,
                      Ownership_InpersonDateMeet,
                      Attachment_MobilePhone,
                      Sleep_PhoneAction,Sleep_WakeBefore,Sleep_WakeDuring,
                      SMSite_NumCounts,SMAccount_NumTotal,MostUse_SMSite,
                      weekday_TPD,weekend_TPD,TimePerDay_SM,
                      CBCL_syn_anxdep_t,CBCL_syn_withdep_t,CBCL_syn_somatic_t,
                      CBCL_syn_social_t,CBCL_syn_thought_t,CBCL_syn_attention_t,
                      CBCL_syn_rulebreak_t,CBCL_syn_aggressive_t,
                      CBCL_syn_internal_t,CBCL_syn_external_t,CBCL_syn_totprob_t,
                      CBCL_dsm5_depress_t,CBCL_dsm5_anxdisord_t,
                      CBCL_dsm5_somaticpr_t,CBCL_dsm5_adhd_t,
                      CBCL_dsm5_opposit_t,CBCL_dsm5_conduct_t,
                      CBCL_07_sct_t,CBCL_07_ocd_t,CBCL_07_stress_t,
                      UPPS_Sum,BAS_Sum,BIS_Sum,
                      PPS_Number_Sum,PPS_Severity_Mean,
                      PLE_BadEvent_Sum,PLE_Affected_BadEvent_Mean,
                      PEQ_Rela_Victim,PEQ_Repu_Victim,PEQ_Overt_Victim,
                      PEQ_Rela_Aggs,PEQ_Repu_Aggs,PEQ_Overt_Aggs,
                      Mania_7UP_Sum,GISH_Male_Sum,GISH_Female_Sum,SleepDistub_sum,
                      NIHTB_picvocab_fc,NIHTB_flanker_fc,
                      NIHTB_list_fc,NIHTB_cardsort_fc,
                      NIHTB_pattern_fc,NIHTB_picture_fc,
                      NIHTB_reading_fc,NIHTB_fluidcomp_fc,
                      NIHTB_cryst_fc,NIHTB_totalcomp_fc,
                      MID_QC_Flag,MID_RT_RewardDiff,MID_RT_LossDiff,
                      MID_ACC_Reward_byAllReward,MID_ACC_Loss_byAllLoss,
                      SST_QC_Flag,SST_SSRT_IntegEst,
                      ENback_PerformFlag,ENback_Acc_all,REC_Mean_CorrAcc,
                      WISCV_ScaledScore,
                      RAVLT_Immediate,RAVLT_Learning,RAVLT_Forgetting,RAVLT_PercForget,
                      DDT_QC_Flag,DDT_k,
                      EmST_Acc_Diff,EmST_MeanRT_Diff,
                      GDT_NetScore,SIT_Counts_Flips,BEDFS_Sum,
                      PBS_Sum,WPS_Sum,Pet_Ownership,MNBS_All_Sum,MNBS_MonSupv_Sum,
                      MNBS_EduSupp_Sum,PMS_Sum,CRPBI_Parent_Sum,CRPBI_Caregiver_Sum,
                      FES_Conflict_Sum,FES_IntCult_Sum,FES_ActRec_Sum,
                      FES_Org_Sum,FES_Cohesion_Sum,FES_Expre_Sum,
                      NSC_Youth,NSC_Parent_Mean,PBP_Prosocial_Sum,
                      PBP_RuleBreakDel_Sum,PNH_Protective_Sum,
                      SRPFS_Env_Sum,SRPFS_Inv_Sum,SRPFS_Dis_Sum,
                      MACVS_Referrent_Sum,MACVS_Support_Sum,MACVS_Obligation_Sum,
                      MACVS_Religion_Sum,MACVS_ISR_Sum,
                      Accult_English_Proficiency,Accult_OtherLang_Ownership,
                      Accult_OtherLang_Proficiency,Accult_OtherLang_Type,
                      Accult_OtherLang_Other,Accult_LangWithFriends,
                      Accult_LangWithFamily))
# fill the empty holes about demographics in data_behav  ----------------
baseline <- subset(data_behav,eventname=="baseline_year_1_arm_1")
followup <- subset(data_behav,eventname!="baseline_year_1_arm_1")
for (i_subID in baseline$src_subject_id){
  cat(sprintf('#SubID:%s Filling holes in demographics for follow-up data...\n',i_subID))
  sub_index <- grep(i_subID,followup$src_subject_id)
  followup[sub_index,6:26] <- baseline[i_subID,6:26]
}
data_behav <- rbind(baseline,followup)

data = merge(data_behav,data_brain,by = intersect(colnames(data_behav),colnames(data_brain)),all = T)

# fill the empty ACS weights in follow-up data 
acs_weight <- readABCDdata('I:\\ABCDStudyNDA\\Download_ABCDV4.0_skr220403\\Package_1199282\\acspsw03.txt')
acs_weight <- subset(acs_weight,
                     select = c(src_subject_id,interview_age,interview_date,eventname,sex,
                                acs_raked_propensity_score,
                                genetic_paired_subjectid_1,genetic_paired_subjectid_2,
                                genetic_paired_subjectid_3,genetic_paired_subjectid_4,
                                genetic_af_american,genetic_af_african,
                                genetic_af_european,genetic_af_east_asian))
acs_weight$interview_age <- as.numeric(acs_weight$interview_age)
colnames(acs_weight)[6:ncol(acs_weight)] <- c("ACS_weight",
                                              "genetic_paired_SubID_1",
                                              "genetic_paired_SubID_2",
                                              "genetic_paired_SubID_3",
                                              "genetic_paired_SubID_4",
                                              "genetic_ancestry_american",
                                              "genetic_ancestry_african",
                                              "genetic_ancestry_european",
                                              "genetic_ancestry_eastasian")
data <- merge(select(data,-ACS_weight),acs_weight,
              by = intersect(colnames(select(data,-ACS_weight)),colnames(acs_weight)),all = T)
data <- select(data,c(src_subject_id,interview_age,interview_date,eventname,sex,
                      EducationC,EducationR,
                      SexAssigned,GenderIdentity_PrntRep,
                      Race_PrntRep,Ethnicity_PrntRep,RaceEthnicity,
                      ParentMarital,ParentsEdu,
                      ParentsMaritalEmploy,ParentMaritalC,
                      FamilyIncomeR,FamilyIncome,
                      HouseholdStructure,HouseholdSizeR,HouseholdSize,
                      BirthCountry,Religon_PrntRep,
                      FamilyID,Relationship,Handedness,
                      ACS_weight,site_id_l,BMI_calc,everything()))
data$ACS_weight <- as.numeric(data$ACS_weight)
data = mutate(data,across(.cols=101:110, .fns=as.numeric))
data = mutate(data,across(.cols=133:ncol(data), .fns=as.numeric))
sapply(data, typeof)
saveRDS(data,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_Demo_STQ_NeuroCog_MH_CulEnv_NonTaskMRI.rds")
data_behav = data[,1:160]
write.csv(data_behav,'I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_Demo_STQ_NeuroCog_MH_CulEnv.csv')
data_brain = data[,c(1:5,161:ncol(data))]
write.csv(data_brain,'I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_NonTaskMRI.csv')
