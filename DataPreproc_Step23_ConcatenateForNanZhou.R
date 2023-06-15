library(bruceR)

setwd("I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing")

Demographic = readRDS('ABCD4.0_Demographic_imputed.rds')
STQ = readRDS('ABCD4.0_STQ_RECODE_Comb.rds')
MH = readRDS('ABCD4.0_MentalHealth_CBCL.rds')
NIHTB = readRDS('ABCD4.0_NIHTB.rds')

Demographic <- select(Demographic,-c(subjectkey,
                                     AdoptionYear,
                                     AdoptionFlag,
                                     SexAssigned,
                                     GenderIdentity_PrntRep,
                                     USALiveYears,
                                     ParentMaritalC,
                                     ParentHighEdu,
                                     PartnerHighEdu,
                                     FamliyEmploy.V1,
                                     FamliyEmploy.V2,
                                     RaceEthnicity,
                                     SameSexTwinFlag,
                                     EducationR))
STQ <- select(STQ,-c(subjectkey,
                     open_weekday_TV,open_weekday_Video,
                     open_weekday_SingleGame,open_weekday_MultiGame,open_weekday_Text,open_weekday_SocialApps,
                     open_weekday_EditPhoto,open_weekday_VideoChat,open_weekday_Surf,open_weekend_TV,
                     open_weekend_Video,open_weekend_SingleGame,open_weekend_MultiGame,open_weekend_Text,
                     open_weekend_SocialApps,open_weekend_EditPhoto,open_weekend_VideoChat,open_weekend_Surf,
                     NumAccount_facebook,NumAccount_Instagram,NumAccount_snapchat,NumAccount_twitter,
                     NumAccount_youtube,NumAccount_pinterest,NumAccount_tumblr,NumAccout_reddit,
                     NumAccout_mg_chat,NumAccount_tiktok,NumAccount_other,SMS_Own_facebook,
                     SMS_Own_instagrm,SMS_Own_snapchat,SMS_Own_twitter,SMS_Own_youtube,
                     SMS_Own_pinterest,SMS_Own_tumblr,SMS_Own_reddit,SMS_Own_mg_chat,
                     SMS_Own_other,SMS_Own_tiktok,
                     Ownership_PubSM_Account,Ownership_SMFollowers,
                     Ownership_SMFollowing,Ownership_SecretAccount))
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

MH$UPPS_Sum = MH$UPPS_NegUrge + MH$UPPS_PosUrge + MH$UPPS_SenSeek + 
  MH$UPPS_LackPlan + MH$UPPS_LackPersev

MH$BAS_Sum = MH$BAS_Drive + MH$BAS_RR + MH$BAS_FS

MH <- select(MH,-c(subjectkey))
NIHTB <- select(NIHTB,-c(subjectkey))

MID_behav <- readRDS('ABCD4.0_MID_behavWithQC.rds')
MID_behav <- select(MID_behav,c(src_subject_id,interview_age,sex,eventname,
                                TrialFlag,PerformFlag,FeedbackFlag,
                                RT_Reward,RT_Loss,RT_Neutral,
                                ACC_Reward_byAllReward,
                                ACC_Loss_byAllLoss,
                                ACC_Neutral))
sapply(Demographic, typeof)

sapply(STQ, typeof)
STQ$interview_age = as.numeric(STQ$interview_age)

sapply(MH, typeof)
MH = mutate(MH,across(.cols=5:ncol(MH), .fns=as.numeric))

sapply(MID_behav, typeof)
MID_behav$interview_age = as.numeric(MID_behav$interview_age)

sapply(NIHTB, typeof)
NIHTB = mutate(NIHTB,across(.cols=5:ncol(NIHTB), .fns=as.numeric))


# Concatenate Demographics, ScreenTime and MentalHealth
data = merge(Demographic,STQ,by = intersect(colnames(Demographic),colnames(STQ)),all = T)
data = merge(data,MH,by = intersect(colnames(data),colnames(MH)),all = T)
# Concatenate the neurocognition (NIH toolbox) and MID (DK atlas beta values across 10 contrasts)
# and the substance use summary score (SUSS) and the Sleep Disturbance Scale (SDS)
data = merge(data,NIHTB,by = intersect(colnames(data),colnames(NIHTB)),all = T)
data = merge(data,MID_behav,by = intersect(colnames(data),colnames(MID_behav)),all = T)

data$TrialFlag <- as.numeric(data$TrialFlag)
data$FeedbackFlag <- as.numeric(data$FeedbackFlag)

write.csv(data,'ABCD4.0_Merged_Demo_STQ_MH_NIHTB_MIDbehav.csv',na = '')

data %>% 
  group_by(eventname) %>%
  summarise(count_na = sum(is.na(NIHTB_totalcomp_ac)))






