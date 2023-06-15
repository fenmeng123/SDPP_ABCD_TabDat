library(bruceR)

setwd("I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataPreprocessing")
data = readRDS('ABCD4.0_Merged_Behav_7LSTQ_DemoRaw.rds')
colnames(data)

# remove redundant variables ------------------------------------------------

# data <- subset(data, select = -c(AdoptionYear,AdoptionFlag,USALiveYears,
#                                  Religon_PrntRep,ParentMaritalC,ParentHighEdu,
#                                  PartnerHighEdu,ParentEmploy,PartnerEmploy,
#                                  GenderIdentity_PrntRep,SexAssigned,EducationC,
#                                  SameSexTwinFlag,NIHTB_picvocab_fc,NIHTB_flanker_fc,
#                                  NIHTB_list_fc,NIHTB_cardsort_fc,NIHTB_pattern_fc,
#                                  NIHTB_picture_fc,NIHTB_reading_fc,NIHTB_fluidcomp_fc,
#                                  NIHTB_cryst_fc,NIHTB_totalcomp_fc,
#                                  PEQ_Rela_Victim,PEQ_Repu_Victim,PEQ_Overt_Victim,
#                                  PEQ_Rela_Aggs, PEQ_Repu_Aggs,PEQ_Overt_Aggs,
#                                  Mania_7UP_Sum))

data <- subset(data, select = -c(subjectkey,AdoptionFlag,USALiveYears,
                                 ParentHighEdu,PartnerHighEdu,
                                 ParentEmploy,PartnerEmploy,
                                 FamliyEmploy.V1,FamliyEmploy.V2,NumInLF,
                                 SameSexTwinFlag))

# calculating and recoding some variables ---------------------------------
data <- naniar::replace_with_na(data,list(VGAS_1 = 777,
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
data$VGAS_Sum = data$VGAS_1 + data$VGAS_2 + data$VGAS_3 + data$VGAS_4 + 
  data$VGAS_5 + data$VGAS_6
data$SMAS_Sum = data$SMAS_1 + data$SMAS_2 + data$SMAS_3 + data$SMAS_4 + 
  data$SMAS_5 + data$SMAS_6
data$MPIQ_Sum = data$MPIQ_1 + data$MPIQ_2 + data$MPIQ_3 + data$MPIQ_4 + 
  data$MPIQ_5 + data$MPIQ_6 + data$MPIQ_7 + data$MPIQ_8

data$UPPS_Sum = data$UPPS_NegUrge + data$UPPS_PosUrge + data$UPPS_SenSeek + 
  data$UPPS_LackPlan + data$UPPS_LackPersev

data$BAS_Sum = data$BAS_Drive + data$BAS_RR + data$BAS_FS

data$Race_PrntRep = RECODE(data$Race_PrntRep,"'Mixed' = 'Mixed/other';
                                              'Other' = 'Mixed/other';")
data$FamilyIncomeR <- as.numeric(data$FamilyIncome)
data$HouseholdSizeR <- as.numeric(data$HouseholdSize)
data$EducationR <- as.numeric(data$EducationR)
# attach two frequency items to the STQ data
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
stq = readABCDdata('I:\\ABCDStudyNDA\\Download_ABCDV4.0_skr220403\\Package_1199282\\abcd_stq01.txt')
stq = subset(stq,select = c(src_subject_id,interview_date,sex,eventname,screen13_y,screen14_y))
colnames(stq)[5:6] <- c("MatureRated_VideoGame","R_Rated_Movies")

data = merge(data,stq,by = intersect(colnames(data),colnames(stq)),all = T)


# Re-order columns --------------------------------------------------------
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
                      ACS_weight,site_id_l,BMI_calc,
                      weekday_TV,weekday_Video,weekday_Game,
                      weekday_Text,weekday_SocialNet,weekday_VideoChat,
                      weekend_TV,weekend_Video,weekend_Game,
                      weekend_Text,weekend_SocialNet,weekend_VideoChat,
                      MatureRated_VideoGame,R_Rated_Movies,
                      Ownership_SMAccount,Ownership_PubSM_Account,
                      Ownership_SecretAccount,
                      Ownership_MobilePhone,
                      Ownership_DeviceInBedroom,
                      Ownership_Past_DatingApps,Ownership_Current_DatingApps,
                      Ownership_InpersonDateMeet,
                      SMAS_Sum,VGAS_Sum,MPIQ_Sum,Attachment_MobilePhone,
                      SABT_TV,SABT_Game,SABT_Music,SABT_Text,SABT_SocialNet,
                      SABT_ChatRoom,SABT_Surf,SABT_Study,SABT_Read,
                      Sleep_PhoneAction,Sleep_WakeBefore,Sleep_WakeDuring,
                      weekday_TPD,weekend_TPD,TimePerDay_SM,TimePerWeek_DatingApps,
                      CBCL_syn_anxdep_t,CBCL_syn_withdep_t,CBCL_syn_somatic_t,
                      CBCL_syn_social_t,CBCL_syn_thought_t,CBCL_syn_attention_t,CBCL_syn_rulebreak_t,
                      CBCL_syn_aggressive_t,CBCL_syn_internal_t,CBCL_syn_external_t,CBCL_syn_totprob_t,
                      CBCL_dsm5_depress_t,CBCL_dsm5_anxdisord_t,CBCL_dsm5_somaticpr_t,CBCL_dsm5_adhd_t,
                      CBCL_dsm5_opposit_t,CBCL_dsm5_conduct_t,CBCL_07_sct_t,CBCL_07_ocd_t,CBCL_07_stress_t,
                      NIHTB_picvocab_fc,NIHTB_flanker_fc,NIHTB_list_fc,NIHTB_cardsort_fc,
                      NIHTB_pattern_fc,NIHTB_picture_fc,NIHTB_reading_fc,
                      NIHTB_fluidcomp_fc,NIHTB_cryst_fc,NIHTB_totalcomp_fc,
                      UPPS_Sum,BAS_Sum,BIS_Sum,
                      PPS_Number_Sum,PPS_Severity_Mean,
                      PLE_BadEvent_Sum,PLE_Affected_BadEvent_Mean,
                      PEQ_Rela_Victim,PEQ_Repu_Victim,PEQ_Overt_Victim,
                      PEQ_Rela_Aggs,PEQ_Repu_Aggs,PEQ_Overt_Aggs,
                      Mania_7UP_Sum,GISH_Male_Sum,GISH_Female_Sum,
                      everything()))
colnames(data)

# fill the empty holes about demographics in dataframe  -------------------
baseline <- subset(data,eventname=="baseline_year_1_arm_1")
followup <- subset(data,eventname!="baseline_year_1_arm_1")
for (i_subID in baseline$src_subject_id){
  cat(sprintf('#SubID:%s Filling holes in demographics for follow-up data...\n',i_subID))
  sub_index <- grep(i_subID,followup$src_subject_id)
  followup[sub_index,6:26] <- baseline[i_subID,6:26]
}
data <- rbind(baseline,followup)

# fill the empty ACS weights in follow-up data ----------------------------

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
# Missing values Auto-reporting -------------------------------------------

data[,table(eventname,useNA = 'if')]
baseline = subset(data,eventname=='baseline_year_1_arm_1')
year1FU = subset(data,eventname=='1_year_follow_up_y_arm_1')
year2FU = subset(data,eventname=='2_year_follow_up_y_arm_1')
year3FU = subset(data,eventname=='3_year_follow_up_y_arm_1')

naniar::miss_var_summary(baseline) %>% 
  print_table(file = "MissVarRep_baseline_Behav_7LSTQ_DemoRaw.doc")

naniar::miss_var_summary(year1FU) %>% 
  print_table(file = "MissVarRep_1_yearFU_Behav_7LSTQ_DemoRaw.doc")

naniar::miss_var_summary(year2FU) %>% 
  print_table(file = "MissVarRep_2_yearFU_Behav_7LSTQ_DemoRaw.doc")

naniar::miss_var_summary(year3FU) %>% 
  print_table(file = "MissVarRep_3_yearFU_Behav_7LSTQ_DemoRaw.doc")

# Separate and save the 'data' variable for MATLAB -------------------------

rm(baseline,stq,year1FU,year2FU,year3FU)
gc()
# Step 1 save all the behave data
saveRDS(data,file = "I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataPreprocessing/ABCD4.0_Prepared_Behav_7LSTQ_DemoRaw.rds")
write.csv(data,file = "I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataPreprocessing/ABCD4.0_Prepared_Behav_7LSTQ_DemoRaw.csv")
saveRDS(data,
        "I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataAnalysis/SMA_TrajectoryTransition/Res_3_IntermediateData/ABCD4.0_Prepared_Behav_7LSTQ_DemoRaw.rds")
write.csv(data,
          "I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataAnalysis/SMA_TrajectoryTransition/Res_3_IntermediateData/ABCD4.0_Prepared_Behav_7LSTQ_DemoRaw.csv")
# Step 2 data for Cluster Analysis
saveRDS(data[,1:43],
        "I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataAnalysis/SMA_TrajectoryTransition/Res_3_IntermediateData/ABCD4.0_Prepared_STQ_14Items.rds")
write.csv(data[,1:43],
        "I:/ABCDStudyNDA/ABCD_DataAnalysis_4.0/DataAnalysis/SMA_TrajectoryTransition/Res_3_IntermediateData/ABCD4.0_Prepared_STQ_14Items.csv")
gc()
