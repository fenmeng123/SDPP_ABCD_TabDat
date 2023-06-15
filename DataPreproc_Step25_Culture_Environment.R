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

data <- readABCDdata('abcd_sscey01.txt')
data <- select(data,-c(abcd_sscey01_id,subjectkey))
data = mutate(data,across(.cols=6:ncol(data), .fns=as.numeric))

# 1. ABCD Parental Monitoring Survey --------------------------------------
# The Parental Monitoring Survey is a subset of questions that assess a parent’s
# active efforts to keep track of a child’s whereabouts, both at home,
# and when they are not at home (e.g., who they are with; what they are doing).
# V1. pmq_y_ss_mean: Parental Monitoring: Mean (parent_monitor_q1_y + parent_monitor_q2_y +
#                    parent_monitor_q3_y + parent_monitor_q4_y parent_monitor_q5_y)/5;
data$pmq_y_ss_mean[(data$pmq_y_ss_mean_nm!=0) & (!is.na(data$pmq_y_ss_mean_nm))] <- NA

# 2. ABCD Family Environment Scale: Family Conflict Subscale --------------
# The Conflict subscale from the Family Environment Scale (FES; Moos & Moos, 1994) 
# consist of 9 items assessing the amount of openly expressed conflict among 
# family members. These questions are answered independently by youth and parent.
# V2. fes_y_ss_fc: Conflict Subscale from the Family Environment Scale Sum of
#                  Youth Report (RAW Score): fes_youth_q1 + fes_youth_q2 +
#                  fes_youth_q3 + fes_youth_q4 + fes_youth_q5 + fes_youth_q6 + 
#                  fes_youth_q7 + fes_youth_q8 + fes_youth_q9; 
# V3. fes_y_ss_fc_pr: Conflict Subscale from the Family Environment Scale,
#                     Sum of Youth Report: Prorated Score 
#                     (Based on minimum of five items answered):
#                     [fes_y_ss_fc]*[fes_y_ss_fc_nt] /[fes_y_ss_fc_na]
data$fes_y_ss_fc[(data$fes_y_ss_fc_nm!=0) & (!is.na(data$fes_y_ss_fc_nm))] <- NA


# 3. Prosocial Behavior Survey --------------------------------------------
# The Prosocial Behaviors questionnaire is a subscale from the “Strengths and
# Difficulties Questionnaire” (Goodman et al., 1998). We use a shortened version
# of the “Prosocial Behavior” subscale from this instrum ent. The original
# subscale has 5 items. We have retained the three items with the highest
# factor loadings. The Prosocial Behavior subscale assesses tendency to engage
# in behaviors to help others. This measure is administered to both parent and
# youth. Parent reports on youth behavior, and youth reports on self.
# V4. psb_y_ss_mean: Prosocial Behavior Subscale Mean of Youth Self Report:
#                   (prosocial_q1_y + prosocial_q2_y + prosocial_q3_y)/3;
data$psb_y_ss_mean[(data$psb_y_ss_mean_nm!=0) & (!is.na(data$psb_y_ss_mean_nm))] <- NA


# 4. ABCD Children's Report of Parental Behavioral Inventory --------------
# Children's Report of Parental Behavioral Inventory (CRPBI)
# The Acceptance Scale is a subscale of the  (CRPBI). We use a shortened version
# of the original scale which had 10 items. We kept the 5 items with the highest
# factor loadings. The acceptance subscale examines children’s perceptions of
# caregiver warmth, acceptance, and responsiveness. The acceptance subscale can
# be reported on for any significant adult caregiver. First, the scale is
# answered for the “parent participant”, the adult who completes the parent
# surveys (variable names are labeled with "mom" below, even if this person is
# not the mother). Next, the survey should be answered for a second primary
# caregiver (variable names are labeled with "caregiver" below), 
# a caregiver the child spends a significant amount of time with (e.g., other
# parent, step-parent, grandparent, aunt, uncle). If there is not a second
# primary caregiver, the child can skip the second administration of the questions.
# V5. crpbi_y_ss_parent: CRPBI - Acceptance Subscale Mean of Report by Parent
#                       Completing Protocol by youth: (crpbi_parent1_y +
#                       crpbi_parent2_y + crpbi_parent3_y + crpbi_parent4_y +
#                       crpbi_parent5_y)/5;
# V6. crpbi_y_ss_caregiver: CRPBI - Acceptance Subscale Mean of Report by Secondary
#                           Caregiver by youth: (crpbi_caregiver12_y +
#                           crpbi_caregiver13_y + crpbi_caregiver14_y + 
#                           crpbi_caregiver15_y + crpbi_caregiver16_y)/5;
data$crpbi_y_ss_parent[(data$crpbi_y_ss_parent_nm>4) & (!is.na(data$crpbi_y_ss_parent_nm))] <- NA
data$crpbi_y_ss_caregiver[(data$crpbi_y_ss_caregiver_nm>4) & (!is.na(data$crpbi_y_ss_caregiver_nm))] <- NA


# 5. ABCD School Risk and Protective Factors Survey -----------------------
# The School Risk and Protective Factors (SRPF) Survey is from the PhenX School
# Risk and Protective Factors protocol. These items were derived from the “The
# Communities That Care (CTC) Youth Survey” (Arthur et al., 2007). 
# Two items were cut due to redundancy across the battery
# (grade in school and skipping classes), and items were re-worded for age 
# appropriateness. The SRPF examines youth’s perceptions of the school climate
# and school engagement and asks youth to report on school grades. 
# Responses are used to derive three subscale scores: School Environment, 
# School Involvement, and School Disengagement.
# V7. srpf_y_ss_ses: SRPF School Environment Subscale, Sum: 
#                    school_2_y + school_3_y + school_4_y + school_5_y + 
#                    school_6_y + school_7_y;
#                    Validation: Minimum of five items answered
# V8. srpf_y_ss_iiss: SRPF School Involvement Subscale, Sum: 
#                     school_8_y + school_9_y + school_10_y + school_12_y;
#                     Validation: Minimum of three items answered
# V9. srpf_y_ss_dfs: SRPF School Disengagement Subscale, Sum: 
#                    school_15_y + school_17_y;
#                    Validation: All items must be answered
data$srpf_y_ss_ses[(data$srpf_y_ss_ses_nm>5) & (!is.na(data$srpf_y_ss_ses_nm))] <- NA
data$srpf_y_ss_iiss[(data$srpf_y_ss_iiss_nm>3) & (!is.na(data$srpf_y_ss_iiss_nm))] <- NA
data$srpf_y_ss_dfs[(data$srpf_y_ss_dfs_nm!=0) & (!is.na(data$srpf_y_ss_dfs_nm))] <- NA


# 6. The Wills Problem Solving Scale --------------------------------------
# This scale was derived from earlier coping measures (Wills, Health Psychology 1986) 
# and has good internal consistency in studies with children and adolescents
# from a variety of populations (Wills et al., Health Psychology 2013).
# It taps a systematic behavioral approach to deal with problem situations
# through gathering information about the problem, considering alternative
# solutions to the problem, deciding about a plan of action, and implementing
# an active approach to do something to resolve the problem. 
# Different usages of the scale have had 6-8 items; items with the highest
# correlation with early substance use were selected for this study.
# Summary Scores can be found in the following instruments.
# Prefixes used in summary score variables are listed at the end of each instrument name.
# V10. wps_ss_sum: WPS Sum[wps_q1_y. wps_q2_y. wps_q3_y,wps_q4_y, wps_q5_y, wps_q6_y]
#                  Validation: Minimum of 4 items answered
#                  (4 answers other than not applicable = 0)
data$wps_ss_sum[(data$wps_ss_sum_nm>4) & (!is.na(data$wps_ss_sum_nm))] <- NA


# 7. ABCD Perceived Discrimination Scale (PDS) ----------------------------
# In order to assess participant perceptions of experiencing ethnic 
# (and other types of) discrimination, we selected and modified selected items
# from two existing scales: The 2006 Boston Youth Survey and 
# the Measure of Perceived Discrimination (Phinney, Madden, and Santos, 1998).
# The first four items are from the 2006 Boston Youth Survey and
# query about being discriminated against due to race, ethnicity, color, 
# country of origin, sexual identity, and body type during the past 12 months.
# The remaining seven items come from the Measure of Perceived Discrimination
# and query about frequency of being treated unfairly or negatively 
# because of ethnic background by others, as well as general feelings
# of experiencing ethnic discrimination.
# V11.dim_y_ss_mean: Discrimination Measure mean(dim_matrix_q1,dim_matrix_q2,
#                    dim_matrix_q3,dim_matrix_q4,dim_matrix_q5,dim_matrix_q6,
#                    dim_matrix_q7); Only add if value 1 - 5;
#                    Validation: Minimum of four items must be answered
data$dim_y_ss_mean[(data$dim_y_ss_mean_nm>4) & (!is.na(data$dim_y_ss_mean_nm))] <- NA

# 

# 8. ABCD Mexican American Cultural Values Scale Modified -----------------
# The Cultural Values Scale is a subset of items derived from the
# Mexican American Cultural Values Scale (MACVS; Knight et al, 2010). 
# The original measure consists of 50 items across 9 subscales reflecting
# values associated with Mexican/Mexican American and contemporary mainstream
# American beliefs, behaviors, and traditions. 
# We have retained 5 of the original subscales (28 items) assessing Familism
# (Familism referent, Familism support, Familism obligation), 
# Religion, and Independence/self-reliance that are applicable across
# numerous cultures, races, and ethnicities and are of relevance to 
# substance use trajectories. 
# This is applicable to all subjects (not just subjects of Mexican heritage).
# Parents complete all 5 subscales (28 items) while youth complete only the
# 3 subscales for Familism (16 items).
# The youth measure was collected starting at the two-year follow-up.
# V12. macv_y_ss_fr: Family Referrent Sum Score mean[macv_q4], [macv_q9],
#                    [macv_q18], [macv_q23], [macv_q27])
#                    Validation: All items must be answered	
# V13. macv_y_ss_fs: Family Support Sum Score mean([macv_q2], [macv_q7], 
#                    [macv_q12], [macv_q16], [macv_q21], [macv_q26])
#                    Validation: All items must be answered
# V14. macv_y_ss_fo: Family Obligation mean([macv_q3], [macv_q8], 
#                    [macv_q13], [macv_q17], [macv_q22])
#                    Validation: All questions must be answered
# V15. macv_y_ss_isr: Independence/Self Reliance: mean:indep/self reliance = 5, 10, 14, 19, 24	
# V16. macv_y_ss_r: Religion: mean: religion = 1, 6, 11, 15, 20, 25, 28
data$macv_y_ss_fr[(data$macv_y_ss_fr_nm!=0) & (!is.na(data$macv_y_ss_fr))] <- NA
data$macv_y_ss_fs[(data$macv_y_ss_fs_nm!=0) & (!is.na(data$macv_y_ss_fs))] <- NA
data$macv_y_ss_fo[(data$macv_y_ss_fo_nm!=0) & (!is.na(data$macv_y_ss_fo))] <- NA

macv_y <- readABCDdata('abcd_macvsy01.txt')
macv_y = mutate(macv_y,across(.cols=8:ncol(macv_y), .fns=as.numeric))
macv_y$MACVS_ISR_Sum <- macv_y$macv_q5 + macv_y$macv_q10 + macv_y$macv_q14 + 
  macv_y$macv_q19 + macv_y$macv_q24
macv_y$MACVS_Religion_Sum <- macv_y$macv_q1 + macv_y$macv_q6 + macv_y$macv_q11 + 
  macv_y$macv_q15 + macv_y$macv_q20 + macv_y$macv_q25 + macv_y$macv_q28
macv_y <- select(macv_y,c(src_subject_id,interview_age,sex,eventname,
                          MACVS_Religion_Sum,MACVS_ISR_Sum))

# 9. ABCD Peer Behavior Profile: Prosocial & Delinquent Peer Invol --------
# The Youth Peer Behavior Profile consists of two 3-item self-rated subscales,
# the Prosocial Peer Involvement subscale and the Rule Breaking/Delinquent Peer
# Involvement subscale, that assess the extent to which the youth’s friendship
# network consists of (a) prosocial peers (e.g., friends who are excellent
# students, are athletes, etc.), and/or (b) rule breaking/delinquent peers
# (e.g., friends who skip school, shoplift, etc.); the two sub-scales are
# not mutually exclusive. The measure uses a subset of 8 items drawn from the
# 54 item Peer Behavior Profile/Peer Activities Questionnaire, which assesses
# degree of involvement with peers engaged in varying types of conventional,
# nonconventional, and rule/breaking behavior. Participants report what
# proportion (based on a five-point scale ranging from “none or almost none”
# to “all or nearly all”) of their peers are involved in these behaviors.
# Items were derivative from earlier measures assessing these kinds of social
# influence networks. This instrument was administered to youth starting in
# the two-year follow-up.
# V17. pbp_ss_prosocial_peers: Involvement with Prosocial Peers
#                              sum(pbp_athletes, pbp_church, pbp_good_student)
#                              Validation: Minimum of two items must be answered	
# V18. pbp_ss_rule_break: Involvement with Rule Breaking/Delinquent Peers
#                         sum(pbp_skip_school, pbp_suspended, pbp_shop_lifted)
#                         Validation: Minimum of two items must be answered
data$pbp_ss_prosocial_peers[(data$pbp_ss_prosocial_peers==3) & (!is.na(data$pbp_ss_prosocial_peers))] <- NA
data$pbp_ss_rule_break[(data$pbp_ss_rule_break==3) & (!is.na(data$pbp_ss_rule_break))] <- NA


# 10. ABCD Peer Network Health: Protective Scale --------------------------
# Youth report on three of their close friends’ protective behaviors against
# substance use such as encouraging not using substances, or reducing use,
# and providing instrumental and psychological support.
# V19. pnh_ss_protective_scale: Peer Network Health: Protective Scale Score;
#                               sum (pnh_substance, pnh_help, pnh_how_much_help,
#                               pnh_encourage, pnh_how_much_encourage);
#                               Validation: Items #1,#2, and #3 must be answered


# 11. ABCD Multidimensional Neglectful Behavior Scale ---------------------
# This 8-item scale was adapted from the “LONGSCAN About My Parents” measure 
# (LONGSCAN investigators, 1998) to assess neglectful parental behaviors via
# youth self-report. The ABCD adaptation includes items with the highest
# factor loadings from the Monitoring Supervision subscale, as well as
# age-appropriate items from the Educational Support subscale.
# Items query how often parents met the youth’s needs in a variety of areas,
# in the last year. Responses range from “0 = never to 3 = a lot.”
# The measure yield one overall mean score, as well as two mean scores for each
# of the two subscales. The LONGSCAN measure was originally adapted from the
# Revised Neglectful Behavioral Scale (Dubowitz, et. al, 2011).
# V20. mnbs_ss_mean_all: all: mean 1-10
# V21. mnbs_ss_monitor_supervision: Monitoring/Supervision: mean of 1,2,3,4
# V22. mnbs_ss_ed_support: Educational/support: mean of 8,9,10


# Summary: abcd_sscey01 ---------------------------------------------------
data <- select(data,c(src_subject_id,interview_age,sex,eventname,
                      pmq_y_ss_mean,
                      fes_y_ss_fc,fes_y_ss_fc_pr,
                      psb_y_ss_mean,
                      crpbi_y_ss_parent,crpbi_y_ss_caregiver,
                      srpf_y_ss_ses,srpf_y_ss_iiss,srpf_y_ss_dfs,
                      wps_ss_sum,
                      dim_y_ss_mean,
                      macv_y_ss_fr,macv_y_ss_fs,macv_y_ss_fo,
                      macv_y_ss_isr,macv_y_ss_r,
                      pbp_ss_prosocial_peers,pbp_ss_rule_break,
                      pnh_ss_protective_scale,
                      mnbs_ss_mean_all,mnbs_ss_monitor_supervision,mnbs_ss_ed_support))
colnames(data) <- c('src_subject_id','interview_age','sex','eventname',
                    'PMS_Mean',
                    'FES_Conflict_Sum','FES_Conflict_Sum_PR',
                    'PBS_Mean',
                    'CRPBI_Parent_Mean','CRPBI_Caregiver_Mean',
                    'SRPFS_Env_Sum','SRPFS_Inv_Sum','SRPFS_Dis_Sum',
                    'WPS_Sum',
                    'PDS_Mean',
                    'MACVS_Referrent_Mean','MACVS_Support_Mean','MACSV_Obligation_Mean',
                    'MACVS_ISR_Mean','MACVS_Religion_Mean',
                    'PBP_Prosocial_Sum','PBP_RuleBreakDel_Sum',
                    'PNH_Protective_Sum',
                    'MNBS_All_Mean','MNBS_MonSupv_Mean','MNBS_EduSupp_Mean')

# 12. ABCD Acculturation Survey Modified from PhenX (ACC) -----------------
# These items aim at assessing level of participant acculturation – that is,
# the process by which an individual from one cultural group adapts and borrows
# traits and values from another culture – by assessing proficiency and
# preferences for speaking a given language in different settings.
acc <- readABCDdata('yacc01.txt')
acc <- select(acc,-c(yacc01_id,subjectkey,interview_date))
# V1. accult_q1_y: How well do you speak English?
#                  1 = Poor; 2 = Fair; 3 = Good; 4 = Excellent
# V2. accult_q2_y: Besides English, do you speak or understand another language
#                  or dialect? If child asks about languages learned in school,
#                  the RA should state: That's OK, as long as it is a language
#                  or dialect that you speak or understand.
#                  0 = No; 1 = Yes; 777 = Refused; 999 = Don't Know
# V3. accult_q3_dropdwn_y: What other language or dialect do you speak or understand (besides English)?
# 1 = American Sign Language (ASL); 2 = Arabic; 3 = Arawak; 4 = Bangla;
# 5 = Bosnian; 6 = Cantonese; 7 = Cham; 8 = An American Indian or Alaska Native language;
# 9 = Creole; 10 = Croatian; 11 = Dutch; 12 = Filipino; 13 = Finnish; 14 = Flemish;
# 15 = French; 16 = Frisian; 17 = Gaelic; 18 = Garifuna; 19 = German; 20 = Greek;
# 21 = Hakka; 22 = Hebrew; 23 = Hindi; 24 = Hokkien; 25 = Hungarian; 26 = Italian;
# 27 = Japanese; 28 = Jarai; 29 = Khmer; 30 = Korean; 31 = Kuy; 32 = Luganda;
# 33 = Mandarin; 34 = Nepali; 35 = Papiamento; 36 = Patois; 37 = Persian;
# 38 = Polish; 39 = Portuguese; 40 = Punjabi; 41 = Romanian; 42 = Russian; 
# 43 = Samoan; 44 = Serbian; 45 = Sinhala; 46 = Sourashtra; 47 = Spanish;
# 48 = Stieng; 49 = Swahili; 50 = Tagalog; 51 = Tamil; 52 = Telugu; 53 = Thai;
# 54 = Turkish; 55 = Urdu; 56 = Vietnamese; 57 = Other
# V4. accult_q3_other_y: What "other" language do you speak or understand besides English?
# V5. accult_q4_y: What language do you speak with most of your friends?
#                  1 = (Other language) all the time;
#                  2 = (Other language) most of the time;
#                  3 = (Other language) and English equally;
#                  4 = English most of the time;
#                  5 = English all the time
# V6. accult_q5_y: What language do you speak with most of your family?
#                 Coding scheme is same with V5
# V7. accult_q3b: How well do you speak [accult_q3_dropdwn_y]? 
#                 1 = Poor; 2 = Fair; 3 = Good; 4 = Excellent
acc <- select(acc,-c(accult_admin))
colnames(acc) <- c('src_subject_id','interview_age','sex','eventname',
                   'Accult_English_Proficiency',
                   'Accult_OtherLang_Ownership',
                   'Accult_OtherLang_Type',
                   'Accult_OtherLang_Other',
                   'Accult_LangWithFriends',
                   'Accult_LangWithFamily',
                   'Accult_OtherLang_Proficiency')
acc = mutate(acc,across(.cols=5:7, .fns=as.numeric))
acc = mutate(acc,across(.cols=9:ncol(acc), .fns=as.numeric))
acc$Accult_OtherLang_Ownership <- RECODE(acc$Accult_OtherLang_Ownership,'777=NA;999=NA;')



# 13. ABCD Family Environment Scale: Intellectual/Cultural, Active --------
#       13.1 Intellectual/Cultural Subscale
#       13.2 Active/Recreational Subscale
#       13.3 Organization Subscale
#       13.4 Cohesion Subscale
#       13.5 Expressiveness Subscales
# Each of these additional subscales involves 9 items assessing the extent to
# which the family has an Intellectual-Cultural orientation,
#                      an Active-Recreational orientation,
#                  is Organized,
#                  is Supportive/Cohesive (Cohesion), and
#                  is Expressive in style.
# These subscales evaluate the three underlying dimensions of the family
# environment: Family Relationships, Personal Growth, and System Maintenance
# and Change, and have excellent external validity. 
# These subscale items are answered by the parent starting in the two-year follow up.
fes <- readABCDdata('abcd_sscep01.txt')
fes <- select(fes,c(src_subject_id,interview_age,sex,eventname,
                    fes_p_ss_int_cult_sum,
                    fes_p_ss_act_rec_sum,
                    fes_p_ss_org_sum,
                    fes_p_ss_cohesion_sum,
                    fes_p_ss_exp_sum))
fes = mutate(fes,across(.cols=5:ncol(fes), .fns=as.numeric))
colnames(fes) <- c('src_subject_id','interview_age','sex','eventname',
                   'FES_IntCult_Sum',
                   'FES_ActRec_Sum',
                   'FES_Org_Sum',
                   'FES_Cohesion_Sum',
                   'FES_Expre_Sum')

# 14. ABCD Neighborhood Safety/Crime Survey Modified from PhenX (N --------
# V1. neighborhood_crime_y: My neighborhood is safe from crime.
#                           1 = Strongly Disagree; 2 = Disagree;
#                           3 = Neutral (neither agree nor disagree);
#                           4 = Agree; 5 = Strongly
# V2. nsc_p_ss_mean_3_items: Neighborhood Safety Protocol: Mean of Parent Report,
#                     (neighborhood1r_p + neighborhood2r_p + neighborhood3r_p)/3; 
#                     Validation: No minimum
nsc <- readABCDdata('abcd_nsc01.txt')
nsc <- select(nsc,c(src_subject_id,interview_age,sex,eventname,
                    neighborhood_crime_y))
colnames(nsc) <- c('src_subject_id','interview_age','sex','eventname',
                   'NSC_Youth')
nsc$NSC_Youth <- as.numeric(nsc$NSC_Youth)
nsc_p <- readABCDdata('abcd_sscep01.txt')
nsc_p <- select(nsc_p,c(src_subject_id,interview_age,sex,eventname,
                        nsc_p_ss_mean_3_items,nsc_p_ss_mean_3_items_nm))
nsc_p = mutate(nsc_p,across(.cols=5:ncol(nsc_p), .fns=as.numeric))
nsc_p$nsc_p_ss_mean_3_items[(nsc_p$nsc_p_ss_mean_3_items_nm!=0) & (!is.na(nsc_p$nsc_p_ss_mean_3_items_nm))] <- NA
nsc_p <- select(nsc_p,-nsc_p_ss_mean_3_items_nm)
colnames(nsc_p) <- c('src_subject_id','interview_age','sex','eventname',
                   'NSC_Parent_Mean')
nsc <- merge(nsc,nsc_p,by = c('src_subject_id','interview_age','sex','eventname'),all = T)

# 15. ABCD Pet Ownership --------------------------------------------------
pet <- readABCDdata('pet_ownership01.txt')
pet <- select(pet,c(src_subject_id,interview_age,sex,eventname,
                    pet_identify___0))
pet$pet_identify___0 <- as.numeric(pet$pet_identify___0)
colnames(pet) <- c('src_subject_id','interview_age','sex','eventname',
                     'Pet_Ownership')

data <- merge(data,acc,by = c('src_subject_id','interview_age','sex','eventname'),all = T)
data <- merge(data,fes,by = c('src_subject_id','interview_age','sex','eventname'),all = T)
data <- merge(data,nsc,by = c('src_subject_id','interview_age','sex','eventname'),all = T)
data <- merge(data,pet,by = c('src_subject_id','interview_age','sex','eventname'),all = T)
data <- merge(data,macv_y,by = c('src_subject_id','interview_age','sex','eventname'),all = T)

# Convert mean to sum -----------------------------------------------------
data$PMS_Sum <- round((5*data$PMS_Mean),digits = 0)
data$PBS_Sum <- round((3*data$PBS_Mean),digits = 0)
data$PDS_Sum <- round((7*data$PDS_Mean),digits = 0)
data$CRPBI_Parent_Sum <- round((5*data$CRPBI_Parent_Mean),digits = 0)
data$CRPBI_Caregiver_Sum <- round((5*data$CRPBI_Caregiver_Mean),digits = 0)
data$MACVS_Referrent_Sum <- round((5*data$MACVS_Referrent_Mean),digits = 0)
data$MACVS_Support_Sum <- round((6*data$MACVS_Support_Mean),digits = 0)
data$MACVS_Obligation_Sum <- round((5*data$MACSV_Obligation_Mean),digits = 0)

mnbs <- readABCDdata('neglectful_behavior01.txt')
mnbs <- select(mnbs,-c(neglectful_behavior01_id,subjectkey,interview_date,
                       mnbs_admin))
mnbs = mutate(mnbs,across(.cols=5:ncol(mnbs), .fns=as.numeric))
for (i in 5:ncol(mnbs)){
  mnbs[[i]] <- RECODE(mnbs[[5]],'777=NA');
}
mnbs$MNBS_All_Sum <- mnbs$mnbs_bad + mnbs$mnbs_doing + mnbs$mnbs_friend + 
  mnbs$mnbs_homework + mnbs$mnbs_play + mnbs$mnbs_school + mnbs$mnbs_trouble + 
  mnbs$mnbs_understand
mnbs$MNBS_MonSupv_Sum <- mnbs$mnbs_bad + mnbs$mnbs_doing + mnbs$mnbs_friend + mnbs$mnbs_play
mnbs$MNBS_EduSupp_Sum <- mnbs$mnbs_homework + mnbs$mnbs_school + mnbs$mnbs_trouble + mnbs$mnbs_understand
mnbs <- select(mnbs,-c(mnbs_bad,mnbs_doing,mnbs_friend,mnbs_homework,mnbs_play,
                       mnbs_school,mnbs_trouble,mnbs_understand))
data <- merge(data,mnbs,by = c('src_subject_id','interview_age','sex','eventname'),all = T)

# Reorder data colmuns ----------------------------------------------------

data <- select(data,c(src_subject_id,interview_age,sex,eventname,
                      PBS_Sum,WPS_Sum,Pet_Ownership,
                      MNBS_All_Sum,MNBS_MonSupv_Sum,MNBS_EduSupp_Sum,
                      PMS_Sum,CRPBI_Parent_Sum,CRPBI_Caregiver_Sum,
                      FES_Conflict_Sum,FES_IntCult_Sum,FES_ActRec_Sum,
                      FES_Org_Sum,FES_Cohesion_Sum,FES_Expre_Sum,
                      NSC_Youth,NSC_Parent_Mean,
                      PBP_Prosocial_Sum,PBP_RuleBreakDel_Sum,PNH_Protective_Sum,
                      SRPFS_Env_Sum,SRPFS_Inv_Sum,SRPFS_Dis_Sum,
                      MACVS_Referrent_Sum,MACVS_Support_Sum,MACVS_Obligation_Sum,
                      MACVS_Religion_Sum,MACVS_ISR_Sum,
                      Accult_English_Proficiency,Accult_OtherLang_Ownership,
                      Accult_OtherLang_Proficiency,Accult_OtherLang_Type,
                      Accult_OtherLang_Other,Accult_LangWithFriends,
                      Accult_LangWithFamily,
                      ))
saveRDS(data,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_CulEnv.rds")
