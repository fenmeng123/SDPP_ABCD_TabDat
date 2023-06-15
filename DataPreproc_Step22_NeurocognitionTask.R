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

# Variable 1: ABCD Cash Choice Task ----------------------------------------
# ===========================Task Introduction==================================
# Impulsivity, delayed gratification;
# this single-item task asked the child “Let’s pretend a kind person wanted to 
# give you some money. Would you rather have $75 in three days or $115 in 3 months?”.
# The child indicates one of these two options or a third “can’t decide” option.
# ===========================Data coding Scheme=================================
# URL: https://nda.nih.gov/data_structure.html?short_name=cct01
# cash_choice_task: 1 = $75 in three days; 2 = $115 in 3 months; 3 = Don't Know
cash_choice <- readABCDdata('cct01.txt')
cash_choice <- select(cash_choice,-c(cct01_id,subjectkey,interview_date))
cash_choice$cash_choice_task <- RECODE(cash_choice$cash_choice_task,
                                       "'1'='Prefer to immediate reward';
                                        '2'='Prefer to delayed reward';
                                        '3'='Cannot decide';")
cash_choice$cash_choice_task <- factor(cash_choice$cash_choice_task,
                                       levels = c('Prefer to immediate reward',
                                                  'Prefer to delayed reward',
                                                  'Cannot decide'))

# Variable 2: ABCD Little Man Task  ----------------------------------------
# ===========================Task Introduction==================================
# Visuospatial processing flexibility, attention; 
# participants view pictures of a figure (little man) presented in different 
# orientations and holding a suitcase and must use mental rotation skills to 
# assess which hand (left or right) is holding the suitcase.
# Accuracy and latency scores are provided for each trial.
# Note that the Little Man Task used in the baseline assessment was administered
# using a customized program designed by ABCD, whereas the two-year follow-up 
# assessment used a task presented in the Inquisit system from Millisecond.
# ===========================Data coding Scheme=================================
# URL: https://nda.nih.gov/data_structure.html?short_name=lmtp201
# lmt_scr_perc_correct: Percentage correct of all 32 presented trials
# lmt_scr_rt_correct: Average reaction for correct trials
# lmt_scr_efficiency: LMT Efficiency, Percentage correct divided by average reaction for correct trials
little_man <- readABCDdata('lmtp201.txt')
little_man <- select(little_man,c(src_subject_id,interview_age,sex,eventname,
                                  lmt_run,
                                  lmt_scr_perc_correct,
                                  lmt_scr_rt_correct,
                                  lmt_scr_efficiency))
sapply(little_man, typeof)
little_man = mutate(little_man,across(.cols=5:ncol(little_man), .fns=as.numeric))
sapply(little_man, typeof)

# Variable 3: ABCD Pearson Scores  ----------------------------------------
# ===========================Task Introduction==================================
# 1. Rey Auditory Verbal Learning Test – Verbal learning and memory; 
# the task is administered according to standard instructions using a 15-item word list;
# there are five learning trials (Trials I-V), a distractor trial (List B), 
# measures of immediate recall (Trial VI) and 30-minute delayed recall (Trial VII); 
# for all trials, the total correct is recorded together with the number of perseverations and intrusions.
# 2. Matrix Reasoning Task – Measures fluid intelligence, visuospatial reasoning; 
# the task is from the Wechsler Intelligence Scale for Children-V and administered
# using Pearson Clinical Assessment-s Q-interactive platform. 
# Total raw scores, scaled scores (ranging from 0-19; mean = 10, SD = 3) and 
# scores for each item are available.
# ===========================Data coding Scheme=================================
# URL: https://nda.nih.gov/data_structure.html?short_name=abcd_ps01
# pea_ravlt_sd_trial_i_tc: RAVLT Short Delay Trial I Total Correct	
# pea_ravlt_sd_trial_ii_tc: RAVLT Short Delay Trial II Total Correct
# pea_ravlt_sd_trial_iii_tc: RAVLT Short Delay Trial III Total Correct
# pea_ravlt_sd_trial_iv_tc: RAVLT Short Delay Trial IV Total Correct
# pea_ravlt_sd_trial_v_tc: RAVLT Short Delay Trial V Total Correct
# pea_ravlt_sd_listb_tc: RAVLT Short Delay List B Total Correct
# pea_ravlt_sd_trial_vi_tc: RAVLT Short Delay Trial VI Total Correct
# pea_ravlt_ld_trial_vii_tc: RAVLT Long Delay Trial VII Total Correct
# pea_wiscv_trs: WISC-V Matrix Reasoning Total Raw Score
# pea_wiscv_tss: WISC-V Matrix Reasoning Total Scaled Score
person_score <- readABCDdata('abcd_ps01.txt')
person_score <- select(person_score,c(src_subject_id,interview_age,sex,eventname,
                                      pea_ravlt_sd_trial_i_tc,
                                      pea_ravlt_sd_trial_ii_tc,
                                      pea_ravlt_sd_trial_iii_tc,
                                      pea_ravlt_sd_trial_iv_tc,
                                      pea_ravlt_sd_trial_v_tc,
                                      pea_ravlt_sd_listb_tc,
                                      pea_ravlt_sd_trial_vi_tc,
                                      pea_ravlt_ld_trial_vii_tc,
                                      pea_wiscv_trs,pea_wiscv_tss))
colnames(person_score) <- c('src_subject_id','interview_age','sex','eventname',
                            'RAVLT_Trial_I_tc',
                            'RAVLT_Trial_II_tc',
                            'RAVLT_Trial_III_tc',
                            'RAVLT_Trial_IV_tc',
                            'RAVLT_Trial_V_tc',
                            'RAVLT_ListB_tc',
                            'RAVLT_Trial_VI_tc',
                            'RAVLT_Trial_VII_tc',
                            'WISCV_RawScore',
                            'WISCV_ScaledScore')
sapply(person_score, typeof)
person_score = mutate(person_score,across(.cols=5:ncol(person_score), .fns=as.numeric))
sapply(person_score, typeof)
# Calculating Scheme for RAVLT summary scores:
# RAVLT Immediate (the sum of scores from 5 first trials (Trials 1 to 5)),
# RAVLT Learning (the score of Trial 5 minus the score of Trial 1),
# RAVLT Forgetting (the score of Trial 5 minus score of the delayed recall) and
# RAVLT Percent Forgetting (RAVLT Forgetting divided by the score of Trial 5). 
# (Ref: Moradi E, Hallikainen I, Hänninen T, Tohka J; Alzheimer's Disease Neuroimaging Initiative. 
# Rey's Auditory Verbal Learning Test scores can be predicted from whole brain MRI in Alzheimer's disease. 
# Neuroimage Clin. 2016;13:415-427. Published 2016 Dec 18. doi:10.1016/j.nicl.2016.12.011)
person_score$RAVLT_Immediate = person_score$RAVLT_Trial_I_tc + 
  person_score$RAVLT_Trial_II_tc + person_score$RAVLT_Trial_III_tc +
  person_score$RAVLT_Trial_IV_tc + person_score$RAVLT_Trial_V_tc
person_score$RAVLT_Learning = person_score$RAVLT_Trial_V_tc - person_score$RAVLT_Trial_I_tc
person_score$RAVLT_Forgetting = person_score$RAVLT_Trial_V_tc - person_score$RAVLT_Trial_VII_tc
person_score$RAVLT_PercForget = person_score$RAVLT_Forgetting / person_score$RAVLT_Trial_V_tc

# Variable 4: ABCD Youth Delay Discounting Scores -------------------------
# ===========================Task Introduction==================================
# The participant makes several choices between a small-immediate hypothetical 
# reward right now, or a standard hypothetical $100 reward at different time 
# points (6h, 1 day, 1 week, 1 month, 3-month, 1 year, and 5 years) in the future.
# Each block of choices features the same delay to the larger reward, and the
# immediate reward is titrated after each choice, until both the smaller-sooner
# reward and the delayed-$100 reward have equal subjective value to the participant.
# The summary results file calculates the “indifference point” (the small-immediate
# amount deemed to have the same subjective value as the $100 delayed reward) at 
# each of the seven delay intervals. When plotted, the area under the (hyperbolic)
# curve formed by these indifference points is frequently used to quantify severity
# of discounting of delayed rewards. The summary file also has a two-part validity
# check called “JB Pass” 1 and 2, the criteria for which can be found in 
# (Johnson, M.W., and Bickel, W.K. (2008) Experimental and Clinical Psychopharmacology 16(3): 264–274).
# Failure on these criteria suggests inconsistent responding, such as a lack of
# an orderly decay in subjective value of a reward with progressively lengthening
# delays to its presentation. Users should consider restricting data analysis to
# participants for whom “values. Consistent per_JBcriterion1” and 
# “values.Consistent per_JBcriterion2” are both “yes.”
# ===========================Data coding Scheme=================================
# URL: https://nda.nih.gov/data_structure.html?short_name=abcd_yddss01
# ddis_scr_values_completed: 0 = script was not completed (prematurely aborted); 1 = script was completed (all conditions run)
# ddis_scr_val_immedcho: 	values.immediateChoiceValidity. (Added in June 2018) 
#                         Tally of immediate choices in the three validity questions 
#                         presented after the "3months", "1year", and "5years" delay
#                         blocks (e.g. "get $100 NOW" vs. "get $100 in 5 years").
#                         Values < 3 indicate some inattentive (or at least irrational)
#                         behavior by the participant.
# ddis_scr_val_indif_point_6h: 6-hour delay "indifference point"
# ddis_scr_val_indif_pnt_1da: 1-day delay "indifference point"
# ddis_scr_val_indif_pnt_1week: 1-week delay "indifference point"
# ddis_scr_val_indif_pnt_1mth: 1-month delay "indifference point"
# ddis_scr_val_indif_pnt_3mth: 3-month delay "indifference point"
# ddis_scr_val_indif_pnt_1yr: 1-year delay "indifference point"
# ddis_scr_val_indif_pnt_5yr: 5-year delay "indifference point"
# ddis_scr_expr_mnrt_immcho: 	meanRT_immedChoices: mean latency (in ms) of 'immediate' choices
# ddis_scr_expr_medrt_immedcho: medianRT_immedChoices: median latency (in ms) of 'immediate' choices
# ddis_scr_expr_mnrt_delaycho: meanRT_delayedChoices: mean latency (in ms) of 'delayed' choices
# ddis_scr_expr_medrt_delaycho: medianRT_delayedChoices: median latency (in ms) of 'delayed' choices

ddt <- readABCDdata('abcd_yddss01.txt')
ddt <- select(ddt,c(src_subject_id,interview_age,sex,eventname,
                    ddis_scr_values_completed,
                    ddis_scr_val_immedcho,
                    ddis_scr_val_indif_point_6h,
                    ddis_scr_val_indif_pnt_1da,
                    ddis_scr_val_indif_pnt_1week,
                    ddis_scr_val_indif_pnt_1mth,
                    ddis_scr_val_indif_pnt_3mth,
                    ddis_scr_val_indif_pnt_1yr,
                    ddis_scr_val_indif_pnt_5yr,
                    ddis_scr_expr_mnrt_immcho,
                    ddis_scr_expr_medrt_immedcho,
                    ddis_scr_expr_mnrt_delaycho,
                    ddis_scr_expr_medrt_delaycho))
colnames(ddt) <- c('src_subject_id','interview_age','sex','eventname',
                    'DDT_CompletedFlag',
                    'DDT_Validity',
                    'DDT_IndifPnt_6h',
                    'DDT_IndifPnt_1day',
                    'DDT_IndifPnt_1week',
                    'DDT_IndifPnt_1mth',
                    'DDT_IndifPnt_3mth',
                    'DDT_IndifPnt_1yr',
                    'DDT_IndifPnt_5yr',
                    'DDT_MeanRT_Immcho',
                    'DDT_MedianRT_Immcho',
                    'DDT_MeanRT_Delaycho',
                    'DDT_MedianRT_Delaycho')
sapply(ddt, typeof)
ddt = mutate(ddt,across(.cols=5:ncol(ddt), .fns=as.numeric))
sapply(ddt, typeof)

# Variable 5: ABCD Emotional Stroop Task ----------------------------------
# ===========================Task Introduction==================================
# The emotional Stroop task measures cognitive control under conditions of 
# emotional salience. The task-relevant dimension is an emotional word, 
# which participants categorize as either a "good" feeling (happy, joyful)
# or a "bad" feeling (angry, upset). The task-irrelevant dimension is an image,
# which is of a teenager’s face with either a happy or an angry facial expression.
# Trials are of two types. On congruent trials, the word and facial emotion are
# of the same valence (e.g. a happy face paired with word "joyful").
# The location of the word varies from trial-to-trial, presented either on the
# top of the image or at the bottom. 
# On incongruent trials, the word and facial expression are of different valence
# (e.g., a happy face paired with word "angry").
# Participants work through 2 test blocks:one block consists of 50% congruent
# and 50% incongruent trials; the other consists of 25% incongruent trials
# and 75% congruent trials. The composition of the former type of block helps
# individuals keep the task set in mind more so than the latter.
# The 25% incongruent/75% congruent block is always administered first,
# followed by the 50% incongruent/50% congruent block. Accuracy and response times
# for congruent versus incongruent trials for the total task and within each
# emotion subtype (happy/joyful; angry/upset) are calculated. Relative difficulties
# with cognitive control are indexed by lower accuracy rates and longer reaction
# times for incongruent relative to congruent trials.
# ===========================Data coding Scheme=================================
# URL: https://nda.nih.gov/data_structure.html?short_name=abcd_yest01
# strp_scr_values_completed: 0 = script was not completed (prematurely aborted); 1 = script was completed (all conditions run)
# strp_scr_acc_all: Proportion correct overall (across all test trials)
# strp_scr_acc_congr: Proportion correct in "congruent" pairings trials (across test blocks)
# strp_scr_acc_incongr: Proportion correct in "incongruent" pairings trials (across test blocks)
# strp_scr_mnrt_all: Mean correct latency (in ms) overall
# strp_scr_mnrt_congr: mean correct latency (in ms) in "congruent" pairings trials (across test blocks)
# strp_scr_mnrt_incongr: Mean correct latency (in ms) in "incongruent" pairings trials (across test blocks)
stroop <- readABCDdata('abcd_yest01.txt')
stroop <- select(stroop,c(src_subject_id,interview_age,sex,eventname,
                          strp_scr_values_completed,
                          strp_scr_acc_all,
                          strp_scr_acc_congr,
                          strp_scr_acc_incongr,
                          strp_scr_mnrt_all,
                          strp_scr_mnrt_congr,
                          strp_scr_mnrt_incongr))
colnames(stroop) <- c('src_subject_id','interview_age','sex','eventname',
                   'EmST_CompletedFlag',
                   'EmST_Acc_all',
                   'EmST_Acc_Congr',
                   'EmST_Acc_Incongr',
                   'EmST_MeanRT_all',
                   'EmST_MeanRT_Congr',
                   'EmST_MeanRT_Incongr')
sapply(stroop, typeof)
stroop = mutate(stroop,across(.cols=5:ncol(stroop), .fns=as.numeric))
sapply(stroop, typeof)
# Calculating the difference of Acc and meanRT between congrument and incongrument condition
# Notes: lower Acc indicates poor cognitive control
# longer mean RT indicates poor cognitive control
stroop$EmST_Acc_Diff = stroop$EmST_Acc_Incongr - stroop$EmST_Acc_Congr
stroop$EmST_MeanRT_Diff = stroop$EmST_MeanRT_Incongr - stroop$EmST_MeanRT_Congr

# Variable 6: ABCD Game of Dice Task --------------------------------------
# ===========================Task Introduction==================================
# The Game of Dice Task assesses decision-making under conditions of specified
# risk and has been successfully used with adolescent samples. Risk taking is
# assessed by having participants attempt to predict the outcome of a dice roll
# by choosing among different options that vary on their outcome probability
# and pay-off across 18 trials. Specific rules and probabilities for monetary
# gains and losses are evident throughout the task. On each trial, participants
# predict the outcome of a dice roll by choosing from four different options
# (e.g., one number vs. multiple numbers). Options with more numbers 
# (i.e. higher probability of winning) are associated with a lesser reward
# compared to those with one or two possible numbers (i.e. lower probability of winning).
# The two options with the lowest probability of winning are considered as
# ‘risky choices.’ The total number of risky choices is often used to quantify performance.
# Summary scores are provided.
# ===========================Data coding Scheme=================================
# URL: https://nda.nih.gov/data_structure.html?short_name=abcd_gdss01
# gdt_scr_session: Session number (01,02,03)
# gdt_scr_event: Event selected by RA if not 2-year follow-up (HIDEFROMCOMPLETION)
# gdt_scr_parameters_gdtversion: parameters.GDTversion: 1 = original version with feedback (default); 2 = version without feedback
# gdt_scr_values_account_balance: values.account_balance: Amount participant owns at the end
# gdt_scr_values_single: Counts how many times participant has bet on 1 specific dice face
# gdt_scr_values_double: Counts how many times participant has bet on 2 specific dice face
# gdt_scr_values_triple: Counts how many times participant has bet on 3 specific dice face
# gdt_scr_values_quadruple: Counts how many times participant has bet on 4 specific dice face
# gdt_scr_values_safe: Counts how many times participants selected a safe bet (bets on 3 or 4 dice faces)
# gdt_scr_values_risky: Counts how many times participants selected a risky bet (bets on 1 or 2 dice faces)
# gdt_scr_expressions_net_score: Reflects the number of safe (bets on 3 or 4 numbers) vs. risky (bets on 1 or 2 numbers) choices.
#                                It is computed as number of safe bets minus number of risky bets.
# gdt_scr_values_wins: Adds the number of winning bets
# gdt_scr_values_losses: Adds the number of losing bets
gdt <- readABCDdata('abcd_gdss01.txt')
gdt <- select(gdt,c(src_subject_id,interview_age,sex,eventname,
                    gdt_scr_session,
                    gdt_scr_event,
                    gdt_scr_parameters_gdtversion,
                    gdt_scr_values_account_balance,
                    gdt_scr_values_single,
                    gdt_scr_values_double,
                    gdt_scr_values_triple,
                    gdt_scr_values_quadruple,
                    gdt_scr_values_safe,
                    gdt_scr_values_risky,
                    gdt_scr_expressions_net_score,
                    gdt_scr_values_wins,
                    gdt_scr_values_losses))
colnames(gdt) <- c('src_subject_id','interview_age','sex','eventname',
                    'GDT_SessionNumber',
                    'GDT_event',
                    'GDT_Version',
                    'GDT_AccountBalance',
                    'GDT_Counts_1',
                    'GDT_Counts_2',
                    'GDT_Counts_3',
                    'GDT_Counts_4',
                    'GDT_Counts_Safe',
                    'GDT_Counts_Risky',
                    'GDT_NetScore',
                    'GDT_Counts_Win',
                    'GDT_Counts_Loss')
sapply(gdt, typeof)
gdt = mutate(gdt,across(.cols=8:ncol(gdt), .fns=as.numeric))
sapply(gdt, typeof)


# Variable 7: ABCD Social Influence Task ----------------------------------
# ===========================Task Introduction==================================
# The Social Influence Task (SIT) assesses risk perception and propensity for
# risk taking, as well as susceptibility to perceived peer influence.
# Over the course of 40 trials, participants are presented with a variety of
# risky scenarios. Participants are asked to rate an activity’s risk by moving
# a slider bar between “very LOW risk” (left) and “very HIGH risk” (right).
# After submitting an initial rating, participants are shown a risk rating
# of the same activity that is seemingly provided by a group of peers.
# This peer rating condition is either 4 points lower (‘-4’ condition),
# 2 points lower (‘-2’ condition), 2 points higher (‘+2’ condition) or
# 4 points higher (‘+4’ condition) than the participant’s initial rating.
# Participants are asked to rate the riskiness of the scenario again.
# For both the initial and final rating trials, participants have a time limit
# of 4500 ms to provide their rating.
# The task is designed to try to ensure ~25% of trials (~10 trials) are in each
# of the peer rating conditions. To do this, the task script restricts random
# sampling to only those conditions that can be run given the participant's
# initial ratings (e.g., if a participant selected a rating of 1.8,
# condition -4 and condition -2 cannot be run as both of those conditions would
# result in a peer rating < 0). If none of the unselected peer conditions can
# be run due to rating constraints, yet 10 trials have already been in run in
# all the realistic peer conditions, the script uses the 'switch sign' method;
# it (randomly) selects from the unselected peer conditions and then switches
# the sign (e.g., selected peer condition -4 will be run as peer condition +4
# and vice versa). The script tracks how many such switches had to be made.
# Summary scores are provided.
# ===========================Data coding Scheme=================================
# URL: https://nda.nih.gov/data_structure.html?short_name=abcd_siss01
# sit_scr_version: Task Version Number
# sit_scr_event: Event selected by RA if not 2-year follow-up
# sit_scr_session: Session number (01,02,03)
# sit_scr_expr_minitialrat: The mean initial rating
# sit_scr_expr_mrt_initialrat: Mean reaction time (in ms) of submitting initial rating after onset of rating scale
# sit_scr_expr_mfinalrat: The mean final rating
# sit_scr_expr_mrt_finalrat: Mean reaction time (in ms) of submitting final rating after onset of rating scale
# sit_scr_expr_mratdiff1: The mean difference in ratings (final - initial) for peer rating condition '-4'
#   Note: positive values => final rating was higher than initial rating
#         negative values => final rating was lower than initial rating
# sit_scr_expr_mratdiff2: The mean difference in ratings (final - initial) for peer rating condition '-2'
# sit_scr_expr_mratdiff3: The mean difference in ratings (final - initial) for peer rating condition '+2'
# sit_scr_expr_mratdiff4: The mean difference in ratings (final - initial) for peer rating condition '+4'
# sit_scr_values_count1: Counts the number peer rating condition '-4' was run
# sit_scr_values_count2: Counts the number peer rating condition '-2' was run
# sit_scr_values_count3: Counts the number peer rating condition '+2' was run
# sit_scr_values_count4: Counts the number peer rating condition '+4' was run
# sit_scr_values_countflips: Counts the number of rating flips
# sit_scr_values_countnr_initial: Counts the number of 'no response' for initial rating trials
# sit_scr_values_countnr_final: Counts the number of 'no response' for final rating trials
sit <- readABCDdata('abcd_siss01.txt')
sit <- select(sit,c(src_subject_id,interview_age,sex,eventname,
                    sit_scr_version,
                    sit_scr_event,
                    sit_scr_session,
                    sit_scr_expr_minitialrat,
                    sit_scr_expr_mrt_initialrat,
                    sit_scr_expr_mfinalrat,
                    sit_scr_expr_mrt_finalrat,
                    sit_scr_expr_mratdiff1,
                    sit_scr_expr_mratdiff2,
                    sit_scr_expr_mratdiff3,
                    sit_scr_expr_mratdiff4,
                    sit_scr_values_count1,
                    sit_scr_values_count2,
                    sit_scr_values_count3,
                    sit_scr_values_count4,
                    sit_scr_values_countflips,
                    sit_scr_values_countnr_initial,
                    sit_scr_values_countnr_final))
colnames(sit) <- c('src_subject_id','interview_age','sex','eventname',
                   'SIT_Version',
                   'SIT_event',
                   'SIT_Session',
                   'SIT_MeanRating_Initial',
                   'SIT_MeanRT_Initial',
                   'SIT_MeanRating_Final',
                   'SIT_MeanRT_Final',
                   'SIT_MeanRating_Diff_1',
                   'SIT_MeanRating_Diff_2',
                   'SIT_MeanRating_Diff_3',
                   'SIT_MeanRating_Diff_4',
                   'SIT_Counts_1',
                   'SIT_Counts_2',
                   'SIT_Counts_3',
                   'SIT_Counts_4',
                   'SIT_Counts_Flips',
                   'SIT_Counts_NR_Initial',
                   'SIT_Counts_NR_Final')
sapply(sit, typeof)
sit = mutate(sit,across(.cols=8:ncol(sit), .fns=as.numeric))
sapply(sit, typeof)

# Variable 8: ABCD Stanford Mental Arithmetic Response Time Evaluation ----
# ===========================Task Introduction==================================
# The Stanford Mental Arithmetic Response Time Evaluation (SMARTE) is a youth
# measure that assess math fluency and single- and double-digit arithmetic
# operations via an iPad or smart phone app. Multiple accuracy and reaction time
# summary scores are calculated. See Starkey & McCandliss BD (2014).
# ===========================Data coding Scheme=================================

smarte <- readABCDdata('smarte_sumscores01.txt')

# Variable 9: Barkley Executive Function Scale ----------------------------
# ===========================Task Introduction==================================
# The short form of the Barkley Deficits in Executive Functioning Scale for
# Children and Adolescents on which a parent reports several different dimensions
# of their child or adolescent’s day-to-day executive functioning, such as
# organization, acting without thinking, clarity of expression, and procrastination
# that are predictive of future impairments in psychosocial functioning.
# See Barkley (2012).
# ===========================Data coding Scheme=================================
# bdefs_calm_down_p
# bdefs_consequences_p
# bdefs_distract_upset_p
# bdefs_explain_idea_p
# bdefs_explain_pt_p
# bdefs_explain_seq_p
# bdefs_impulsive_action_p
# bdefs_inconsistant_p
# bdefs_lazy_p
# bdefs_process_info_p
# bdefs_rechannel_p
# bdefs_sense_time_p
# bdefs_shortcuts_p
# bdefs_stop_think_p
barkley <- readABCDdata('barkley_exec_func01.txt')
barkley <- select(barkley,c(src_subject_id,interview_age,sex,eventname,
                    bdefs_calm_down_p,
                    bdefs_consequences_p,
                    bdefs_distract_upset_p,
                    bdefs_explain_idea_p,
                    bdefs_explain_pt_p,
                    bdefs_explain_seq_p,
                    bdefs_impulsive_action_p,
                    bdefs_inconsistant_p,
                    bdefs_lazy_p,
                    bdefs_process_info_p,
                    bdefs_rechannel_p,
                    bdefs_sense_time_p,
                    bdefs_shortcuts_p,
                    bdefs_stop_think_p))
colnames(barkley) <- c('src_subject_id','interview_age','sex','eventname',
                   'BDEFS_1',
                   'BDEFS_2',
                   'BDEFS_3',
                   'BDEFS_4',
                   'BDEFS_5',
                   'BDEFS_6',
                   'BDEFS_7',
                   'BDEFS_8',
                   'BDEFS_9',
                   'BDEFS_10',
                   'BDEFS_11',
                   'BDEFS_12',
                   'BDEFS_13',
                   'BDEFS_14')
sapply(barkley, typeof)
barkley = mutate(barkley,across(.cols=5:ncol(barkley), .fns=as.numeric))
sapply(barkley, typeof)
for (i in 5:18){
  barkley[[i]] <- RECODE(barkley[[i]],'777=NA')
}
naniar::miss_var_summary(barkley)
barkley$BEDFS_Sum = SUM(barkley,'BDEFS_',1:14)


# List out the time points for each variable ------------------------------
table(cash_choice$eventname)
table(little_man$eventname)
table(person_score$eventname)
table(ddt$eventname)
table(stroop$eventname)
table(gdt$eventname)
table(sit$eventname)
table(smarte$eventname)
table(barkley$eventname)

data = merge(cash_choice,little_man,by = intersect(colnames(cash_choice),colnames(little_man)),all = T)
data = merge(data,person_score,by = intersect(colnames(data),colnames(person_score)),all = T)
data = merge(data,ddt,by = intersect(colnames(data),colnames(ddt)),all = T)
data = merge(data,stroop,by = intersect(colnames(data),colnames(stroop)),all = T)
data = merge(data,gdt,by = intersect(colnames(data),colnames(gdt)),all = T)
data = merge(data,sit,by = intersect(colnames(data),colnames(sit)),all = T)
data = merge(data,barkley,by = intersect(colnames(data),colnames(barkley)),all = T)

saveRDS(data,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_NeurocognitionTask.rds")
