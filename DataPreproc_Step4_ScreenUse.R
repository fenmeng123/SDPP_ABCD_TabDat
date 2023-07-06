# =============================================================================#
# SDPP Step 3: Read and re-coding Screen Use-related data
# R Packages Dependency: bruceR, forcats, naniar
# Step File Notes: 
# 1. The imputation about the core demographics was referred to ABCD-DAIRC
# 2. Ref Link:
# https://github.com/fenmeng123/2022_JAACAP_ABCD_SMA_pattern
# https://wiki.abcdstudy.org/release-notes/non-imaging/novel-technologies.html#screen-time-questionnaire
# 3. Target File: ABCD4.0_Demographics_Recode.rds
# Update Date: 2023.06.16 By Kunru Song
# Update Date: 2023.07.06 By Kunru Song
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_4.txt'
AutoLogFilePath = fullfile(ProjectDirectory,'Res_1_Logs',AutoLogFileName)
sink(file = AutoLogFilePath)
ResultsOutputDir = S4_ResultsOutputDir
library(bruceR)
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load abcd_p_demo.csv and perform re-coding --------------------------------
stq_FileDir = fullfile(TabulatedDataDirectory,'/novel-technologies/nt_y_st.csv')
stq = readABCDdata(stq_FileDir)
fprintf("Youth Screen Time Questionnaire Raw Data-Variable Data type:\n")
sapply(stq,typeof)
stq %>% MVA.Report.By.Wave() %>% 
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_SMA_RawData.doc'),
              row.names = F,
              nsmalls = 1)
# 3. Re-coding Youth's Screen Use Time (Screen Time Questionnaire) -------------
#------------------- Screen Time Survey (baseline and 1-year FU) --------------#
# Re-code and rename to a 7-level ordinal variable as numeric data type
# 0 = None; .25 = < 30 minutes; 0.5 = 30 minutes; 1 = 1 hour; 2 = 2 hours; 3 = 3 hours; 4 = 4+ hours 
# //Example: 1½ hours would be coded as 1 hour, rather than 2 hours.
# screen1_wkdy_y	weekday Watch TV shows or movies 
# screen2_wkdy_y  weekday Watch videos (such as YouTube)
# screen3_wkdy_y  weekday Play video games on a computer, console, phone or other device (Xbox, Play Station, iPad)
# screen4_wkdy_y  weekday Text on a cell phone, tablet, or computer
# screen5_wkdy_y  weekday Visit social networking sites like Facebook, Twitter, Instagram, etc. 
# screen_wkdy_y   weekday Video chat (Skype, Facetime, etc.)
# 
# screen7_wknd_y  weekend Watch TV shows or movies
# screen8_wknd_y  weekend Watch videos (such as YouTube)
# screen9_wknd_y  weekend Play video games on a computer, console, phone or other device (Xbox, Play Station, iPad)
# screen10_wknd_y weekend Text on a cell phone, tablet, or computer (GChat, Whatsapp, etc.)
# screen11_wknd_y weekend Visit social networking sites like Facebook, Twitter, Instagram, etc.
# screen12_wknd_y weekend Video chat (Skype, Facetime, etc.)
stq = rename(stq,
             weekday_TV = screen1_wkdy_y,
             weekday_Video = screen2_wkdy_y,
             weekday_Game = screen3_wkdy_y,
             weekday_Text = screen4_wkdy_y,
             weekday_SocialNet = screen5_wkdy_y,
             weekday_VideoChat = screen_wkdy_y,
             weekend_TV = screen7_wknd_y,
             weekend_Video = screen8_wknd_y,
             weekend_Game = screen9_wknd_y,
             weekend_Text = screen10_wknd_y,
             weekend_SocialNet = screen11_wknd_y,
             weekend_VideoChat = screen12_wknd_y)
TPD_STQ_OLD = select(stq,
                     c(weekday_TV, weekday_Video,weekday_Game,
                       weekday_Text,weekday_SocialNet,weekday_VideoChat,
                       weekend_TV,weekend_Video,weekend_Game,
                       weekend_Text,weekend_SocialNet,weekend_VideoChat))
# (Mature-rated games and movies items were kept from baseline to 4-year FU)
# 0 = Never; 1 = Once in a while; 2 = Regularly; 3 = All the time
# screen13_y How often do you play mature-rated video games (e.g., Call of Duty, Grand Theft Auto, Assassin's Creed, etc.)?
# screen14_y How often do you watch R-rated movies?
stq = rename(stq,
             R_rated_Games = screen13_y,
             R_rated_Movies = screen14_y)
#------------------- Screen Time Survey (2-year FU) ---------------------------#
# please see ReadMe Usage Note 4
# hr: hours 0::23 
# min: minutes 0,15,30,45
# 
# weekday screen use time
# screentime_1_wkdy_hr weekday Watch or stream TV shows or movies? (such as Hulu, Netflix or Amazon, not including videos on YouTube)
# screentime_1_wkdy_min
# screentime_2_wkdy_hr weekday Watch or stream videos or live stream (such as YouTube, Twitch)
# screentime_2_wkdy_min
# For 3-year and 4-year follow-up:
# Includes variables: screentime_1_wkdy2_hr, screentime_1_wkdy2_min, screentime_7_8_wknd_hr, screentime_7_8_wknd_min.
# 
# screentime_3_wkdy_hr weekday Play single-player video games on a computer, console, phone or other device (Xbox, Play Station, iPad, AppleTV)
# screentime_3_wkdy_min
# screentime_4_wkdy_hr weekday Play multiplayer video games on a computer, console, phone, or other device (Xbox, Play Station, iPad, AppleTV) where you can interact with others in the game?
# screentime_4_wkdy_min
# screentime_5_wkdy_hr weekday Text on a cell phone, tablet, computer, iPod, or other electronic device (e.g., GChat, Whatsapp, Kik etc.)
# screentime_5_wkdy_min
# screentime_6_wkdy_hr weekday Visit social media apps (e.g., Snapchat, Facebook, Twitter, Instagram, TikTok, etc.? (Do not include time spent editing photos or videos to post on social media.)
# screentime_6_wkdy_min
# 
# screentime_7_wkdy_hr weekday Edit photos or videos to post on social media.
# screentime_7_wkdy_min
# 
# screentime_8_wkdy_hr weekday Video chat (Skype, FaceTime, VRchat, etc.)
# screentime_8_wkdy_min
# screentime_9_wkdy_hr weekday Searching or browsing the internet (e.g., using Google) that is NOT for school
# screentime_9_wkdy_min
# 
# weekend day screen use time
# 
# screentime_7_wknd_hr  weekend Watch or stream TV shows or movies? (such as Hulu, Netflix or Amazon, not including videos on YouTube)
# screentime_7_wknd_min
# 
# screentime_8_wknd_hr  weekend Watch or stream videos or live stream (such as YouTube, Twitch)
# screentime_8_wknd_min
# 
# screentime_9_wknd_hr  weekend Play single-player video games on a computer, console, phone or other device (Xbox, Play Station, iPad, AppleTV)
# screentime_9_wknd_min
# screentime_10_wknd_hr weekend Play multiplayer video games on a computer, console, phone, or other device (Xbox, Play Station, iPad, AppleTV) where you can interact with others in the game
# screentime_10_wknd_min
# screentime_11_wknd_hr weekend Text on a cell phone, tablet, computer, iPod, or other electronic device (e.g., GChat, Whatsapp, Kik etc.)
# screentime_11_wknd_min
# screentime_12_wknd_hr weekend Visit social media apps (e.g., Snapchat, Facebook, Twitter, Instagram, TikTok, etc.? (Do not include time spent editing photos or videos to post on social media.)
# screentime_12_wknd_min
# 
# screentime_13_wknd_hr weekend Edit photos or videos to post on social media.
# screentime_13_wknd_min
# 
# screentime_14_wknd_hr weekend Video chat (Skype, FaceTime, VRchat, etc.)
# screentime_14_wknd_min
# screentime_15_wknd_hr weekend Searching or browsing the internet (e.g., using Google) that is NOT for school
# screentime_15_wknd_min
stq_new_18item = subset(stq,select = c(screentime_1_wkdy_hr,screentime_1_wkdy_min,
                                       screentime_2_wkdy_hr,screentime_2_wkdy_min,
                                       screentime_3_wkdy_hr,screentime_3_wkdy_min,
                                       screentime_4_wkdy_hr,screentime_4_wkdy_min,
                                       screentime_5_wkdy_hr,screentime_5_wkdy_min,
                                       screentime_6_wkdy_hr,screentime_6_wkdy_min,
                                       screentime_7_wkdy_hr,screentime_7_wkdy_min,
                                       screentime_8_wkdy_hr,screentime_8_wkdy_min,
                                       screentime_9_wkdy_hr,screentime_9_wkdy_min,
                                       screentime_7_wknd_hr,screentime_7_wknd_min,
                                       screentime_8_wknd_hr,screentime_8_wknd_min,
                                       screentime_9_wknd_hr,screentime_9_wknd_min,
                                       screentime_10_wknd_hr,screentime_10_wknd_min,
                                       screentime_11_wknd_hr,screentime_11_wknd_min,
                                       screentime_12_wknd_hr,screentime_12_wknd_min,
                                       screentime_13_wknd_hr,screentime_13_wknd_min,
                                       screentime_14_wknd_hr,screentime_14_wknd_min,
                                       screentime_15_wknd_hr,screentime_15_wknd_min,
                                       screentime_1_wkdy2_hr, screentime_1_wkdy2_min,
                                       screentime_7_8_wknd_hr, screentime_7_8_wknd_min))
# Recode to a 7-level ordinal variable
# 2-year open-answered STQ
open_weekday_TV_T2 = stq_new_18item$screentime_1_wkdy_hr + stq_new_18item$screentime_1_wkdy_min/60
open_weekend_TV_T2 = stq_new_18item$screentime_7_wknd_hr + stq_new_18item$screentime_7_wknd_min/60
open_weekday_TV_T3_T4 = stq_new_18item$screentime_1_wkdy2_hr + stq_new_18item$screentime_1_wkdy2_min/60
open_weekend_TV_T3_T4 = stq_new_18item$screentime_7_8_wknd_hr + stq_new_18item$screentime_7_8_wknd_min/60
stq_new_18item$open_weekday_TV  = Merge.Value.NA(open_weekday_TV_T2,open_weekday_TV_T3_T4)
stq_new_18item$open_weekday_Video  = stq_new_18item$screentime_2_wkdy_hr + stq_new_18item$screentime_2_wkdy_min/60
stq_new_18item$open_weekday_SingleGame  = stq_new_18item$screentime_3_wkdy_hr + stq_new_18item$screentime_3_wkdy_min/60
stq_new_18item$open_weekday_MultiGame  = stq_new_18item$screentime_4_wkdy_hr + stq_new_18item$screentime_4_wkdy_min/60
stq_new_18item$open_weekday_Text  = stq_new_18item$screentime_5_wkdy_hr + stq_new_18item$screentime_5_wkdy_min/60
stq_new_18item$open_weekday_SocialApps  = stq_new_18item$screentime_6_wkdy_hr + stq_new_18item$screentime_6_wkdy_min/60
stq_new_18item$open_weekday_EditPhoto  = stq_new_18item$screentime_7_wkdy_hr + stq_new_18item$screentime_7_wkdy_min/60
stq_new_18item$open_weekday_VideoChat  = stq_new_18item$screentime_8_wkdy_hr + stq_new_18item$screentime_8_wkdy_min/60
stq_new_18item$open_weekday_Surf  = stq_new_18item$screentime_9_wkdy_hr + stq_new_18item$screentime_9_wkdy_min/60
stq_new_18item$open_weekend_TV  = Merge.Value.NA(open_weekend_TV_T2,open_weekend_TV_T3_T4)
stq_new_18item$open_weekend_Video  = stq_new_18item$screentime_8_wknd_hr + stq_new_18item$screentime_8_wknd_min/60
stq_new_18item$open_weekend_SingleGame  = stq_new_18item$screentime_9_wknd_hr + stq_new_18item$screentime_9_wknd_min/60
stq_new_18item$open_weekend_MultiGame  = stq_new_18item$screentime_10_wknd_hr + stq_new_18item$screentime_10_wknd_min/60
stq_new_18item$open_weekend_Text  = stq_new_18item$screentime_11_wknd_hr + stq_new_18item$screentime_11_wknd_min/60
stq_new_18item$open_weekend_SocialApps  = stq_new_18item$screentime_12_wknd_hr + stq_new_18item$screentime_12_wknd_min/60
stq_new_18item$open_weekend_EditPhoto  = stq_new_18item$screentime_13_wknd_hr + stq_new_18item$screentime_13_wknd_min/60
stq_new_18item$open_weekend_VideoChat  = stq_new_18item$screentime_14_wknd_hr + stq_new_18item$screentime_14_wknd_min/60
stq_new_18item$open_weekend_Surf  = stq_new_18item$screentime_15_wknd_hr + stq_new_18item$screentime_15_wknd_min/60
# Combing the single-player game time and multiple-player game time
stq_new_18item$open_weekday_Game = SUM(stq_new_18item,
                                       vars = c('open_weekday_SingleGame',
                                                'open_weekday_MultiGame'),
                                       na.rm = F)
stq_new_18item$open_weekend_Game = SUM(stq_new_18item,
                                       vars = c('open_weekend_SingleGame',
                                                'open_weekend_MultiGame'),
                                       na.rm = F)
for (i in grep('^open.*',colnames(stq_new_18item),value = T) ){
  stq_new_18item[ str_replace(i,'open_','') ] = Recode.STQ(stq_new_18item[[i]],
                                                           Scheme = '7 Levels')
}
stq_new_18item %>%
  select(starts_with(c('weekday','weekend'))) %>%
  rename(weekday_SocialNet = weekday_SocialApps,
         weekend_SocialNet = weekend_SocialApps) -> TPD_STQ_NEW
# Concatenate 12 Items in STQ
TPD_STQ = select(stq,c(src_subject_id,eventname))
TPD_STQ = cbind(TPD_STQ,
                TPD_STQ_NEW[,!colnames(TPD_STQ_NEW) %in% colnames(TPD_STQ_OLD)])
for (i in colnames(TPD_STQ_OLD)){
  TPD_STQ[[i]] <- Merge.Value.NA(TPD_STQ_OLD[[i]],TPD_STQ_NEW[[i]])
}
TPD_STQ = cbind(TPD_STQ,
                select(stq_new_18item,starts_with('open_')))
TPD_STQ %>% MVA.Report.By.Wave() %>% 
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_SMA_Recode_TPD_STQ.doc'),
              row.names = F,
              nsmalls = 1)
# Variables measured the TOTAL time in ABCD STQ
# 
# screentime_wkdy_typical_hr
# screentime_wkdy_typical_min
# 
# On a typical WEEKDAY (during the school year), how much TIME per day do you spend 
# in TOTAL on a computer, phone, tablet, iPod, or other device or video game? 
# Please do NOT include time spent on school related work, but do include 
# watching TV, shows or videos, texting or chatting, playing games, 
# or visiting social networking sites (Facebook, Twitter, Instagram)
# 
# screentime_wknd_typical_hr weekend Use Time Per day
# screentime_wknd_t_min
# 
# On a typical WEEKEND DAY (or non-school day, like during summer or holiday breaks), 
# how much TIME per day do you spend in TOTAL on a computer......
TPD_STQ$open_weekday_TOTAL = stq$screentime_wkdy_typical_hr + (stq$screentime_wkdy_typical_min)/60
TPD_STQ$open_weekend_TOTAL = stq$screentime_wknd_typical_hr + (stq$screentime_wknd_t_min)/60
TPD_STQ$weekday_TOTAL = Recode.STQ(TPD_STQ$open_weekday_TOTAL,
                                   Scheme = '7 Levels')
TPD_STQ$weekend_TOTAL = Recode.STQ(TPD_STQ$open_weekend_TOTAL,
                                    Scheme = '7 Levels')
# Variables measured the time spent on school-related work in ABCD STQ
# 
# screentime_wkdy_school_hr
# screentime_wkdy_school_min
# 
# Spend in TOTAL on school-related work on a phone, tablet, computer, or other
# computerized device? Please do not include time during school.
# 
# screentime_wknd_school_hr
# screentime_wknd_school_min
# 
TPD_STQ$open_weekday_School = stq$screentime_wkdy_school_hr + (stq$screentime_wkdy_school_min)/60
TPD_STQ$open_weekend_School = stq$screentime_wknd_school_hr + (stq$screentime_wknd_school_min)/60
TPD_STQ$weekday_School = Recode.STQ(TPD_STQ$open_weekday_School,
                                   Scheme = '7 Levels')
TPD_STQ$weekend_School = Recode.STQ(TPD_STQ$open_weekend_School,
                                   Scheme = '7 Levels')
# 4. Mobile Phone related measurements ---------------------------------------
# MPIQ 8 Items
stq %>% select(c(src_subject_id,eventname,
                 screentime_phone1,screentime_phone2,screentime_phone3,
                 screentime_phone4,screentime_phone5,screentime_phone6,
                 screentime_phone7,screentime_phone8)) %>%
  rename(MPIQ_1 = screentime_phone1,
         MPIQ_2 = screentime_phone2,
         MPIQ_3 = screentime_phone3,
         MPIQ_4 = screentime_phone4,
         MPIQ_5 = screentime_phone5,
         MPIQ_6 = screentime_phone6,
         MPIQ_7 = screentime_phone7,
         MPIQ_8 = screentime_phone8) -> MPIQ
for (i in grep("^MPIQ_.*", colnames(MPIQ),value = T)){
  MPIQ[[i]] <- Recode.ABCD.NA(MPIQ[[i]])
}
# screentime_phone_yn: Do you have your own mobile phone, smart watch or ipod? 
# 0=No; 1=Yes; 777=Refuse to Answer

# Mobile PHone Attachment (MPHA)
# Single Item measures the degree of phone attachment
# screentime_phone_scale: On a scale of 1-10 
# (with 1=barely check it/can go days without it, 
# and 10=check at least hourly when awake), how attached are you to your phone?
# 1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; 8=8; 9=9; 10=10; 777=Refuse to Answer



# social media related measurements ---------------------------------------

stq_SocialMedia = subset(stq,select = c(screentime_usesoc,screentime_smq_facebook,screentime_smq_instagram,screentime_smq_snapchat,screentime_smq_twitter,
                                        screentime_smq_youtube,screentime_smq_pinterest,screentime_smq_tumblr,screentime_smq_reddit,screentime_smq_mg_chat,
                                        screentime_smq_musical_ly,screentime_smq_other,screentime_smq_use___1,screentime_smq_use___2,screentime_smq_use___3,
                                        screentime_smq_use___4,screentime_smq_use___5,screentime_smq_use___6,screentime_smq_use___7,screentime_smq_use___8,
                                        screentime_smq_use___9,screentime_smq_use___10,screentime_smq_use___11,
                                        screentime_smq_use_most,screentime_smq_account,screentime_smq_followers,screentime_smq_following,screentime_smq_secret,
                                        screentime_smq_soc_med_hr,screentime_smq_sm_min))
# Q1： screentime_usesoc: Do you have a at least one social media account?
# 0=No; 1=Yes;
stq_SocialMedia$Ownership_SMAccount = RECODE(stq_SocialMedia$screentime_usesoc,"0='No';1='Yes';else=NA")
stq_SocialMedia$Ownership_SMAccount = factor(stq_SocialMedia$Ownership_SMAccount,levels = c('No','Yes'))
# Q2: screentime_smq_description: Which social media sites do you have an account on?
# List the number of accounts you have on each of the social media sites below.
# Number of accounts in social media sites
# 0=0; 1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; 8=8; 9=9; 10=10
# screentime_smq_facebook
# screentime_smq_instagram
# screentime_smq_snapchat
# screentime_smq_twitter
# screentime_smq_youtube
# screentime_smq_pinterest
# screentime_smq_tumblr
# screentime_smq_reddit
# screentime_smq_mg_chat： Multiplayer Videogame Online Chatting
# screentime_smq_musical_ly： TikTok
# screentime_smq_other
# screentime_smq_other_name(empty): Please fill in Other social media site
# screentime_smq_use_other(empty): If Other, please fill in name Social media 
# sites that you have an account on
stq_SocialMedia$SMAccount_NumTotal = SUM(stq_SocialMedia,
                                         vars = c('screentime_smq_facebook','screentime_smq_instagram','screentime_smq_snapchat','screentime_smq_twitter',
                                         'screentime_smq_youtube','screentime_smq_pinterest','screentime_smq_tumblr','screentime_smq_reddit',
                                         'screentime_smq_mg_chat','screentime_smq_musical_ly','screentime_smq_other'),na.rm = F)

# Q3: Which social media sites do you have an account on? 
# Select all that are applicable: 1=Yes=0 No
# 1, Facebook; 2, Instagram; 3, Snapchat; 4, Twitter; 5, YouTube;
# 6, Pinterest; 7, Tumblr; 8, Reddit; 
# 9, Multiplayer Videogame Online Chatting;
# 10, Other; 11, TikTok
# screentime_smq_use___1 ~ 	screentime_smq_use___11
stq_SocialMedia$SMSite_NumCounts = SUM(stq_SocialMedia,
                                       vars = c('screentime_smq_use___1','screentime_smq_use___2','screentime_smq_use___3',
                                       'screentime_smq_use___4','screentime_smq_use___5','screentime_smq_use___6',
                                       'screentime_smq_use___7','screentime_smq_use___8',
                                       'screentime_smq_use___9','screentime_smq_use___10','screentime_smq_use___11'),na.rm = F)

# Q4: The Most Frequency usage about social media sites
# screentime_smq_use_most: Which social media site do you use the most?
# 0=I do not have a social media account; 1=Facebook; 2=Instagram; 3=Snapchat;
# 4=Twitter; 5=YouTube; 6=Pinterest=| 7=Tumblr; 8=Reddit; 11=TikTok;
# 9=Multiplayer Videogame Online Chatting; 10=Other
stq_SocialMedia$MostUse_SMSite = RECODE(stq_SocialMedia$screentime_smq_use_most,
                                            "0='No Account';1='Facebook'; 2='Instagram'; 3='Snapchat'; 4='Twitter';
                                            5='YouTube'; 6='Pinterest';7='Tumblr'; 8='Reddit'; 11='TikTok'; 
                                            9='MPVG online chat'; 10='Other';else=NA")
# Q5: screentime_smq_account: On (screentime_smq_use_most), is your account public or private?
# -1=Not Applicable; 1=Public; 2=Private; 999=Don't Know; 777=Refuse to Answer
stq_SocialMedia$Ownership_PubSM_Account = factor(RECODE(stq_SocialMedia$screentime_smq_account,
                                         "-1='Not Applicable';1='Public'; 2='Private';else=NA"))

# Q6: screentime_smq_followers: On (screentime_smq_use_most), how many followers do you have?	
# Q7: screentime_smq_following: On (screentime_smq_use_most), how many people or groups are you following?
stq_SocialMedia$Ownership_SMFollowers = stq_SocialMedia$screentime_smq_followers
stq_SocialMedia$Ownership_SMFollowing = stq_SocialMedia$screentime_smq_following
# Q8: screentime_smq_secret: Do you have a social media account that you keep secret from your parents?
# 1=Yes; 0=No; 777=Refuse to Answer
stq_SocialMedia$Ownership_SecretAccount = RECODE(stq_SocialMedia$screentime_smq_secret,"1='Yes';0='No';else=NA")
stq_SocialMedia$Ownership_SecretAccount = factor(stq_SocialMedia$Ownership_SecretAccount,levels = c('No','Yes'))
# Q9: screentime_smq_soc_med_hr： How much TIME per day do you spend on social media apps?
# screentime_smq_sm_min
stq_SocialMedia$TimePerDay_SM = stq_SocialMedia$screentime_smq_soc_med_hr + stq_SocialMedia$screentime_smq_sm_min/60

# |Social Media Addiation Scale (6 Items)
# |1=Never; 2=Very rarely; 3=Rarely; 4=Sometimes; 5=Often; 6=Very often; 777=Refuse to answer
# |screentime_smqa1: I spend a lot of time thinking about social media apps or planning my use of social media apps.
# |screentime_smqa2: I feel the need to use social media apps more and more
# |screentime_smqa3: I use social media apps so I can forget about my problems.
# |screentime_smqa4: I've tried to use my social media apps less but I can't.
# |screentime_smqa5: I've become stressed or upset if I am not allowed to use my social media apps.
# |screentime_smqa6: I use social media apps so much that it has had a bad effect on my schoolwork or job
# 
# |Video Game Addiction Scale (same with above but replace the "social media" with "playing video games") 
# |screentime_vgaq1
# |screentime_vgaq2
# |screentime_vgaq3
# |screentime_vgaq4
# |screentime_vgaq5
# |screentime_vgaq6	
stq_SMAS_VGAS = subset(stq,select = c(screentime_smqa1,screentime_smqa2,screentime_smqa3,screentime_smqa4,screentime_smqa5,screentime_smqa6,
                                      screentime_vgaq1,screentime_vgaq2,screentime_vgaq3,screentime_vgaq4,screentime_vgaq5,screentime_vgaq6))
colnames(stq_SMAS_VGAS) = c('SMAS_1','SMAS_2','SMAS_3','SMAS_4','SMAS_5','SMAS_6',
                            'VGAS_1','VGAS_2','VGAS_3','VGAS_4','VGAS_5','VGAS_6')


# Online dating questionnaire ---------------------------------------------
stq_DateingApps = subset(stq,select = c(screentime_odq1,screentime_odq2,screentime_odq3,screentime_odq4))
colnames(stq_DateingApps) = c('Ownership_Past_DatingApps','Ownership_Current_DatingApps',
                              'TimePerWeek_DatingApps','Ownership_InpersonDateMeet')
# Q1: screentime_odq1: Have you ever used a dating app?
# 1=Yes; 0=No; 999=I don't know what that is?; 777=Refuse to answer
stq_DateingApps$Ownership_Past_DatingApps = RECODE(stq_DateingApps$Ownership_Past_DatingApps,
                                                   "1='Yes';0='No';999='Dont know';else=NA")
stq_DateingApps$Ownership_Past_DatingApps = factor(stq_DateingApps$Ownership_Past_DatingApps,
                                                   levels = c('No','Yes',"Dont know"))
# Q2: screentime_odq2: Are you currently using a dating app?; 
# 1=Yes; 0=No; 777=Refuse to answer
stq_DateingApps$Ownership_Current_DatingApps = RECODE(stq_DateingApps$Ownership_Current_DatingApps,
                                                      "1='Yes';0='No';else=NA")
stq_DateingApps$Ownership_Current_DatingApps = factor(stq_DateingApps$Ownership_Current_DatingApps,
                                                      levels = c('No','Yes'))
# Q3: screentime_odq3: How much time per week do you spend on online dating apps?
# 1=None; 2=< 30 minutes; 3=30 minutes; 4=1 hour; 5=2 hours; 6=3 hours; 7=4+ hours; 777=Refuse to answer
stq_DateingApps$TimePerWeek_DatingApps = RECODE(stq_DateingApps$TimePerWeek_DatingApps,
                                                "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;else=NA")
# Q4: screentime_odq4: Have you arranged an in-person meeting with someone you met only on the dating app? 
# 1=Yes; 0=No; 777=Refuse to answer
stq_DateingApps$Ownership_InpersonDateMeet = RECODE(stq_DateingApps$Ownership_InpersonDateMeet,
                                                    "1='Yes';0='No';else=NA")
stq_DateingApps$Ownership_InpersonDateMeet = factor(stq_DateingApps$Ownership_InpersonDateMeet,
                                                    levels = c('No','Yes'))



# Screen usage around bed time (SABT) --------------------------------------------
stq_Sleep = subset(stq,select = c(screentime_sq1,screentime_sq2,
                                  screentime_sq3,screentime_sq4,screentime_sq5,screentime_sq6,
                                  screentime_sq7,screentime_sq8,screentime_sq9,
                                  screentime_sq10,screentime_sq11,
                                  screentime_sq12,screentime_sq13))
colnames(stq_Sleep) = c('Ownership_DeviceInBedroom','Sleep_PhoneAction',
                        'SABT_TV','SABT_Game','SABT_Music','SABT_Text',
                        'SABT_SocialNet','SABT_ChatRoom','SABT_Surf',
                        'SABT_Study','SABT_Read',
                        'Sleep_WakeBefore','Sleep_WakeDuring')
# |A 9-item measure was administered to youth to assess engagement in
# |activities, including screen time activities, prior to sleeping. Items were
# |modified from Lemola et al. (2015) and Arora et al. (2014). On a 5-point
# |Likert scale ranging from 1 (never) to 5 (every night), youth reported
# |how often (in the past week) they engaged in the following activities
# |while already in bed before going to sleep: watch TV or movies, play
# |video games, play music, talk on the phone or text, spend time online on
# |social media, browse the internet, use a computer/laptop for studying.
# |sq3-sq13 shared the same answer items
# |1=0 nights in the past week; 2=1-2 nights; 3=3-4 nights; 4=5-7 nights; 777=Refused to answer
# |screentime_sq3: Watch or stream movies, videos, or TV shows
# |screentime_sq4: Play video games
# |screentime_sq5: Play music
# |screentime_sq6: Talk on the phone or text (If you do not own a phone, choose 0 nights)
# |screentime_sq7: Spend time online on social media (e.g. Facebook)
# |screentime_sq8: Spend time in chat rooms
# |screentime_sq9: Browse the Internet, Google-ing (not school related)
# |screentime_sq10: Use a computer/laptop for studying
# |screentime_sq11: Reading
for (i in (grep('SABT_.*',colnames(stq_Sleep),value = T)) ){
  stq_Sleep[i] = RECODE(stq_Sleep[[i]],'777=NA')
}

# Four additional items were asked related to sleep and media use which
# were adapted from questions in a National Sleep Foundation poll
# (Gradisar et al., 2013).
# screentime_sq1: Is there a TV set or an Internet connected electronic device (computer, iPad, phone) in your bedroom?
# 1=Yes; 0=No; 777=Refuse to answer	
# screentime_sq2: What do you usually do with your phone when you are ready to go to sleep? Do you
# -1=Not Applicable ; 1=Turn the phone off ; 2=Put the ringer on silent or vibrate ;
# 3=Leave the ringer on ; 4=Put it outside of the room where I sleep ; 777=Refuse to answer
# 
# screentime_sq12: In the past week, how often have you had phone calls, 
# text messages or emails that wake you after trying to go to sleep?
# 1=0 nights in the past week; 2=1-2 nights; 3=3-4 nights; 4=5-7 nights; 777=Refused to answer
# screentime_sq13: In the past week, when you woke up during the night, 
# how often have you used your phone or other device to send messages/play games/search 
# or browse the internet/use social media/read or write emails?
# 1=0 nights in the past week; 2=1-2 nights; 3=3-4 nights; 4=5-7 nights; 777=Refused to answer
stq_Sleep$Ownership_DeviceInBedroom = RECODE(stq_Sleep$Ownership_DeviceInBedroom,"1='Yes';2='No';else=NA")
stq_Sleep$Ownership_DeviceInBedroom = factor(stq_Sleep$Ownership_DeviceInBedroom,levels = c('No','Yes'))
stq_Sleep$Sleep_PhoneAction = RECODE(stq_Sleep$Sleep_PhoneAction,
                                  "-1='Not Applicable' ; 1='Turn off' ; 2='Silent' ; 3='Ringer On' ; 4='Outside';else=NA")
stq_Sleep$Sleep_PhoneAction = factor(stq_Sleep$Sleep_PhoneAction,
                                    levels = c('Outside','Turn off','Silent','Ringer On','Not Applicable'))
stq_Sleep$Sleep_WakeBefore = RECODE(stq_Sleep$Sleep_WakeBefore,'777=NA')
stq_Sleep$Sleep_WakeDuring = RECODE(stq_Sleep$Sleep_WakeDuring,'777=NA')

save(stq_anchor,stq_DateingApps,stq_MobilePhone,stq_new_18item,stq_old_12item,
     stq_Sleep,stq_SMAS_VGAS,stq_SocialMedia,stq_TimePerDay,
     file = "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_STQ_RECODE.Rda")


# concatenate all recoded STQ variables -----------------------------------

load("I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_STQ_RECODE.Rda")
#remove redundant columns (raw item score)
colnames(stq_DateingApps)
colnames(stq_MobilePhone)
colnames(stq_new_18item)
stq_new_18item = select(stq_new_18item,-c(screentime_1_wkdy_hr,screentime_1_wkdy_min,screentime_2_wkdy_hr,screentime_2_wkdy_min,screentime_3_wkdy_hr,   
                                          screentime_3_wkdy_min,screentime_4_wkdy_hr,screentime_4_wkdy_min,screentime_5_wkdy_hr,screentime_5_wkdy_min,  
                                          screentime_6_wkdy_hr,screentime_6_wkdy_min,screentime_7_wkdy_hr,screentime_7_wkdy_min,screentime_8_wkdy_hr,   
                                          screentime_8_wkdy_min,screentime_9_wkdy_hr,screentime_9_wkdy_min,screentime_7_wknd_hr,screentime_7_wknd_min,  
                                          screentime_8_wknd_hr,screentime_8_wknd_min,screentime_9_wknd_hr,screentime_9_wknd_min,screentime_10_wknd_hr,  
                                          screentime_10_wknd_min,screentime_11_wknd_hr,screentime_11_wknd_min,screentime_12_wknd_hr,screentime_12_wknd_min, 
                                          screentime_13_wknd_hr,screentime_13_wknd_min,screentime_14_wknd_hr,screentime_14_wknd_min,screentime_15_wknd_hr,  
                                          screentime_15_wknd_min ))
colnames(stq_old_12item)
colnames(stq_Sleep)
colnames(stq_SocialMedia)
stq_SocialMedia = select(stq_SocialMedia,-c(screentime_usesoc,screentime_smq_use_most,
                                            screentime_smq_account,screentime_smq_followers,
                                            screentime_smq_following,screentime_smq_secret,
                                            screentime_smq_soc_med_hr,screentime_smq_sm_min))
colnames(stq_SocialMedia) <- c("NumAccount_facebook","NumAccount_Instagram",
                               "NumAccount_snapchat","NumAccount_twitter",
                               "NumAccount_youtube","NumAccount_pinterest",
                               "NumAccount_tumblr","NumAccout_reddit",
                               "NumAccout_mg_chat","NumAccount_tiktok",
                               "NumAccount_other",
                               "SMS_Own_facebook","SMS_Own_instagrm",
                               "SMS_Own_snapchat","SMS_Own_twitter",
                               "SMS_Own_youtube","SMS_Own_pinterest",
                               "SMS_Own_tumblr","SMS_Own_reddit",
                               "SMS_Own_mg_chat","SMS_Own_other",
                               "SMS_Own_tiktok",
                               "Ownership_SMAccount","SMAccount_NumTotal",
                               "SMSite_NumCounts","MostUse_SMSite",
                               "Ownership_PubSM_Account","Ownership_SMFollowers",
                               "Ownership_SMFollowing","Ownership_SecretAccount",
                               "TimePerDay_SM")
colnames(stq_TimePerDay)
stq_TimePerDay = select(stq_TimePerDay,-c(screentime_wkdy_typical_hr,
                                          screentime_wkdy_typical_min,
                                          screentime_wknd_typical_hr,
                                          screentime_wknd_t_min))

stq_new_18item$weekday_Game = SUM(stq_new_18item,vars = c('open_weekday_SingleGame','open_weekday_MultiGame'),na.rm = F)
stq_new_18item$weekend_Game = SUM(stq_new_18item,vars = c('open_weekend_SingleGame','open_weekend_MultiGame'),na.rm = F)
stq_new_18item$weekday_Game = RECODE(stq_new_18item$weekday_Game,
                                                       "0=0;
                                                       0.25:0.75=1;
                                                       1:1.75=2;
                                                       2:2.75=3;
                                                       3:3.75=4;
                                                       4:hi=5;else=NA")
stq_new_18item$weekend_Game = RECODE(stq_new_18item$weekend_Game,
                                     "0=0;
                                                       0.25:0.75=1;
                                                       1:1.75=2;
                                                       2:2.75=3;
                                                       3:3.75=4;
                                                       4:hi=5;else=NA")

stq_new_18item$weekday_SocialNet = stq_new_18item$weekday_SocialApps
stq_new_18item$weekend_SocialNet = stq_new_18item$weekend_SocialApps

for (i in intersect(colnames(stq_new_18item),colnames(stq_old_12item)) ){
  merged_values = str_c( 
    replace_na(as.character(stq_old_12item[[i]]),""),
    replace_na(as.character(stq_new_18item[[i]]),"" )
    )
  stq_new_18item[i] = as.numeric(merged_values)
}


stq_recode = cbind(stq_anchor,stq_new_18item,stq_TimePerDay,stq_SocialMedia,
                   stq_SMAS_VGAS,stq_MobilePhone,stq_Sleep,stq_DateingApps)

saveRDS(stq_recode,file = "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_STQ_RECODE_Comb.rds")

# Missing values report
miss_var_summary(stq_recode) %>% 
  print_table(file = "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\MissVarRep_STQ_Recode.doc")

# |7-Level Recoding -----------------------------------------------------------|
# |
# |Recode screen use time to a 7-level categorical variable ----------------

stq = readRDS('I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_STQ_RawItem.rds')
stq_old_12item = subset(stq,select = c(screen1_wkdy_y,screen2_wkdy_y,screen3_wkdy_y,screen4_wkdy_y,screen5_wkdy_y,screen_wkdy_y,
                                       screen7_wknd_y,screen8_wknd_y,screen9_wknd_y,screen10_wknd_y,screen11_wknd_y,screen12_wknd_y))
colnames(stq_old_12item) = c('weekday_TV','weekday_Video','weekday_Game','weekday_Text','weekday_SocialNet','weekday_VideoChat',
                             'weekend_TV','weekend_Video','weekend_Game','weekend_Text','weekend_SocialNet','weekend_VideoChat')

stq_new_18item = subset(stq,select = c(screentime_1_wkdy_hr,screentime_1_wkdy_min,screentime_2_wkdy_hr,screentime_2_wkdy_min,
                                       screentime_3_wkdy_hr,screentime_3_wkdy_min,screentime_4_wkdy_hr,screentime_4_wkdy_min,
                                       screentime_5_wkdy_hr,screentime_5_wkdy_min,screentime_6_wkdy_hr,screentime_6_wkdy_min,
                                       screentime_7_wkdy_hr,screentime_7_wkdy_min,screentime_8_wkdy_hr,screentime_8_wkdy_min,
                                       screentime_9_wkdy_hr,screentime_9_wkdy_min,
                                       screentime_7_wknd_hr,screentime_7_wknd_min,screentime_8_wknd_hr,screentime_8_wknd_min,
                                       screentime_9_wknd_hr,screentime_9_wknd_min,screentime_10_wknd_hr,screentime_10_wknd_min,
                                       screentime_11_wknd_hr,screentime_11_wknd_min,screentime_12_wknd_hr,screentime_12_wknd_min,
                                       screentime_13_wknd_hr,screentime_13_wknd_min,screentime_14_wknd_hr,screentime_14_wknd_min,
                                       screentime_15_wknd_hr,screentime_15_wknd_min))

stq_new_18item$open_weekday_TV  = stq_new_18item$screentime_1_wkdy_hr + stq_new_18item$screentime_1_wkdy_min/60
stq_new_18item$open_weekday_Video  = stq_new_18item$screentime_2_wkdy_hr + stq_new_18item$screentime_2_wkdy_min/60
stq_new_18item$open_weekday_SingleGame  = stq_new_18item$screentime_3_wkdy_hr + stq_new_18item$screentime_3_wkdy_min/60
stq_new_18item$open_weekday_MultiGame  = stq_new_18item$screentime_4_wkdy_hr + stq_new_18item$screentime_4_wkdy_min/60
stq_new_18item$open_weekday_Text  = stq_new_18item$screentime_5_wkdy_hr + stq_new_18item$screentime_5_wkdy_min/60
stq_new_18item$open_weekday_SocialApps  = stq_new_18item$screentime_6_wkdy_hr + stq_new_18item$screentime_6_wkdy_min/60
stq_new_18item$open_weekday_EditPhoto  = stq_new_18item$screentime_7_wkdy_hr + stq_new_18item$screentime_7_wkdy_min/60
stq_new_18item$open_weekday_VideoChat  = stq_new_18item$screentime_8_wkdy_hr + stq_new_18item$screentime_8_wkdy_min/60
stq_new_18item$open_weekday_Surf  = stq_new_18item$screentime_9_wkdy_hr + stq_new_18item$screentime_9_wkdy_min/60
stq_new_18item$open_weekend_TV  = stq_new_18item$screentime_7_wknd_hr + stq_new_18item$screentime_7_wknd_min/60
stq_new_18item$open_weekend_Video  = stq_new_18item$screentime_8_wknd_hr + stq_new_18item$screentime_8_wknd_min/60
stq_new_18item$open_weekend_SingleGame  = stq_new_18item$screentime_9_wknd_hr + stq_new_18item$screentime_9_wknd_min/60
stq_new_18item$open_weekend_MultiGame  = stq_new_18item$screentime_10_wknd_hr + stq_new_18item$screentime_10_wknd_min/60
stq_new_18item$open_weekend_Text  = stq_new_18item$screentime_11_wknd_hr + stq_new_18item$screentime_11_wknd_min/60
stq_new_18item$open_weekend_SocialApps  = stq_new_18item$screentime_12_wknd_hr + stq_new_18item$screentime_12_wknd_min/60
stq_new_18item$open_weekend_EditPhoto  = stq_new_18item$screentime_13_wknd_hr + stq_new_18item$screentime_13_wknd_min/60
stq_new_18item$open_weekend_VideoChat  = stq_new_18item$screentime_14_wknd_hr + stq_new_18item$screentime_14_wknd_min/60
stq_new_18item$open_weekend_Surf  = stq_new_18item$screentime_15_wknd_hr + stq_new_18item$screentime_15_wknd_min/60

for (i in grep('^open.*',colnames(stq_new_18item),value = T) ){
  stq_new_18item[ str_replace(i,'open_','') ] = RECODE(stq_new_18item[[i]],
                                                       "0=0;
                                                       0.25=0.25;
                                                       c(0.5,0.75)=0.5;
                                                       1:1.75=1;
                                                       2:2.75=2;
                                                       3:3.75=3;
                                                       4:hi=4;else=NA")
}

stq_new_18item = select(stq_new_18item,-c(screentime_1_wkdy_hr,screentime_1_wkdy_min,screentime_2_wkdy_hr,screentime_2_wkdy_min,screentime_3_wkdy_hr,   
                                          screentime_3_wkdy_min,screentime_4_wkdy_hr,screentime_4_wkdy_min,screentime_5_wkdy_hr,screentime_5_wkdy_min,  
                                          screentime_6_wkdy_hr,screentime_6_wkdy_min,screentime_7_wkdy_hr,screentime_7_wkdy_min,screentime_8_wkdy_hr,   
                                          screentime_8_wkdy_min,screentime_9_wkdy_hr,screentime_9_wkdy_min,screentime_7_wknd_hr,screentime_7_wknd_min,  
                                          screentime_8_wknd_hr,screentime_8_wknd_min,screentime_9_wknd_hr,screentime_9_wknd_min,screentime_10_wknd_hr,  
                                          screentime_10_wknd_min,screentime_11_wknd_hr,screentime_11_wknd_min,screentime_12_wknd_hr,screentime_12_wknd_min, 
                                          screentime_13_wknd_hr,screentime_13_wknd_min,screentime_14_wknd_hr,screentime_14_wknd_min,screentime_15_wknd_hr,  
                                          screentime_15_wknd_min ))

stq_new_18item$weekday_Game = SUM(stq_new_18item,vars = c('open_weekday_SingleGame','open_weekday_MultiGame'),na.rm = F)
stq_new_18item$weekend_Game = SUM(stq_new_18item,vars = c('open_weekend_SingleGame','open_weekend_MultiGame'),na.rm = F)
stq_new_18item$weekday_Game = RECODE(stq_new_18item$weekday_Game,
                                     "0=0;
                                      0.25=0.25;
                                      c(0.5,0.75)=0.5;
                                      1:1.75=1;
                                      2:2.75=2;
                                      3:3.75=3;
                                      4:hi=4;else=NA")
stq_new_18item$weekend_Game = RECODE(stq_new_18item$weekend_Game,
                                     "0=0;
                                      0.25=0.25;
                                      c(0.5,0.75)=0.5;
                                      1:1.75=1;
                                      2:2.75=2;
                                      3:3.75=3;
                                      4:hi=4;else=NA")

stq_new_18item$weekday_SocialNet = stq_new_18item$weekday_SocialApps
stq_new_18item$weekend_SocialNet = stq_new_18item$weekend_SocialApps

for (i in intersect(colnames(stq_new_18item),colnames(stq_old_12item)) ){
  merged_values = str_c( 
    replace_na(as.character(stq_old_12item[[i]]),""),
    replace_na(as.character(stq_new_18item[[i]]),"" )
  )
  stq_new_18item[i] = as.numeric(merged_values)
}


stq_recode = cbind(stq_anchor,stq_new_18item,stq_TimePerDay,stq_SocialMedia,
                   stq_SMAS_VGAS,stq_MobilePhone,stq_Sleep,stq_DateingApps)

saveRDS(stq_recode,file = "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_STQ_RECODE_Comb_7level.rds")
