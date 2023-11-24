# =============================================================================#
# SDPP Step 4: Read and re-coding Screen Use-related data
# R Packages Dependency: bruceR, forcats, naniar
# Step File Notes: 
# 1. Ref Link:
# https://github.com/fenmeng123/2022_JAACAP_ABCD_SMA_pattern
# https://wiki.abcdstudy.org/release-notes/non-imaging/novel-technologies.html#screen-time-questionnaire
# 2. Target File: nt_y_st.csv
# Update Date: 2023.06.16 By Kunru Song
# Update Date: 2023.07.06 By Kunru Song
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_4.txt'
s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(naniar)
library(forcats)
# ==============================MAIN CODES=====================================#
# 2. Load nt_y_st.csv and perform re-coding --------------------------------
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
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_SMA_Rec_TPD_STQ.doc'),
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

# Variables measured the TIME Per Day spent on social media apps
TPD_STQ$open_SOC_TPD = stq$screentime_smq_soc_med_hr + stq$screentime_smq_sm_min/60
TPD_STQ$SOC_TPD = Recode.STQ(TPD_STQ$open_TPD_SOC,
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
  MPIQ[[i]] <- Recode.ABCD.NA(MPIQ[[i]],VarName = i)
}
# screentime_phone_yn: Do you have your own mobile phone, smart watch or ipod? 
# 0=No; 1=Yes; 777=Refuse to Answer
MPIQ$OS_MP <- Recode.ABCD.YN(stq$screentime_phone_yn)
# Mobile PHone Attachment (MPHA)
# Single Item measures the degree of phone attachment
# screentime_phone_scale: On a scale of 1-10 
# (with 1=barely check it/can go days without it, 
# and 10=check at least hourly when awake), how attached are you to your phone?
# 1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; 8=8; 9=9; 10=10; 777=Refuse to Answer
MPIQ$MP_Attachment <- Recode.ABCD.NA(stq$screentime_phone_scale)

# 5. Social Media related measurements ---------------------------------------

stq %>% select(c(src_subject_id,eventname,
                 screentime_usesoc,screentime_smq_secret,
                 screentime_smq_facebook,screentime_smq_instagram,
                 screentime_smq_snapchat,screentime_smq_twitter,
                 screentime_smq_youtube,screentime_smq_pinterest,
                 screentime_smq_tumblr,screentime_smq_reddit,screentime_smq_mg_chat,
                 screentime_smq_musical_ly,screentime_smq_other,
                 screentime_smq_use___1,screentime_smq_use___2,screentime_smq_use___3,
                 screentime_smq_use___4,screentime_smq_use___5,screentime_smq_use___6,
                 screentime_smq_use___7,screentime_smq_use___8,screentime_smq_use___9,
                 screentime_smq_use___10,screentime_smq_use___11,
                 screentime_smqa1,screentime_smqa2,
                 screentime_smqa3,screentime_smqa4,
                 screentime_smqa5,screentime_smqa6,
                 screentime_smq_use_most,screentime_smq_account,
                 screentime_smq_followers,screentime_smq_followers_dk,screentime_smq_followers_na,
                 screentime_smq_following,screentime_smq_following_dk,screentime_smq_following_na)) %>%
  rename(SMAS_1 = screentime_smqa1,
         SMAS_2 = screentime_smqa2,
         SMAS_3 = screentime_smqa3,
         SMAS_4 = screentime_smqa4,
         SMAS_5 = screentime_smqa5,
         SMAS_6 = screentime_smqa6,
         SOC_OS_Account = screentime_usesoc,
         SOC_OS_Secret = screentime_smq_secret,
         SOC_OS_Facebook = screentime_smq_use___1,
         SOC_OS_Instagram = screentime_smq_use___2,
         SOC_OS_SnapChat = screentime_smq_use___3,
         SOC_OS_Twitter = screentime_smq_use___4,
         SOC_OS_YouTuBe = screentime_smq_use___5,
         SOC_OS_Printerest = screentime_smq_use___6,
         SOC_OS_Tumblr = screentime_smq_use___7,
         SOC_OS_Reddit = screentime_smq_use___8,
         SOC_OS_MVOC = screentime_smq_use___9,
         SOC_OS_Other = screentime_smq_use___10,
         SOC_OS_TikTok = screentime_smq_use___11,
         SOC_NOA_Facebook = screentime_smq_facebook,
         SOC_NOA_Instagrm = screentime_smq_instagram,
         SOC_NOA_SnapChat = screentime_smq_snapchat,
         SOC_NOA_Twitter = screentime_smq_twitter,
         SOC_NOA_YouTuBe = screentime_smq_youtube,
         SOC_NOA_Printerest = screentime_smq_pinterest,
         SOC_NOA_Tumblr = screentime_smq_tumblr,
         SOC_NOA_Reddit = screentime_smq_reddit,
         SOC_NOA_MVOC = screentime_smq_mg_chat,
         SOC_NOA_Other = screentime_smq_other,
         SOC_NOA_TikTok = screentime_smq_musical_ly,
         SOC_UM_SMS = screentime_smq_use_most,
         SOC_UM_AccountType = screentime_smq_account) -> SMQ
Merge.Value.NA(SMQ$screentime_smq_followers_na,
               SMQ$screentime_smq_followers_dk) %>% 
  RECODE("1=-1;999=-2;") %>%
  Merge.Value.NA(SMQ$screentime_smq_followers) -> SMQ$SOC_UM_NOFE
Merge.Value.NA(SMQ$screentime_smq_following_na,
               SMQ$screentime_smq_following_dk) %>% 
  RECODE("1=-1;999=-2;") %>%
  Merge.Value.NA(SMQ$screentime_smq_following) -> SMQ$SOC_UM_NOFI
SMQ <- select(SMQ,-contains("screentime"))
for (i in (grep("(^SMAS.*)|(^SOC_NOA*)",colnames(SMQ),value = T))){
  SMQ[[i]] = Recode.ABCD.NA(SMQ[[i]],VarName = i)
}
for (i in (grep("(^SOC_OS*)",colnames(SMQ),value = T))){
  SMQ[[i]] = Recode.ABCD.YN(SMQ[[i]],VarName = i)
}

SMQ$SOC_UM_SMS %>% Recode.ABCD.RADT(VarName = 'SOC_UM_SMS') %>% 
  RECODE("'0'='Non-user';
  '1'='Facebook'; '2'='Instagram'; '3'='Snapchat'; '4'='Twitter';
  '5'='YouTube'; '6'='Pinterest'; '7'='Tumblr'; '8'='Reddit';
  '11'='TikTok'; '9'='MVOC'; '10'='Other';") %>%
  factor(levels = c('Non-user','Facebook','Instagram','Snapchat','Twitter',
                    'YouTube','Pinterest','Tumblr','Reddit','TikTok','MVOC',
                    'Other','Dont Know','Refuse to Answer'),
         ordered = F) -> SMQ$SOC_UM_SMS
df.print.mva.counts(SMQ$SOC_UM_SMS)

SMQ$SOC_UM_AccountType %>% Recode.ABCD.RADT(VarName = 'SOC_UM_SMS') %>% 
  RECODE("'1' = 'Public';'2' = 'Private';") %>%
  factor(levels = c('Public','Private','Dont Know','Refuse to Answer'),
         ordered = F) -> SMQ$SOC_UM_AccountType
df.print.mva.counts(SMQ$SOC_UM_AccountType)


# 6. Online Dating Questionnaire ---------------------------------------------
# Q1: screentime_odq1: Have you ever used a dating app?
# 1=Yes; 0=No; 999=I don't know what that is?; 777=Refuse to answer
# Q2: screentime_odq2: Are you currently using a dating app?; 
# 1=Yes; 0=No; 777=Refuse to answer
# Q3: screentime_odq3: How much time per week do you spend on online dating apps?
# 1=None; 2=< 30 minutes; 3=30 minutes; 4=1 hour; 5=2 hours; 6=3 hours; 7=4+ hours; 777=Refuse to answer
# Q4: screentime_odq4: Have you arranged an in-person meeting with someone you met only on the dating app? 
# 1=Yes; 0=No; 777=Refuse to answer
stq %>% select(c(src_subject_id,eventname,
                 screentime_odq1,screentime_odq2,
                 screentime_odq3,screentime_odq4)) %>%
  rename(ODQ_OS_Past = screentime_odq1,
         ODQ_OS_Current = screentime_odq2,
         ODQ_TPW = screentime_odq3,
         ODQ_Meeting = screentime_odq4) -> ODQ
ODQ$ODQ_OS_Past %>% Recode.ABCD.RADT(VarName = 'Ever Used Online Dating Apps') %>%
  RECODE("'1' = 'Yes'; '0' = 'No';") %>%
  factor(levels = c("No","Yes","Dont Know")) -> ODQ$ODQ_OS_Past
df.print.mva.counts(ODQ$ODQ_OS_Past)

ODQ$ODQ_OS_Current %>% Recode.ABCD.YN(VarName = 'Currently Using Online Dating Apps') -> ODQ$ODQ_OS_Current

# 7. Screen Usage Around Bed time (SUAB) --------------------------------------------
stq %>% select(c(src_subject_id,eventname,
                 screentime_sq1,screentime_sq2,
                 screentime_sq3,screentime_sq4,
                 screentime_sq5,screentime_sq6,
                 screentime_sq7,screentime_sq8,
                 screentime_sq9,screentime_sq10,
                 screentime_sq11,screentime_sq12,screentime_sq13)) %>%
  rename(SUAB_1 = screentime_sq3,
         SUAB_2 = screentime_sq4,
         SUAB_3 = screentime_sq5,
         SUAB_4 = screentime_sq6,
         SUAB_5 = screentime_sq7,
         SUAB_6 = screentime_sq8,
         SUAB_7 = screentime_sq9,
         SUAB_8 = screentime_sq10,
         SUAB_9 = screentime_sq11,
         SUAB_OS_DeviceInBedroom = screentime_sq1,
         SUAB_PhoneAction = screentime_sq2,
         SUAB_Freq_WakeBefore = screentime_sq12,
         SUAB_Freq_WakeDuring = screentime_sq13) -> SUAB
for(i in grep("SUAB_[1-9]",colnames(SUAB),value = T)){
  SUAB[[i]] = Recode.ABCD.NA(SUAB[[i]],VarName = i)
} 
SUAB$SUAB_OS_DeviceInBedroom = Recode.ABCD.YN(SUAB$SUAB_OS_DeviceInBedroom)
SUAB$SUAB_PhoneAction <- RECODE(SUAB$SUAB_PhoneAction,
                                "-1=NA; 1='Turn the phone off' ;
                                2='Put the ringer on silent or vibrate';
                                3='Leave the ringer on';
                                4='Put it outside of the room where I sleep';
                                777=NA;
                                else=NA;")
SUAB$SUAB_Freq_WakeBefore <- Recode.ABCD.NA(SUAB$SUAB_Freq_WakeBefore)
SUAB$SUAB_Freq_WakeDuring <- Recode.ABCD.NA(SUAB$SUAB_Freq_WakeDuring)

# 8. Video Game Addiction Scale (VGAS) ------------------------------------
stq %>% select(c(src_subject_id,eventname,
                 screentime_vgaq1,screentime_vgaq2,
                 screentime_vgaq3,screentime_vgaq4,
                 screentime_vgaq5,screentime_vgaq6)) %>%
  rename(VGAS_1 = screentime_vgaq1,
         VGAS_2 = screentime_vgaq2,
         VGAS_3 = screentime_vgaq3,
         VGAS_4 = screentime_vgaq4,
         VGAS_5 = screentime_vgaq5,
         VGAS_6 = screentime_vgaq6) -> VGAS

for (i in grep("VGAS_[1-6]",colnames(VGAS),value = T)){
  VGAS[[i]] <- Recode.ABCD.NA(VGAS[[i]],VarName = i)
}

# 9. Bind all re-coded data and save it to files ) ------------------------

ScreenUse = Hmisc::Merge(TPD_STQ,VGAS,SMQ,MPIQ,SUAB,ODQ, id = ~src_subject_id*eventname)

ScreenUse %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_SMA_Rec.doc'),
              row.names = F,
              nsmalls = 1)

SDPP.save.file(ScreenUse,
               FileName = "SMA_Rec.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

# End of script -------------------------------------------------------
s_close_sink()

