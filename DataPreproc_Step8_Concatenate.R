library(bruceR)

setwd("I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing")


Demographic = readRDS('ABCD4.0_Demographic_imputed.rds')
STQ = readRDS('ABCD4.0_STQ_RECODE_Comb.rds')
MH = readRDS('ABCD4.0_MentalHealth_CBCL.rds')
NIHTB = readRDS('ABCD4.0_NIHTB.rds')
SMRI = readRDS('ABCD4.0_SMRI.rds')
T1_ImgIncl = readRDS('ABCD4.0_T1_ImgIncl.rds')

# convert columns data type to keep consistent
sapply(STQ, typeof)
STQ$interview_age = as.numeric(STQ$interview_age)

sapply(MH, typeof)
MH = mutate(MH,across(.cols=6:ncol(MH), .fns=as.numeric))

sapply(NIHTB, typeof)
NIHTB = mutate(NIHTB,across(.cols=6:ncol(NIHTB), .fns=as.numeric))

sapply(SMRI, typeof)
SMRI = mutate(SMRI,across(.cols=6:ncol(SMRI), .fns=as.numeric))

T1_ImgIncl = subset(T1_ImgIncl,select = -c(iqc_t1_ok_ser,fsqc_qc,smri_t1w_scs_cbwmatterlh ))

# Concatenate Demographics, ScreenTime and MentalHealth
data = merge(Demographic,STQ,by = intersect(colnames(Demographic),colnames(STQ)),all = T)
data = merge(data,MH,by = intersect(colnames(data),colnames(MH)),all = T)
# Concatenate the neurocognition (NIH toolbox) and SMRI (4 morphormetric measures)
data = merge(data,NIHTB,by = intersect(colnames(data),colnames(NIHTB)),all = T)
data = merge(data,SMRI,by = intersect(colnames(data),colnames(SMRI)),all = T)
data = merge(data,T1_ImgIncl,by = intersect(colnames(data),colnames(T1_ImgIncl)),all = T)

colnames(data)
saveRDS(data,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_DemoSTQMHCogSMRI.rds")
rm(Demographic,MH,NIHTB,SMRI,STQ,T1_ImgIncl)
gc()
# Auto-report missing values (grouped by eventname(time waves) )
data[,table(eventname,useNA = 'if')]
baseline = subset(data,eventname=='baseline_year_1_arm_1')
year1FU = subset(data,eventname=='1_year_follow_up_y_arm_1')
year2FU = subset(data,eventname=='2_year_follow_up_y_arm_1')
year3FU = subset(data,eventname=='3_year_follow_up_y_arm_1')

naniar::miss_var_summary(baseline) %>% 
  print_table(file = "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\MissVarRep_Merged_baseline.doc")

naniar::miss_var_summary(year1FU) %>% 
  print_table(file = "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\MissVarRep_Merged_1_year_FU.doc")

naniar::miss_var_summary(year2FU) %>% 
  print_table(file = "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\MissVarRep_Merged_2_year_FU.doc")

naniar::miss_var_summary(year3FU) %>% 
  print_table(file = "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\MissVarRep_Merged_3_year_FU.doc")






