library(bruceR)

setwd("I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing")



Demographic = readRDS('ABCD4.0_Demographic_imputed.rds')
STQ = readRDS('ABCD4.0_STQ_RECODE_Comb.rds')
MH = readRDS('ABCD4.0_MentalHealth_CBCL.rds')
NIHTB = readRDS('ABCD4.0_NIHTB.rds')
MID = readRDS('ABCD4.0_MID_BetaWeight.rds')
SubstanceUse = readRDS('ABCD4.0_suss.rds')
Imgincl = readRDS('ABCD4.0_ImgIncl.rds')
Sleep = readRDS('ABCD4.0_SleepDistub.rds')
SMRI = readRDS('ABCD4.0_SMRI.rds')

SubstanceUse = unite(SubstanceUse,"caff_intake",c('su_caff_ss_sum_calc','su_caff_ss_sum_calc_l')
      ,sep='',remove = FALSE)

# convert columns data type to keep consistent
sapply(Demographic, typeof)
Demographic = select(Demographic,-subjectkey)

sapply(STQ, typeof)
STQ$interview_age = as.numeric(STQ$interview_age)

sapply(MH, typeof)
MH = mutate(MH,across(.cols=6:ncol(MH), .fns=as.numeric))

sapply(NIHTB, typeof)
NIHTB = mutate(NIHTB,across(.cols=6:ncol(NIHTB), .fns=as.numeric))

sapply(MID, typeof)
MID = mutate(MID,across(.cols=6:ncol(MID), .fns=as.numeric))
MID$interview_age = as.numeric(MID$interview_age)

sapply(SubstanceUse, typeof)
SubstanceUse = mutate(SubstanceUse,across(.cols=5:ncol(SubstanceUse), .fns=as.numeric))
SubstanceUse$interview_age = as.numeric(SubstanceUse$interview_age)

sapply(Imgincl, typeof)
Imgincl = mutate(Imgincl,across(.cols=7:ncol(Imgincl), .fns=as.numeric))
Imgincl$interview_age = as.numeric(Imgincl$interview_age)

sapply(SMRI, typeof)
SMRI = mutate(SMRI,across(.cols=6:ncol(SMRI), .fns=as.numeric))


# Concatenate Demographics, ScreenTime and MentalHealth
data = merge(Demographic,STQ,by = intersect(colnames(Demographic),colnames(STQ)),all = T)
data = merge(data,MH,by = intersect(colnames(data),colnames(MH)),all = T)
# Concatenate the neurocognition (NIH toolbox) and MID (DK atlas beta values across 10 contrasts)
# and the substance use summary score (SUSS) and the Sleep Disturbance Scale (SDS)
data = merge(data,NIHTB,by = intersect(colnames(data),colnames(NIHTB)),all = T)
data = merge(data,MID,by = intersect(colnames(data),colnames(MID)),all = T)
data = merge(data,SubstanceUse,by = intersect(colnames(data),colnames(SubstanceUse)),all = T)
data = merge(data,Sleep,by = intersect(colnames(data),colnames(Sleep)),all = T)
data = merge(data,SMRI,by = intersect(colnames(data),colnames(SMRI)),all = T)

data = merge(data,Imgincl,by = intersect(colnames(data),colnames(Imgincl)),all = T)

# save data into rds file
colnames(data)
saveRDS(data,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_Caffine.rds")