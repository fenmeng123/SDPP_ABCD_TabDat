library(bruceR)

setwd("I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing")

Demographic = readRDS('ABCD4.0_Demographic_imputed.rds')
STQ = readRDS('ABCD4.0_STQ_RECODE_Comb.rds')
MH = readRDS('ABCD4.0_MentalHealth_CBCL.rds')
NIHTB = readRDS('ABCD4.0_NIHTB.rds')
DTI = readRDS('ABCD4.0_DTI.rds')
Imgincl = readRDS('ABCD4.0_ImgIncl.rds')

# convert columns data type to keep consistent
sapply(Demographic, typeof)
Demographic = select(Demographic,-subjectkey)

sapply(STQ, typeof)
STQ$interview_age = as.numeric(STQ$interview_age)

sapply(MH, typeof)
MH = mutate(MH,across(.cols=6:ncol(MH), .fns=as.numeric))

sapply(NIHTB, typeof)
NIHTB = mutate(NIHTB,across(.cols=6:ncol(NIHTB), .fns=as.numeric))

sapply(DTI, typeof)
DTI = mutate(DTI,across(.cols=6:ncol(DTI), .fns=as.numeric))
DTI$interview_age = as.numeric(DTI$interview_age)

sapply(Imgincl, typeof)
Imgincl = mutate(Imgincl,across(.cols=7:ncol(Imgincl), .fns=as.numeric))
Imgincl$interview_age = as.numeric(Imgincl$interview_age)


# Concatenate Demographics, ScreenTime and MentalHealth
data = merge(Demographic,STQ,by = intersect(colnames(Demographic),colnames(STQ)),all = T)
data = merge(data,MH,by = intersect(colnames(data),colnames(MH)),all = T)
# Concatenate the neurocognition (NIH toolbox) and DTI 
data = merge(data,NIHTB,by = intersect(colnames(data),colnames(NIHTB)),all = T)
data = merge(data,DTI,by = intersect(colnames(data),colnames(DTI)),all = T)
# Concatenate the ABCD Recommended Image Inclusion
data = merge(data,Imgincl,by = intersect(colnames(data),colnames(Imgincl)),all = T)

# save data into rds file
colnames(data)
saveRDS(data,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_DTI_STQ_DemoImputed.rds")
rm(data)
gc()
Demographic = readRDS('ABCD4.0_Demographic_no_impute.rds')
sapply(Demographic, typeof)
Demographic = select(Demographic,-subjectkey)
# Concatenate Demographics, ScreenTime and MentalHealth
data = merge(Demographic,STQ,by = intersect(colnames(Demographic),colnames(STQ)),all = T)
data = merge(data,MH,by = intersect(colnames(data),colnames(MH)),all = T)
# Concatenate the neurocognition (NIH toolbox) and DTI 
data = merge(data,NIHTB,by = intersect(colnames(data),colnames(NIHTB)),all = T)
data = merge(data,DTI,by = intersect(colnames(data),colnames(DTI)),all = T)
# Concatenate the ABCD Recommended Image Inclusion
data = merge(data,Imgincl,by = intersect(colnames(data),colnames(Imgincl)),all = T)
saveRDS(data,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_DTI_STQ_NonImputed.rds")
