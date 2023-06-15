library(bruceR)

mergeABCDbehav <- function(Demo,STQ,MH,NIHTB,OutFileName){
  data = merge(Demo,STQ,by = intersect(colnames(Demo),colnames(STQ)),all = T)
  data = merge(data,MH,by = intersect(colnames(data),colnames(MH)),all = T)
  data = merge(data,NIHTB,by = intersect(colnames(data),colnames(NIHTB)),all = T)
  # save behavioral data into rds file
  colnames(data)
  saveRDS(data,OutFileName)
}

# load RDS data files -----------------------------------------------------

setwd("I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing")

STQ = readRDS('ABCD4.0_STQ_RECODE_Comb.rds')
MH = readRDS('ABCD4.0_MentalHealth_CBCL.rds')
NIHTB = readRDS('ABCD4.0_NIHTB.rds')

DTI = readRDS('ABCD4.0_DTI.rds')
Imgincl = readRDS('ABCD4.0_ImgIncl.rds')

# behavioral developmental measures ---------------------------------------

# convert columns data type to keep consistent
sapply(STQ, typeof)
STQ$interview_age = as.numeric(STQ$interview_age)

sapply(MH, typeof)
MH = mutate(MH,across(.cols=6:ncol(MH), .fns=as.numeric))

sapply(NIHTB, typeof)
NIHTB = mutate(NIHTB,across(.cols=6:ncol(NIHTB), .fns=as.numeric))

# brain imaging derived measures ----------------------------------------

sapply(DTI, typeof)
DTI = mutate(DTI,across(.cols=6:ncol(DTI), .fns=as.numeric))
DTI$interview_age = as.numeric(DTI$interview_age)

sapply(Imgincl, typeof)
Imgincl = mutate(Imgincl,across(.cols=7:ncol(Imgincl), .fns=as.numeric))
Imgincl$interview_age = as.numeric(Imgincl$interview_age)


# Data concatenation ------------------------------------------------------

# Concatenate Demographics, ScreenTime, MentalHealth and neurocognition (NIH toolbox)
Demographic = readRDS('ABCD4.0_Demographic_imputed.rds')
sapply(Demographic, typeof)
Demographic = select(Demographic,-subjectkey)
mergeABCDbehav(Demographic,STQ,MH,NIHTB,
               "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_Behav_DemoImputed.rds")

# ---------------------------------------------------------------------------- #
# Re-load the demographic data without imputation
Demographic = readRDS('ABCD4.0_Demographic_no_impute.rds')
sapply(Demographic, typeof)
Demographic = select(Demographic,-subjectkey)
mergeABCDbehav(Demographic,STQ,MH,NIHTB,
               "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_Behav_DemoRaw.rds")
# ---------------------------------------------------------------------------- #
# Re-load STQ data for the 7-level recoding
STQ = readRDS('ABCD4.0_STQ_RECODE_Comb_7level.rds')
STQ$interview_age = as.numeric(STQ$interview_age)
Demographic = readRDS('ABCD4.0_Demographic_imputed.rds')
sapply(Demographic, typeof)
Demographic = select(Demographic,-subjectkey)
mergeABCDbehav(Demographic,STQ,MH,NIHTB,
               "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_Behav_7LSTQ_DemoImputed.rds")
# ---------------------------------------------------------------------------- #
Demographic = readRDS('ABCD4.0_Demographic_no_impute.rds')
sapply(Demographic, typeof)
Demographic = select(Demographic,-subjectkey)
mergeABCDbehav(Demographic,STQ,MH,NIHTB,
               "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_Merged_Behav_7LSTQ_DemoRaw.rds")
#==============================================================================#
# Concatenate SMRI, RS-fMRI (RSFNC), DTI, task-fMRI(MID) and ABCD Recommended Image Inclusion

data = merge(data,DTI,by = intersect(colnames(data),colnames(DTI)),all = T)
data = merge(data,Imgincl,by = intersect(colnames(data),colnames(Imgincl)),all = T)


