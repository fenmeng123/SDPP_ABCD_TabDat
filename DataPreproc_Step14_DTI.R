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
dti_p1 <- readABCDdata('abcd_dmdtifp101.txt')
dti_p2 <- readABCDdata('abcd_dmdtifp201.txt')
dti_p1 <- select(dti_p1,-c(abcd_dmdtifp101_id,subjectkey,dmri_dtifull_visitid))
dti_p2 <- select(dti_p2,-c(abcd_dmdtifp201_id,subjectkey,dmri_dtifull_visitid))
dti <- merge(dti_p1,dti_p2,by = c("src_subject_id","interview_age",
                                  "interview_date","sex",
                                  "eventname"),all = T)
rm(dti_p1,dti_p2)
gc()
# Extract 1-42 (mean FA from 42 ROIs, which are in DTI atlas)
FA.index <- grepl("dmdtifp1_([1-9]$|([123][0-9]$)|([4][0-2])$)",colnames(dti))
colnames(dti)[FA.index]
# Extract 162-270 (Fiber tract volume in mm^3 of 42 ROIs(DTI atlas) and 30 ASEG ROIs)
FiberVol.index <- grepl("dmdtifp1_(16[2-9]$|(1[7-9][0-9]$)|(2[0-6][0-9])$|(270$))",colnames(dti))
colnames(dti)[FiberVol.index]
# Extract 331-401 (mean FA within parcellation of sub-adjacent white matter associated with cortical APARC ROIs)
AdjAPARC.index <- grepl("dmdtifp1_(33[1-9]$|(3[4-9][0-9]$)|(4[0][0-1])$)",colnames(dti))
colnames(dti)[AdjAPARC.index]
# Extract 615-685 (mean FD  within parcellation of cortical gray matter associated with cortical ROI)
Gray.index <- grepl("dmdtifp1_(61[5-9]$|(6[2-7][0-9]$)|(68[0-5])$)",colnames(dti))
colnames(dti)[Gray.index]
# Extract 899-969 (mean FA within n parcellation of gray matter-white matter contrast associated with cortical ROI)
GWcAPARC.index <- grepl("dmdtifp1_(899$|(9[0-6][0-9]$))",colnames(dti))
colnames(dti)[GWcAPARC.index]
# Extract the head motion as covariates
DTI_meanFD <- dti$dmdtifp1_1183
# Extract the demographics for table keys
demo <- dti[,1:5]
dti <- dti [,(FA.index|FiberVol.index|AdjAPARC.index|Gray.index|GWcAPARC.index)]
dti <- cbind(demo,dti)
dti$DTI_meanFD <- DTI_meanFD
saveRDS(dti,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_DTI.rds")
