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
mriqc = readABCDdata('mriqcrp10301.txt')
freesufqc = readABCDdata('abcd_fsurfqc01.txt')
derivedresqc = readABCDdata('abcd_smrip20201.txt')

mriqc = subset(mriqc,select = c(src_subject_id,interview_date,sex,eventname,iqc_t1_ok_ser))
freesufqc = subset(freesufqc,select = c(src_subject_id,interview_date,sex,eventname,fsqc_qc))
derivedresqc = subset(derivedresqc,select = c(src_subject_id,interview_date,sex,eventname,smri_t1w_scs_cbwmatterlh))

T1w_qc_Index = merge(mriqc,freesufqc,by = c('src_subject_id','interview_date','sex','eventname'),all = T)
T1w_qc_Index = merge(T1w_qc_Index,derivedresqc,by = c('src_subject_id','interview_date','sex','eventname'),all = T)


T1w_Recommend_Idx_1 = as.numeric(T1w_qc_Index$iqc_t1_ok_ser)>0
T1w_Recommend_Idx_2 = T1w_qc_Index$fsqc_qc!="0"
T1w_Recommend_Idx_3 = !is.na(as.numeric(T1w_qc_Index$smri_t1w_scs_cbwmatterlh))

T1w_qc_Index$T1w_Recommend_QC = T1w_Recommend_Idx_1 & T1w_Recommend_Idx_2 & T1w_Recommend_Idx_3

saveRDS(T1w_qc_Index,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_T1_ImgIncl.rds")

# ImgIncl3.0 = readABCDdata('I:\\ABCDStudyNDA\\Package_1187743\\abcd_imgincl01.txt')#imgincl in ABCD 3.0

