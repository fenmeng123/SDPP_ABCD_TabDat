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
  return(list(data,var.descrip))
}

nihtb = readABCDdata('abcd_tbss01.txt')
nihtb_sum = nihtb[[1]]

nihtb_anchor = subset(nihtb_sum,select = c(subjectkey,src_subject_id,interview_date,sex,eventname))

nihtb_score = subset(nihtb_sum,select = c(grep('.*_agecorrected$',colnames(nihtb_sum),value = T),
                                  grep('.*_fc$',colnames(nihtb_sum),value = T)))
colnames(nihtb_score) = stringr::str_replace(colnames(nihtb_score),'_agecorrected','_ac')
colnames(nihtb_score) = stringr::str_replace(colnames(nihtb_score),'nihtbx','NIHTB')

NIHTB_Cognition = cbind(nihtb_anchor,nihtb_score)
saveRDS(NIHTB_Cognition,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_NIHTB.rds") 
