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

suss <- readABCDdata('abcd_suss01.txt')
suss <- select(suss,c(src_subject_id,interview_date,interview_age,eventname,
                      su_caff_ss_sum_calc,
                      su_caff_ss_sum_calc_l,
                      meeq_positive_expectancies_ss,
                      meeq_negative_expectancies_ss,
                      aeq_positive_expectancies_ss,
                      aeq_negative_expectancies_ss))
saveRDS(suss,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_suss.rds")
