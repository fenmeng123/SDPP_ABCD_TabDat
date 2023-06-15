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

sleep <- readABCDdata('abcd_sds01.txt')
sleep <- select(sleep,-c(abcd_sds01_id,subjectkey,sleep_dis_select_language___1))
sleep <- mutate(sleep,across(.cols=6:ncol(sleep), .fns=as.numeric))
sleep$interview_age = as.numeric(sleep$interview_age)


sleep$SleepDistub_sum <- SUM(sleep,vars = c('sleepdisturb1_p','sleepdisturb2_p','sleepdisturb3_p',
    'sleepdisturb4_p','sleepdisturb5_p','sleepdisturb6_p','sleepdisturb7_p',
    'sleepdisturb8_p','sleepdisturb9_p','sleepdisturb10_p','sleepdisturb11_p',
    'sleepdisturb12_p','sleepdisturb13_p','sleepdisturb14_p','sleepdisturb15_p',
    'sleepdisturb16_p','sleepdisturb17_p','sleepdisturb18_p','sleepdisturb19_p',
    'sleepdisturb20_p','sleepdisturb21_p','sleepdisturb22_p','sleepdisturb23_p',
    'sleepdisturb24_p','sleepdisturb25_p','sleepdisturb26_p'))

saveRDS(sleep,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_SleepDistub.rds")