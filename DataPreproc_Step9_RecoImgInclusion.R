library(bruceR)

setwd('I:\\ABCDStudyNDA\\Package_1193928')
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
imgincl <- readABCDdata('abcd_imgincl01.txt')
imgincl <- select(imgincl,-c(abcd_imgincl01_id,study_cohort_name,subjectkey))
saveRDS(imgincl,
        'I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_ImgIncl.rds')
