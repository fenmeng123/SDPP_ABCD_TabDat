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
  for (i in 1:length(var.descrip)){
    comment(data[,i])<-var.descrip[1,i]
  }
  return(data)
}

demo_base = readABCDdata('pdem02.txt')