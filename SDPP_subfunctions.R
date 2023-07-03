read4.0 <- function(filename){
  data = read.table(filename,header = TRUE,sep = '\t')
  # remove some specific columns which are same across all .txt files
  data <- subset(data, select = -c(collection_id, dataset_id,collection_title))
  # get variable descriptions
  var.descrip <- data[1,]
  # remove the first row
  data<-data[-1,]
  # # add comments to all columns
  # for (i in 1:length(var.descrip)){
  #   comment(data[,i])<-var.descrip[1,i]
  # }
  return(data)
}
read5.0 <- function(filename){
  data <- read.csv(filename)
  return(data)
}




readABCDdata<-function(filename,version="5.0"){
  cat(sprintf("Reading tabulated data from: %s\n",filename))
  data <- switch (version,
    `5.0` = read5.0(filename),
    `4.0` = read4.0(filename)
  )
  cat("Subjects Counts (stratified by eventname):\n")
  print(table(data$eventname))
  return(data)
}

SDPP.ABCD.TabDat.PrepareProject <- function(ProjectDirectory){
  if (!file.exists(ProjectDirectory)){
    options(warn = -1)
    cat("Project Directory not found! Auto-creating SDPP Project Folder....\n")
    dir.create(ProjectDirectory)
    dir.create(normalizePath(paste(ProjectDirectory,'Res_1_Logs',sep = '/'),winslash = '/'))
    dir.create(normalizePath(paste(ProjectDirectory,'Res_2_Results',sep = '/'),winslash = '/'))
    dir.create(normalizePath(paste(ProjectDirectory,'Res_3_IntermediateData',sep = '/'),winslash = '/'))
    dir.create(normalizePath(paste(ProjectDirectory,'Res_4_Reports',sep = '/'),winslash = '/'))
    dir.create(normalizePath(paste(ProjectDirectory,'Step_0_Preprocessing',sep = '/'),winslash = '/'))
    cat(sprintf("SDPP Project has been created at %s\n",normalizePath(ProjectDirectory)))
    cat("Project Folder Structure:\n")
    cat(list.files(normalizePath(ProjectDirectory)))
    options(warn = 1)
  }
  AutoLogFolderPath = normalizePath(paste(ProjectDirectory,'Res_1_Logs',sep = '/'),winslash = '/')
  return(AutoLogFolderPath)
}

fullfile <- function(...){
  PathStr = normalizePath(paste(...,sep = '/'),winslash = '/',mustWork = F)
  return(PathStr)
}

BOCF.Variables <- function(data,anchor_wave,variable_name){
  cat(sprintf('Carry Forward variable:%s from baseline to 1-year, 2-year and 3-year FU\n',variable_name))
  data_anchor = subset(data,eventname == anchor_wave)
  data_BOFC = subset(data,eventname != anchor_wave)
  for (i in data_anchor$subjectkey){
    data_BOFC[data_BOFC$subjectkey == i,variable_name] <- data_anchor[data_anchor$subjectkey == i,variable_name]
  }
  data = rbind(data_anchor,data_BOFC)
  cat(sprintf('Variable:%s BOCF finished!\n',variable_name))
  return(data)
}
