# SDPP_subfunctions.R is a script contains all user-defined functions that are
# needed when using the SDPP-ABCD-TabDat pipelin. Please make sure you have
# sourced this script before running any steps in SDPP-ABCD-TabDat.
# Author: Kunru Song
# 1. Basic File I/O Functions ---------------------------------------------
addprefix <- function(prefix,filename,postfix=NA){
  if (!is.na(prefix)){
    filename = paste(prefix,filename,sep = '_')
  }
  if (is.na(postfix)){
    return(filename)
  }else{
    filename = paste(filename,postfix,sep = '.')
    return(filename)
  }
}
fullfile <- function(...){
  PathStr = normalizePath(paste(...,sep = '/'),winslash = '/',mustWork = F)
  return(PathStr)
}
fprintf <- function(StringVar,...){
  cat(sprintf(StringVar,...))
}
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
SDPP.save.file <- function(Data,FileName,Prefix,ProjectDirectory,FileFormat=NA,DataLabel=NA){
  if (is.na(FileFormat)){
    OutputFileName = addprefix(Prefix,FileName)
    FileFormat = unlist(lapply(strsplit(FileName,split = ".",fixed = T),tail,1))
  }else{
    OutputFileName = addprefix(Prefix,FileName,postfix = FileFormat)
  }
  OutputFileDir = fullfile(ProjectDirectory,'Res_3_IntermediateData',OutputFileName)
  if (is.na(DataLabel)){
    fprintf('Variable: %s will be saved into: %s\n',deparse(substitute(Data)),OutputFileDir)
  }else{
    fprintf('%s will be saved into: %s\n',DataLabel,OutputFileDir)
  }
  if (FileFormat == 'rds'){
    saveRDS(Data,OutputFileDir)
    cat(sprintf('Saving Data into %s File: %s......\nFinished!\n',toupper(FileFormat),OutputFileDir))
  } else if (FileFormat == 'csv'){
    write.csv(Demographic,OutputFileDir,fileEncoding = 'UTF-8')
    cat(sprintf('Saving Data into %s File: %s......\nFinished!\n',toupper(FileFormat),OutputFileDir))
  } else {
    fprintf("Did not find appropraite file postfix! Please Check your code!")
    fprintf("SDPP.save file will be skipped! Save data failed!")
  }
}
SDPP.read.intdat <- function(FileName,ProjectDirectory){
  FileFormat = unlist(lapply(strsplit(FileName,split = ".",fixed = T),tail,1))
  InputFileDir = fullfile(ProjectDirectory,"Res_3_IntermediateData",FileName)
  fprintf('Reading Intermediate Data from: %s......\n',InputFileDir)
  if (FileFormat == 'rds'){
    data = readRDS(InputFileDir)
    fprintf("Finished!\n")
  } else if (FileFormat == 'csv'){
    data = read.csv(InputFileDir,fileEncoding = 'UTF-8')
  } else {
    fprintf("Did not find appropraite file postfix! Please Check your code!")
    fprintf("SDPP.read file will be skipped! Read data failed!")
  }
  return(data)
}
# 2. SDPP Project Managing Functions --------------------------------------
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
# 3. Data Processing Functions --------------------------------------------
dt.print.mva.counts <- function(dt_name,var_name){
  print(eval(parse(text = sprintf("%s[,table(%s,useNA = 'if')]",dt_name,var_name))))
}
MODE.Row <- function(df){
  uniqv <- unique(df)
  uniqv[which.max(tabulate(match(df, uniqv)))]
}
Check.Numeric <- function(Class_String){
  Flag = ("numeric" %in% Class_String) | 
    ("double" %in% Class_String) | 
    ("integer" %in% Class_String)
  return(Flag)
}
BOCF.Variables <- function(data,anchor_wave,variable_name,autocheck=F){
  cat(sprintf('Carry Forward variable:%s from baseline to Follow-up Waves\n',variable_name))
  data_anchor = subset(data,eventname == anchor_wave)
  data_BOFC = subset(data,eventname != anchor_wave)
  for (i in data_anchor$src_subject_id){
    if (autocheck){
      Flag = which(is.na(data_BOFC[data_BOFC$src_subject_id == i,variable_name]))
        data_BOFC[Flag,variable_name] <- data_anchor[data_anchor$src_subject_id == i,
                                                variable_name]
    }else{
      data_BOFC[data_BOFC$src_subject_id == i,variable_name] <- data_anchor[data_anchor$src_subject_id == i,variable_name]
    }
  }
  data = rbind(data_anchor,data_BOFC)
  cat(sprintf('Variable:%s BOCF finished!\n',variable_name))
  return(data)
}
Comb.MICE <- function(dat.imp,var.ls.imp){
  original.dat <- complete(dat.imp,0)
  completed.dat <- complete(dat.imp,"long")
  imputed.dat <- select(original.dat,src_subject_id)
  for (i in var.ls.imp){
    fprintf("Combining Variable: %s among all imputed datasets.\n",i)
    subset(completed.dat,select = -.id) %>%
      pivot_wider(id_cols = src_subject_id,
                  names_from = .imp,
                  values_from = matches(i)) %>%
      as.data.frame() -> single.var.dat
    tmp.dat <- select(single.var.dat,src_subject_id)
    if ("factor" %in% class(original.dat[[i]])){
      tmp.dat[i] <- apply(single.var.dat[,-1],MARGIN = 1, MODE.Row)
      if ("character" %in% class(tmp.dat[[i]])){
        tmp.dat[i] = factor(tmp.dat[[i]],
                            levels = levels(original.dat[[i]]),
                            ordered = "ordered" %in% class(original.dat[[i]]),
        )
      }
      
    }else if ( Check.Numeric(class(original.dat[[i]])) ){
      tmp.dat[i] <- apply(single.var.dat[,-1],MARGIN = 1, median)
    }
    imputed.dat = merge(imputed.dat,tmp.dat,by = "src_subject_id", all = F)
    fprintf('Compare variable values before and after multiple imputation:\n')
    fprintf('Before MI:')
    print(table(original.dat[i],useNA = 'if'))
    fprintf('After MI:')
    print(table(imputed.dat[i],useNA = 'if'))
  }
  return(imputed.dat)
}


