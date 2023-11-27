# SDPP_subfunctions.R is a script contains all user-defined functions that are
# needed when using the SDPP-ABCD-TabDat pipeline. Please make sure you have
# sourced this script before running any steps in SDPP-ABCD-TabDat.
# Author: Kunru Song
# 
# 
# 
# 1. Basic File I/O Functions ---------------------------------------------
addprefix <- function(prefix,filename,postfix=NA){
  if (!is.na(prefix)){
    filename = paste(prefix,
                     filename,
                     sep = '_')
  }
  if (is.na(postfix)){
    return(filename)
  }else{
    filename = paste(filename,
                     postfix,
                     sep = '.')
    return(filename)
  }
}
fullfile <- function(...){
  PathStr = normalizePath(
    paste(...,sep = '/'),
    winslash = '/',
    mustWork = F)
  return(PathStr)
}
fprintf <- function(StringVar,...){
  cat(sprintf(StringVar,...))
}
s_sink <- function(LogFileDir){
  # Close all opened log files
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
  # Start a new logging session
  fprintf('Start a new log file: %s\n',LogFileDir)
  sink(file = LogFileDir,
       type = 'output',
       append = F,
       split = T)
}
s_get_script_name <- function(){
  return(basename(rstudioapi::getSourceEditorContext()$path))
}
s_close_sink <- function(){
  StepNumStr <- s_get_script_name() %>%
    str_extract(pattern = 'Step(\\d{1,})') %>%
    str_remove_all('Step')
  fprintf("SDPP-ABCD-TabDat Step %s finished! Finish Time:%s\n",
          StepNumStr,Sys.time())
  sink()
}

read4.0 <- function(filename){
  data = read.table(filename,header = TRUE,sep = '\t')
  # remove some specific columns which are same across all .txt files
  data <- subset(data, 
                 select = -c(collection_id,
                             dataset_id,
                             collection_title))
  # get variable descriptions
  var.descrip <- data[1,]
  # remove the first row
  data<-data[-1,]
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
  cat("[Single File] Subjects Counts (stratified by eventname):\n")
  print(table(data$eventname,useNA = 'if'))
  return(data)
}
read.in.batch <- function(DownloadedDataDir,TableNames,FolderName=NA){
  if (is.na(FolderName)){
    DataFileDir = DownloadedDataDir
  }else{
    DataFileDir = fullfile(DownloadedDataDir,FolderName)
  }
  for (i in TableNames){
    if (i == TableNames[1]){
      fprintf("[%s] is the first element in TableNames vector, which would be set at anchor data frame.\n",i)
      MergedDF = readABCDdata(filename = fullfile(DataFileDir,i))
    }else{
      tmpDF = readABCDdata(filename = fullfile(DataFileDir,i))
      fprintf("Appending new data frame [%s] ......\n",i)
      MergedDF = merge(MergedDF,tmpDF,by = c('src_subject_id','eventname'),all=T)
    }
  }
  cat("[Merged Data Frame] Subjects Counts (stratified by eventname):\n")
  print(table(MergedDF$eventname,useNA = 'if'))
  return(MergedDF)
}
eval_s <- function(user.string,...){
  res = eval(parse(text = sprintf(user.string,...)))
  return(res)
}
# 2. SDPP Project Managing Functions --------------------------------------
SDPP.clearall <- function(){
  rm(list=ls())
  gc()
}
SDPP.check.package <- function(package_name = c("readxl",
                                                "bruceR",
                                                "forcats",
                                                "mice",
                                                "svDialogs",
                                                "naniar",
                                                "R.matlab",
                                                "Hmisc",
                                                "tidyverse",
                                                "knitr")){
  res = data.frame(name = package_name)
  for (i in 1:length(package_name)){
    package_path = system.file(package = package_name[i])
    if(!nzchar(package_path)){
      package_status = "Not Installed!"
    }else{
      package_status = "Installed."
    }
    res$status[i] = package_status
    res$path[i] = package_path
  }
  if (any(res$status %in% "Not Installed!")){
    fpirntf("Some required R packageds have not been installed! Please see the table below:\n")
    print(res)
    fprintf("SDPP-ABCD-TabDat rely on these packages, please make sure you have installed all required R packages!\n")
    stop("SDPP-ABCD-TabDat Check Required Packages: Failed! See above information.")
  }else{
    fprintf("SDPP-ABCD-TabDat Check Required Packages: Passed!\n")
    fprintf("All required R packages have been installed: \n")
    print(res)
  }
  return(res)
}
SDPP.ABCD.TabDat.PrepareProject <- function(ProjectDirectory,verbose = T){
  SDPPFolderStruct <- list(
    Res_1_Dir = fullfile(ProjectDirectory,'Res_1_Logs'),
    Res_2_Dir = fullfile(ProjectDirectory,'Res_2_Results'),
    Res_3_Dir = fullfile(ProjectDirectory,'Res_3_IntermediateData'),
    Res_4_Dir = fullfile(ProjectDirectory,'Res_4_Reports'),
    Res_5_Dir = fullfile(ProjectDirectory,'Res_5_GitIgnore'),
    Step_0_Dir = fullfile(ProjectDirectory,'Step_0_Preprocessing'),
    Supp_1_Dir = fullfile(ProjectDirectory,'Supp_1_Subfunctions'),
    Supp_2_Dir = fullfile(ProjectDirectory,'Supp_2_References')
  )
  if (!dir.exists(ProjectDirectory)){
    if (verbose) cat("Project Directory not found! Auto-creating SDPP Project Folder...\n")
    dir.create(ProjectDirectory)
    if (verbose) cat(sprintf("SDPP Project has been created at %s\n",normalizePath(ProjectDirectory)))
  }
  for (i in names(SDPPFolderStruct)){
    if (!dir.exists(SDPPFolderStruct[[i]])){
      dir.create(SDPPFolderStruct[[i]])
      if (verbose) fprintf('|SDPP Default Project Structure - [%s]|\n\tCreated folder: [%s]\n',
                           i,SDPPFolderStruct[[i]])
    }else{
      if (verbose) fprintf('|SDPP Default Project Structure - [%s]|\n\tAlready exist: [%s]\n',
                           i,SDPPFolderStruct[[i]])
    }
  }
  if (verbose) cat("Project Folder Structure:\n")
  if (verbose) fprintf('%s\n',list.files(fullfile(ProjectDirectory)))
  AutoLogFolderPath = SDPPFolderStruct[['Res_1_Dir']]
  return(AutoLogFolderPath)
}
SDPP.Initialize <- function(ProjectDirectory){
  fprintf('Starting SDPP-ABCD-TabDat......\n')
  fprintf('R package: bruceR is a common-used package in this pipeline, which would be included in all steps.\n')
  # Check the required packages
  res <- SDPP.check.package()
  basic.info = sessionInfo()
  other.info = Sys.info()
  IntermediateDataDir = fullfile(ProjectDirectory,'Res_3_IntermediateData')
  ResultsOutputDir = SDPP.set.output(
    fullfile(ProjectDirectory,
             'Res_2_Results',
             'Res_Preproc'))
  fprintf("===============================SDPP-ABCD-TabDat Settings===============================\n")
  fprintf("Scripts Executing Date: \t\t %s\n",Sys.time())
  fprintf("Running under OS: \t\t %s\n",basic.info$running)
  fprintf("Platform: \t\t\t %s\n",basic.info$platform)
  fprintf("R Version: \t\t\t %s\n",basic.info$R.version$version.string)
  fprintf("Running on Computer Name: \t %s\n",other.info['nodename'])
  fprintf("Working Directory: \t\t %s\n",getwd())
  fprintf("Project Directory: \t\t\t %s\n",fullfile(ProjectDirectory))
  fprintf("Downloaded NDA Data Directory: \t %s\n",fullfile(TabulatedDataDirectory))
  fprintf("Output Intermediate Data File Prefix: \t %s\n",Prefix)
  fprintf("Output Intermediate Data Directory: \t %s\n",IntermediateDataDir)
  fprintf("Output Derivative Files Directory:\t %s\n",ResultsOutputDir)
  AutoLogFolder = SDPP.ABCD.TabDat.PrepareProject(ProjectDirectory)
}
SDPP.save.file <- function(Data,
                           FileName,
                           Prefix,
                           ProjectDirectory,
                           FileFormat=NA,
                           DataLabel=NA){
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
    fprintf('Saving Data into %s File: %s......\nFinished!\n',toupper(FileFormat),OutputFileDir)
  } else if (FileFormat == 'csv'){
    write.csv(Data,OutputFileDir,fileEncoding = 'UTF-8')
    fprintf('Saving Data into %s File: %s......\nFinished!\n',toupper(FileFormat),OutputFileDir)
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
SDPP.set.output <- function(ResultsOutputDir){
  if (!dir.exists(ResultsOutputDir)){
    fprintf("Output Directory:%s not found, a new folder will be created.",ResultsOutputDir)
    dir.create(ResultsOutputDir)
  }else{
    fprintf("Output Directory Check Passed. Folder at %s\n",ResultsOutputDir)
  }
  return(ResultsOutputDir)
}
SDPP.get.number <- function(FolderPrefix,ProjectDirectory){
  FolderList = dir(path = ProjectDirectory,
                   pattern = sprintf("%s_.*",FolderPrefix))
  Num = grep("\\d",
             unlist(strsplit(FolderList,
                             split = "_",
                             fixed = T)),
             value = T) %>%
    as.numeric()
  return(Num)
}
SDPP.creat.folder <- function(FolderName,
                              ProjectDirectory,
                              FolderNum=NA,
                              Type='Step'){
  if (any(str_ends(dir(ProjectDirectory),FolderName))){
    fprintf("Duplicated subfolder was found in project folder: [%s]\n",
            ProjectDirectory)
    fprintf("\tDuplicated name: [%s]\n",
            dir(ProjectDirectory)[str_ends(dir(ProjectDirectory),FolderName)])
    fprintf("SDPP.creat.folder will be skiped. No folder will be created.\n")
  }else{
    ExistedStepNum = SDPP.get.number(Type,ProjectDirectory)
    TailStepNum = tail(ExistedStepNum,1)
    if (any(is.na(FolderNum))){
      NewStepNum = TailStepNum + 1
      fprintf("Folder Number is NA.
              Automatedly use tail plus one.(Auto set number:%d)\n",
              NewStepNum)
    }else if (FolderNum %in% ExistedStepNum){
      NewStepNum = TailStepNum + 1
      fprintf("The given Folder Number already exists.
              Automatedly use tail plus one.(Auto set number:%d)\n",
              NewStepNum)
    }else{
      NewStepNum = FolderNum
    }
    if (Type %in% c('Step','Res','Supp','RepVis','SensAna')){
      NewFolderName = paste(Type,NewStepNum,FolderName,sep = '_')
      dir.create(fullfile(ProjectDirectory,NewFolderName))
      fprintf("New SDPP Folder: [%s] has been created at Project:%s\n",
              NewFolderName,ProjectDirectory)
    }else{
      fprintf("SDPP Folder Type incorrect! No folder will be created.\n")
      stop("Please reset the requested SDPP folder type.")
    }
  }
}
SDPP.filter.data.dict <- function(DataDictionaryFileDir = dir(pattern = '.*Dictionary.*xlsx'),
                                  filter_col = 'Included_Flag',
                                  filter_key = 'Yes',
                                  search_col = NA,
                                  verbose = T){
  # Exclude the temporal copy file when Data Dictionary is currently opened.
  DataDictionaryFileDir <- DataDictionaryFileDir[
    !str_detect(DataDictionaryFileDir,'^\\~\\$')
  ]
  # Identify multiple Data Dictionary files
  if (length(DataDictionaryFileDir) > 1){
    warning('Multiple ABCD Data Dictionray (EXCEL files) have been found! Using the first one as default.')
    DataDictionaryFileDir <- DataDictionaryFileDir[1]
  }
  if (verbose){
    fprintf('ABCD Data Dictionary used: [%s]\n',DataDictionaryFileDir)
  }
  dict <- DataDictionaryFileDir %>%
    import() %>%
    as.data.frame()
  RowFlag = which(dict[[filter_col]] == filter_key)
  if (any(is.na(search_col))){
    filtered_dict = dict[RowFlag,]
  }else{
    if (length(search_col)==1){
      filtered_dict = dict[[search_col]][RowFlag]
    }else if (length(search_col)>1){
      filtered_dict = dict[RowFlag,search_col]
    }
  }
  return(filtered_dict)
}
SDPP.select.cols.by.dict <- function(data,
                                     TableNames,
                                     DataDictionaryFileDir = 
                                       dir(pattern = '.*ABCD Data Dictionary\\.xlsx$')){
  if (any(str_detect(TableNames,'\\.csv'))){
    TableNames = str_remove_all(TableNames,
                                '\\.csv')
  }
  filtered_dict <- SDPP.filter.data.dict(DataDictionaryFileDir,
                                         search_col = c('table_name','var_name')) %>%
    subset(table_name %in% TableNames)
  fprintf('%d columns will be selected according to data dictionary file.\n',
          nrow(filtered_dict))
  fprintf('Selected columns in data frame:\n')
  print(knitr::kable(filtered_dict,row.names = F,format = 'simple'))
  selected_data <- data %>%
    select(c(src_subject_id,
             eventname,
             all_of(filtered_dict$var_name))
    )
  return(selected_data)
}

# 3. Data Processing Functions --------------------------------------------
dt.print.mva.counts <- function(dt_name,var_name){
  if (is.character(dt_name) & is.character(var_name)){
    if (!eval_s("is.data.table(%s)",dt_name)){
      eval_s('%s = as.data.table(%s)',dt_name,dt_name)
    }
    if (eval_s("is.numeric(%s$%s)",dt_name,var_name)){
      print(eval_s("print(psych::describe(%s$%s,check=T))",dt_name,var_name))
    }else{
      print(eval_s("%s[,table(%s,useNA = 'if')]",dt_name,var_name))
    }
  }else if (is.data.table(dt_name) & is.character(var_name)){
    print(table(dt_name[[var_name]],useNA = 'if',dnn = var_name))
  }
}
df.print.mva.counts <- function(var,df=NA) {
  if (!"data.frame" %in% class(df)){
    if (is.factor(var) | is.character(var)){
      print(table(var,useNA = 'if',dnn = deparse(substitute(var))))
    }else if (is.numeric(var)){
      print(psych::describe(var,check = T))
    }
  }else if (is.character(var)){
    if (is.factor(df[[var]]) | is.character(df[[var]]) ){
      print(table(df[[var]],useNA = 'if',dnn = var))
    }else if (is.numeric(df[[var]])){
      print(psych::describe(df[[var]],check = T))
    }
  }
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
Check.YN <- function(var){
  if (is.numeric(var)){
    Flag = sum(c(0,1) %in% unique(var))==2
  }else if (is.character(var)){
    Flag = sum(c('0','1') %in% unique(var))==2
  }
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
    df.print.mva.counts(i,original.dat)
    fprintf('After MI:')
    df.print.mva.counts(i,imputed.dat)
  }
  return(imputed.dat)
}
Recode.Eventname <- function(df){
  if ('eventname' %in% colnames(df)){
    if("baseline_year_1_arm_1" %in% unique(df$eventname)){
      df$eventname = RECODE(df$eventname,
                            "'baseline_year_1_arm_1'='T0';")
      fprintf("Re-code 'baseline_year_1_arm_1' to 'T0'.\n")
    }
    if("1_year_follow_up_y_arm_1" %in% unique(df$eventname)){
      df$eventname = RECODE(df$eventname,
                            "'1_year_follow_up_y_arm_1'='T1';")
      fprintf("Re-code '1_year_follow_up_y_arm_1' to 'T1'.\n")
    }
    if("2_year_follow_up_y_arm_1" %in% unique(df$eventname)){
      df$eventname = RECODE(df$eventname,
                            "'2_year_follow_up_y_arm_1'='T2';")
      fprintf("Re-code '2_year_follow_up_y_arm_1' to 'T2'.\n")
    }
    if("3_year_follow_up_y_arm_1" %in% unique(df$eventname)){
      df$eventname = RECODE(df$eventname,
                            "'3_year_follow_up_y_arm_1'='T3';")
      fprintf("Re-code '3_year_follow_up_y_arm_1' to 'T3'.\n")
    }
    if("4_year_follow_up_y_arm_1" %in% unique(df$eventname)){
      df$eventname = RECODE(df$eventname,
                            "'4_year_follow_up_y_arm_1'='T4';")
      fprintf("Re-code '4_year_follow_up_y_arm_1' to 'T4'.\n")
    }
    if("5_year_follow_up_y_arm_1" %in% unique(df$eventname)){
      df$eventname = RECODE(df$eventname,
                            "'5_year_follow_up_y_arm_1'='T5';")
      fprintf("Re-code '5_year_follow_up_y_arm_1' to 'T5'.\n")
    }
    return(df)
  }else{
    stop("Can not re-code eventname from data frame! Column 'eventname' not found!")
  }
}
Recode.STQ <- function(var,Scheme = '7 Levels'){
  if (Scheme == '7 Levels'){
    V_NEW = RECODE(var,
                   "0=0;
                   0.25=0.25;
                   0.5=0.5;
                   c(0.75,1,1.25,1.5)=1;
                   c(1.75,2,2.25,2.5)=2;
                   c(2.75,3,3.25,3.5)=3;
                   3.75:hi=4;
                   else=NA")
  }
  return(V_NEW)
}
Recode.ABCD.NA <- function(var,VarName = NA){
  if (is.na(VarName)){
    VarName = deparse(substitute(var))
  }
  if (Check.Numeric(class(var))){
    fprintf("Auto-recode 777 and 999 into NA for %s. \t",VarName)
    fprintf("The number of '777':  %d. The number of '999': %d.\n",
            sum(var==777,na.rm = T),
            sum(var==999,na.rm = T))
    fprintf("Re-coding these values to NA......\t")
    V_NEW = RECODE(var,"777=NA;999=NA")
    fprintf(" Finished! \n")
    return(V_NEW)
  }else {
    stop("Input vector is not a numeric variable!Please Check your code!")
  }
}
Recode.ABCD.RADT <- function(var,VarName = NA,charFlag=T){
  if (is.na(VarName)){
    VarName = deparse(substitute(var))
  }
  if (Check.Numeric(class(var))){
    fprintf("Auto-recode 777 to 'Refuse Answer' and 999 to 'Dont Know' for %s. \t",VarName)
    fprintf("The number of '777':  %d. The number of '999': %d.\n",
            sum(var==777,na.rm = T),
            sum(var==999,na.rm = T))
    if (charFlag){
      fprintf("Re-coding these values to RA and DT......\t")
      V_NEW = RECODE(var,"777='Refuse to Answer';999='Dont Know';")
    }else{
      fprintf("Re-coding these values to -1 and -2......\t")
      V_NEW = RECODE(var,"777=-1;999=-2;")
    }
    fprintf(" Finished! \n")
    return(V_NEW)
  }else {
    stop("Input vector is not a numeric variable!Please Check your code!")
  }
}
Recode.ABCD.YN <- function(var,VarName=NA){
  if (is.na(VarName)){
    VarName = deparse(substitute(var))
  }
  fprintf("Input: %s \t Check for ABCD-style YN variable:\t",VarName)
  if (Check.YN(var)){
    fprintf('Passed.\n')
    var = Recode.ABCD.NA(var,VarName = VarName)
    V_NEW = RECODE(var,
                   "0='No';1='Yes';else=NA;")
    V_NEW = factor(V_NEW,
                   levels = c('No','Yes'))
    fprintf('Re-coded YN Variable (from %s):\n',VarName)
    print(table(V_NEW,useNA = 'if'))
  }else{
    fprintf('Falied!\n')
    stop("The input vector is not a ABCD-style YN variable.Please Check your code!")
  }
  return(V_NEW)
}
Recode.ABCD.NM.NT <- function(var_df,var_ls){
  for (i in var_ls){
    fprintf('Re-coding NM & NT for variable: %s ......\n',i)
    tmp = select(var_df,contains(i))
    fprintf('\t A temp data frame containing the following variables was extracted: \n')
    fprintf("\t %s \n",colnames(tmp))
    Flag = (tmp[[str_c(i,'_nm')]] == tmp[[str_c(i,'_nt')]])
    Flag = replace_na(Flag,FALSE)
    fprintf("\t Number: %d rows were flagged that NM is equals to NT\n",sum(Flag,na.rm = T))
    tmp[Flag,i] <- NA
    var_df[[i]] <- tmp[[i]]
    fprintf("\t replacing these values with NAs....... \t Completed!\n")
  }
  var_df %>% select(-ends_with('_nm')) %>% select(-ends_with('_nt')) -> var_df
  return(var_df)
}
MVA.Report.By.Wave <- function(df){
  fprintf("Auto-MVA Report Starting......\n")
  if ('eventname' %in% colnames(df)){
    df %>% Recode.Eventname() %>% 
      group_by(eventname) %>% miss_var_summary() %>% 
      pivot_wider(id_cols = variable,
                  values_from = c(n_miss,pct_miss),
                  names_from = eventname,
                  names_sort = T,
                  names_vary = "slowest") %>%
      as.data.frame() %>% arrange(variable) -> MVA_Report
    
  }else{
    fprintf("Column 'eventname' not found! group_by function was ignored!")
    df %>% miss_var_summary() %>% 
      as.data.frame() %>% arrange(pct_miss) -> MVA_Report
  }
  fprintf("Auto-MVA Report Finished!\n")
  head(MVA_Report, n = 5L)
  return(MVA_Report)
}
MVA.Report.CaseMiss.By.Wave <- function(df,verbose = T){
  if ('eventname' %in% colnames(df)){
    df <- Recode.Eventname(df)
  }
  fprintf("Included variables in MVA: \n")
  print(colnames(df)[!colnames(df) %in% c('src_subject_id','eventname') ])
  Report_case_miss <- df %>% 
    group_by(eventname) %>%
    select(-src_subject_id) %>%
    miss_case_summary()
  Report_case_miss$pct_miss <- sprintf('%2.2f',Report_case_miss$pct_miss)
  Report_case_miss$pct_miss <- factor(Report_case_miss$pct_miss,
                                      levels = unique(Report_case_miss$pct_miss))
  Report_summary <- Report_case_miss %>% 
    count(pct_miss) %>% 
    as.data.frame() 
  Report_summary$pct_miss <- Report_summary$pct_miss %>% 
    as.character() %>%
    str_c('%') %>% 
    RECODE("'100.00%' = 'Complete Miss (100%)';
           '0.00%' = 'Non-miss (0%)';")
  Report_summary <- rename(Report_summary,
                           `Case Miss Status (Percentage)` = pct_miss,
                           `Number of Cases` = n)
  fprintf("Case Miss Summary Table:\n")
  Report_print <- rename(Report_summary,
                         Status = `Case Miss Status (Percentage)`)
  Report_print$Status <- RECODE(Report_print$Status,
                                "'Complete Miss (100%)' = 'Complete Miss (100%)';
                              'Non-miss (0%)' = 'Non-miss (0%)';
                              else = 'Non-complete Miss (0%<pct<100%)';")
  Report_print %>%
    group_by(eventname,Status) %>% 
    reframe(N=sum(`Number of Cases`)) %>%
    as.data.frame() %>%
    print()
  if (verbose){
    for (i in unique(Report_case_miss$eventname)){
      SubID <- df$src_subject_id[df$eventname==i]
      SUbID_AnyMiss <- SubID[Report_case_miss$case[(Report_case_miss$eventname == i) &
                                                     (Report_case_miss$pct_miss != "0.00")]]
      SubID_CompMiss <- SubID[Report_case_miss$case[(Report_case_miss$eventname == i) &
                                                      (Report_case_miss$pct_miss == "100.00")]]
      SubID_PartMiss <- SubID[Report_case_miss$case[(Report_case_miss$eventname == i) &
                                                      (Report_case_miss$pct_miss != "0.00") & 
                                                      (Report_case_miss$pct_miss != "100.00")]]
      fprintf("At [%s] (time point), any Miss in the above variables (Subject ID):\n",i)
      print(SUbID_AnyMiss)
      fprintf("At [%s] (time point), Where, the following IDs are completedly missing:\n",i)
      print(SubID_CompMiss)
      fprintf("At [%s] (time point), in contrast, the following IDs are partially missing:\n",i)
      print(SUbID_AnyMiss)
    }
  }
  return(Report_summary)
}
Merge.Value.NA <- function(V1,V2){
  fprintf("Merging Two Variables: %s and %s ......\n",deparse(substitute(V1)),deparse(substitute(V2)))
  paste(
    as.character(V1),
    as.character(V2),
    sep = "") %>%
    str_remove_all('NA') %>%
    as.numeric() -> V_NEW
  fprintf("New Variable Value Counts After Merging:\n")
  if (length(unique(V_NEW)>30)){
    print(psych::describe(V_NEW))
    # print(summary(V_NEW))
  }else{
    print(table(V_NEW,useNA = 'if'))
  }
  return(V_NEW)
}
Call.MATLAB.knnimpute <- function(data_matrix,MATLAB_server_obj){
  if (!isOpen(MATLAB_server_obj)) {
    stop("MATLAB server is not connected!")
  }
  setVariable(MATLAB_server_obj,data = data_matrix)
  # evaluate(MATLAB_server_obj,"data = struct2table(data);")
  evaluate(MATLAB_server_obj,"tmp = knnimpute(data);")
  imputed_data <- getVariable(MATLAB_server_obj,"tmp")
  evaluate(MATLAB_server_obj,"clearvars data tmp")
  return(imputed_data)
}
MVA.KNNimpute <- function(df,var_ls,MATLAB_server_obj){
  df %>% select(all_of(var_ls)) %>% sapply(as.numeric) %>%
    Call.MATLAB.knnimpute(MATLAB_server_obj) -> imputed_data
  df[,var_ls] <- imputed_data$tmp
  return(df)
}
Bind.imp.By.Wave <- function(raw_dat_name,imp_dat){
  data_masking <- paste(sprintf("(eventname != '%s')",
                                unique(imp_dat$eventname)),
                        collapse = ' & ')
  unimp_dat <- eval_s("subset(%s,%s)",
                      raw_dat_name,
                      data_masking)
  
  data_masking <- paste(sprintf("(eventname == '%s')",
                                unique(imp_dat$eventname)),
                        collapse = ' | ')
  wait_imp_dat <- eval_s("subset(%s,%s)",
                         raw_dat_name,
                         data_masking)
  imp_var_ls <- colnames(imp_dat)[!colnames(imp_dat) %in% c('src_subject_id','eventname')]
  wait_imp_dat <- select(wait_imp_dat,!all_of(imp_var_ls))
  
  new_dat <- base::merge(wait_imp_dat,imp_dat,
                         by = c('src_subject_id','eventname'))
  new_dat <- rbind(new_dat,unimp_dat)
  return(new_dat)
}

