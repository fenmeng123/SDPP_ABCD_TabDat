SDPP.RunConcat <- function(FileLabelList = NULL,
                          OutputFileName = 'Merge_tmp',
                          FileType = 'rds',
                          BOCF_VarList = NULL,
                          TabulatedDataDirectory = NULL,
                          ProjectDirectory = "../DataAnalysis/Tmp_Project",
                          AutoLogFolder = "../DataAnalysis/Tmp_Project/Res_1_Logs",
                          ResultsOutputDir = "../DataAnalysis/Tmp_Project/Res_2_Results/Res_Preproc",
                          IntermediateDataDir = "../DataAnalysis/Tmp_Prokect/Res_3_IntermediateData",
                          Prefix = "ABCDtmp",
                          ...){
  if (is.null(FileLabelList)){
    fprintf("|SDPP Concat| Please check the input argument: [FileLabelList] !\n")
    stop("|SDPP Concat| The list of data files to be merged must be explicitly specified!\n")
  }
  if (is.null(BOCF_VarList)){
    fprintf("|SDPP Concat| [BOCF_VarList] is null !\n")
    fprintf("|SDPP Concat| Using SDPP default BOCF covariates list:\n")
    BOCF_VarList <- c("SexAssigned",
                      "GroupID",
                      "Race_6L","Ethnicity_PrntRep",
                      "Handedness",
                      "ParentsMarital_2L","ParentsHighEdu_2L","ParentsMarital_X_Employ",
                      "Relationship_3L",
                      "AdoptionChild",
                      "GenderIdentity_PrntRep",
                      "RaceEthnicity",
                      "Relationship",
                      "Religon_PrntRep",
                      "AdoptionAge",
                      "USALiveYears",
                      "GenderIdentity_PrntSelf",
                      "Race_PrntSelf",
                      "ParentHighEdu","PartnerHighEdu",
                      "ParentEmploy","PartnerEmploy","FamilyNumInLF",
                      "SameSexTwin",
                      "GI_PairedSubID_Sub_1","GI_PairProb_Sub_1","GI_Zygosity_Sub_1",
                      "GI_PairedSubID_Sub_2","GI_PairProb_Sub_2","GI_Zygosity_Sub_2",
                      "GI_PairedSubID_Sub_3","GI_PairProb_Sub_3","GI_Zygosity_Sub_3",
                      "GI_PairedSubID_Sub_4","GI_PairProb_Sub_4","GI_Zygosity_Sub_4",
                      "GI_AncestryPC_1","GI_AncestryPC_2","GI_AncestryPC_3",
                      "GI_AncestryPC_4","GI_AncestryPC_5","BirthID",
                      "Education_R",
                      "Race_PrntRep","ParentsMarital_6L","Race_4L",
                      "Religon_8L",
                      "FamilyIncome",
                      "YouthNativeLang",
                      "Religon_2L",
                      "HouseholdSize","HouseholdStructure",
                      "BirthCountry","ParentsHighEdu_5L")
    print(BOCF_VarList)
  }
  
  pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
  bruceR::set.wd()
  source('SDPP_subfunctions.R')
  SDPP.check.package(package_name = c('tidyverse',
                                      'stringr',
                                      'naniar'))
  require(bruceR)
  require(tidyverse)
  require(stringr)
  require(naniar)
  
  AutoLogFileName = sprintf('Log_SDPP-ABCD-TabDat_Concat_%s_%s.txt',
                            OutputFileName,
                            str_trunc(str_replace_all(as.character(Sys.time()),"( )|(:)|([.])|(-)","_"),
                                      width = 21,
                                      ellipsis = ""))
  AutoLogFilePath = fullfile(AutoLogFolder,AutoLogFileName)
  s_sink(AutoLogFilePath)
  
  IntermediateFileList <- str_c(Prefix,'_',
                                FileLabelList,
                                '.',
                                FileType)
  IntermediateFileDir <- vector(mode = "character",length = length(IntermediateFileList))
  for (ifileNo in 1:length(IntermediateFileList)){
    tmpFileDir <- common::file.find(
      path = IntermediateDataDir,
      pattern = IntermediateFileList[ifileNo],
      up = 0,
      down = 0) %>%
      fullfile() 
    if (length(tmpFileDir) == 0) tmpFileDir <- NA
    IntermediateFileDir[ifileNo] <- tmpFileDir
  }
  T_IntermediateFile <- data.frame(TargetFile = IntermediateFileList,
                                   MachtedFile = IntermediateFileDir)
  fprintf("|SDPP Concat| %d intermediate data files were found:\n",
          nrow(na.omit(T_IntermediateFile)))
  knitr::kable(T_IntermediateFile,format = 'simple') %>%
    print()
  fprintf("|SDPP Concat| %d intermediate data files will be ignored because lost of directory!\n",
          nrow(T_IntermediateFile) - nrow(na.omit(T_IntermediateFile)))
  if (nrow(T_IntermediateFile) > nrow(na.omit(T_IntermediateFile))){
    OutputFileName <- str_c(OutputFileName,'_Incomp')
    fprintf("|SDPP Concat| A post-fix [Incomp] will be appended to the output file name: [%s]\n",
            OutputFileName)
  }
  
  T_IntermediateFile <- na.omit(T_IntermediateFile)
  
  dat.file.ls = T_IntermediateFile$MachtedFile
  
  preprocessed.dat.ls = list()
  iter_num = 1
  for (irds in dat.file.ls){
    dat.path = fullfile(irds)
    fprintf("Load data from RDS file: %s\n",dat.path)
    preprocessed.dat.ls[[iter_num]] = readRDS(dat.path)
    iter_num = iter_num + 1
  }
  
  concatenated_data <- preprocessed.dat.ls %>%
    reduce(full_join,
           by = c("src_subject_id","eventname"))
  fprintf("|SDPP Concat| Concatenate %s data... Finished!\n",
          paste(T_IntermediateFile$TargetFile,collapse = ", "))
  
  # load ABCD longitudinal tractor and fill NA blank in concatenated_data
  lt = readABCDdata(fullfile(TabulatedDataDirectory,'/abcd-general/abcd_y_lt.csv'))
  
  Flag = which(is.na(concatenated_data$interview_age) | is.na(concatenated_data$interview_date))
  
  for (i in Flag){
    fprintf("|SDPP Concat| Fill blank for Subject: %s [Wave: %s] (from ABCD Longitudinal Tractor)...\n",
            concatenated_data$src_subject_id[i],
            concatenated_data$eventname[i])
    SingleSubMask = (lt$src_subject_id == concatenated_data$src_subject_id[i]) & 
      (lt$eventname == concatenated_data$eventname[i])
    concatenated_data$interview_age[i] = lt$interview_age[SingleSubMask]
    concatenated_data$interview_date[i] = lt$interview_date[SingleSubMask]
    concatenated_data$VisitType[i] = RECODE(lt$visit_type[SingleSubMask],
                                            "1 = 'On-site';
                                          2 = 'Remote';
                                          3 = 'Hybrid';")
    concatenated_data$SiteID[i] = lt$site_id_l[SingleSubMask]
    concatenated_data$FamilyID[i] = lt$rel_family_id[SingleSubMask]
    concatenated_data$SchoolID[i] = lt$school_id[SingleSubMask]
    concatenated_data$DistrictID[i] = lt$district_id[SingleSubMask]
    concatenated_data$BirthID[i] = lt$rel_birth_id[SingleSubMask]
    fprintf("|SDPP Concat| \t\t\t interview_age, date, VisitType, SiteID, FamilyID, SchoolID, DistrictID, BirthID were filled.\n")
    fprintf("|SDPP Concat| \t\t\t %d,%s,%s,%s,%s,%s,%s,%s.\n",
            concatenated_data$interview_age[i],
            concatenated_data$interview_date[i],
            concatenated_data$VisitType[i],
            concatenated_data$SiteID[i],
            concatenated_data$FamilyID[i],
            concatenated_data$SchoolID[i],
            concatenated_data$DistrictID[i],
            concatenated_data$BirthID[i])
    if (is.na(concatenated_data$FamilyID[i])){
      fprintf("|SDPP Concat| \t\t\t FamilyID is Still NA, Executing BOFC for FamilyID....\n")
      tmpT0Flag = (concatenated_data$src_subject_id == concatenated_data$src_subject_id[i]) &
        (concatenated_data$eventname == "baseline_year_1_arm_1")
      concatenated_data[i,"FamilyID"] = concatenated_data[which(tmpT0Flag),"FamilyID"]
      fprintf("\t\t\t BOCF for FamilyID Finished! FamilyID: %s\n",concatenated_data$FamilyID[i])
    }
    if (sum(is.na(concatenated_data[i,BOCF_VarList])) > 16 ){
      fprintf("|SDPP Concat| BOCF for Subject: %s [Wave: %s] from baseline_year_1_arm_1... Finished!\n",
              concatenated_data$src_subject_id[i],
              concatenated_data$eventname[i])
      tmpT0Flag = (concatenated_data$src_subject_id == concatenated_data$src_subject_id[i]) &
        (concatenated_data$eventname == "baseline_year_1_arm_1")
      concatenated_data[i,BOCF_VarList] = concatenated_data[which(tmpT0Flag),BOCF_VarList]
    }else{
      fprintf("|SDPP Concat| \t\t\t Subject: %s [Wave: %s]: The number of missing values in demographics\n\t\t\t did not reach the threshold. Do not need to run BOCF.\n",
              concatenated_data$src_subject_id[i],
              concatenated_data$eventname[i])
    }
  }
  
  DataType <- sapply(concatenated_data, typeof) %>% unlist() %>% as.data.frame()
  DataType <- rename(DataType,VariableTypes = .)
  DataType$VariableNames <- rownames(DataType)
  rownames(DataType) <- NULL
  DataType$VariableTypes[sapply(concatenated_data, is.factor)] <- "char"
  DataType$VariableTypes <- str_replace_all(DataType$VariableTypes,'integer','double')
  DataType$VariableTypes <- str_replace_all(DataType$VariableTypes,'character','char')
  
  SDPP.save.file(DataType,
                 FileName = sprintf('%s_DataType.csv',OutputFileName),
                 Prefix = Prefix,
                 ProjectDirectory = ProjectDirectory)
  SDPP.save.file(concatenated_data,
                 FileName = sprintf("%s.rds",OutputFileName),
                 Prefix = Prefix,
                 ProjectDirectory = ProjectDirectory)
  SDPP.save.file(concatenated_data,
                 FileName = sprintf("%s.csv",OutputFileName),
                 Prefix = Prefix,
                 ProjectDirectory = ProjectDirectory)
  
  concatenated_data %>% MVA.Report.By.Wave() %>%
    print_table(file = fullfile(ResultsOutputDir,
                                sprintf("MVA_Report_ALL_%s.doc",OutputFileName)),
                row.names = F,
                digits = 2)
  # End of Script -----------------------------------------------------------
  
  fprintf("|SDPP Concat| SDPP-ABCD-TabDat Concatenate finished! Finish Time:%s\n",Sys.time())
  
  sink()
}