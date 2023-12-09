SDPP.RunBatch <- function(SDPP_Step_SpecVec = NULL,
                          TabulatedDataDirectory = NULL,
                          ProjectDirectory = "../DataAnalysis/Tmp_Project",
                          AutoLogFolder = "../DataAnalysis/Tmp_Project/Res_1_Logs",
                          ResultsOutputDir = "../DataAnalysis/Tmp_Project/Res_2_Results/Res_Preproc",
                          IntermediateDataDir = "../DataAnalysis/Tmp_Prokect/Res_3_IntermediateData",
                          Prefix = "ABCDtmp",
                          ...){
  
  if (is.null(TabulatedDataDirectory)){
    fprintf("Please check the input argument: [TabulatedDataDirectory] !\n")
    stop("The directory of downloaded ABCD tabulated data must be explictly specified!\n")
  }
  SDPP.get.StepList <- function(){
    fprintf("|SDPP Batch| Searching avaiable Steps' script...\n")
    SDPP_Step_List <- data.frame(
      R_Script = list.files(pattern = "DataPreproc.*"))
    SDPP_Step_List$StepNum <- SDPP_Step_List$R_Script %>%
      str_extract_all(pattern = "Step\\d{1,}",simplify = T) %>%
      str_remove_all("Step") %>%
      as.numeric()
    SDPP_Step_List <- SDPP_Step_List$R_Script %>%
      str_extract_all(pattern = "Step\\d{1,}_.*\\.R",simplify = T) %>%
      str_remove_all("Step\\d{1,}_") %>%
      str_remove_all("\\.R$") %>%
      str_split_fixed(pattern = "_",n = Inf) %>%
      as.data.frame() %>%
      rename(
        Domain = V1,
        MeasureLevel = V2,
        SubDomain = V3,
        StepType = V4
      ) %>%
      cbind(SDPP_Step_List,.)
    SDPP_Step_List <- SDPP_Step_List %>%
      arrange(StepNum)
    RowFlag_EmptyType <- (SDPP_Step_List$StepType == "")
    SDPP_Step_List$StepType[RowFlag_EmptyType] <- SDPP_Step_List$SubDomain[RowFlag_EmptyType]
    RowFlag_MRIModality <- str_detect(SDPP_Step_List$Domain,
                                      pattern = "(sMRI)|(dMRI)|(rsfMRI)|(tfMRI)")
    SDPP_Step_List$SubDomain[RowFlag_MRIModality] <- SDPP_Step_List$Domain[RowFlag_MRIModality]
    SDPP_Step_List$SubDomain[RowFlag_EmptyType] <- NA
    SDPP_Step_List$Domain <- SDPP_Step_List$Domain %>%
      RECODE("c('sMRI','dMRI','rsfMRI','tfMRI') = 'MRI';
            'NT' = 'Novel Technology';
            'MH' = 'Mental Health';
            'NC' = 'Neurocognition';
            'CE' = 'Culture & Environment';")
    SDPP_Step_List$MeasureLevel <- SDPP_Step_List$MeasureLevel %>%
      RECODE("'Y' = 'Youth-report';
            'P' = 'Parent-report';
            'YP' = 'Youth & Parent-report'")
    SDPP_Step_List$StepType <- SDPP_Step_List$StepType %>%
      RECODE("'Reorg' = 'Re-organizing Data Format';
            'Rec' = 'Re-coding';
            'Imp' = 'Imputing';")
    return(SDPP_Step_List)
  }
  
  DateTime <- str_trunc(
    str_replace_all(
      as.character(Sys.time()),
      "( )|(:)|([.])|(-)",
      "_"),
    width = 16,
    ellipsis = "")
  BatchLogFileName = sprintf('Log_SDPP-ABCD-TabDat_Batch_%s.txt',DateTime)
  
  s_sink(fullfile(AutoLogFolder,BatchLogFileName),pause_flag = T)
  fprintf("|SDPP Batch| Start! %s", Sys.time())
  
  SDPP_Step_List <- SDPP.get.StepList()
  fprintf("|SDPP Batch| %d R scripts for SDPP steps were found!\n",nrow(SDPP_Step_List))
  fprintf("|SDPP Batch| %d elements in SDPP_Step_SpecVec were found!\n",length(SDPP_Step_SpecVec))
  if (!is.null(SDPP_Step_SpecVec)){
    fprintf("|SDPP Batch| SDPP_Step_SpecVec has been provided, using values in this logical vector.\n")
    if (length(SDPP_Step_SpecVec) != nrow(SDPP_Step_List)){
      stop("The length of user-sepcified SDPP_Step_SpecVec is not equal to all aviable SDPP Steps!")
    }
    if (any(str_detect(SDPP_Step_SpecVec,pattern = "SDPP\\.Run\\.Step\\d{1,}"))){
      stop("Some elements name in SDPP_Step_SpecVec do not follow the formed specification 'SDPP.Run.Step[1-99]'")
    }
    
    SepcVec_StepNum <- names(SDPP_Step_SpecVec) %>%
      str_split_fixed(pattern = '\\.',n=3) %>%
      .[,ncol(.)] %>%
      str_remove_all(pattern = 'Step') %>%
      as.numeric()
    SDPP_Step_SpecVec <- SDPP_Step_SpecVec[order(SepcVec_StepNum)]
    SDPP_Step_List$SpecVec_FuncName <- names(SDPP_Step_SpecVec)
    SDPP_Step_List$SpecVec_Flag <- SDPP_Step_SpecVec
  }else{
    fprintf("|SDPP Batch| SDPP_Step_SpecVec is null, exectuing all avaiable steps as default.")
    SDPP_Step_List$SpecVec_Flag <- T
    SDPP_Step_List$SpecVec_FuncName <- sprintf("SDPP.Run.Step%d",1:nrow(SDPP_Step_List))
  }
  fprintf("|SDPP Batch| %d SDPP Steps will be executed.\n",sum(SDPP_Step_List$SpecVec_Flag))
  
  
  SDPP_Step_List$Command <- NA
  for (istep in 1:nrow(SDPP_Step_List)){
    if (SDPP_Step_List$SpecVec_Flag[istep]){
      fprintf("|SDPP Batch| +Run+ SDPP Step [%d], source file: [%s], execute function: [%s]\n",
              SDPP_Step_List$StepNum[istep],
              SDPP_Step_List$R_Script[istep],
              SDPP_Step_List$SpecVec_FuncName[istep])
      source(file = SDPP_Step_List$R_Script[istep])
      fprintf("|SDPP Batch| +Run+ SDPP Step [%d], sourced! %s\n",
              SDPP_Step_List$StepNum[istep],
              Sys.time())
      fprintf("|SDPP Batch| +Run+ SDPP Step [%d] will be executed soon, pause Batch logging. %s\n",
              SDPP_Step_List$StepNum[istep],
              Sys.time())
      sink()
      
      Exectued_R_Arguments <- sprintf(
        "(Prefix = '%s',
        TabulatedDataDirectory = '%s',
        ProjectDirectory = '%s',
        AutoLogFolder = '%s',
        ResultsOutputDir = '%s',
        IntermediateDataDir = '%s',
        SourceScriptName = '%s',
       ...)",
        Prefix,
        TabulatedDataDirectory,
        ProjectDirectory,
        AutoLogFolder,
        ResultsOutputDir,
        IntermediateDataDir,
        SDPP_Step_List$R_Script[istep]) %>%
        str_squish()
      
      Exectued_R_Command <- eval_s(
        str_c(SDPP_Step_List$SpecVec_FuncName[istep],
              Exectued_R_Arguments),
        return.res = F)
      
      Exectued_R_Command <- str_squish(Exectued_R_Command)
      
      fprintf("|SDPP Batch| +Run+ SDPP Step [%d], R command: %s\n",
              SDPP_Step_List$StepNum[istep],
              Exectued_R_Command)
      fprintf("|SDPP Batch| +Run+ SDPP Step [%d], finished! Continue Batch Logging. %s\n",
              SDPP_Step_List$StepNum[istep],
              Sys.time())
      
      s_sink(fullfile(AutoLogFolder,BatchLogFileName),pause_flag = T)
      SDPP_Step_List$Command[istep] <- Exectued_R_Command
    }else{
      fprintf("|SDPP Batch| -Skip- SDPP Step [%d], domain: %s-%s, Step Type: %s\n",
              SDPP_Step_List$StepNum[istep],
              SDPP_Step_List$Domain[istep],SDPP_Step_List$SubDomain[istep],
              SDPP_Step_List$StepType[istep])
      next
    }
  }
  fprintf("|SDPP Batch| Done! %s", Sys.time())
  
  BatchSpecFileName <- fullfile(AutoLogFolder,
                                sprintf('Log_SDPP_Batch_Specification_%s.xlsx',DateTime))
  
  fprintf("|SDPP Batch| Saveing batch specification into %s \n",BatchSpecFileName)
  
  export(SDPP_Step_List,
         file = BatchSpecFileName,
         sheet = "BatchSpec",
         verbose = T)
  
  sink()
}


