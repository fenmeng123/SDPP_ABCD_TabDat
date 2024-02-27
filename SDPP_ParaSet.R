rm(list = ls())
gc()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
bruceR::set.wd()
#==============================DO NOT CHANGE!==================================#
# Prepare Environment -----------------------------------------------------
library(bruceR)
source('SDPP_subfunctions.R')
source('SDPP_Batch.R')
source('SDPP_Concat.R')
source('SDPP_UserDefined_Input.R')
# Create Project Folders and get the log-file directory
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_0.txt'
AutoLogFolder <- SDPP.ABCD.TabDat.PrepareProject(ProjectDirectory,verbose = F)
s_sink(fullfile(AutoLogFolder,AutoLogFileName))

# Initialize SDPP ---------------------------------------------------------
OutputDirectory <- SDPP.Initialize(ProjectDirectory,TabulatedDataDirectory,Prefix)
IntermediateDataDir <- OutputDirectory$IntermediateDataDir
ResultsOutputDir <- OutputDirectory$ResultsOutputDir
AutoLogFolder <- OutputDirectory$AutoLogFolder
rm(OutputDirectory)
fprintf("-----------------------------SDPP-ABCD-TabDat Step-specific Parameters---------------------------------\n")
fprintf("|SDPP ParaSet| Step 3 Parameter Settings\n")
fprintf('\t Number of Multiple Imputation Replicates: %d\n',n.imp)
fprintf('\t Number of Maximum Iterations in Multiple Imputation: %d\n',n.iter)
fprintf("\n")
# End of Script -----------------------------------------------------------
rm(AutoLogFileName)

if (Flag_RunBatch){
  fprintf("|SDPP ParaSet| SDPP Batch will start soon......\n")
  if (Flag_RunConcat)
    fprintf("|SDPP ParaSet| SDPP Concat will be executed after finishing SDPP Batch.\n")
  else{
    fprintf("|SDPP ParaSet| SDPP Concat will be ignored!\n")
  }
}else{
  fprintf("|SDPP ParaSet| Dry-run finished, you can check all specifications and parameters in your R environment!\n")
  if (Flag_RunConcat){
    fprintf("|SDPP ParaSet| SDPP Concat will be executed directly, based on the existing intermediate data files.\n")
    fprintf("|SDPP ParaSet| Please make sure all required intermediate data have been created, see details in SDPP Concat's log.\n")
  }else{
    fprintf("|SDPP ParaSet| SDPP Concat will be ignored!\n")
  }
}

fprintf("|SDPP ParaSet| finished! Finish Time:%s\n",Sys.time())
sink()

if (Flag_RunBatch){
  SDPP.RunBatch(SDPP_Step_SpecVec = SDPP_Step_SpecVec,
                TabulatedDataDirectory = TabulatedDataDirectory,
                ProjectDirectory = ProjectDirectory,
                AutoLogFolder = AutoLogFolder,
                ResultsOutputDir = ResultsOutputDir,
                IntermediateDataDir = IntermediateDataDir,
                Prefix = Prefix,
                n.imp,
                n.iter)
}

if (Flag_RunConcat){
  SDPP.RunConcat(FileLabelList = FileLabelList,
                 OutputFileName = OutputFileName,
                 FileType = FileType,
                 BOCF_VarList = BOCF_VarList,
                 TabulatedDataDirectory = TabulatedDataDirectory,
                 ProjectDirectory = ProjectDirectory,
                 AutoLogFolder = AutoLogFolder,
                 ResultsOutputDir = ResultsOutputDir,
                 IntermediateDataDir = IntermediateDataDir,
                 Prefix = Prefix)
}