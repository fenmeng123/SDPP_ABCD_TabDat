rm(list = ls())
gc()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
bruceR::set.wd()
#==============================DO NOT CHANGE!==================================#
# Prepare Environment -----------------------------------------------------
library(bruceR)
source('SDPP_subfunctions.R')
source('SDPP_Batch.R')
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
  fprintf("|SDPP ParaSet| finished! Finish Time:%s\n",Sys.time())
  sink()
  SDPP.RunBatch(SDPP_Step_SpecVec = SDPP_Step_SpecVec,
                TabulatedDataDirectory = TabulatedDataDirectory,
                ProjectDirectory = ProjectDirectory,
                AutoLogFolder = AutoLogFolder,
                ResultsOutputDir = ResultsOutputDir,
                IntermediateDataDir = IntermediateDataDir,
                Prefix = Prefix,
                n.imp,
                n.iter)
}else{
  fprintf("|SDPP ParaSet| Dry-run finished, you can check all specifications and parameters in your R environment!\n")
  fprintf("|SDPP ParaSet| finished! Finish Time:%s\n",Sys.time())
  sink()
}
