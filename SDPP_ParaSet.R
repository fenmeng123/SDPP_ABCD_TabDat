# SDPP-ABCD-TabDat
# 
# The core script for executing preprocessing pipeline
# 
# Author: Kunru Song 2024.0
# 
# After appropirately set all variables in the "SDPP_UserDefine_Input.R", you
# can simply run "source('SDPP_ParaSet.R')". Then, all specified preprocessing
# steps will be automatically executed. The pre-processed tabulated data will
# be output to the folder "Res_3_IntermediateData" in your specified "ProjectDirectory"
# 
# When debugging is required, run the following code in R console:
# "source('SDPP_DebugTool.R')", all dependices and environment variables will
# be set appropriately in R Environment.

#==============================DO NOT CHANGE!==================================#
# 0. Clean and re-start the R session ----------------------------------------
rm(list = ls())
gc()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
rstudioapi::restartSession(command = "bruceR::set.wd()",
                           clean = TRUE)

# 1. Prepare Environment -----------------------------------------------------
library(bruceR)
source('SDPP_subfunctions.R')
source('SDPP_Batch.R')
source('SDPP_Concat.R')
source('SDPP_UserDefined_Input.R')
# Create Project Folders and get the log-file directory
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_0.txt'
AutoLogFolder <- SDPP.ABCD.TabDat.PrepareProject(ProjectDirectory,verbose = F)
s_sink(fullfile(AutoLogFolder,AutoLogFileName))
rm(AutoLogFileName) # remove this variable to prevent global variable settings

# 2. Initialize SDPP ---------------------------------------------------------
# Step 1: Create the data analysis project directory
OutputDirectory <- SDPP.Initialize(ProjectDirectory,TabulatedDataDirectory,Prefix)
# Step 2: Get the directory for saving intermediate data (i.e., the output for SDPP-ABCD-TabDat)
IntermediateDataDir <- OutputDirectory$IntermediateDataDir
# Step 3: Get the directory for outputting results of preprocessing
# which contains Missing Value Report (MVA) and Variable Summary Overview (VSO)
ResultsOutputDir <- OutputDirectory$ResultsOutputDir
# Step 4: Get the directory for automatic logging
AutoLogFolder <- OutputDirectory$AutoLogFolder
# Step 5: Remove the redundant `OutputDirectory`, all elements have been extracted.
rm(OutputDirectory)

# 3. Print the step-specific parameter settings ---------------------------
fprintf("---------------------SDPP-ABCD-TabDat Step-specific Parameters---------------------\n")
fprintf("|SDPP ParaSet| Step 3 Parameter Settings\n")
fprintf('\t Number of Multiple Imputation Replicates: %d\n',n.imp)
fprintf('\t Number of Maximum Iterations in Multiple Imputation: %d\n',n.iter)
fprintf("\n")

# 4. SDPP_ParaSet-related execution -----------------------------------------
fprintf("-----------------------------SDPP Batch Infomation---------------------------------\n")
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
sink() # Stop the SDPP_Paraset logging, because each SDPP step will log itself in an independent text file.
# 5. SDPP_Batch-related execution -----------------------------------------

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

# 6. SDPP_Concat-related execution -----------------------------------------

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
# End of Script -----------------------------------------------------------