rm(list = ls())
gc()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
bruceR::set.wd()
# SDPP Overall Parameter Settings -----------------------------------------
# 1. Specify the directory of downloaded ABCD tabulated data files
TabulatedDataDirectory = '../../ABCD_V5.1/core/'
# Please replace the "TabulatedDataDirectory" for your own downloaded data directory.
# Notes: Relative Path is required! (relative to the path of the current R script file)
# 2. Specify the directory of secondary data analysis project
# ProjectDirectory = '../DataAnalysis/Tmp_Trajectory' # only for example
ProjectDirectory = '../DataAnalysis/SMA_Trajectory'
# Please replace the above string for your own data analysis project directory
# 3. Specify the prefix of output intermediate data files
Prefix = 'ABCD5.1'
# 4. (Important!) Specify the SDPP steps that are required to be execute
SDPP_Step_SpecVec = c(
  "SDPP.Run.Step1" = T,
  "SDPP.Run.Step2" = F,
  "SDPP.Run.Step3" = F,
  "SDPP.Run.Step4" = F,
  "SDPP.Run.Step5" = F,
  "SDPP.Run.Step6" = F,
  "SDPP.Run.Step7" = F,
  "SDPP.Run.Step8" = F,
  "SDPP.Run.Step9" = F,
  "SDPP.Run.Step10" = F,
  "SDPP.Run.Step11" = F,
  "SDPP.Run.Step12" = F,
  "SDPP.Run.Step13" = F,
  "SDPP.Run.Step14" = F,
  "SDPP.Run.Step15" = F
)
# ==============================DO NOT CHANGE!=====================================#

# Prepare Environment -----------------------------------------------------
library(bruceR)
source('SDPP_subfunctions.R')
# Create Project Folders and get the log-file directory
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_0.txt'
AutoLogFolder <- SDPP.ABCD.TabDat.PrepareProject(ProjectDirectory,verbose = F)
s_sink(fullfile(AutoLogFolder,AutoLogFileName))
# SDPP Step Parameters Setting --------------------------------------------
# Step 3 Parameters
n.imp <- 1000 # Number of multiple imputed datasets
n.iter <- 25 # maximum number of iterations 

# Initialize SDPP ---------------------------------------------------------
OutputDirectory <- SDPP.Initialize(ProjectDirectory,TabulatedDataDirectory,Prefix)
IntermediateDataDir <- OutputDirectory$IntermediateDataDir
ResultsOutputDir <- OutputDirectory$ResultsOutputDir
AutoLogFolder <- OutputDirectory$AutoLogFolder
rm(OutputDirectory)
fprintf("=================================Step 3 Specifications=================================\n")
fprintf('Number of Multiple Imputation Replicates: %d\n',n.imp)
fprintf('Number of Maximum Iterations in Multiple Imputation: %d\n',n.iter)


# ==============================DO NOT CHANGE!=====================================#
# End of Script -----------------------------------------------------------
fprintf("SDPP-ParaSet finished! Finish Time:%s\n",Sys.time())
sink()
rm(AutoLogFileName)
