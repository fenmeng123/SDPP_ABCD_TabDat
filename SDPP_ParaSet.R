rm(list = ls())
gc()
# Prepare Environment -----------------------------------------------------
library(bruceR)
set.wd()
source('SDPP_subfunctions.R')
# SDPP Parameter Settings -------------------------------------------------
TabulatedDataDirectory = '../../ABCD_V5.0/core/'
# Please replace the above string for your own downloaded data directory.
# Relative Path is required! (relative to the path of the current R script file)
ProjectDirectory = '../DataAnalysis/SMA_Trajectory'
Prefix = 'ABCD5.0'
IntermediateDataDir = fullfile(ProjectDirectory,'Res_3_IntermediateData')
# Auto-log ----------------------------------------------------------------
# Create Project Folders and get the log-file directory
AutoLogFolder = SDPP.ABCD.TabDat.PrepareProject(ProjectDirectory)
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_0.txt'
AutoLogFilePath = fullfile(AutoLogFolder,AutoLogFileName)
sink(file = AutoLogFilePath)
fprintf('Starting SDPP-ABCD-TabDat......\n')
fprintf('R package: bruceR is a common-used package in this pipeline, which would be included in all steps.\n')
basic.info = sessionInfo()
other.info = Sys.info()
fprintf("===============================SDPP-ABCD-TabDat Settings===============================\n")
fprintf("Scripts Executing Date: %s\n",Sys.time())
fprintf("Running under OS: %s\n",basic.info$running)
fprintf("Platform: %s\n",basic.info$platform)
fprintf("R Version: %s\n",basic.info$R.version$version.string)
fprintf("Running on Computer Name: %s\n",other.info['nodename'])
fprintf("Working Directory: %s\n",getwd())
fprintf("Project Directory: %s\n",fullfile(ProjectDirectory))
fprintf("Downloaded NDA Data Directory: %s\n",fullfile(TabulatedDataDirectory))
fprintf("Output Intermediate Data File Prefix: %s\n",Prefix)
fprintf("Output Intermediate Data Directory: %s\n",IntermediateDataDir)
fprintf("=======================================================================================\n")
# 1. Step 1-specification -------------------------------------------------
fprintf("=================================Step 1 Specifications=================================\n")
fprintf('Nothing\n')
# 2. Step 2-Specification -------------------------------------------------
fprintf("=================================Step 2 Specifications=================================\n")
fprintf('Nothing\n')
# 3. Step 3-specification -------------------------------------------------
fprintf("=================================Step 3 Specifications=================================\n")
ResultsOutputDir = fullfile(ProjectDirectory,'Res_2_Results','Res_Preproc')
if (!dir.exists(ResultsOutputDir)){
  fprintf("Output Directory:%s not found, a new folder will be created.",ResultsOutputDir)
  dir.create(ResultsOutputDir)
}
# Number of multiple imputed datasets & maximum number of iterations 
n.imp = 5
n.iter = 50
fprintf("Step 3 Results Output Directory: %s\n",ResultsOutputDir)
fprintf('Number of Multiple Imputation Replicates: %d\n',n.imp)
fprintf('Number of Maximum Iterations in Multiple Imputation: %d',n.iter)


# End of Script -----------------------------------------------------------
sink()
