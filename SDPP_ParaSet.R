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
# Step 3 Parameters
n.imp = 100 # Number of multiple imputed datasets
n.iter = 25 # maximum number of iterations 
S3_ResultsOutputDir = SDPP.set.output(
  fullfile(ProjectDirectory,
           'Res_2_Results',
           'Res_Preproc')) # Result Ouput Directory

# ==============================DO NOT CHANGE!=====================================#
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
rm(basic.info,other.info)

fprintf("=================================Step 1 Specifications=================================\n")
fprintf('Nothing\n')

fprintf("=================================Step 2 Specifications=================================\n")
fprintf('Nothing\n')

fprintf("=================================Step 3 Specifications=================================\n")
fprintf("Step 3 Results Output Directory: %s\n",S3_ResultsOutputDir)
fprintf('Number of Multiple Imputation Replicates: %d\n',n.imp)
fprintf('Number of Maximum Iterations in Multiple Imputation: %d\n',n.iter)

fprintf("=================================Step 4 Specifications=================================\n")
S4_ResultsOutputDir = S3_ResultsOutputDir
fprintf("Step 4 Results Output Directory will inherit from Step 3:%s \n",S4_ResultsOutputDir)
fprintf("=================================Step 5 Specifications=================================\n")
S5_ResultsOutputDir = S4_ResultsOutputDir
fprintf("Step 5 Results Output Directory will inherit from Step 4:%s \n",S5_ResultsOutputDir)
fprintf("=================================Step 6 Specifications=================================\n")
S6_ResultsOutputDir = S5_ResultsOutputDir
fprintf("Step 6 Results Output Directory will inherit from Step 5:%s \n",S6_ResultsOutputDir)
fprintf("=================================Step 7 Specifications=================================\n")
S7_ResultsOutputDir = S6_ResultsOutputDir
fprintf("Step 7 Results Output Directory will inherit from Step 6:%s \n",S7_ResultsOutputDir)
fprintf("=================================Step 7 Specifications=================================\n")
S8_ResultsOutputDir = S7_ResultsOutputDir
fprintf("Step 8 Results Output Directory will inherit from Step 7:%s \n",S7_ResultsOutputDir)



# ==============================DO NOT CHANGE!=====================================#

# End of Script -----------------------------------------------------------
fprintf("SDPP-ParaSet finished! Finish Time:%s\n",Sys.time())
sink()
