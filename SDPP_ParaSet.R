rm(list = ls())
gc()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
# SDPP Overall Parameter Settings -----------------------------------------
TabulatedDataDirectory = '../../ABCD_V5.0/core/'
# Please replace the above string for your own downloaded data directory.
# Relative Path is required! (relative to the path of the current R script file)

ProjectDirectory = '../DataAnalysis/SMA_Trajectory'
Prefix = 'ABCD5.0'


# Prepare Environment -----------------------------------------------------

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('SDPP_subfunctions.R')
# Create Project Folders and get the log-file directory
AutoLogFolder = SDPP.ABCD.TabDat.PrepareProject(ProjectDirectory)
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_0.txt'
AutoLogFilePath = fullfile(AutoLogFolder,AutoLogFileName)
sink(file = AutoLogFilePath)
# Check the required packages ---------------------------------------------
pkg_ls = c("readxl","bruceR","forcats","mice","svDialogs","naniar","R.matlab","Hmisc","tidyverse")

SDPP.check.package <- function(package_name){
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
  return(res)
}
res <- SDPP.check.package(pkg_ls)
if (any(res$status %in% "Not Installed!")){
  cat("Some required R packageds have not been installed! Please see the table below:\n")
  print(res)
  cat("SDPP-ABCD-TabDat rely on these packages, please make sure you have installed all required R packages!\n")
  stop("SDPP-ABCD-TabDat Check Required Packages: Failed! See above information.")
}else{
  cat("SDPP-ABCD-TabDat Check Required Packages: Passed!\n")
  cat("All required R packages have been installed: \n")
  print(res)
}
library(bruceR)




# SDPP Step Parameters Setting --------------------------------------------
# Step 3 Parameters
n.imp = 100 # Number of multiple imputed datasets
n.iter = 25 # maximum number of iterations 
S3_ResultsOutputDir = SDPP.set.output(
  fullfile(ProjectDirectory,
           'Res_2_Results',
           'Res_Preproc')) # Result Output Directory
IntermediateDataDir = fullfile(ProjectDirectory,'Res_3_IntermediateData')

# ==============================DO NOT CHANGE!=====================================#

# Auto-log ----------------------------------------------------------------

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
fprintf("=================================Step 8 Specifications=================================\n")
S8_ResultsOutputDir = S7_ResultsOutputDir
fprintf("Step 8 Results Output Directory will inherit from Step 7:%s \n",S7_ResultsOutputDir)
fprintf("=================================Step 9 Specifications=================================\n")
S9_ResultsOutputDir = S8_ResultsOutputDir
fprintf("Step 9 Results Output Directory will inherit from Step 8:%s \n",S9_ResultsOutputDir)


# ==============================DO NOT CHANGE!=====================================#

# End of Script -----------------------------------------------------------
fprintf("SDPP-ParaSet finished! Finish Time:%s\n",Sys.time())
sink()
