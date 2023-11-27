rm(list = ls())
gc()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# SDPP Overall Parameter Settings -----------------------------------------
TabulatedDataDirectory = '../../ABCD_V5.0/core/'
# Please replace the above string for your own downloaded data directory.
# Relative Path is required! (relative to the path of the current R script file)
ProjectDirectory = '../DataAnalysis/SMA_Trajectory'
# Please replace the above string for your own data analysis project directory
Prefix = 'ABCD5.0'
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_0.txt'

# Prepare Environment -----------------------------------------------------
library(bruceR)
source('SDPP_subfunctions.R')
# Create Project Folders and get the log-file directory
AutoLogFolder = SDPP.ABCD.TabDat.PrepareProject(ProjectDirectory)
s_sink(fullfile(AutoLogFolder,AutoLogFileName))
# SDPP Step Parameters Setting --------------------------------------------
# Step 3 Parameters
n.imp = 100 # Number of multiple imputed datasets
n.iter = 25 # maximum number of iterations 

# ==============================DO NOT CHANGE!=====================================#
SDPP.Initialize(ProjectDirectory)
IntermediateDataDir = SDPP.set.output(fullfile(ProjectDirectory,'Res_3_IntermediateData'))
ResultsOutputDir = SDPP.set.output(fullfile(ProjectDirectory,'Res_2_Results','Res_Preproc'))
fprintf("=================================Step 3 Specifications=================================\n")
fprintf('Number of Multiple Imputation Replicates: %d\n',n.imp)
fprintf('Number of Maximum Iterations in Multiple Imputation: %d\n',n.iter)


# ==============================DO NOT CHANGE!=====================================#
# End of Script -----------------------------------------------------------
fprintf("SDPP-ParaSet finished! Finish Time:%s\n",Sys.time())
sink()
