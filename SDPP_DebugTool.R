# SDPP-ABCD-TabDat
# 
# A simple script for developing and debugging.
# 
# Author: Kunru Song 2024.02.27
# 
# When debugging is required, run the following code in R console:"
# source('SDPP_DebugTool.R')
# ", all dependices and environment variables will be set appropriately in R Environment.
# 



# DO NOT CHANGE  ----------------------------------------------------------


TabulatedDataDirectory = '../../ABCD_V5.1/core/'
ProjectDirectory = '../DataAnalysis/SMA_Trajectory'
Prefix = 'ABCD5.1'
n.imp <- 500 # Number of multiple imputed datasets
n.iter <- 25 # maximum number of iterations 
rm(list = ls())
gc()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
bruceR::set.wd()
library(bruceR)
source('SDPP_subfunctions.R')
source('SDPP_Batch.R')
source('SDPP_Concat.R')
source('SDPP_UserDefined_Input.R')