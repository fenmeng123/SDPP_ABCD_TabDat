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
  "SDPP.Run.Step2" = T,
  "SDPP.Run.Step3" = T,
  "SDPP.Run.Step4" = T,
  "SDPP.Run.Step5" = T,
  "SDPP.Run.Step6" = T,
  "SDPP.Run.Step7" = T,
  "SDPP.Run.Step8" = T,
  "SDPP.Run.Step9" = T,
  "SDPP.Run.Step10" = T,
  "SDPP.Run.Step11" = T,
  "SDPP.Run.Step12" = T,
  "SDPP.Run.Step13" = T,
  "SDPP.Run.Step14" = T,
  "SDPP.Run.Step15" = F
)

# 5. User-controlled logical flag for running SDPP_Batch or not -----------
Flag_RunBatch <- T

# 6. SDPP Step Parameters Setting -----------------------------------------
# Step 3 Parameters
n.imp <- 5 # Number of multiple imputed datasets
n.iter <- 5 # maximum number of iterations 