# 1. SDPP_ParaSet Argument Settings --------------------------------------------
# 1.1 Specify the directory of downloaded ABCD tabulated data files
TabulatedDataDirectory = '../../ABCD_V5.1/core/'
# Please replace it with your own directory, where you store all ABCD tabulated
# data files (.csv files).
# Notes: 1. Relative Path is required! 
#        2. Must follow the raw folder structure from the NDA platform.

# 1.2 Specify the directory of secondary data analysis project
ProjectDirectory = '../DataAnalysis/SMA_Trajectory'
# Please replace the above string for your own data analysis project directory.
# Notes: 1. If it doesn't exist, a data analysis project template will be
# created automatically.

# 1.3 Specify the prefix of output intermediate data files
Prefix = 'ABCD5.1'

# 2 SDPP_Batch Argument Settings -----------------------------------------------
# 2.1 (Important!) Specify the SDPP steps to be execute -------------------
SDPP_Step_SpecVec = c(
  "SDPP.Run.Step1" =  F,
  "SDPP.Run.Step2" =  F,
  "SDPP.Run.Step3" =  T,
  "SDPP.Run.Step4" =  T,
  "SDPP.Run.Step5" =  T,
  "SDPP.Run.Step6" =  T,
  "SDPP.Run.Step7" =  T,
  "SDPP.Run.Step8" =  T,
  "SDPP.Run.Step9" =  T,
  "SDPP.Run.Step10" = T,
  "SDPP.Run.Step11" = T,
  "SDPP.Run.Step12" = T,
  "SDPP.Run.Step13" = T,
  "SDPP.Run.Step14" = T,
  "SDPP.Run.Step15" = F
)

# 2.2 User-controlled logical flag for running SDPP_Batch or not -----------
Flag_RunBatch <- T

# 3. SDPP Steps Parameters Setting ---------------------------------------------
# 3.1 Step 3 Parameters
n.imp <- 500 # Number of multiple imputed datasets
n.iter <- 25 # maximum number of iterations 


# 4. SDPP Concat Argument Settings ---------------------------------------------

# 4.1 Intermediate data files to be concatenated --------------------------
FileLabelList = c("Demographics_Imp",
                  "MH_P_CBCL_Rec",
                  "MH_S_Rec",
                  "NC_NIHTB",
                  "NC_Non-NIHTB",
                  "NC_tfMRI_behav",
                  "CE_Y_Rec",
                  "SMA_Rec_Imp")

# 4.2 File type of all intermediate data files ----------------------------
FileType = "rds"
OutputFileName = "Merge_Demo_NC_MH_CE_SMA"

# 4.3 Variable names in Baseline Observation Carry Forward (BOCF) ---------
BOCF_VarList = c("SexAssigned",
                 "GroupID",
                 "Race_6L","Ethnicity_PrntRep",
                 "Handedness",
                 "ParentsMarital_2L","ParentsHighEdu_2L","ParentsMarital_X_Employ",
                 "Relationship_3L",
                 "AdoptionChild",
                 "GenderIdentity_PrntRep",
                 "RaceEthnicity",
                 "Relationship",
                 "Religon_PrntRep",
                 "AdoptionAge",
                 "USALiveYears",
                 "GenderIdentity_PrntSelf",
                 "Race_PrntSelf",
                 "ParentHighEdu","PartnerHighEdu",
                 "ParentEmploy","PartnerEmploy","FamilyNumInLF",
                 "SameSexTwin",
                 "GI_PairedSubID_Sub_1","GI_PairProb_Sub_1","GI_Zygosity_Sub_1",
                 "GI_PairedSubID_Sub_2","GI_PairProb_Sub_2","GI_Zygosity_Sub_2",
                 "GI_PairedSubID_Sub_3","GI_PairProb_Sub_3","GI_Zygosity_Sub_3",
                 "GI_PairedSubID_Sub_4","GI_PairProb_Sub_4","GI_Zygosity_Sub_4",
                 "GI_AncestryPC_1","GI_AncestryPC_2","GI_AncestryPC_3",
                 "GI_AncestryPC_4","GI_AncestryPC_5","BirthID",
                 "Education_R",
                 "Race_PrntRep","ParentsMarital_6L","Race_4L",
                 "Religon_8L",
                 "FamilyIncome",
                 "YouthNativeLang",
                 "Religon_2L",
                 "HouseholdSize","HouseholdStructure",
                 "BirthCountry","ParentsHighEdu_5L")

# 4.4 User-controlled logical flag for running SDPP_Concat or not ----------
Flag_RunConcat <- T