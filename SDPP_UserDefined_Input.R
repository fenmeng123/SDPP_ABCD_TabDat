# 1. SDPP_ParaSet Argument Settings --------------------------------------------
# 1.1 Specify the directory of downloaded ABCD tabulated data files
TabulatedDataDirectory = '../../ABCD_V5.1/core/'
# Please replace it with your own directory, where you store all ABCD tabulated
# data files (.csv files).
# Notes: 1. Relative Path is required! 
#        2. Must follow the raw folder structure from the NDA platform.

# 1.2 Specify the directory of secondary data analysis project
ProjectDirectory = 'D:/PhDThesis_DataAnalysis_KunruSong/RP1/Res_3_IntermediateData/ABCD5.1_TabulatedData'
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
  "SDPP.Run.Step3" =  F,
  "SDPP.Run.Step4" =  F,
  "SDPP.Run.Step5" =  F,
  "SDPP.Run.Step6" =  F,
  "SDPP.Run.Step7" =  F,
  "SDPP.Run.Step8" =  F,
  "SDPP.Run.Step9" =  F,
  "SDPP.Run.Step10" = F,
  "SDPP.Run.Step11" = F,
  "SDPP.Run.Step12" = F,
  "SDPP.Run.Step13" = F,
  "SDPP.Run.Step14" = F,
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
                  "SMA_Rec_Imp",
                  "MH_P_CBCL_Rec",
                  "MH_S_Rec",
                  "NC_NIHTB",
                  "NC_Non-NIHTB",
                  "NC_tfMRI_behav",
                  "CE_Y_Rec",
                  "MRI_InfoQC",
                  "sMRI_Desikan",
                  "sMRI_Destrieux",
                  "sMRI_ASEG"
                  )

# 4.2 File type of all intermediate data files ----------------------------
FileType = "rds"
OutputFileName = "Merge_Demo_SMA_NC_MH_CE_sMRI"

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