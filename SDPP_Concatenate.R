library(tidyverse)
library(stringr)
library(naniar)

dat.to.be.merged = c("Demographics_Imp","MH_P_CBCL_Rec",
                     "MH_S_Rec","NC_NIHTB","NC_Non-NIHTB",
                     "SMA_Rec_Imp")
file.postfix = "rds"
output.file.name = "Merge_Demo_SMA_NC_MH"


# ==============================DO NOT CHANGE!==================================
AutoLogFileName = sprintf('Log_SDPP-ABCD-TabDat_Concatenate_%s.txt',
                          str_trunc(str_replace_all(as.character(Sys.time()),"( )|(:)|([.])|(-)","_"),
                                    width = 21,
                                    ellipsis = ""))
AutoLogFilePath = fullfile(ProjectDirectory,'Res_1_Logs',AutoLogFileName)
sink(file = AutoLogFilePath)


dat.file.ls = sprintf("%s_%s.%s",Prefix,dat.to.be.merged,file.postfix)
preprocessed.dat.ls = list()
iter_num = 1
for (i in dat.file.ls){
  dat.path = fullfile(IntermediateDataDir,i)
  fprintf("Load data from RDS file: %s\n",dat.path)
  preprocessed.dat.ls[[iter_num]] = readRDS(dat.path)
  iter_num = iter_num + 1
}

preprocessed.dat.ls %>% reduce(full_join,
                           by = c("src_subject_id","eventname")) -> concatenated_data
fprintf("Concatenate %s data... Finished!\n",paste(dat.to.be.merged,collapse = ", "))
# load ABCD longitudinal tractor and fill NA blank in concatenated_data
lt = readABCDdata(fullfile(TabulatedDataDirectory,'/abcd-general/abcd_y_lt.csv'))

Flag = which(is.na(concatenated_data$interview_age) | is.na(concatenated_data$interview_date))
time.invariant.covs.ls = c("SexAssigned","GroupID","Race_6L",
                           "Ethnicity_PrntRep",
                           "Handedness","ParentsMarital_2L","ParentsHighEdu_2L",
                           "ParentsMarital_X_Employ","Relationship_3L","AdoptionChild",
                           "GenderIdentity_PrntRep","RaceEthnicity","Relationship",
                           "Religon_PrntRep","AdoptionAge",
                           "USALiveYears","GenderIdentity_PrntSelf",
                           "Race_PrntSelf","ParentHighEdu","PartnerHighEdu",
                           "PartnerEmploy","FamilyNumInLF","SameSexTwin",
                           "GI_PairedSubID_Sub_1","GI_PairProb_Sub_1","GI_Zygosity_Sub_1",
                           "GI_PairedSubID_Sub_2","GI_PairProb_Sub_2","GI_Zygosity_Sub_2",
                           "GI_PairedSubID_Sub_3","GI_PairProb_Sub_3","GI_Zygosity_Sub_3",
                           "GI_PairedSubID_Sub_4","GI_PairProb_Sub_4","GI_Zygosity_Sub_4",
                           "GI_AncestryPC_1","GI_AncestryPC_2","GI_AncestryPC_3",
                           "GI_AncestryPC_4","GI_AncestryPC_5","BirthID",
                           "Education_R","Race_4L","Religon_8L",
                           "FamilyIncome","YouthNativeLang","Religon_2L",
                           "HouseholdSize","Race_PrntRep","ParentsMarital_6L",
                           "HouseholdStructure","ParentEmploy",
                           "BirthCountry","ParentsHighEdu_5L")


for (i in Flag){
  fprintf("Fill blank for Subject: %s [Wave: %s] (from ABCD Longitudinal Tractor)...\n",
          concatenated_data$src_subject_id[i],
          concatenated_data$eventname[i])
  SingleSubMask = (lt$src_subject_id == concatenated_data$src_subject_id[i]) & 
    (lt$eventname == concatenated_data$eventname[i])
  concatenated_data$interview_age[i] = lt$interview_age[SingleSubMask]
  concatenated_data$interview_date[i] = lt$interview_date[SingleSubMask]
  concatenated_data$VisitType[i] = RECODE(lt$visit_type[SingleSubMask],
                                          "1 = 'On-site';
                                          2 = 'Remote';
                                          3 = 'Hybrid';")
  concatenated_data$SiteID[i] = lt$site_id_l[SingleSubMask]
  concatenated_data$FamilyID[i] = lt$rel_family_id[SingleSubMask]
  concatenated_data$SchoolID[i] = lt$school_id[SingleSubMask]
  concatenated_data$DistrictID[i] = lt$district_id[SingleSubMask]
  concatenated_data$BirthID[i] = lt$rel_birth_id[SingleSubMask]
  fprintf("\t\t\t interview_age, date, VisitType, SiteID, FamilyID, SchoolID, DistrictID, BirthID were filled.\n")
  if (sum(is.na(concatenated_data[i,time.invariant.covs.ls])) > 16 ){
    fprintf("\t\t\t BOCF for Subject: %s [Wave: %s] from baseline_year_1_arm_1... Finished!\n",
            concatenated_data$src_subject_id[i],
            concatenated_data$eventname[i])
    tmpT0Flag = (concatenated_data$src_subject_id == concatenated_data$src_subject_id[i]) &
      (concatenated_data$eventname == "baseline_year_1_arm_1")
    concatenated_data[i,time.invariant.covs.ls] = concatenated_data[which(tmpT0Flag),time.invariant.covs.ls]
  }else{
    fprintf("\t\t\t Subject: %s [Wave: %s]: The number of missing values in demographics\n\t\t\t did not reach the threshold. Do not need to run BOCF.\n",
            concatenated_data$src_subject_id[i],
            concatenated_data$eventname[i])
  }
}

SDPP.save.file(concatenated_data,
               FileName = sprintf("%s.rds",output.file.name),
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)
SDPP.save.file(concatenated_data,
               FileName = sprintf("%s.csv",output.file.name),
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

concatenated_data %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(S3_ResultsOutputDir,
                              sprintf("MVA_Report_ALL_%s.doc",output.file.name)),
              row.names = F,
              nsmalls = 1)
# End of Script -----------------------------------------------------------

fprintf("SDPP-ABCD-TabDat Concatenate finished! Finish Time:%s\n",Sys.time())

sink()
