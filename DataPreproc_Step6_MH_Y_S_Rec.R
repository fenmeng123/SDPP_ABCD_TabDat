# =============================================================================#
# SDPP Step 6: Read and re-coding the mental health summary data
# R Packages Dependency: bruceR, naniar, readxl
# Step File Notes: 
# 
# Target File: 
# ABCD 4.0 : abcd_mhy02.txt
# ABCD 5.0: /core/mental-health/mh_y_upps.csv, mh_y_pps.csv, mh_y_peq.csv, 
#           mh_y_le.csv, mh_y_erq.csv, mh_y_bisbas.csv, mh_y_7up.csv
# Update Date: 2023.7.10
#              2023.7.17
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_6.txt'
s_sink(fullfile(AutoLogFolder,AutoLogFileName))
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load Youth Mental Health Summary and select the relevant columns ------------------

file_ls = SDPP.filter.data.dict(DataDictionaryFileDir = dir(pattern = '.*ABCD Data Dictionary.xlsx'),
                                filter_col = "table_name_nda",
                                filter_key = "abcd_mhy02",
                                search_col = "table_name")
file_ls = unique(file_ls)
file_ls = paste(file_ls,"csv",sep = '.')
MH = read.in.batch(DownloadedDataDir = TabulatedDataDirectory,
              TableNames = file_ls,
              FolderName = 'mental-health')
var_ls = SDPP.filter.data.dict(DataDictionaryFileDir = dir(pattern = '.*ABCD Data Dictionary.xlsx'),
                               filter_col = "table_name_nda",
                               filter_key = "abcd_mhy02",
                               search_col = "var_name")
var_ls = append(var_ls,c('eventname','src_subject_id'))

MH[,var_ls] %>% MVA.Report.By.Wave() %>% 
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_MH_S_Raw.doc'),
              row.names = F,
              nsmalls = 1)
MH[,var_ls] %>% rename(upps_y_ss_lack_of_pers = upps_y_ss_lack_of_perseverance) -> MH_S



# 3. Perform nm nt re-coding ------------------------------------------------
fprintf("=======================Start ABCD NM NT Re-coding=======================\n")
colnames(MH_S) %>% str_remove_all("_nm") %>% str_remove_all("_nt") %>%
  table() -> var_counts
var_ls = names(var_counts)[var_counts==3]
select(MH_S,contains(var_ls)) %>% Recode.ABCD.NM.NT(var_ls) %>%
  cbind(select(MH_S,-contains(var_ls))) -> NEW_MH_S

# additional processing are needed for youth life events
Flag = MH$ple_y_ss_total_number_nm == MH$ple_y_ss_total_number_nt
Flag = replace_na(Flag,T)
NEW_MH_S[['ple_y_ss_total_good']][Flag] <- NA
NEW_MH_S[['ple_y_ss_total_bad']][Flag] <- NA
NEW_MH_S[['ple_y_ss_affect_sum']][Flag] <- NA
NEW_MH_S[['ple_y_ss_affected_good_sum']][Flag] <- NA
NEW_MH_S[['ple_y_ss_affected_good_mean']][Flag] <- NA
NEW_MH_S[['ple_y_ss_affected_bad_sum']][Flag] <- NA
NEW_MH_S[['ple_y_ss_affected_bad_mean']][Flag] <- NA
fprintf("Notes: For all Youth-reported Life Events-related Variables (i.e. start with 'ple_y_ss_')\n")
fprintf("\t Cases flagged out by 'ple_y_ss_total_number_nm' and 'ple_y_ss_total_number_nt' were also peformed NAs replacements\n")
fprintf("\t In addition, NAs in Flag were replaced with True\n")
fprintf("\t Number: %d rows were replaced by NAs\n",sum(Flag))
rm(Flag)
fprintf("=======================ABCD NM NT Re-coding. Finished!=======================\n")

# 4. Filter Columns and Re-naming it --------------------------------------

NEW_MH_S %>% select(-ends_with('_nm')) %>% select(-ends_with('_nt')) %>%
  # BIS/BAS
  select(-c(bis_y_ss_bis_sum,# Remove 3 columns that are raw BIS/BAS sum scores,
            bis_y_ss_bas_rr, #  only the ABCD-modified BIS/BAS sum scores were retained.
            bis_y_ss_bas_drive)) %>% 
  # Life Events
  select(-c(ple_y_ss_total_number, # Remove 1 column - total number of events
            ple_y_ss_affect_sum, # Remove 3 columns, sum scores of "How Much Affected" (total)
            ple_y_ss_affected_good_sum, # (good events)
            ple_y_ss_affected_bad_sum,  # (bad events)
            )) %>%
  # Prodromal Psychosis Scale (Psychotic-like Experience)
  select(-c(pps_y_ss_bother_sum,# Remove PPS "Did it Bother You?" question
            pps_y_ss_bother_n_1)) %>%
  # Remove Parent-reported Perceived Stress Scale
  select(-pstr_ss_pr) %>% 
  # Rename all retained columns into SDPP-ABCD-TabDat Format
  rename(BAS_FS = bis_y_ss_bas_fs,
         BAS_Drive = bis_y_ss_basm_drive,
         BAS_RR = bis_y_ss_basm_rr,
         BIS_Sum = bis_y_ss_bism_sum,
         PEQ_Overt_Agg = peq_ss_overt_aggression,
         PEQ_Overt_Vic = peq_ss_overt_victim,
         PEQ_Rel_Agg = peq_ss_relational_aggs,
         PEQ_Rel_Vic = peq_ss_relational_victim,
         PEQ_Rep_Agg = peq_ss_reputation_aggs,
         PEQ_Rep_Vic = peq_ss_reputation_victim,
         PPS_Sum = pps_y_ss_number,
         PPS_Severity = pps_y_ss_severity_score,
         PPS_Severity_Mean = pps_ss_mean_severity,
         Mania_Sum = sup_y_ss_sum,
         UPPS_LPers = upps_y_ss_lack_of_pers,
         UPPS_LPlan = upps_y_ss_lack_of_planning,
         UPPS_NU = upps_y_ss_negative_urgency,
         UPPS_PU = upps_y_ss_positive_urgency,
         UPPS_SS = upps_y_ss_sensation_seeking,
         YLE_Good = ple_y_ss_total_good,
         YLE_Bad = ple_y_ss_total_bad,
         YLE_Good_Affected = ple_y_ss_affected_good_mean,
         YLE_Bad_Affected = ple_y_ss_affected_bad_mean,
         ERQ_Reappraisal = erq_ss_reappraisal_pr,
         ERQ_Suppression = erq_ss_suppress_pr) %>%
  # Re-order columns
  select(c(src_subject_id,eventname,everything())) -> NEW_MH_S


# 5. Calculate some additional sum scores ---------------------------------

NEW_MH_S$BAS_Sum = NEW_MH_S$BAS_FS + NEW_MH_S$BAS_Drive + NEW_MH_S$BAS_RR

NEW_MH_S$UPPS_Sum = NEW_MH_S$UPPS_LPers + NEW_MH_S$UPPS_LPlan + NEW_MH_S$UPPS_NU + 
  NEW_MH_S$UPPS_PU + NEW_MH_S$UPPS_SS


# 6. Save the re-organized and re-coded data -------------------------------

SDPP.save.file(NEW_MH_S,
               FileName = "MH_S_Rec.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

NEW_MH_S %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_MH_S_Rec.doc'),
              row.names = F,
              nsmalls = 1)


# End of Script -----------------------------------------------------------

s_close_sink()

