# =============================================================================#
# SDPP Step 7: Read and re-coding Parent-reported Child Behavior Checklist Sum Scores
# R Packages Dependency: bruceR, naniar, readxl
# Step File Notes: 
# 
# Target File: 
# ABCD 4.0 : abcd_cbcls01.txt
# ABCD 5.0: /core/mental-health/mh_p_cbcl.csv
# Update Date: 2023.7.10
#              2023.7.17
# =============================================================================#
# 1. Library Packages and Prepare Environment --------------------------------
AutoLogFileName = 'Log_SDPP-ABCD-TabDat_7.txt'
AutoLogFilePath = fullfile(ProjectDirectory,'Res_1_Logs',AutoLogFileName)
sink(file = AutoLogFilePath)
ResultsOutputDir = S7_ResultsOutputDir 
library(naniar)
# ==============================MAIN CODES=====================================#
# 2. Load CBCL and its sum scores data ---------------------------------------
cbcl_FileDir = fullfile(TabulatedDataDirectory,'/mental-health/mh_p_cbcl.csv')
CBCL = readABCDdata(cbcl_FileDir)
fprintf("Parent-reported CBCL Data type:\n")
sapply(CBCL,typeof)
CBCL %>% MVA.Report.By.Wave() %>% 
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_MH_P_CBCL_Raw.doc'),
              row.names = F,
              nsmalls = 1)

# 3. Filter columns and re-name it ----------------------------------------
# Only t-scores will be retained.
fprintf("Only T-scores will be retained in preprocessed data, all variable names are showed as follows:\n")
CBCL %>% select(c(src_subject_id,eventname,ends_with('_t'))) %>% 
  rename(CBCL_AnxDep = cbcl_scr_syn_anxdep_t,
         CBCL_WithDep = cbcl_scr_syn_withdep_t,
         CBCL_SomaticCo = cbcl_scr_syn_somatic_t,
         CBCL_Social = cbcl_scr_syn_social_t,
         CBCL_Thought = cbcl_scr_syn_thought_t,
         CBCL_Attention = cbcl_scr_syn_attention_t,
         CBCL_RuleBreak = cbcl_scr_syn_rulebreak_t,
         CBCL_Aggressive = cbcl_scr_syn_aggressive_t,
         CBCL_Internal = cbcl_scr_syn_internal_t,
         CBCL_EXternal = cbcl_scr_syn_external_t,
         CBCL_TotProb = cbcl_scr_syn_totprob_t,
         CBCL_Depress = cbcl_scr_dsm5_depress_t,
         CBCL_Anxiety = cbcl_scr_dsm5_anxdisord_t,
         CBCL_SomaticPr = cbcl_scr_dsm5_somaticpr_t,
         CBCL_ADHD = cbcl_scr_dsm5_adhd_t,
         CBCL_Opposit = cbcl_scr_dsm5_opposit_t,
         CBCL_Conduct = cbcl_scr_dsm5_conduct_t,
         CBCL_SCT = cbcl_scr_07_sct_t,
         CBCL_OCD = cbcl_scr_07_ocd_t,
         CBCL_Stress = cbcl_scr_07_stress_t,
         ) -> NEW_CBCL
print(colnames(NEW_CBCL))
fprintf("Here, SDPP-ABCD-TabDat doesn't indicate t-score in variable names, which should be noted for users.\n")
fprintf("For abbreviation reference table, please see the section 'CBCL' in user manual.\n")

fprintf('CBCL Sum Scores - Brief Overview:\n')
select(NEW_CBCL,-c(src_subject_id,eventname)) %>% 
  psych::describeBy(group = Recode.Eventname(NEW_CBCL)$eventname,
                    mat = T,digits =2) %>%
  print_table(file = fullfile(ResultsOutputDir,'VSO_ALL_MH_P_CBCL.doc'),
              row.names = T,
              nsmalls = 1,
              digits = 2)
# 5. Save Re-coded CBCL data ----------------------------------------------

SDPP.save.file(NEW_CBCL,
               FileName = "MH_P_CBCL_Rec.rds",
               Prefix = Prefix,
               ProjectDirectory = ProjectDirectory)

NEW_CBCL %>% MVA.Report.By.Wave() %>%
  print_table(file = fullfile(ResultsOutputDir,'MVA_Report_ALL_MH_P_CBCL_Rec.doc'),
              row.names = F,
              nsmalls = 1)

# End of Script -----------------------------------------------------------

fprintf("SDPP-ABCD-TabDat Step 7 finished! Finish Time:%s\n",Sys.time())

sink()
