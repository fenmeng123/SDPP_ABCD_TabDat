library(bruceR)

setwd('I:\\ABCDStudyNDA\\Download_ABCDV4.0_skr220403\\Package_1199282')

readABCDdata<-function(filename){
  data = read.table(filename,header = TRUE,sep = '\t')
  # remove some specific columns which are same across all .txt files
  data <- subset(data, select = -c(collection_id, dataset_id,collection_title))
  # get variable descriptions
  var.descrip <- data[1,]
  # remove the first row
  data<-data[-1,]
  # add comments to all columns
  # for (i in 1:length(var.descrip)){
  #   comment(data[,i])<-var.descrip[1,i]
  # }
  return(data)
}

rsmri_1 = readABCDdata('abcd_betnet02.txt')
rsmri_2 = readABCDdata('mrirscor02.txt')
# smri_3 = readABCDdata('abcd_smrip30201.txt')

rsmri_1 = subset(rsmri_1,select = -c(abcd_betnet02_id,rsfmri_c_ngd_visitid,
                                     rsfmri_c_ngd_tr,rsfmri_c_ngd_numtrs,
                                     rsfmri_c_ngd_nvols,rsfmri_c_ngd_stnvols,
                                     rsfmri_c_ngd_stcontignvols,rsfmri_c_ngd_ntpoints,
                                     rsfmri_c_ngd_meantrans,
                                     rsfmri_c_ngd_maxmotion,rsfmri_c_ngd_maxtrans,
                                     rsfmri_c_ngd_meanrot,rsfmri_c_ngd_maxrot))
rsmri_2 = subset(rsmri_2,select = -c(mrirscor02_id,rsfmri_cor_ngd_scs_visitid,
                                     rsfmri_cor_ngd_scs_tr,rsfmri_cor_ngd_scs_numtrs,
                                     rsfmri_cor_ngd_scs_nvols,rsfmri_cor_ngd_scs_stcgnvols,
                                     rsfmri_cor_ngd_scs_stnvols,
                                     rsfmri_cor_ngd_scs_ntpoints,rsfmri_cor_ngd_scs_meanmn,
                                     rsfmri_cor_ngd_scs_maxmn,rsfmri_cor_ngd_scs_meantrans,
                                     rsfmri_cor_ngd_scs_maxtrans,rsfmri_cor_ngd_scs_meanrot,
                                     rsfmri_cor_ngd_scs_maxrot))

rsmri_anchor = subset(rsmri_1,select = c(subjectkey,src_subject_id,interview_date,sex,eventname))

# load the duplicate RSFNC variable names ---------------------------------
# becasue the RSFNC data from ABCD have some duplicate columns (different names but same values)
# we need to find these duplicated LMM results and remove the redundant
# RSFNC, which seems like 'Auditory Network - None Network' and 'None Network - Auditory Network'

dupRSFNC <- read.table(
  'I:\\ABCDStudyNDA\\ABCD_DataAnalysis\\Code_ScreenAnalysis2021\\Pipeline\\JAACAP_Revision220920\\Preprocessing\\DuplicateRSFNC_VarName.txt',
  sep = ',')
dupRSFNC <- t(dupRSFNC)

RSFNC <- merge(rsmri_1,rsmri_2,by = c('subjectkey','src_subject_id','interview_age','interview_date','sex','eventname'))

RSFNC <- RSFNC[, !colnames(RSFNC) %in% dupRSFNC]

saveRDS(RSFNC,
        "I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_RSFNC.rds")
