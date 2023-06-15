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

smri_1 = readABCDdata('abcd_smrip10201.txt')
# smri_2 = readABCDdata('abcd_smrip20201.txt')
# smri_3 = readABCDdata('abcd_smrip30201.txt')

smri_1 = subset(smri_1,select = -c(abcd_smrip10201_id,smri_visitid))

smri_anchor = subset(smri_1,select = c(subjectkey,src_subject_id,interview_date,sex,eventname))
# Extract 4 brain Morphometric measures and rename them (from the DK atlas)
# includes: cortical thickness (mm), cortical sulcal depth (mm), cortical area (mm^2), cortical volume and subcortical volume (mm^3)

smri_CorticalThickness_DK = subset(smri_1,select = grep('smri_thick_cdk.*',colnames(smri_1),value = T) ) 

colnames(smri_CorticalThickness_DK) = stringr::str_replace(colnames(smri_CorticalThickness_DK),'smri_thick','CorticalThickness')
colnames(smri_CorticalThickness_DK) = stringr::str_replace(colnames(smri_CorticalThickness_DK),'_cdk_','_DK_')
colnames(smri_CorticalThickness_DK) = stringr::str_replace(colnames(smri_CorticalThickness_DK),'rh','_Right')
colnames(smri_CorticalThickness_DK) = stringr::str_replace(colnames(smri_CorticalThickness_DK),'lh','_Left')

smri_SulcalDepth_DK = subset(smri_1,select = grep('smri_sulc_cdk.*',colnames(smri_1),value = T) ) 

colnames(smri_SulcalDepth_DK) = stringr::str_replace(colnames(smri_SulcalDepth_DK),'smri_sulc','SulcalDepth')
colnames(smri_SulcalDepth_DK) = stringr::str_replace(colnames(smri_SulcalDepth_DK),'_cdk_','_aparc_')
colnames(smri_SulcalDepth_DK) = stringr::str_replace(colnames(smri_SulcalDepth_DK),'rh','_Right')
colnames(smri_SulcalDepth_DK) = stringr::str_replace(colnames(smri_SulcalDepth_DK),'lh','_Left')

smri_CorticalArea_DK = subset(smri_1,select = grep('smri_area_cdk.*',colnames(smri_1),value = T) ) 

colnames(smri_CorticalArea_DK) = stringr::str_replace(colnames(smri_CorticalArea_DK),'smri_area','CorticalArea')
colnames(smri_CorticalArea_DK) = stringr::str_replace(colnames(smri_CorticalArea_DK),'_cdk_','_aparc_')
colnames(smri_CorticalArea_DK) = stringr::str_replace(colnames(smri_CorticalArea_DK),'rh','_Right')
colnames(smri_CorticalArea_DK) = stringr::str_replace(colnames(smri_CorticalArea_DK),'lh','_Left')

smri_CorticalVolume_DK = subset(smri_1,select = grep('smri_vol_cdk.*',colnames(smri_1),value = T) ) 

colnames(smri_CorticalVolume_DK) = stringr::str_replace(colnames(smri_CorticalVolume_DK),'smri_vol','Volume')
colnames(smri_CorticalVolume_DK) = stringr::str_replace(colnames(smri_CorticalVolume_DK),'_cdk_','_aparc_')
colnames(smri_CorticalVolume_DK) = stringr::str_replace(colnames(smri_CorticalVolume_DK),'rh','_Right')
colnames(smri_CorticalVolume_DK) = stringr::str_replace(colnames(smri_CorticalVolume_DK),'lh','_Left')

smri_SubcorticalVolume_DK = subset(smri_1,select = grep('smri_vol_scs.*',colnames(smri_1),value = T) ) 

colnames(smri_SubcorticalVolume_DK) = stringr::str_replace(colnames(smri_SubcorticalVolume_DK),'smri_vol','Volume')
colnames(smri_SubcorticalVolume_DK) = stringr::str_replace(colnames(smri_SubcorticalVolume_DK),'_scs_','_aseg_')
colnames(smri_SubcorticalVolume_DK) = stringr::str_replace(colnames(smri_SubcorticalVolume_DK),'rh','_Right')
colnames(smri_SubcorticalVolume_DK) = stringr::str_replace(colnames(smri_SubcorticalVolume_DK),'lh','_Left')

smri_Volume_DK = cbind(smri_CorticalVolume_DK,smri_SubcorticalVolume_DK)

# concatenate the 4 morphometric measures
smri = cbind(smri_anchor,smri_CorticalThickness_DK,smri_SulcalDepth_DK,smri_CorticalArea_DK,smri_Volume_DK)

saveRDS(smri,"I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing\\ABCD4.0_SMRI.rds")
