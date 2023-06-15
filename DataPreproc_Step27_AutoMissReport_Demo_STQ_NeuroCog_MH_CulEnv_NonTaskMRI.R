library(bruceR)

setwd("I:\\ABCDStudyNDA\\ABCD_DataAnalysis_4.0\\DataPreprocessing")

data = readRDS('ABCD4.0_Merged_Demo_STQ_NeuroCog_MH_CulEnv_NonTaskMRI.rds')

var.list = colnames(data)[6:ncol(data)]

count_na_by_wave <- function(data,var.name){
  count.formula = sprintf('count_na = sum(is.na(%s))',var.name)
  command <- sprintf('res <- summarise(data,%s,.by = eventname)',count.formula)
  eval(parse(text = command))
  colnames(res) <- c('eventname',var.name)
  res <- res[order(res$eventname),]
  return(res)  
}

for (i in 1:length(var.list)){
  if (i == 1){
    res <- count_na_by_wave(data,var.list[i])
  }else{
    tmp <- count_na_by_wave(data,var.list[i])
    res <- merge(res,tmp,by = 'eventname')
  }
  cat(sprintf('# %d | variable name:%s\n',i,var.list[i]))
}

write.csv(res,'MissVarRep_Script26.csv')  
