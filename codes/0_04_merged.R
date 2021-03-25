#merge
library(data.table)
#merge biomarker and characteristics--------------
path="/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step0"
setwd(path)
biomarkers<-readRDS("biomarkers.rds")
individual_cov<-readRDS("individual_covariates.rds")

merged<-merge(individual_cov,biomarkers,by="eid",no.dups = FALSE)



#only use the one with array ID and time ID both as 0----------------
dataframe=merged
dataframe=dataframe[,sub("^[^.]*.", "", colnames(dataframe))=='0.0']
dataframe=cbind(merged$eid,dataframe)
rownames(dataframe)=dataframe$`extracted$eid`
colnames(dataframe)[1]<-'eid'

colnames(dataframe)<-sub("\\..*", "", colnames(dataframe))

#save---------------------
saveRDS(dataframe,"merged.rds")



