#merge
library(data.table)
#merge biomarker and characteristics--------------
path="/rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data"
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


merged<-merge(new_var_no_withdraw,merged_new,by="eid",no.dups = FALSE)
saveRDS(merged,"data/merged_new_var_acc.rds")


merged_new_var<-merge(merged_new_var_acc,merged_only00,by="eid",no.dups = FALSE)
saveRDS(merged_new_var,"data/merged_new_00.rds")

new_df <- merged_new_var %>%
  select(eid, `Lung Cancer`,`Colon Cancer`,`Stomach Cancer`,`Breast Cancer`,`Other Cancer`,accidents, everything())
saveRDS(new_df,"data/merged_new_00.rds")
