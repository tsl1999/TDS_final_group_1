path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)

#remove withdrawn for risk factors--------------------------------------------------
merged<- readRDS("TDS_final_group_1/result_data/step0/cate_merge_disease(final).rds")
withdrawn=as.character(read.csv("data/w19266_20200204.csv")[,1])
print(withdrawn)
merged_no_withdraw<-merged[!merged$eid %in% withdrawn,]

#recheck with another method
merged_2<- merged[-which(merged$eid %in% withdrawn), ]

saveRDS(merged_no_withdraw,"TDS_final_group_1/result_data/step0/merged_no_withdraw.rds")


#outcome of interest remove withdrawn---------------------------------------------
disease_outcome<-readRDS("TDS_final_group_1/result_data/step0/disease_outcomes.rds")
disease_outcome_withdrawn<-disease_outcomes[!row.names(disease_outcomes) %in% withdrawn,]


#101 rows removed

#merge disease and characteristics
merge_disease<-merge(disease_outcome_withdrawn,merged_no_withdraw,by="eid")
merge_disease$age<-2018-merge_disease$`Year of birth `

saveRDS(merge_disease,"TDS_final_group_1/result_data/step0/combined_data.rds")
#subset data-------------------------------------------------------

dataframe<-merge_disease

subset<-dataframe[1:1000,]
saveRDS(subset,"TDS_Group-1/subset_new.rds")
