#extract biomarkers info from ukb27725 uning biomarker annotation

path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)
annot<-read_xlsx("data/Biomarker_annotation.xlsx")

anno_id<-paste("X",annot$`UK Biobank Field`,sep="")
anno_id<-c("eid",anno_id)
ukb27725<-read.csv("data/ukb27725.csv")
col_ukb<-sub("\\..*", "", colnames(ukb27725))
col_ukb_unique<-unique(sub("\\..*", "", colnames(ukb27725)))
ukb27725_col<-data.frame(colnames(ukb27725),col_ukb)


biomarkers<-ukb27725[,c(ukb27725_col$col_ukb%in%anno_id)]


saveRDS(biomarkers,"/rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data/biomarkers.rds")

#check
length(unique(sub("\\..*", "", colnames(biomarkers))))==31

#


