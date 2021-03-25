# confounder preparation
setwd("/rds/general/project/hda_students_data/live/Group1")
pcs<-readRDS("data/GWAS_PCs.rds")
data1<-readRDS("TDS_final_group_1/result_data/step1/combined_nutrition_code.rds")
confounder<-data1[,c("eid","sex","age")]
saveRDS(confounder,"TDS_final_group_1/result_data/step2/confounder.rds")
covars=readRDS("TDS_final_group_1/result_data/step2/confounder.rds")


covars<-merge(covars,pcs,by="eid")
conf_list=c("eid", colnames(covars)[2:which(colnames(covars)=="PC.10")])
print(conf_list)
mymodel=model.matrix(as.formula(paste0("~", paste(conf_list, collapse="+"))), data=covars)
print(head(mymodel))
mymodel=mymodel[,-1]
colnames(mymodel)[1]="IID"
mymodel=cbind(FID=mymodel[,1], mymodel)
colnames(mymodel)=gsub(" ", "", colnames(mymodel))
colnames(mymodel)[5:14]=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
# Saving confounders file
write.table(mymodel, "TDS_final_group_1/result_data/step2/confounders.txt", row.names=FALSE, col.names=TRUE, quote=FALSE, sep=" ")



