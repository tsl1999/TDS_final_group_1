# make six column fam
#geno _fam

library(data.table)
setwd("/rds/general/project/hda_students_data/live/Group1")

mydata=readRDS("tds_final_group_1/result_data/step1/combined_nutrition_code.rds")
rownames(mydata)<-mydata$eid

#case=rep(0,nrow(mydata)) # 0 for excluded
#case[which(mydata$LungCancer=="no")]=1 # 1 for controls
#case[which(mydata$LungCancer=="yes")]=2 # 2 for cases

case<-rep(-9,nrow(mydata))
case<-mydata$NutritionScore

names(case)=rownames(mydata)


fam<-data.frame(fread("/rds/general/project/hda_students_data/live/Group1/General/ukb_geno.fam"))
rownames(fam)=fam[,1]

ids=intersect(rownames(fam), names(case))
fam[,6]=-9
fam[ids,6]=case[ids]
print(table(fam[,6]))


write.table(fam, "/rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data/step2/gene.fam", row.names=FALSE, col.names=FALSE, quote=FALSE)







#imp_fam
# make six column fam

library(data.table)
setwd("/rds/general/project/hda_students_data/live/Group1/data")

mydata=readRDS("no_na_nu.rds")
rownames(mydata)<-mydata$eid

#case=rep(0,nrow(mydata)) # 0 for excluded
#case[which(mydata$LungCancer=="no")]=1 # 1 for controls
#case[which(mydata$LungCancer=="yes")]=2 # 2 for cases

case<-rep(-9,nrow(mydata))
case<-mydata$NutritionScore

names(case)=rownames(mydata)


fam<-data.frame(fread("/rds/general/project/hda_students_data/live/Group1/General/ukb_imp.fam"))
rownames(fam)=fam[,1]

ids=intersect(rownames(fam), names(case))
fam[,6]=-9
fam[ids,6]=case[ids]
print(table(fam[,6]))


write.table(fam, "/rds/general/project/hda_students_data/live/Group1/UKB_genetic_data_scripts/imp.fam", row.names=FALSE, col.names=FALSE, quote=FALSE)

fam<-data.frame(fread("/rds/general/project/hda_students_data/live/Group1/UKB_genetic_data_scripts/Genetic_data_extraction/ukb_gen_merged.fam"))