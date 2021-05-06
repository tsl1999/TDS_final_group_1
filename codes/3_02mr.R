#mandelian randomisation
library(data.table)
setwd('/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step2')
GWAS_PCs<-readRDS('/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/data/GWAS_PCs.rds')
data<-readRDS("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step1/combined_nutrition_code.rds")
prs<-readRDS("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step3/prs.rds")
no_na_nu<-data
rownames(data)<-data$eid
mr<-no_na_nu[,c("eid","BMI","NutritionScore","sex","Smoking","age","LungCancer","alcohol_intake_frequency","ColonCancer","StomachCancer","BreastCancer")]
rownames(mr)<-mr$eid
mr_prs<-mr[rownames(mr)%in%rownames(data),]
rownames(GWAS_PCs)<-GWAS_PCs$eid
PC_n<-GWAS_PCs[rownames(GWAS_PCs)%in%rownames(data),1:11]
mr_prs<-merge(mr_prs,PC_n,by="eid")

mr_prs<-merge(mr_prs,prs,by="eid")
rownames(mr_prs)<-mr_prs$eid



hist(prs$prs)
ggplot(data=mr_prs)+geom_density(aes(x=prs,color=LungCancer,fill=LungCancer),alpha=0.5)+
  theme_clean()

mr_prs$LungCancer<-ifelse(mr_prs$LungCancer=="no",0,1)
#prs and lung cancer
e<-glm(LungCancer~ prs+.,data=mr_prs[,c(7,12:22)],family="binomial")
summary(e)#not significant, coefficient 0.013
exp(confint(e))


boxplot(mr_prs$prs~mr_prs$LungCancer)
t.test(mr_prs$prs,mr_prs$LungCancer)#significant mean difference


