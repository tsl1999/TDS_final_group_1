#some final safety checks
# 1 nutrition score and lung cancer

library(data.table)
data1<-readRDS("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step1/combined_nutrition_code.rds")
prs<-readRDS("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step3/prs.rds")
GWAS_PCs<-readRDS('/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/data/GWAS_PCs.rds')
no_na_nu<-data1
mr<-no_na_nu[,c("eid","BMI","NutritionScore","sex","Smoking","age","LungCancer","alcohol_intake_frequency","ColonCancer","StomachCancer","BreastCancer","HDL_Cholesterol")]
rownames(mr)<-mr$eid
mr_prs<-mr[rownames(mr)%in%data1$eid,]
rownames(GWAS_PCs)<-GWAS_PCs$eid
PC_n<-GWAS_PCs[rownames(GWAS_PCs)%in%data1$eid,1:11]
mr_prs<-merge(mr_prs,PC_n,by="eid")
mr_prs<-merge(mr_prs,prs,by="eid")
rownames(mr_prs)<-mr_prs$eid
no_na_nu<-data1
mr_prs$LungCancer<-ifelse(mr_prs$LungCancer=="no",0,1)
nutrition_prs=mr_prs

e<-glm(LungCancer~NutritionScore+age+sex+Smoking,data=mr_prs,family="binomial")
summary(e)#stats significant

#check c stats(discrimination)
require(DescTools)
Cstat(e)#0.800, 0.713 without smoking

e<-glm(BreastCancer~NutritionScore+age+sex+Smoking,data=data1,family="binomial")
summary(e)#stats significant

e<-lm(no_na_nu_age$NutritionScore~no_na_nu_age$LungCancer+no_na_nu_age$age+no_na_nu_age$sex+no_na_nu_age$Smoking)
summary(e)#r squred 9%, significant, 
#without smoking, still significant, 0.1%

t.test(data1$NutritionScore,data1$LungCancer)#different in mean

#2 nutrition score and prs

cor(nutrition_prs$NutritionScore,nutrition_prs$prs,use="complete.obs")#0.07, very low
b<-lm(mr_prs$NutritionScore~.,data=mr_prs[,c(13:23)])
summary(b)#with pcs, r squred 2% ish, significant prs, f stats significant

e<-lm(nutrition_prs$NutritionScore~ nutrition_prs$prs)
summary(e)#significant prs, r squred 0.6%, f stats significant
anova(b)

# 3 nutrition score and BMI

cor(nutrition_prs$NutritionScore,nutrition_prs$BMI,use="complete.obs")#-0.1
e<-lm(mr_prs$NutritionScore~mr_prs$BMI+mr_prs$age+mr_prs$sex+mr_prs$Smoking)
summary(e)#adjust for smoking, 9%, not 1%
#both are significant

#prs and BMI

cor(nutrition_prs$prs,nutrition_prs$BMI,use="complete.obs")#-0.003
prs2<-prs$prs^2
e<-lm(BMI~prs+.,data=mr_prs[,c(2,13:23)])
summary(e)#stats significant but low r squred, with pcs, 0.6%

#nutrition score and HDL cholesterol

cor(nutrition_prs$HDL_Cholesterol,nutrition_prs$NutritionScore,use="complete.obs")#0.14
e<-lm(mr_prs$NutritionScore~mr_prs$HDL_Cholesterol+mr_prs$age+mr_prs$sex+mr_prs$Smoking)
summary(e)#with smoking 9%, without 7%, stats significant

#prs and HDL cholesterol

cor(nutrition_prs$HDL_Cholesterol,nutrition_prs$prs,use="complete.obs")#-0.016
e<-lm(HDL_Cholesterol~prs+.,data=mr_prs[,c(12:23)])
summary(e)#low r squred but stats significant prs

#prs and lung cancer
e<-glm(LungCancer~ prs+.,data=mr_prs[,c(7,13:23)],family="binomial")
summary(e)#not significant
boxplot(mr_prs$prs~mr_prs$LungCancer)
t.test(mr_prs$prs,mr_prs$LungCancer)#significant mean difference

#prs and other cancer
#Breast
e<-glm(BreastCancer~ prs+.,data=mr_prs[,c(11,13:23)],family="binomial")
summary(e)#significant prs, with or without pcs
require(DescTools)
Cstat(e)#0.800, 0.713 without smoking
summary(e)

t.test(mr_prs$prs,mr_prs$BreastCancer)#significant mean difference

boxplot(mr_prs$prs~mr_prs$BreastCancer)

#stomach
e<-glm(StomachCancer~ prs+.,data=mr_prs[,c(10,13:23)],family="binomial")
summary(e)#not significant

boxplot(mr_prs$prs~mr_prs$StomachCancer)
t.test(mr_prs$prs,mr_prs$StomachCancer)
