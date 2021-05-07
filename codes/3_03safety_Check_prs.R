#some final safety checks


library(data.table)
require(DescTools)
data1<-readRDS("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step1/combined_nutrition_code.rds")
prs<-readRDS("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step3/prs.rds")
GWAS_PCs<-readRDS('/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/data/GWAS_PCs.rds')
no_na_nu<-data1
mr<-no_na_nu[,c("eid","BMI","NutritionScore","sex","Smoking","age","LungCancer","alcohol_intake_frequency","ColonCancer","StomachCancer","BreastCancer","HDL_Cholesterol","accidents")]
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


# 1 nutrition score and lung cancer-----------------------
e<-glm(LungCancer~NutritionScore+age+sex+Smoking,data=mr_prs,family="binomial")
summary(e)#stats significant
exp(coef(e))
exp(confint(e))

#check c stats(discrimination)

Cstat(e)#0.800, 0.713 without smoking


#2 nutrition score and prs -----------------------------------------

cor(nutrition_prs$NutritionScore,nutrition_prs$prs,use="complete.obs")#0.07, very low
b<-lm(mr_prs$NutritionScore~.,data=mr_prs[,c(14:24)])
summary(b)#with pcs, r squred 2% ish, significant prs, f stats significant

e<-lm(nutrition_prs$NutritionScore~ nutrition_prs$prs)
summary(e)#significant prs, r squred 0.6%, f stats significant
anova(b)

# 3 nutrition score and BMI---------------------------------------------

cor(nutrition_prs$NutritionScore,nutrition_prs$BMI,use="complete.obs")#-0.1
e<-lm(mr_prs$NutritionScore~mr_prs$BMI+mr_prs$age+mr_prs$sex+mr_prs$Smoking)
summary(e)#adjust for smoking, 9%, not 1%
#both are significant
confint(e)


#4. prs and BMI ----------------------------------------------

cor(nutrition_prs$prs,nutrition_prs$BMI,use="complete.obs")#-0.003

e<-lm(BMI~prs+.,data=mr_prs[,c(2,14:24)])
summary(e)#stats significant but low r squred, with pcs, 0.6%

confint(e)



#5. nutrition score and HDL cholesterol--------------------------

cor(nutrition_prs$HDL_Cholesterol,nutrition_prs$NutritionScore,use="complete.obs")#0.14
e<-lm(mr_prs$NutritionScore~mr_prs$HDL_Cholesterol+mr_prs$age+mr_prs$sex+mr_prs$Smoking)
summary(e)#with smoking 9%, without 7%, stats significant
confint(e)

#6. prs and HDL cholesterol---------------------------------------

cor(nutrition_prs$HDL_Cholesterol,nutrition_prs$prs,use="complete.obs")#-0.016
e<-lm(HDL_Cholesterol~prs+.,data=mr_prs[,c(12,14:24)])
summary(e)#low r squred but stats significant prs
confint(e)


#7. prs and lung cancer------------------------------------------
e<-glm(LungCancer~ prs+.,data=mr_prs[,c(7,14:24)],family="binomial")
summary(e)#not significant
boxplot(mr_prs$prs~mr_prs$LungCancer)
t.test(mr_prs$prs,mr_prs$LungCancer)#significant mean difference

#prs and other cancer---------------------------------

#Breast
#8. breast cancer with nutrition score--------
e<-glm(BreastCancer~ mr_prs$sex+mr_prs$age+mr_prs$Smoking+mr_prs$NutritionScore,data=mr_prs[,c(3,11,14:23)],family="binomial")
summary(e)#significant prs, with or without pcs

Cstat(e)
exp(coef(e))
exp(confint(e))

#9. breast cancer with prs---------------------------------
e<-glm(BreastCancer~ prs+.,data=mr_prs[,c(11,14:24)],family="binomial")
summary(e)#significant prs, with or without pcs

Cstat(e)
summary(e)
exp(confint(e))

t.test(mr_prs$prs,mr_prs$BreastCancer)#significant mean difference

boxplot(mr_prs$prs~mr_prs$BreastCancer)

#10.nutrition score with stomach cancer-----------------------
e<-glm(StomachCancer~ prs+.,data=mr_prs[,c(10,13:23)],family="binomial")
summary(e)#not significant
exp(confint(e))

boxplot(mr_prs$prs~mr_prs$StomachCancer)
t.test(mr_prs$prs,mr_prs$StomachCancer)


#11. prs with accidents -----------------------------------------------------
e<-glm(accidents~ prs+.,data=mr_prs[,c(13:24)],family="binomial")
summary(e)
exp(coef(e))
exp(confint(e))


