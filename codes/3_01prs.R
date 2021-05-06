#prs construction
library(data.table)
setwd('/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step2')
#weight
iv.clump<-readRDS("clump.rds")
weight<-iv.clump[,c(2,7)]
gene_data<-readRDS("imp_clump_full_obs.rds")


df <- gene_data %>% 
  mutate_all(funs(case_when(. == 01 ~ '2',
                            . == 02 ~ '1',
                            . == 03 ~ '0',
                            . == 00 ~ 'NA')))#01 refers to homozygous minor alleles, make it to 2
df[ df == "NA" ] <- NA
df<-data.frame(df)




#check for weight names and col names------------------
weight$rsid==colnames(df)
df<-as.matrix(df)
w<-as.vector(weight$BETA)






#snp call rate, threshold 1%----------------

apply(df, 2,function(x){sum(is.na(x)==T)/length(rownames(df))})



#sample call rate, threshold 1%-----------------------
sample_call<-apply(df, 1,function(x){sum(is.na(x)==T)/length(rownames(df))})
sample_call<-data.frame(sample_call)
sample_call_n<-sample_call[,"sample_call"<=0.01]#0

#use matrix multiplication to derive prs
df_n<-apply(df, 2,as.numeric)

apply(df_n, 2,class)

df_n<-as.matrix(df_n)


w<-as.vector(weight$BETA)


prs<-df_n %*% w

prs<-data.frame(prs)
prs$eid<-rownames(df)
rownames(prs)<-prs$eid
# check for NAs
sum(is.na(prs$prs)==T)#2481

df<-data.frame(df)
rownames(prs)<-rownames(prs)

#check for matrix multiplication------------------
a<-df[1,]
a<-as.numeric(t(a))
b<-a*w
sum(b)#the same as prs on the first row




#prs and nutrition correlation check--------------------------------------------
data<-readRDS("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step1/combined_nutrition_code.rds")
nutrition<-data[,c("eid","BMI","NutritionScore","sex","age",'Smoking','qualification','alcohol_intake_frequency','LungCancer')]
rownames(nutrition)<-nutrition$eid
GWAS_PCs<-readRDS('/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/data/GWAS_PCs.rds')
nutrition_prs<-nutrition[rownames(nutrition)%in%rownames(df),]
rownames(GWAS_PCs)<-GWAS_PCs$eid
PC_n<-GWAS_PCs[rownames(GWAS_PCs)%in%rownames(df),1:11]
nutrition_prs<-merge(nutrition_prs,PC_n,by="eid")
nutrition_prs<-merge(nutrition_prs,prs,by="eid")
rownames(nutrition_prs)<-nutrition_prs$eid

cor(nutrition_prs$NutritionScore,nutrition_prs$prs,use="complete.obs")
cor(nutrition_prs$BMI,nutrition_prs$prs,use="complete.obs")



# prs vs nutrition score----------------

b<-lm(nutrition_prs$NutritionScore~.,data=nutrition_prs[,10:20])
summary(b)
confint(b)

#check for smoking------------------------
nutrition_prs$Smoking<-as.factor(nutrition_prs$Smoking)
nutrition_prs$Smoking<-relevel(nutrition_prs$Smoking,1)

b<-lm(nutrition_prs$prs ~nutrition_prs$Smoking+.,data=nutrition_prs[,10:20])

summary(b)
confint(b)

#qualification----------------------------------------
b<-lm(nutrition_prs$prs ~nutrition_prs$qualification+.,data=nutrition_prs[,10:20])

summary(b)
confint(b)


#alcohol----------------------------------------
b<-lm(nutrition_prs$prs ~nutrition_prs$alcohol_intake_frequency+.,data=nutrition_prs[,10:20])
c<-lm(nutrition_prs$prs ~.,data=nutrition_prs[,9:19])
anova(b,c)
summary(b)
confint(b)

#age----------------------------------------
b<-lm(nutrition_prs$prs ~nutrition_prs$age+.,data=nutrition_prs[,10:20])
summary(b)
confint(b)

#sex----------------------------------------
b<-lm(nutrition_prs$prs ~nutrition_prs$sex+.,data=nutrition_prs[,10:20])
summary(b)
confint(b)


#BMI vs prs, significant association
b<-lm(nutrition_prs$BMI~.,data=nutrition_prs[,c(2,10:20)])
summary(b)
confint(b)
saveRDS(nutrition_prs,"/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step3/nutrition_prs.rds")

saveRDS(prs,"/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step3/prs.rds")
