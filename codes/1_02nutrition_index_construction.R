library(tidyverse)
library(readxl)


setwd("/rds/general/project/hda_students_data/live/Group1")
dataframe <- readRDS('TDS_final_group_1/result_data/step0/combined_data.rds')
colnames(dataframe) <- trimws(colnames(dataframe))

dir.create("results_nu")

print("stage1")

IntakesNum <- c("cooked vegetable in take","salad/raw vege intake", "dried fruit intake", "fresh fruit intake", "bread intake",
                "cereal intake","tea intake","coffee intake(per day)","water intake")
IntakesCat <- c("beef intake","pork intake","lamb/mutton intake","processed meat intake(per day)","cheese intake",
                "poultry intake","oily fish intake","non-oily fish intake")
TypesCat <- c("never eat eggs dairy wheat sugar","spread type","milk type used","bread type","cereal type","coffee type",
              "salt added to food","variation in diet","smoking status")



for (var in IntakesNum){
  dataframe[var] <- as.numeric(ifelse(dataframe[var] == 'Less than one',"0.5",
                                      ifelse(dataframe[var] == 'Do not know',NA,
                                             ifelse(dataframe[var] == 'Prefer not to answer',NA,dataframe[[var]]))))}

for (var in IntakesCat){
  dataframe[var] <- as.numeric(ifelse(dataframe[var] == 'Never', "0",
                                      ifelse(dataframe[var] == 'Less than once a week',"0.5",
                                             ifelse(dataframe[var] == 'Once a week',"1",
                                                    ifelse(dataframe[var] == '2-4 times a week',"3",
                                                           ifelse(dataframe[var] == '5-6 times a week',"5.5",
                                                                  ifelse(dataframe[var] == 'Once or more daily',"7", NA)))))))}
for (var in TypesCat){
  dataframe[var] <- as.factor(dataframe[[var]])}







#dataframe1 <- dataframe[complete.cases(dataframe[IntakesNum]),]
#dataframe1 <- dataframe[complete.cases(dataframe[IntakesCat]),]


print("stage2")

data <- dataframe[,c(1:7)]
colnames(data) <- gsub(" ", "", colnames(data), fixed = TRUE)

data["SysBP"] <- dataframe[["systolic blood pressure"]]
data["BMI"] <- dataframe[["BMI"]]
data["Glucose"] <- dataframe[["glucose"]]
data["Cholesterol"] <- dataframe[["cholestorl"]]
data["HDL_Cholesterol"] <- dataframe[["HDL cholestorl"]]
data["LDL_Direct"] <- dataframe[["LDL direct"]]
data["Triglycerides"] <- dataframe[["Triglycerides"]]
data["Vitamin_D"] <- dataframe[["Vitamin D"]]


data["age"] <- dataframe[["age"]]
data["ethnic_background"] <- dataframe[["ethnic baackground"]]
data["sex"] <- dataframe[["sex"]]
data["qualification"] <- dataframe[["qualification"]]
data["alcohol_intake_frequency"]<- dataframe[["alcohol intake frequency"]]
data["Smoking"] <- dataframe[["smoking status"]]

data["Vegetable_Intake"] <- dataframe[["cooked vegetable in take"]] + dataframe[["salad/raw vege intake"]]
data["Fruit_Intake"] <- dataframe[["dried fruit intake"]] + dataframe[["fresh fruit intake"]]
data["OilyFish_Intake"] <- dataframe[["oily fish intake"]]
data["NonOilyFish_Intake"] <- dataframe[["non-oily fish intake"]]
data["RedMeat_Intake"] <- dataframe[["beef intake"]] + dataframe[["pork intake"]] + dataframe[["lamb/mutton intake"]] 
data["ProcessedMeat_Intake"] <- dataframe[["processed meat intake(per day)"]]
data["WhiteMeat_Intake"] <- dataframe[["poultry intake"]]
data["Bread_Intake"] <- dataframe[["bread intake"]]
data["Cheese_Intake"] <- dataframe[["cheese intake"]]
data["Cereal_Intake"] <- dataframe[["cereal intake"]]
data["Tea_Intake"] <- dataframe[["tea intake"]]
data["Coffee_Intake"] <- dataframe[["coffee intake(per day)"]]
data["Water_Intake"] <- dataframe[["water intake"]]
data["EDWS"] <- dataframe[["never eat eggs,dairy,wheat,sugar"]]
data["Milk_Type"] <- dataframe[["milk type used"]]
data["Spread_Type"] <- dataframe[["spread type"]]
data["Bread_Type"] <- dataframe[["bread type"]]
data["Cereal_Type"] <- dataframe[["cereal type"]]
data["Coffee_Type"] <- dataframe[["coffee type"]]
data["SaltAdded"] <- dataframe[["salt added to food"]]
data["DietVariation"] <- dataframe[["variation in diet"]]




Names <- colnames(data)[22:ncol(data)]

#Define Scores for numerical variable
for (var in Names[1:13]){data[paste("Score",var)] <- ecdf(data[[var]])(data[[var]])}

#Define Scores for categorical variable
data["Score SaltAdded" ] <- as.numeric(ifelse(data["SaltAdded"] == 'Never/rarely', "0.8",
                                              ifelse(data["SaltAdded"] == 'Sometimes',"0.4",
                                                     ifelse(data["SaltAdded"] == 'Usually',"0.2",
                                                            ifelse(data["SaltAdded"] == 'Always',"0",
                                                                   ifelse(data["SaltAdded"] == 'Prefer not to answer',"0.5",NA))))))

data["Score BreadType"]  <- as.numeric(ifelse(is.na(data[["Bread_Type" ]]),0.5,ifelse(data[["Bread_Type" ]]=="White",0,1)))
data["Score SpreadType"]  <- as.numeric(ifelse(is.na(data[["Spread_Type" ]]),0.5,ifelse(data[["Spread_Type"]]=="Butter/spreadable butter",0,1)))

#Define Nutrition Score by aggregating relevant scores
data[["NutritionScore"]] <- (data[["Score Vegetable_Intake" ]]*10 + data[["Score Fruit_Intake" ]]*10 
                           + (1-data[["Score Cereal_Intake"]])*5 + data[["Score Cheese_Intake"]]*5 +
                             (1-data[["Score RedMeat_Intake"]])*10 + data[["Score WhiteMeat_Intake"]]*5 + 
                             data[["Score NonOilyFish_Intake"]]*5+ (1-data[["Score OilyFish_Intake"]])*5 +
                             (1-data[["Score Coffee_Intake"]])*5 + (1-data[["Score Tea_Intake"]])*5+ 
                             (1-data[["Score Bread_Intake"]])*5 + data[["Score BreadType"]]*5 + 
                             (1-data[["Score ProcessedMeat_Intake"]])*10 + data[["Score SaltAdded" ]]*10+
                             data[["Score SpreadType"]]*5)

#no water

data1<-data[complete.cases(data[,c(43:54,56:58)]),]


#Nutrition Score distribution and characteristics

print("stage3")
data1$LungCancer<-ifelse(data1$LungCancer==1,'yes','no')
mu=data.frame(c('no','yes'),c(mean(data1$NutritionScore[data1$LungCancer=='no'],na.rm=T),mean(data1$NutritionScore[data1$LungCancer=='yes'],na.rm=T)))
colnames(mu)<-c('LungCancer',"grp.mean")





ggplot(data=data1,aes(x=NutritionScore))+geom_density(aes(col=LungCancer,fill=LungCancer),alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=LungCancer),linetype="dashed")+
  labs(title="nutrition score histgram for Lung cancer type",x="Nutrition score", y = "Density")+theme_classic()

ggsave("TDS_final_group_1/result_graph/step1/hist_nu.png",plot = last_plot())



qqnorm(data1$NutritionScore,plot.it = T)
dev.copy(device = png,"TDS_final_group_1/result_graph/step1/nutrition_score_qqplot.png")
dev.off()



dim(data1)
print("stage4")

data1$LungCancer<-ifelse(data1$LungCancer=="no",0,1)
#Logistic regression with Cancers and Car accidents
Cancers <- colnames(data1[c(2:7)])
pval=lapply(Cancers,
            function(var) {
              formula1    <- as.formula(paste(var, " ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency"))
              res.logist1 <- glm(formula1, data = data1, family = 'binomial')
              formula2    <- as.formula(paste(var, " ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency + NutritionScore"))
              res.logist2 <- glm(formula2, data = data1, family = 'binomial')
              pval=c(anova(res.logist1,res.logist2,test="Chisq")$'Pr(>Chi)'[2])
              names(pval)='pval'
              return(pval)})

pval_adjusted=lapply(Cancers,
                     function(var) {
                       formula3    <- as.formula(paste(var, " ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency + Smoking"))
                       res.logist3 <- glm(formula3, data = data1, family = 'binomial')
                       formula4    <- as.formula(paste(var, " ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency + Smoking + NutritionScore"))
                       res.logist4 <- glm(formula4, data = data1, family = 'binomial')
                       pval_adjusted=c(anova(res.logist3,res.logist4,test="Chisq")$'Pr(>Chi)'[2])
                       names(pval_adjusted)='pval_adjusted'
                       return(pval_adjusted)})

pval <- data.frame(t(data.frame((pval))))
saveRDS(pval,"TDS_final_group_1/result_data/step1/pval_nu.rds")
pval_adjusted <- data.frame(t(data.frame((pval_adjusted))))
saveRDS(pval_adjusted,"TDS_final_group_1/result_data/step1/pval_adjusted.rds")
par(mar=c(6,6,3,3))

png("TDS_final_group_1/result_graph/step1/pval_nutrition.png")
plot(-log(pval$pval),xaxt="n",xlab='',pch = 16,ylab = '-ln(pvalue)',col=ifelse(pval$pval<=0.05/6,'black','pink'),cex=1,ylim=c(0,200))
points(-log(pval_adjusted$pval_adjusted),xaxt="n",xlab='',pch = 17,ylab = '-ln(pvalue)',col=ifelse(pval_adjusted$pval_adjusted<=0.05/6,'black','pink'),cex=1.2)
abline(h=-log(0.05/6))
axis(1,labels=Cancers,at=c(1:6),las=2)
legend("topright", legend=c("Not Adjusted For Smoking", "Adjusted For Smoking"),
       col="black", pch = 16:17, cex=1)




dev.off()

print("stage5")
#Linear regression with Biomarkers
Biomarkers <- colnames(data[c(8:15)])
pval=lapply(Biomarkers,
            function(var) {
              formula1    <- as.formula(paste(var, " ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency"))
              res.logist1 <- lm(formula1, data = data1)
              formula2    <- as.formula(paste(var, " ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency + NutritionScore"))
              res.logist2 <- lm(formula2, data = data1)
              pval=c(anova(res.logist1,res.logist2,test="Chisq")$'Pr(>Chi)'[2])
              names(pval)='pval'
              return(pval)})

pval_adjusted=lapply(Biomarkers,
                     function(var) {
                       formula3    <- as.formula(paste(var, " ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency + Smoking"))
                       res.logist3 <- lm(formula3, data = data1)
                       formula4    <- as.formula(paste(var, " ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency + Smoking + NutritionScore"))
                       res.logist4 <- lm(formula4, data = data1)
                       pval_adjusted=c(anova(res.logist3,res.logist4,test="Chisq")$'Pr(>Chi)'[2])
                       names(pval_adjusted)='pval_adjusted'
                       return(pval_adjusted)})

pval <- data.frame(t(data.frame((pval))))
saveRDS(pval,"TDS_final_group_1/result_data/step1/pval_nu_lin.rds")
pval_adjusted <- data.frame(t(data.frame((pval_adjusted))))
saveRDS(pval_adjusted,"TDS_final_group_1/result_data/step1/pval_adjusted_lin.rds")

plot(-log(pval$pval),xaxt="n",xlab='',pch = 16,ylab = '-ln(pvalue)',col=ifelse(pval$pval<=0.05/8,'black','pink'),cex=1.2,ylim=c(-1,500))
points(-log(pval_adjusted$pval_adjusted),xaxt="n",xlab='',pch = 17,ylab = '-ln(pvalue)',col=ifelse(pval_adjusted$pval_adjusted<=0.05/8,'black','pink'),cex=1.2)
abline(h=-log(0.05/8))
+axis(1,labels=Biomarkers,at=c(1:8),las=2)
legend("topleft", legend=c("Not Adjusted For Smoking", "Adjusted For Smoking"),
       col="black", pch = 16:17, cex=0.8)
png("TDS_final_group_1/result_graph/step1/pval_biomarker.png")
dev.off()

#Create Table 1
data1$LungCancer=ifelse(data1$LungCancer==1,"yes","no")
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))}
table1(as.formula(paste("~ Smoking + ", paste(colnames(data1)[c(9,60,17:21,59)], collapse=" + "),paste("|LungCancer"))), data=data1, overall="Total",
       caption = 'Table 1. Patient Participant Demographic and Clinical Characteristics',
       render.continuous=my.render.cont, render.categorical=my.render.cat)

saveRDS(data1,"TDS_final_group_1/result_data/step1/combined_nutrition_code.rds")
