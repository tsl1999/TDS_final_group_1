library(data.table)
library(openxlsx)

path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)
### Creating the dataset

mydata=data.frame(fread("data/ukb26390.csv", select=1))
rownames(mydata)=mydata[,1]
diseases=c("lung_cancer","stomach_cancer","colon_cancer","breast_cancer","othercancer","accidents")
names(diseases)=c("Lung Cancer","Stomach Cancer","Colon Cancer","Breast Cancer","Other Cancer","accidents")
x=matrix(0,nrow=nrow(mydata),ncol=length(diseases))
colnames(x)=names(diseases)
mydata=cbind(mydata,x)

### Disease definition
# Cancer (all except benign)

icd10_othercancer=c(c(paste0("C0", 0:9), paste0("C", 10:14)), # lip, oral cavity, larynx
               paste0("C", 15,17,19:26), # digestive
               paste0("C", 30:33,35:39), # respiratory
               paste0("C", 40:41), # bone, cartilage
               paste0("C", 43:44), # skin
               paste0("C", 45:49), # mesothelial
              
               paste0("C", 51:58), # female genital
               paste0("C", 60:63), # male genital
               paste0("C", 64:68), # urinary
               paste0("C", 69:72), # nervous
               paste0("C", 73:75), # endocrin
               paste0("C", 76:80), # other sites
               paste0("C", 81:96), # lymphoid
               paste0("C", 97), # multiple
               paste0("D0", 0:9), # in situ
               paste0("D", 37:48)) # uncertain behaviour

icd9_othercancer=c(paste0(140:149), # lip, oral cavity, larynx
              paste0(150,152,154:159), # digestive
              paste0(160:161,163:165), # respiratory
              paste0(170:173,176), # bone, cartilage
              paste0(179:189), # genital
              paste0(190:199), # unspecified
              paste0(200:209), # lymphoid
              paste0(230:234), # in situ
              paste0(235:238), # uncertain 
              paste0(239)) # unspecified
icd9_lung_cancer=162
icd9_stomach_cancer=151
icd9_colon_cancer=153
icd9_breast_cancer=174:175

icd10_colon_cancer="C18"
icd10_breast_cancer="C50"
icd10_lung_cancer="C34"
icd10_stomach_cancer="C16"

### accident
icd10_accidents=c(c(paste0("V0", 01:09), #Pedestrian injured in transport accident
                    paste0("V", 40:49)))#Car occupant injured in transport accident

icd9_accidents=c(paste0("E",810:819)) # motor vehicle traffic injuries




### HES: Hospital Episode Statistics

hes=data.frame(fread("data/hesin_diag.txt"))

for (d in 1:length(diseases)){
  print(names(diseases)[d])
  disease=diseases[d]
  icd10_list=eval(parse(text=paste0("icd10_",disease)))
  icd9_list=eval(parse(text=paste0("icd9_",disease)))
  myeids=NULL
  
  # ICD10 codes
  pb=txtProgressBar(style=3)
  for (k in 1:length(icd10_list)){
    setTxtProgressBar(pb, k/length(icd10_list))
    tmp=as.character(hes$eid[grepl(paste0("^", icd10_list[k]), hes$diag_icd10)])
    myeids=c(myeids, tmp)
  }
  myeids=unique(myeids)
  cat("\n")
  print(length(myeids))
  
  # ICD9 codes
  pb=txtProgressBar(style=3)
  for (k in 1:length(icd9_list)){
    setTxtProgressBar(pb, k/length(icd9_list) )
    tmp=as.character(hes$eid[grepl(paste0("^", icd9_list[k]), hes$diag_icd9)])
    myeids=c(myeids, tmp)
  }
  myeids=unique(myeids)
  cat("\n")
  print(length(myeids))
  
  print(table(mydata[myeids,names(diseases)[d]]))
  mydata[myeids,names(diseases)[d]]=1
  print(table(mydata[,names(diseases)[d]]))
  cat("\n")
}

print(apply(mydata[,names(diseases)[1:6]],2,sum))
saveRDS(mydata, "tds_final_group_1/result_data/disease_outcomes.rds")




