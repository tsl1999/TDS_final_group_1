#categorise, withdrawn removed 


#read data----------------------------------------------
path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)

data_dictionary <- read_excel("TDS_Group-1/data_dictionary.xlsx")
data_cat<-data.frame(data_dictionary[,c("Field ID","code ID")])
disease_outcomes <- readRDS("/rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data/disease_outcomes.rds")
merged<-readRDS("/rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data/merged.rds")
#categorise-----------------------------------------------------
data_cat$Field.ID<-paste("X",data_cat$Field.ID,sep="")
rownames(data_cat)<-data_cat$Field.ID
mycoding=data.frame(read.csv("data/Codings_Showcase.csv"))


#categorise function--------------------------------------
dataframe<-merged

  for(i in 1: length(colnames(dataframe))){
      if (is.na(data_cat[sub("\\..*", "", colnames(dataframe)[i]),2])==F) 
      {
        coding_id=data_cat[sub("\\..*", "", colnames(dataframe)[i]),2]
        mycoding=read.csv("data/Codings_Showcase.csv")
        mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
        mycoding_field=mycoding_field[,-1]
        rownames(mycoding_field)=mycoding_field$Value
        dataframe[,i]<-as.character(mycoding_field[as.character(dataframe[,i]),"Meaning"])
      }else{
        dataframe[,i]=dataframe[,i]
      }
      
  }

#change column name--------------------------------
data_cat<-data.frame(data_dictionary[,c("Field ID","code ID","name")])
data_cat$Field.ID<-paste("X",data_cat$Field.ID,sep="")
rownames(data_cat)<-data_cat$Field.ID
column_name<-c()
for (i in 2:102){
  column_name[i]=paste(data_cat[sub("\\..*", "", colnames(dataframe)[i]),3],
                       sub("^[^.]*.", "", colnames(dataframe)[i]))
}

colnames(dataframe)[2:102]<-column_name[2:102]



dataframe$`cooked vegetable in take `<-ifelse(dataframe$`cooked vegetable in take `==-10,"Less than one",
                                              ifelse(dataframe$`cooked vegetable in take `==-1,"Do not know",
                                                     ifelse(dataframe$`cooked vegetable in take `==-3,"Prefer not to answer",dataframe$`cooked vegetable in take `)))



dataframe$`salad/raw vege intake `<-ifelse(dataframe$`salad/raw vege intake `==-10,"Less than one",
                                              ifelse(dataframe$`salad/raw vege intake `==-1,"Do not know",
                                                     ifelse(dataframe$`salad/raw vege intake `==-3,"Prefer not to answer",dataframe$`salad/raw vege intake `)))

dataframe$`dried fruit intake `<-ifelse(dataframe$`dried fruit intake `==-10,"Less than one",
                                           ifelse(dataframe$`dried fruit intake `==-1,"Do not know",
                                                  ifelse(dataframe$`dried fruit intake `==-3,"Prefer not to answer",dataframe$`dried fruit intake `)))

dataframe$`fresh fruit intake `<-ifelse(dataframe$`fresh fruit intake `==-10,"Less than one",
                                        ifelse(dataframe$`fresh fruit intake `==-1,"Do not know",
                                               ifelse(dataframe$`fresh fruit intake `==-3,"Prefer not to answer",dataframe$`fresh fruit intake `)))

dataframe$`bread intake `<-ifelse(dataframe$`bread intake `==-10,"Less than one",
                                        ifelse(dataframe$`bread intake `==-1,"Do not know",
                                               ifelse(dataframe$`bread intake `==-3,"Prefer not to answer",dataframe$`bread intake `)))



dataframe$`cereal intake `<-ifelse(dataframe$`cereal intake `==-10,"Less than one",
                                  ifelse(dataframe$`cereal intake `==-1,"Do not know",
                                         ifelse(dataframe$`cereal intake `==-3,"Prefer not to answer",dataframe$`cereal intake `)))


dataframe$`tea intake `<-ifelse(dataframe$`tea intake `==-10,"Less than one",
                                   ifelse(dataframe$`tea intake `==-1,"Do not know",
                                          ifelse(dataframe$`tea intake `==-3,"Prefer not to answer",dataframe$`tea intake `)))


dataframe$`coffee intake(per day) `<-ifelse(dataframe$`coffee intake(per day) `==-10,"Less than one",
                                ifelse(dataframe$`coffee intake(per day) `==-1,"Do not know",
                                       ifelse(dataframe$`coffee intake(per day) `==-3,"Prefer not to answer",dataframe$`coffee intake(per day) `)))

dataframe$`water intake `<-ifelse(dataframe$`water intake `==-10,"Less than one",
                                            ifelse(dataframe$`water intake `==-1,"Do not know",
                                                   ifelse(dataframe$`water intake `==-3,"Prefer not to answer",dataframe$`water intake `)))


saveRDS(dataframe,"tds_final_group_1/result_data/cate_merge_disease(final).rds")
