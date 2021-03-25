#path=/rds/general/project/hda_students_data/live/Group1
  
#group 1 codes
#project 3 Mandelian randomisation

#"ukb26390.csv" dataset
#it's the observation dataset, with eid on the first column 

#all other columns are for different are characteristics of the participants
# in the form
#"Field ID"."ID of data collection"."Array ID"

#The field IDs link to explicit descriptions of the variable

#The ID of data collection is an integer: 0 indicates that the information was collected at baseline,
# 1 is at a second time point, and 2 is a third time point. You will see that additional time points are
# available only for some variables, and of these, only a subset participants have data on all time points.

# The array ID is available for variables where multiple answers can be collected.
#The data collected is stored in multiple columns for the same variable.
#For example, for the data on qualifications collected at baseline
# we have 6138.0.0 containing one of the boxes that was ticked by the participant, and 6138.0.1 containing
# data from a second box ticked by the participant, etc. Participants who ticked only one degree will have 
# missing values for the array IDs > 0. 


#You can list the field IDs of variables you would like to look into
# as illustrated in the folder "example_extraction".
# Loading the data
path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)
library(data.table)
mydata=data.frame(fread("data/ukb26390.csv", nrows=5))
myfields=unname(unlist(read.table("TDS_final_group_1/data/extract.txt", header=FALSE)))

## Extracting the column ids 
column_id=grep("eid", colnames(mydata))
found_fieldids=NULL
for (k in 1:length(myfields)){
  mygrep=grep(paste0("X",myfields[k],"."), fixed=TRUE, colnames(mydata))
  if (length(mygrep)>0){
    found_fieldids=c(found_fieldids, myfields[k])
  }
  column_id=c(column_id, mygrep)
}

# Extracting required columns from dataset
extracted=data.frame(fread("/rds/general/project/hda_students_data/live/Group1/data/ukb26390.csv", select=column_id)) 
saveRDS(extracted, "TDS_final_group_1/result_data/step0/individual_covariates.rds")

