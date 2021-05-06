library(snpStats)
library(plyr)
library(readr)
library(RColorBrewer)
library(qqman)
setwd("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/gwas_data/gwas_output")

data<-c()
for (i in 1:22){
  data[i]<-paste0(paste0("linear_geno_chr",i,".assoc.linear"))
}



# read plot
dat_csv = ldply(data, read.table,stringsAsFactors=FALSE,header = T)

saveRDS(dat_csv,"/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step2/gwas.rds")

significant<-dat_csv[dat_csv$P<=5e-8,]

gwas.dat<-significant[is.na(significant$CHR)==F,]#270


#ggplot



# Make the Manhattan plot on the gwasResults dataset

mypal = brewer.pal(n = 12, name = "Paired")
colors = colorRampPalette(c("navy", "blue", "skyblue"))(22)
colors = colorRampPalette(mypal)(22)

manhattan(dat_csv, chr="CHR", bp="BP", snp="SNP", p="P" ,ylim=c(0,40),col=colors,
          suggestiveline = FALSE,annotatePval = 5e-8)

