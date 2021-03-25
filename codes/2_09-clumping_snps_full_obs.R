library(snpStats)
setwd("clump")
setwd("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step2/clump_snps")

# Extract SNP names
bim=read.table("ukb_imp_merged.bim", stringsAsFactors=FALSE)
mysnps=bim[!duplicated(bim[,2]),2]

# Rename participants
fam=read.table("imp.fam")
fam[,1]=fam[,2]=1:nrow(fam)
write.table(fam, "ukb_imp_merged.fam", quote=FALSE, row.names=FALSE, col.names=FALSE)

# Read plink data
mydata=read.plink("ukb_imp_merged", select.snps=mysnps, na.strings="-9")

# Extract genotype data
genodata=mydata$genotypes
genodata=data.frame(genodata)

# Rename participants
fam=read.table("imp.fam")
rownames(genodata)=fam[,1]


data<-readRDS("/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step1/combined_nutrition_code.rds")

rownames(data)<-data$eid
#select those have nutrition score
gene_data<-genodata[rownames(genodata)%in%rownames(data),]

# Save genetic data
saveRDS(gene_data, "/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step2/imp_clump_full_obs.rds")
