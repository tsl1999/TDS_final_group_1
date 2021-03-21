library(ieugwasr)
setwd("/rds/general/project/hda_students_data/live/Group1/tds_final_group_1/gwas_data/gwas_output")
#install_github("MRCIEU/ieugwasr")

data = readRDS("/rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data/step2/gwas.rds")
colnames(data)[2] = "rsid"
colnames(data)[12] = "pval"


iv = data[data$pval<5*10^-8,]
iv=iv[is.na(iv$pval)==F,]
write.table(iv,"bonf_snps.txt",row.names=FALSE, col.names=FALSE, quote=FALSE, sep=" ")
iv.clump = ieugwasr::ld_clump(iv, clump_r2 = 0.001)

saveRDS(iv.clump,"/rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data/step2/clump.rds")

#significant snp pass clumping threshold 0.01
snps<-iv.clump[,"rsid"]
snps<-as.factor(snps)
write.table(snps,"/rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data/step2/clump_snp.txt",row.names=FALSE, col.names=FALSE, quote=FALSE, sep=" ")





