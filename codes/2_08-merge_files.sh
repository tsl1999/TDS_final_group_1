cd /rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data/step2/clump_snps

for chr in $(cat chr_list.txt)
do cat ukb_imp_chr${chr}.bim; done > ukb_imp_merged.bim

(echo -en "\x6C\x1B\x01"; for chr in $(cat chr_list.txt)
do tail -c +4 ukb_imp_chr${chr}.bed; done) > ukb_imp_merged.bed
