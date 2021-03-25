#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=1:mem=3gb
module load plink

UKB=/rds/general/project/uk-biobank-2018/live/reference/sdata_12032018

cd /rds/general/project/hda_students_data/live/Group1/tds_final_group_1/result_data/step2
for chr in $(seq 1 22)
do
echo $chr

plink --bfile $UKB/ukb_imp_chr$chr --fam clump_snps/imp.fam --extract clump_snp.txt --make-bed --out clump_snps/ukb_imp_chr$chr

done


