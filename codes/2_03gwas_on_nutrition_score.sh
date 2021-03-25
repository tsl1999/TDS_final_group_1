#!/bin/bash
#PBS -l walltime=20:00:00
#PBS -l select=1:ncpus=1:mem=10gb
#PBS -N gwas
#PBS -J 1-22

cd /rds/general/project/hda_students_data/live/Group1/UKB_genetic_data_scripts
module load plink

geno_path=/rds/general/project/uk-biobank-2018/live/reference/sdata_12032018
fam_path=/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step2
data_path=/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/result_data/step2
results_path=/rds/general/project/hda_students_data/live/Group1/TDS_final_group_1/gwas_data




plink --bfile $geno_path/ukb_gen_chr$PBS_ARRAY_INDEX \
--fam $fam_path/gene.fam \
--covar $data_path/confounders_age.txt keep-pheno-on-missing-cov --no-const-covar \
--maf 0.01 \
--ci 0.95 \
--covar-name sexMale,age,PC1-PC10 \
--hide-covar \
--linear --out $results_path/linear_geno_chr$PBS_ARRAY_INDEX
