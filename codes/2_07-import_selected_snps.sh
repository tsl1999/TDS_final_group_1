#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=1:mem=3gb
module load plink

UKB=/rds/general/project/uk-biobank-2018/live/reference/sdata_12032018

for chr in $(seq 1 22)
do
echo $chr

plink --bfile $UKB/ukb_imp_chr$chr --fam clump_39/imp.fam --extract clump_43.txt --make-bed --out clump_43/ukb_imp_chr$chr

done


