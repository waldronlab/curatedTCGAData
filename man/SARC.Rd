\name{ SARC }
\alias{ SARC }
\docType{data}
\title{ SARC }

\details{
\preformatted{
> SARC
A MultiAssayExperiment object of 8 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 8: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 265 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 263 columns 
 [3] CNASNP: RangedRaggedAssay with 337377 rows and 516 columns 
 [4] CNVSNP: RangedRaggedAssay with 106739 rows and 513 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 269 columns 
 [6] RPPAArray: ExpressionSet with 192 rows and 226 columns 
 [7] gistica: SummarizedExperiment with 24776 rows and 256 columns 
 [8] gistict: SummarizedExperiment with 24776 rows and 256 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( SARC )
CharacterList of length 8
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-229075 1:229158-813318 ... 24:2650438-59018259
[["CNVSNP"]] 1:3218610-14668768 1:14671052-14673271 ... 23:142928411-154905589
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( SARC )
CharacterList of length 8
[["RNASeq2GeneNorm"]] TCGA-3B-A9HI-01A-11R-A38C-07 ...
[["miRNASeqGene"]] TCGA-3B-A9HI-01A-11R-A38N-13 ...
[["CNASNP"]] TCGA-3B-A9HI-01A-11D-A386-01 ... TCGA-Z4-AAPG-10A-01D-A38Y-01
[["CNVSNP"]] TCGA-3B-A9HI-01A-11D-A386-01 ... TCGA-Z4-AAPG-10A-01D-A38Y-01
[["Methylation"]] TCGA-3B-A9HI-01A-11D-A388-05 ... TCGA-Z4-AAPG-01A-11D-A39H-05
[["RPPAArray"]] TCGA-3B-A9HI-01A-21-A456-20 ... TCGA-Z4-AAPG-01A-21-A454-20
[["gistica"]] TCGA-3B-A9HI-01A-11D-A386-01 ... TCGA-Z4-AAPG-01A-11D-A38Y-01
[["gistict"]] TCGA-3B-A9HI-01A-11D-A386-01 ... TCGA-Z4-AAPG-01A-11D-A38Y-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.2 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP 27.8 Mb
4          CNVSNP 10.1 Mb
5     Methylation 67.4 Mb
6       RPPAArray  0.1 Mb
7         gistica  4.3 Mb
8         gistict  4.3 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   165 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  96.00   96.00    1.83    1.51    2.46 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   53.00   61.00   60.88   71.00   90.00       1 

vital_status:
  0   1 
165  96 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0   319.5   668.0   851.5  1166.8  2694.0     165 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      2     499     881    1197    1585    5723      76 


gender:
female   male 
   142    119 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1994    2007    2010    2009    2012    2013       4 

radiation_therapy:
  no  yes NA's 
 184   71    6 


residual_tumor:
  r0   r1   r2   rx NA's 
 155   70    9   26    1 

race:
                    asian black or african american                     white 
                        6                        18                       228 
                     NA's 
                        9 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     5                    223                     33 

admin.bcr:
nationwide children's hospital 
                           261 

admin.day_of_dcc_upload:
  1 
261 

admin.disease_code:
sarc 
 261 


admin.month_of_dcc_upload:
 10 
261 

admin.patient_withdrawal.withdrawn:
false 
  261 

admin.project_code:
tcga 
 261 

admin.year_of_dcc_upload:
2015 
 261 


patient.clinical_cqcf.anatomic_neoplasm_subdivision:
other (please specify)                   NA's 
                     1                    260 

patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      254         7 

patient.clinical_cqcf.country:
       canada        russia united states       vietnam 
            4             4           249             4 

patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-918.00    0.00   15.00   54.92   35.00 2048.00      11 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  118.0   435.0   767.0   826.6  1276.5  1478.0     254 


patient.clinical_cqcf.history_of_neoadjuvant_treatment:
 no 
261 

patient.clinical_cqcf.normal_tissue_anatomic_site:
adjacent organ         muscle          other           NA's 
             5             16             11            229 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           32           229 

patient.clinical_cqcf.other_anatomic_site:
visceral- genitourinary                    NA's 
                      1                     260 

patient.clinical_cqcf.tumor_type:
primary 
    261 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   13.00   14.68   23.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32873  -26164  -22414  -22420  -19440   -7530       1 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0   314.0   579.0   817.3  1166.8  2575.0     185 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -13.0   160.0   525.0   831.3  1171.0  5324.0      76 

patient.drugs.drug.10.bcr_drug_barcode:
tcga-ie-a3ov-d42519                NA's 
                  1                 260 

patient.drugs.drug.10.bcr_drug_uuid:
a3aa7dbf-c64a-42f1-abfc-cfbdfac513a6                                 NA's 
                                   1                                  260 

patient.drugs.drug.10.day_of_form_completion:
  23 NA's 
   1  260 

patient.drugs.drug.10.days_to_drug_therapy_end:
2293 NA's 
   1  260 

patient.drugs.drug.10.days_to_drug_therapy_start:
2232 NA's 
   1  260 

patient.drugs.drug.10.drug_name:
alimta   NA's 
     1    260 

patient.drugs.drug.10.measure_of_response:
clinical progressive disease                         NA's 
                           1                          260 

patient.drugs.drug.10.month_of_form_completion:
   4 NA's 
   1  260 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
   1  260 

patient.drugs.drug.10.therapy_types.therapy_type:
chemotherapy         NA's 
           1          260 

patient.drugs.drug.10.tx_on_clinical_trial:
  no NA's 
   1  260 

patient.drugs.drug.10.year_of_form_completion:
2013 NA's 
   1  260 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   13.00   13.26   18.00   29.00     208 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   36.0   181.0   407.0   555.9   773.0  3417.0     212 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    65.0   292.0   414.7   568.0  3256.0     208 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                          20                           14 
            partial response               stable disease 
                           5                            3 
                        NA's 
                         219 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   9.000   8.113  11.000  12.000     208 

patient.drugs.drug.2.number_cycles:
   3    8 NA's 
   1    1  259 

patient.drugs.drug.2.prescribed_dose:
 675  900 NA's 
   1    1  259 

patient.drugs.drug.2.prescribed_dose_units:
 g/m2 mg/m2  NA's 
    1     1   259 

patient.drugs.drug.2.regimen_indication:
adjuvant     NA's 
       2      259 

patient.drugs.drug.2.regimen_number:
   1 NA's 
   2  259 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              259 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  50    3  208 

patient.drugs.drug.2.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
             49               3             209 

patient.drugs.drug.2.total_dose:
3240000    NA's 
      1     260 

patient.drugs.drug.2.total_dose_units:
  mg NA's 
   1  260 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  45    4  212 

patient.drugs.drug.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   5   11   35    2  208 



patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00   13.00   13.42   18.00   30.00     230 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     89     390     567     795    1044    2780     234 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   34.0   288.0   489.0   612.2   806.0  2094.0     230 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                          15                            4 
            partial response               stable disease 
                           4                            3 
                        NA's 
                         235 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   9.000   8.742  11.500  12.000     230 

patient.drugs.drug.3.prescribed_dose:
 2.5 NA's 
   1  260 

patient.drugs.drug.3.prescribed_dose_units:
mg/day   NA's 
     1    260 

patient.drugs.drug.3.regimen_indication:
progression        NA's 
          1         260 

patient.drugs.drug.3.regimen_number:
   3 NA's 
   1  260 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
oral NA's 
   1  260 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  27    4  230 

patient.drugs.drug.3.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         1                         27 
           hormone therapy targeted molecular therapy 
                         1                          2 
                      NA's 
                       230 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  25    4  232 

patient.drugs.drug.3.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    5   24    1  230 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00   13.00   14.05   20.75   30.00     241 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  216.0   451.2   537.5   855.6  1140.0  2886.0     241 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   32.0   359.2   488.0   728.6   925.2  2807.0     241 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease             partial response 
                          14                            3 
              stable disease                         NA's 
                           1                          243 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.75    7.50    7.85   12.00   12.00     241 

patient.drugs.drug.4.number_cycles:
   8 NA's 
   1  260 

patient.drugs.drug.4.prescribed_dose:
 100 NA's 
   1  260 

patient.drugs.drug.4.prescribed_dose_units:
mg/m2  NA's 
    1   260 

patient.drugs.drug.4.regimen_indication:
adjuvant     NA's 
       1      260 

patient.drugs.drug.4.regimen_number:
   1 NA's 
   1  260 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              260 

patient.drugs.drug.4.therapy_ongoing:
  no NA's 
  20  241 

patient.drugs.drug.4.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        17                          1 
             immunotherapy targeted molecular therapy 
                         1                          1 
                      NA's 
                       241 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
  16    3  242 

patient.drugs.drug.4.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    2   15    2  241 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.25   14.00   14.56   20.75   30.00     245 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  228.0   524.5   945.0  1087.7  1286.0  3026.0     246 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  123.0   485.8   820.0   990.2  1333.8  2898.0     245 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease                         NA's 
                          11                          250 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.75    7.50    7.75   11.25   12.00     245 

patient.drugs.drug.5.number_cycles:
  11 NA's 
   1  260 

patient.drugs.drug.5.prescribed_dose:
 2.8 NA's 
   1  260 

patient.drugs.drug.5.prescribed_dose_units:
g/day  NA's 
    1   260 

patient.drugs.drug.5.regimen_indication:
progression        NA's 
          1         260 

patient.drugs.drug.5.regimen_number:
   4 NA's 
   1  260 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              260 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
  15    1  245 

patient.drugs.drug.5.therapy_types.therapy_type:
chemotherapy         NA's 
          16          245 

patient.drugs.drug.5.tx_on_clinical_trial:
  no  yes NA's 
  13    2  246 

patient.drugs.drug.5.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    2   12    1  245 



patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   13.00   18.00   16.25   20.75   24.00     253 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  228.0   426.5  1126.0  1134.0  1667.5  2396.0     254 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  123.0   318.5  1289.0  1381.5  1785.5  3939.0     253 


patient.drugs.drug.6.measure_of_response:
clinical progressive disease                         NA's 
                           7                          254 

patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.500   7.500   7.125   9.250  12.000     253 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
   7    1  253 

patient.drugs.drug.6.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         7                          1 
                      NA's 
                       253 

patient.drugs.drug.6.tx_on_clinical_trial:
  no  yes NA's 
   7    1  253 

patient.drugs.drug.6.year_of_form_completion:
2013 2014 2015 NA's 
   1    6    1  253 

patient.drugs.drug.7.bcr_drug_barcode:
tcga-3b-a9hr-d68594 tcga-dx-a3u7-d65092 tcga-ie-a3ov-d42516                NA's 
                  1                   1                   1                 258 

patient.drugs.drug.7.bcr_drug_uuid:
8708aee3-c54b-4cf4-b059-2c68bb6dbb32 95942730-d152-44c2-b097-cee4df9e2ef1 
                                   1                                    1 
b7432680-8022-4606-9337-709b9e42f436                                 NA's 
                                   1                                  258 

patient.drugs.drug.7.day_of_form_completion:
   1   18   23 NA's 
   1    1    1  258 

patient.drugs.drug.7.days_to_drug_therapy_end:
1182 1918 2683 NA's 
   1    1    1  258 

patient.drugs.drug.7.days_to_drug_therapy_start:
1133 1834 2396 NA's 
   1    1    1  258 

patient.drugs.drug.7.drug_name:
 ezn-2968 pazopanib      NA's 
        1         2       258 

patient.drugs.drug.7.measure_of_response:
clinical progressive disease                         NA's 
                           3                          258 

patient.drugs.drug.7.month_of_form_completion:
   4    9   12 NA's 
   1    1    1  258 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   3  258 

patient.drugs.drug.7.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         2                          1 
                      NA's 
                       258 

patient.drugs.drug.7.tx_on_clinical_trial:
  no  yes NA's 
   2    1  258 

patient.drugs.drug.7.year_of_form_completion:
2013 2014 NA's 
   1    2  258 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-dx-a3u7-d65093 tcga-ie-a3ov-d42517                NA's 
                  1                   1                 259 

patient.drugs.drug.8.bcr_drug_uuid:
42e7d909-7056-4ab8-af20-a37a812a4554 b93ea1cc-b0ae-47e1-86b7-c6b65907d6cb 
                                   1                                    1 
                                NA's 
                                 259 

patient.drugs.drug.8.day_of_form_completion:
  18   23 NA's 
   1    1  259 

patient.drugs.drug.8.days_to_drug_therapy_end:
1301 2037 NA's 
   1    1  259 

patient.drugs.drug.8.days_to_drug_therapy_start:
1258 1973 NA's 
   1    1  259 

patient.drugs.drug.8.drug_name:
dacarbazine  enoticumab        NA's 
          1           1         259 

patient.drugs.drug.8.measure_of_response:
clinical progressive disease                         NA's 
                           2                          259 

patient.drugs.drug.8.month_of_form_completion:
   4    9 NA's 
   1    1  259 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   2  259 

patient.drugs.drug.8.therapy_types.therapy_type:
 chemotherapy immunotherapy          NA's 
            1             1           259 

patient.drugs.drug.8.tx_on_clinical_trial:
  no  yes NA's 
   1    1  259 

patient.drugs.drug.8.year_of_form_completion:
2013 2014 NA's 
   1    1  259 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-dx-a3u7-d65094 tcga-ie-a3ov-d42518                NA's 
                  1                   1                 259 

patient.drugs.drug.9.bcr_drug_uuid:
00bc2544-855a-4e58-89e4-f5dc86a187cf e347a30f-d343-4dcf-bcc4-3a650f6f36b0 
                                   1                                    1 
                                NA's 
                                 259 

patient.drugs.drug.9.day_of_form_completion:
  18   23 NA's 
   1    1  259 

patient.drugs.drug.9.days_to_drug_therapy_end:
1427 2093 NA's 
   1    1  259 

patient.drugs.drug.9.days_to_drug_therapy_start:
1352 2073 NA's 
   1    1  259 

patient.drugs.drug.9.drug_name:
   ifosfamide threshold-302          NA's 
            1             1           259 

patient.drugs.drug.9.measure_of_response:
clinical progressive disease                         NA's 
                           2                          259 

patient.drugs.drug.9.month_of_form_completion:
   4    9 NA's 
   1    1  259 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   2  259 

patient.drugs.drug.9.therapy_types.therapy_type:
chemotherapy         NA's 
           2          259 

patient.drugs.drug.9.tx_on_clinical_trial:
  no  yes NA's 
   1    1  259 

patient.drugs.drug.9.year_of_form_completion:
2013 2014 NA's 
   1    1  259 



patient.drugs.drug.clinical_trail_drug_classification:
chemotherapy       th-302         NA's 
           1            2          258 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.75   13.00   13.14   18.00   31.00     189 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   45.0   174.5   270.0   475.5   629.5  2293.0     201 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    82.0   248.0   426.9   574.0  3701.0     190 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          23                           19 
            partial response               stable disease 
                           5                            6 
                        NA's 
                         208 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.750   9.000   7.833  10.250  12.000     189 

patient.drugs.drug.number_cycles:
   1    3 NA's 
   1    1  259 

patient.drugs.drug.prescribed_dose:
  50   80 NA's 
   1    1  259 

patient.drugs.drug.prescribed_dose_units:
 g/m2 mg/m2  NA's 
    1     1   259 

patient.drugs.drug.regimen_indication:
   adjuvant progression        NA's 
          1           1         259 

patient.drugs.drug.regimen_number:
   1    2 NA's 
   1    1  259 

patient.drugs.drug.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              259 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
  63    9  189 


patient.drugs.drug.total_dose:
384000   NA's 
     1    260 

patient.drugs.drug.total_dose_units:
  mg NA's 
   1  260 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  59    8  194 

patient.drugs.drug.year_of_form_completion:
2012 2013 2014 2015 NA's 
   8   14   47    3  189 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     5                    223                     33 

patient.follow_ups.follow_up.2.additional_treatment_completion_success_outcome:
  complete response    partial response progressive disease      stable disease 
                 44                   1                  15                   1 
               NA's 
                200 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      2      13      19      18      25      31     200 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  224.0   478.0   738.0   932.6  1320.0  1970.0     254 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   56.0   674.5  1179.5  1364.3  1831.0  4573.0     207 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  58    2  201 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   9.000   6.721   9.000  12.000     200 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    6  254 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   7  254 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
2218 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  315.0   523.8  1080.0  1334.9  1769.2  3590.0     253 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.discontiguous_lesion_count:
   4 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
          bone          liver           lung other, specify           NA's 
             1              2              1              4            253 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      4                       4                     253 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
retroperitoneum            NA's 
              4             257 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_cellular_differentiation:
de-differentiated              NA's 
                1               260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   6    1  254 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.radiologic_tumor_burden:
 6.2 22.4 NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.tumor_multifocal:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    3  257 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   4  257 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
 662 2509 NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 349  876  916 2480 NA's 
   1    1    1    1  257 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
         liver           lung other, specify           NA's 
             1              2              1            257 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      3                       1                     257 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
retroperitoneum            NA's 
              1             260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_tumor_cellular_differentiation:
de-differentiated              NA's 
                1               260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    2  257 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.radiologic_tumor_burden:
 5.8 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.residual_disease_post_new_tumor_event_margin_status:
  r0   r2 NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.tumor_multifocal:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
 yes NA's 
   2  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no NA's 
   2  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_additional_surgery_procedure:
 626 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 349 1027 NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    2   259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.tumor_multifocal:
  no NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.additional_pharmaceutical_therapy:
 yes NA's 
   2  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.additional_radiation_therapy:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_additional_surgery_procedure:
 662 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
 605 1287 NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             1              1            259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.new_neoplasm_occurrence_anatomic_site_text:
intercostal        NA's 
          1         260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.residual_disease_post_new_tumor_event_margin_status:
  r2 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.5.tumor_multifocal:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.6.additional_pharmaceutical_therapy:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.6.additional_radiation_therapy:
  no NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_after_initial_treatment:
 740 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.6.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    1   260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.6.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.6.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.7.additional_pharmaceutical_therapy:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.7.additional_radiation_therapy:
  no NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.7.days_to_new_tumor_event_after_initial_treatment:
1031 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.7.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.7.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.7.new_neoplasm_occurrence_anatomic_site_text:
peritoneum       NA's 
         1        260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.7.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.8.additional_pharmaceutical_therapy:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.8.additional_radiation_therapy:
  no NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.8.days_to_new_tumor_event_after_initial_treatment:
1031 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.8.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.8.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.8.new_neoplasm_occurrence_anatomic_site_text:
kidney   NA's 
     1    260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.8.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  11    6  244 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  16    1  244 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  173.0   449.0   564.0   890.4  1089.2  3234.0     249 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  116.0   297.5   411.5   573.7   753.2  1407.0     241 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.discontiguous_lesion_count:
   2    3 NA's 
   3    1  257 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
          bone          liver           lung other, specify           NA's 
             1              1              5             11            243 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                     10                      10                     241 


patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_cellular_differentiation:
  de-differentiated well-differentiated                NA's 
                  2                   3                 256 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_cellular_differentiation.2:
de-differentiated              NA's 
                2               259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   6   12  243 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.pathologic_tumor_burden:
 3.1  6.1   19 NA's 
   1    1    1  258 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.radiologic_tumor_burden:
 2.7  2.8 NA's 
   1    1  259 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r1   r2 NA's 
  10    1    1  249 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.tumor_multifocal:
  no  yes NA's 
   8    5  248 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.2.pathologic_tumor_length:
 1.2 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.2.radiologic_tumor_length:
 1.1 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.pathologic_tumor_length:
 1.9 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.radiologic_tumor_length:
 1.6 NA's 
   1  260 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  40   20  201 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        43         17        201 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  48   12  201 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  44   16  201 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   54     7   200 

patient.follow_ups.follow_up.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    4   32   24  200 

patient.follow_ups.follow_up.3.additional_treatment_completion_success_outcome:
  complete response progressive disease      stable disease                NA's 
                  5                   2                   1                 253 



patient.follow_ups.follow_up.3.day_of_form_completion:
   5   13   22   23 NA's 
   3    2    2    1  253 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  478.0   526.2  1045.5  1101.8  1348.2  2491.0     253 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
   7  254 

patient.follow_ups.follow_up.3.month_of_form_completion:
   2    4   11 NA's 
   3    3    2  253 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   2  259 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   2  259 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 132 1792 NA's 
   1    1  259 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             2            259 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                259 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
         pleura retroperitoneum            NA's 
              1               1             259 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  259 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   1  260 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 132 NA's 
   1  260 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            260 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                260 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
heart  NA's 
    1   260 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  260 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    3  256 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   5  256 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 805 1035 1318 NA's 
   1    1    1  258 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    132     668     683     750     964    1303     256 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             2              3            256 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      3                       2                     256 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
         axilla retroperitoneum       upper arm            NA's 
              1               1               1             258 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    3  256 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r1 NA's 
   2    1  258 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.tumor_multifocal:
  no NA's 
   3  258 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   3    5  253 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         4          4        253 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no  yes NA's 
   7    1  253 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
   6    2  253 

patient.follow_ups.follow_up.3.vital_status:
alive  NA's 
    8   253 

patient.follow_ups.follow_up.3.year_of_form_completion:
2012 2014 2015 NA's 
   1    2    5  253 

patient.follow_ups.follow_up.4.additional_treatment_completion_success_outcome:
progressive disease                NA's 
                  1                 260 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-hb-a3l4-f31060                NA's 
                  1                 260 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
c7bc61a3-004d-43d7-985d-deac26b32a54                                 NA's 
                                   1                                  260 

patient.follow_ups.follow_up.4.day_of_form_completion:
  23 NA's 
   1  260 

patient.follow_ups.follow_up.4.days_to_last_followup:
 861 NA's 
   1  260 

patient.follow_ups.follow_up.4.month_of_form_completion:
   4 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 572 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 489 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.discontiguous_lesion_count:
   2 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    1   260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.pathologic_tumor_burden:
  14 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.radiologic_tumor_burden:
13.9 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_multifocal:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.2.pathologic_tumor_depth:
 3.5 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.2.pathologic_tumor_length:
 5.5 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.2.pathologic_tumor_width:
   4 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.2.radiologic_tumor_length:
 4.9 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.2.radiologic_tumor_width:
 4.3 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.pathologic_tumor_depth:
   6 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.pathologic_tumor_length:
 8.5 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.pathologic_tumor_width:
   7 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.radiologic_tumor_length:
   9 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.tumor_sizes.tumor_size.radiologic_tumor_width:
 7.1 NA's 
   1  260 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
with tumor       NA's 
         1        260 

patient.follow_ups.follow_up.4.postoperative_rx_tx:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.4.radiation_therapy:
  no NA's 
   1  260 

patient.follow_ups.follow_up.4.vital_status:
alive  NA's 
    1   260 

patient.follow_ups.follow_up.4.year_of_form_completion:
2012 NA's 
   1  260 

patient.follow_ups.follow_up.5.additional_treatment_completion_success_outcome:
progressive disease                NA's 
                  1                 260 

patient.follow_ups.follow_up.5.bcr_followup_barcode:
tcga-hb-a3l4-f38211                NA's 
                  1                 260 

patient.follow_ups.follow_up.5.bcr_followup_uuid:
9d28eca9-c9f2-48ee-8a88-5afee5d7399d                                 NA's 
                                   1                                  260 

patient.follow_ups.follow_up.5.day_of_form_completion:
  11 NA's 
   1  260 

patient.follow_ups.follow_up.5.days_to_death:
1061 NA's 
   1  260 

patient.follow_ups.follow_up.5.lost_follow_up:
  no NA's 
   1  260 

patient.follow_ups.follow_up.5.month_of_form_completion:
  12 NA's 
   1  260 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.5.person_neoplasm_cancer_status:
with tumor       NA's 
         1        260 

patient.follow_ups.follow_up.5.postoperative_rx_tx:
 yes NA's 
   1  260 

patient.follow_ups.follow_up.5.radiation_therapy:
  no NA's 
   1  260 

patient.follow_ups.follow_up.5.vital_status:
dead NA's 
   1  260 

patient.follow_ups.follow_up.5.year_of_form_completion:
2012 NA's 
   1  260 

patient.follow_ups.follow_up.additional_treatment_completion_success_outcome:
  complete response    partial response progressive disease      stable disease 
                128                   2                  58                  11 
               NA's 
                 62 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0    15.0    15.3    25.0    31.0      56 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   295.2   510.0   808.1  1050.8  2694.0     227 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     15     485     805    1142    1533    5723      92 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 180   18   63 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   7.161  10.000  12.000      56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   2   10  249 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
  11    1  249 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  390.0   740.5   959.5  1151.7  1520.5  2226.0     255 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   98.0   335.0   516.5   602.5   764.5  1857.0     241 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.discontiguous_lesion_count:
   1    2    4 NA's 
   1    1    1  258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
          bone          liver           lung other, specify           NA's 
             1              6              3              6            245 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                     17                       3                     241 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_cellular_differentiation:
de-differentiated              NA's 
                2               259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   8    6  247 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.radiologic_tumor_burden:
 1.4  3.4 NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.residual_disease_post_new_tumor_event_margin_status:
  r0   r1   rx NA's 
   2    1    1  257 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.tumor_multifocal:
  no NA's 
   8  253 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    5  255 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no  yes NA's 
   3    3  255 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
1147 1371 1637 NA's 
   1    1    1  258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  772.0   900.5  1029.0  1175.9  1327.5  1974.0     254 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.discontiguous_lesion_count:
   1    2 NA's 
   2    1  258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
          bone           lung other, specify           NA's 
             1              3              3            254 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      5                       2                     254 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
                     arm left gluteal muscle mass               left thigh 
                       1                        1                        1 
                  sacrum                     NA's 
                       1                      257 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   4    3  254 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.radiologic_tumor_burden:
 3.9  6.5 NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.residual_disease_post_new_tumor_event_margin_status:
  r0   r1 NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.tumor_multifocal:
  no  yes NA's 
   4    1  256 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
 yes NA's 
   4  257 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no NA's 
   4  257 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_additional_surgery_procedure:
1785 NA's 
   1  260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 772  791 1492 1785 NA's 
   1    1    1    1  257 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.discontiguous_lesion_count:
   1    2 NA's 
   2    1  258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
          bone          liver other, specify           NA's 
             1              1              2            257 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 4                257 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_occurrence_anatomic_site_text:
               5th rib axillary ln/chest wall            left kidney 
                     1                      1                      1 
                  NA's 
                   258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   3    1  257 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.radiologic_tumor_burden:
 2.7 NA's 
   1  260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.tumor_multifocal:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.additional_pharmaceutical_therapy:
 yes NA's 
   3  258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.additional_radiation_therapy:
  no NA's 
   3  258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_additional_surgery_procedure:
2197 NA's 
   1  260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
 843 1414 2197 NA's 
   1    1    1  258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.discontiguous_lesion_count:
   2 NA's 
   1  260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             2              1            258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
distant metastasis               NA's 
                 3                258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_neoplasm_occurrence_anatomic_site_text:
right flank and abdominal wall                           NA's 
                             1                            260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    1  258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.tumor_multifocal:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.additional_pharmaceutical_therapy:
 yes NA's 
   2  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.additional_radiation_therapy:
  no NA's 
   2  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_additional_surgery_procedure:
2197 NA's 
   1  260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_after_initial_treatment:
 843 2197 NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.discontiguous_lesion_count:
   1 NA's 
   1  260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             2            259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      1                       1                     259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_neoplasm_occurrence_anatomic_site_text:
left gluteal muscle mass                     neck                     NA's 
                       1                        1                      259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.radiologic_tumor_burden:
 4.8 NA's 
   1  260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.tumor_multifocal:
  no NA's 
   2  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.additional_pharmaceutical_therapy:
  no NA's 
   2  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.additional_radiation_therapy:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.days_to_new_tumor_event_after_initial_treatment:
 975 2435 NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.new_neoplasm_event_occurrence_anatomic_site:
liver  lung  NA's 
    1     1   259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.tumor_multifocal:
  no NA's 
   2  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.additional_pharmaceutical_therapy:
  no NA's 
   2  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.additional_radiation_therapy:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.days_to_new_tumor_event_after_initial_treatment:
 975 2435 NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.discontiguous_lesion_count:
   1 NA's 
   1  260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             2            259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      1                       1                     259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_neoplasm_occurrence_anatomic_site_text:
left gluteal muscle mass          paraspinal mets                     NA's 
                       1                        1                      259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.radiologic_tumor_burden:
 5.4 NA's 
   1  260 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.tumor_multifocal:
  no  yes NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  19   23  219 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  36    5  220 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  242.0   448.0   576.0   769.2   862.0  2681.0     240 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   196.2   336.5   520.8   603.8  2659.0     185 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.discontiguous_lesion_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   3.500   4.833   4.000  23.000     249 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis    locoregional disease locoregional recurrence 
                     41                       1                      28 
      new primary tumor                    NA's 
                      5                     186 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_cellular_differentiation:
  de-differentiated well-differentiated                NA's 
                  5                   1                 255 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  23   21  217 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.pathologic_tumor_burden:
 1.9 13.5 NA's 
   1    1  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.radiologic_tumor_burden:
 4.5   16 31.7 NA's 
   1    1    1  258 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r1   rx NA's 
  14    2    4  241 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.tumor_multifocal:
  no  yes NA's 
  15   12  234 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 113   78   70 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       117         75         69 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
 154   40   67 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 145   48   68 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  169    34    58 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2014    2014    2014    2015      56 

patient.gender:
female   male 
   142    119 

patient.history_of_neoadjuvant_treatment:
 no yes 
260   1 




patient.informed_consent_verified:
yes 
261 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   5.088   6.000  12.000 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   9   14  238 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
  24    2  235 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  106.0   328.5   517.5   737.2  1211.8  1646.0     253 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   27.0   170.5   336.0   448.0   731.0  1372.0     222 

patient.new_tumor_events.new_tumor_event.2.discontiguous_lesion_count:
   1    2    3 NA's 
   4    1    1  255 


patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     28                      10                       1 
                   NA's 
                    222 


patient.new_tumor_events.new_tumor_event.2.new_tumor_cellular_differentiation:
  de-differentiated well-differentiated                NA's 
                  3                   2                 256 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  21    8  232 

patient.new_tumor_events.new_tumor_event.2.pathologic_tumor_burden:
 0.7 10.5   14 NA's 
   1    1    1  258 

patient.new_tumor_events.new_tumor_event.2.radiologic_tumor_burden:
 0.5  1.1    2 NA's 
   1    1    1  258 

patient.new_tumor_events.new_tumor_event.2.residual_disease_post_new_tumor_event_margin_status:
  r0   r1   rx NA's 
   5    1    1  254 

patient.new_tumor_events.new_tumor_event.2.tumor_multifocal:
  no  yes NA's 
   8    3  250 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   3   10  248 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no  yes NA's 
  11    4  246 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
 306  577 1147 2003 NA's 
   1    1    1    1  257 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  103.0   308.0   785.0   786.7  1022.5  2055.0     242 

patient.new_tumor_events.new_tumor_event.3.discontiguous_lesion_count:
   1    2    3 NA's 
   1    1    1  258 


patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                     14                       5                     242 


patient.new_tumor_events.new_tumor_event.3.new_tumor_cellular_differentiation:
  de-differentiated well-differentiated                NA's 
                  1                   1                 259 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  10    4  247 

patient.new_tumor_events.new_tumor_event.3.pathologic_tumor_burden:
 0.4 NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.3.radiologic_tumor_burden:
 6.7 NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.3.residual_disease_post_new_tumor_event_margin_status:
  r0   r1   r2 NA's 
   2    2    1  256 

patient.new_tumor_events.new_tumor_event.3.tumor_multifocal:
  no  yes NA's 
   3    4  254 

patient.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    4  256 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no  yes NA's 
   5    1  255 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  103.0   515.5   788.0   763.7  1023.0  1476.0     251 

patient.new_tumor_events.new_tumor_event.4.discontiguous_lesion_count:
   2 NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
          bone other, specify           NA's 
             3              5            253 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      8                       2                     251 


patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no NA's 
   7  254 

patient.new_tumor_events.new_tumor_event.4.pathologic_tumor_burden:
80.5 NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.4.residual_disease_post_new_tumor_event_margin_status:
  r2 NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.4.tumor_multifocal:
 yes NA's 
   2  259 

patient.new_tumor_events.new_tumor_event.5.additional_pharmaceutical_therapy:
 yes NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.5.additional_radiation_therapy:
  no NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_additional_surgery_procedure:
 635 NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
 511  626 1035 1102 NA's 
   1    1    1    1  257 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site:
         liver other, specify           NA's 
             1              3            257 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
distant metastasis               NA's 
                 4                257 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_occurrence_anatomic_site_text:
      buttock lower abdomen      pancreas          NA's 
            1             1             1           258 

patient.new_tumor_events.new_tumor_event.5.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    1  258 

patient.new_tumor_events.new_tumor_event.5.residual_disease_post_new_tumor_event_margin_status:
  r1 NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.5.tumor_multifocal:
  no NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_after_initial_treatment:
 942 1102 NA's 
   1    1  259 

patient.new_tumor_events.new_tumor_event.6.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             1              1            259 

patient.new_tumor_events.new_tumor_event.6.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                259 

patient.new_tumor_events.new_tumor_event.6.new_neoplasm_occurrence_anatomic_site_text:
pelvis   NA's 
     1    260 

patient.new_tumor_events.new_tumor_event.6.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  260 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  23   32  206 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  44    8  209 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -59.0   264.5   523.0   665.4   758.5  2682.0     226 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.0   130.5   284.0   410.3   513.5  2682.0     174 

patient.new_tumor_events.new_tumor_event.discontiguous_lesion_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.533   3.000   5.000     246 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     50                      32                       6 
                   NA's 
                    173 


patient.new_tumor_events.new_tumor_event.new_tumor_cellular_differentiation:
  de-differentiated well-differentiated                NA's 
                  5                   6                 250 

patient.new_tumor_events.new_tumor_event.new_tumor_cellular_differentiation.2:
de-differentiated              NA's 
                2               259 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  28   37  196 

patient.new_tumor_events.new_tumor_event.pathologic_tumor_burden:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.800   3.325   6.700   8.493  11.000  30.600     247 

patient.new_tumor_events.new_tumor_event.radiologic_tumor_burden:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   2.100   2.700   4.750   4.825  17.000     253 

patient.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   rx NA's 
  19    9  233 

patient.new_tumor_events.new_tumor_event.tumor_multifocal:
  no  yes NA's 
  23   13  225 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 129   89   43 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       132         99         30 

patient.postoperative_rx_tx:
  no  yes NA's 
 178   36   47 

patient.primary_pathology.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  20.00   53.00   61.00   60.87   71.00   90.00 

patient.primary_pathology.contiguous_organ_invaded:
  no  yes NA's 
  43   16  202 


patient.primary_pathology.days_to_initial_pathologic_diagnosis:
  0 
261 

patient.primary_pathology.days_to_well_differentiated_liposarcoma_primary_dx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -7573   -5058   -3991   -4339   -2763   -2310     256 

patient.primary_pathology.days_to_well_differentiated_liposarcoma_resection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -7573   -4649   -3544   -4100   -2846   -2310     255 

patient.primary_pathology.discontiguous_lesion_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.500   1.958   3.000   9.000     213 


patient.primary_pathology.leiomyosarcoma_histologic_subtype:
                                       conventional leiomyosarcoma 
                                                                66 
poorly differentiated or pleomorphic or epithelioid leiomyosarcoma 
                                                                34 
         well-differentiated leiomyosarcoma (resembling leiomyoma) 
                                                                 4 
                                                              NA's 
                                                               157 


patient.primary_pathology.local_disease_recurrence:
  no  yes NA's 
 145   29   87 

patient.primary_pathology.margin_status:
negative positive     NA's 
     137       74       50 

patient.primary_pathology.metastatic_neoplasm_confirmed:
  no  yes NA's 
 122   56   83 

patient.primary_pathology.metastatic_site_at_diagnosis:
          bone          liver           lung other, specify           NA's 
             2              2             40             11            206 


patient.primary_pathology.mitotic_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    8.00   18.50   22.75   30.00  102.00     171 


patient.primary_pathology.pathologic_tumor_burden:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.60    7.55   11.00   24.53   18.90  800.00     186 

patient.primary_pathology.primary_tumor_lower_uterus_segment:
  no  yes NA's 
  71   30  160 

patient.primary_pathology.radiologic_tumor_burden:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.575   9.250  26.756  13.625 800.000     213 

patient.primary_pathology.residual_tumor:
  r0   r1   r2   rx NA's 
 155   70    9   26    1 

patient.primary_pathology.specific_tumor_total_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   15.00   19.46   30.00   80.00     165 

patient.primary_pathology.synovial_sarcoma.mpnst_exisiting_plexiform_neurofibroma:
  no  yes NA's 
   4    4  253 

patient.primary_pathology.synovial_sarcoma.mpnst_neurofibromatosis:
 nf1   no NA's 
   6    4  251 

patient.primary_pathology.synovial_sarcoma.mpnst_neurofibromatosis_heredity:
familial sporadic     NA's 
       4        2      255 

patient.primary_pathology.synovial_sarcoma.mpnst_nf1_genetic_testing:
                       no yes, mutations identified                      NA's 
                        7                         1                       253 

patient.primary_pathology.synovial_sarcoma.mpnst_specific_mutations:
heterozygous truncating mutation,                              NA's 
                                1                               260 

patient.primary_pathology.synovial_sarcoma.ss18_ssx_fusion_status:
positive - ss18-ssx2                 NA's 
                   1                  260 

patient.primary_pathology.synovial_sarcoma.ss18_ssx_testing_method:
fish for ss18 split              rt-pcr                NA's 
                  2                   1                 258 

patient.primary_pathology.tumor_depth:
       deep superficial        NA's 
        159          20          82 

patient.primary_pathology.tumor_multifocal:
  no  yes NA's 
 173   29   59 

patient.primary_pathology.tumor_sizes.tumor_size.2.pathologic_tumor_depth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   0.975   2.050   2.550   3.775   7.500     243 

patient.primary_pathology.tumor_sizes.tumor_size.2.pathologic_tumor_length:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.100   3.000   4.096   6.100  13.000     238 

patient.primary_pathology.tumor_sizes.tumor_size.2.pathologic_tumor_width:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   1.250   3.000   3.905   5.800  11.000     242 

patient.primary_pathology.tumor_sizes.tumor_size.2.radiologic_tumor_length:
 1.4 NA's 
   1  260 

patient.primary_pathology.tumor_sizes.tumor_size.2.radiologic_tumor_width:
   1 NA's 
   1  260 

patient.primary_pathology.tumor_sizes.tumor_size.3.pathologic_tumor_depth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   2.000   2.100   3.244   4.000   8.700     252 

patient.primary_pathology.tumor_sizes.tumor_size.3.pathologic_tumor_length:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   4.250   5.000   7.291   8.750  23.000     250 

patient.primary_pathology.tumor_sizes.tumor_size.3.pathologic_tumor_width:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   3.500   3.500   4.544   5.000  10.000     252 

patient.primary_pathology.tumor_sizes.tumor_size.4.pathologic_tumor_depth:
   1  9.5 NA's 
   1    1  259 

patient.primary_pathology.tumor_sizes.tumor_size.4.pathologic_tumor_length:
   2   18 NA's 
   2    1  258 

patient.primary_pathology.tumor_sizes.tumor_size.4.pathologic_tumor_width:
 1.5   14 NA's 
   2    1  258 

patient.primary_pathology.tumor_sizes.tumor_size.pathologic_tumor_depth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   3.800   6.000   6.608   9.000  19.000      60 

patient.primary_pathology.tumor_sizes.tumor_size.pathologic_tumor_length:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.50    6.90   10.00   12.24   17.00   39.50      45 

patient.primary_pathology.tumor_sizes.tumor_size.pathologic_tumor_width:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   5.100   8.000   9.439  12.000  30.000      55 

patient.primary_pathology.tumor_sizes.tumor_size.radiologic_tumor_depth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.900   3.375   7.850   7.843  10.625  25.000     185 

patient.primary_pathology.tumor_sizes.tumor_size.radiologic_tumor_length:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.80    5.00   10.10   10.89   14.85   30.00     150 

patient.primary_pathology.tumor_sizes.tumor_size.radiologic_tumor_width:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.100   4.275   7.850   8.834  12.350  27.800     155 




patient.primary_pathology.tumor_tissue_sites.tumor_tissue_site.3:
                        lower abdominal/pelvic - pelvic 
                                                      1 
retroperitoneum/upper abdominal - other (please specify 
                                                      2 
                                                   NA's 
                                                    258 

patient.primary_pathology.tumor_total_necrosis_percent:
                     <10% (focal necrosis) 
                                        39 
0% (no necrosis or no mention of necrosis) 
                                        70 
                 extensive necrosis (>50%) 
                                        12 
            moderate necrosis (>=10, <50%) 
                                        61 
                                      NA's 
                                        79 

patient.primary_pathology.well_differentiated_liposarcoma_primary_dx:
  no  yes NA's 
  38    5  218 

patient.primary_pathology.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1994    2007    2010    2009    2012    2013       4 

patient.prior_dx:
 no yes 
219  42 

patient.race:
                    asian black or african american                     white 
                        6                        18                       228 
                     NA's 
                        9 

patient.radiation_therapy:
  no  yes NA's 
 142   73   46 




patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00   12.00   12.15   17.00   26.00     248 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   90.0   165.0   256.0   630.9   671.0  2390.0     248 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0   125.0   235.0   600.9   660.0  2348.0     248 

patient.radiations.radiation.2.measure_of_response:
               complete response                 partial response 
                               3                                1 
radiographic progressive disease                   stable disease 
                               2                                5 
                            NA's 
                             250 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   9.000   7.462  11.000  12.000     248 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   23.00   19.91   30.00   32.00     250 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   53.61 4000.00 3310.54 5280.00 6400.00     249 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
  13  248 

patient.radiations.radiation.2.radiation_type:
external internal     NA's 
      11        1      249 

patient.radiations.radiation.2.units:
 cgy   gy  mci NA's 
   7    3    1  250 

patient.radiations.radiation.2.year_of_form_completion:
2012 2014 2015 NA's 
   1   10    2  248 

patient.radiations.radiation.3.anatomic_treatment_site:
 distant recurrence primary tumor field                NA's 
                  2                   1                 258 

patient.radiations.radiation.3.bcr_radiation_barcode:
tcga-dx-a6yv-r65005 tcga-si-a71q-r71518 tcga-x6-a7wc-r67911                NA's 
                  1                   1                   1                 258 

patient.radiations.radiation.3.bcr_radiation_uuid:
1ebe76a7-57c1-4474-8fe0-907716d6173c 7245d684-4abf-429e-8b6d-9b2ec92b241e 
                                   1                                    1 
cef0387d-0c63-4c5b-8aa5-edad0d55b11c                                 NA's 
                                   1                                  258 

patient.radiations.radiation.3.day_of_form_completion:
  12   17   26 NA's 
   1    1    1  258 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
 175  895 1387 NA's 
   1    1    1  258 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
 164  886 1369 NA's 
   1    1    1  258 

patient.radiations.radiation.3.measure_of_response:
               complete response radiographic progressive disease 
                               1                                1 
                            NA's 
                             259 

patient.radiations.radiation.3.month_of_form_completion:
   3    9   11 NA's 
   1    1    1  258 

patient.radiations.radiation.3.numfractions:
   5 NA's 
   1  260 

patient.radiations.radiation.3.radiation_dosage:
  50 1440 NA's 
   1    1  259 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   3  258 

patient.radiations.radiation.3.radiation_type:
external     NA's 
       3      258 

patient.radiations.radiation.3.units:
  gy NA's 
   1  260 

patient.radiations.radiation.3.year_of_form_completion:
2014 2015 NA's 
   2    1  258 

patient.radiations.radiation.4.anatomic_treatment_site:
distant recurrence               NA's 
                 1                260 

patient.radiations.radiation.4.bcr_radiation_barcode:
tcga-x6-a7wc-r67916                NA's 
                  1                 260 

patient.radiations.radiation.4.bcr_radiation_uuid:
8ed3b09f-419e-4e91-b3d5-78d4aa94e280                                 NA's 
                                   1                                  260 

patient.radiations.radiation.4.day_of_form_completion:
  12 NA's 
   1  260 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
1051 NA's 
   1  260 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
1042 NA's 
   1  260 

patient.radiations.radiation.4.measure_of_response:
partial response             NA's 
               1              260 

patient.radiations.radiation.4.month_of_form_completion:
  11 NA's 
   1  260 

patient.radiations.radiation.4.numfractions:
   5 NA's 
   1  260 

patient.radiations.radiation.4.radiation_dosage:
  50 NA's 
   1  260 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   1  260 

patient.radiations.radiation.4.radiation_type:
external     NA's 
       1      260 

patient.radiations.radiation.4.units:
  gy NA's 
   1  260 

patient.radiations.radiation.4.year_of_form_completion:
2014 NA's 
   1  260 




patient.radiations.radiation.course_number:
   1 NA's 
   1  260 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     5.5    13.0    13.5    18.0    30.0     179 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   114.0   132.0   239.2   167.0  2438.0     180 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.0    61.0    92.5   198.7   123.5  2438.0     181 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              41                                2 
radiographic progressive disease                   stable disease 
                              12                                6 
                            NA's 
                             200 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.293   9.000  12.000     179 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    28.5    31.5    29.1    35.0    39.0     199 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.6  4500.0  6000.0  4846.1  6300.0  8640.0     188 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  82  179 

patient.radiations.radiation.radiation_type:
     external external beam      internal          NA's 
           75             1             1           184 

patient.radiations.radiation.regimen_indication:
adjuvant     NA's 
       1      260 

patient.radiations.radiation.units:
 cgy   gy NA's 
  62   10  189 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2014    2014    2014    2014    2015     179 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 181   60   20 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
  60  181   20 


patient.vital_status:
alive  dead 
  185    76 

patient.year_of_form_completion:
2011 2012 2013 2014 
   2   39   83  137 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
      case matched expression control      case matched methylation control 
                                    2                                     3 
primary tumor and metastatic tumor(s)  primary tumor and recurrent tumor(s) 
                                    1                                     3 
                                 NA's 
                                  252 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                               2                                2 
                            NA's 
                             257 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                  2                 259 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             252                9 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      254         7 

patient.biospecimen_cqcf.country:
       canada        russia united states       vietnam 
            4             4           249             4 

patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      5     120     658    1172    1912    6902       4 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -7.00    0.00   22.00   35.01   39.00  917.00       4 

patient.biospecimen_cqcf.digital_image_submitted:
  no  yes NA's 
  56    8  197 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     5                    149                    107 



patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
 no 
261 

patient.biospecimen_cqcf.method_of_sample_procurement:
     excisional biopsy      incisional biopsy other (please specify) 
                     8                      3                      3 
    surgical resection 
                   247 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -11.00    0.00   11.00   22.22   28.00  114.00     238 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
 0.052 0.0888  0.124 589.73   NA's 
     1      1      1      1    257 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
            nano drop              nanodrop nanodrop spectroscopy 
                    2                     1                     1 
                 NA's 
                  257 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
   2.6   4.44 6.2305     17   NA's 
     1      1      1      1    257 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
  29   50 NA's 
   1    3  257 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
                   blood draw blood draw (preferred method) 
                           12                             1 
           surgical resection                          NA's 
                           10                           238 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
adjacent organ         muscle          other           NA's 
             2              4              8            247 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           18           243 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
  23    5  233 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
2ecf845b-48a1-44ba-9a05-0163966d9785 a2d93680-23a1-4552-8a20-6a68f26b2af3 
                                   1                                    1 
ec1a9cae-a390-44ae-bf4a-0cae860be0d2 f99c8b7a-f6d9-4fbf-ad7b-d49ebfb5b8fd 
                                   1                                    1 
                                NA's 
                                 257 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
  37 NA's 
   1  260 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_conc:
 0.052 0.0888  0.124   NA's 
     1      1      1    258 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qm:
            nano drop              nanodrop nanodrop spectroscopy 
                    1                     1                     1 
                 NA's 
                  258 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qty:
   2.6   4.44 6.2305   NA's 
     1      1      1    258 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_vol:
  50 NA's 
   3  258 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
surgical resection               NA's 
                 1                260 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   2    3 NA's 
   1    3  257 

patient.biospecimen_cqcf.normal_controls.normal_control.4.bcr_sample_uuid:
521e31d7-e7de-41b2-aaee-98dd30d6cad4 b3962ee9-40bb-49a4-8712-310d269bf6ab 
                                   1                                    1 
                                NA's 
                                 259 

patient.biospecimen_cqcf.normal_controls.normal_control.4.days_to_normal_sample_procurement:
 166 NA's 
   1  260 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_conc:
0.052  NA's 
    1   260 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_qm:
nanodrop spectroscopy                  NA's 
                    1                   260 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_qty:
 2.6 NA's 
   1  260 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_vol:
  50 NA's 
   1  260 

patient.biospecimen_cqcf.normal_controls.normal_control.4.method_of_normal_sample_procurement:
blood draw       NA's 
         1        260 

patient.biospecimen_cqcf.normal_controls.normal_control.4.vial_number:
   1    4 NA's 
   1    1  259 

patient.biospecimen_cqcf.normal_controls.normal_control.5.bcr_sample_uuid:
91c9f6d1-a45c-4c07-9f44-ee122e94545f                                 NA's 
                                   1                                  260 

patient.biospecimen_cqcf.normal_controls.normal_control.5.extracted_dna.ncedna_dna_conc:
0.052  NA's 
    1   260 

patient.biospecimen_cqcf.normal_controls.normal_control.5.extracted_dna.ncedna_dna_qm:
nanodrop spectroscopy                  NA's 
                    1                   260 

patient.biospecimen_cqcf.normal_controls.normal_control.5.extracted_dna.ncedna_dna_qty:
 2.6 NA's 
   1  260 

patient.biospecimen_cqcf.normal_controls.normal_control.5.extracted_dna.ncedna_dna_vol:
  50 NA's 
   1  260 

patient.biospecimen_cqcf.normal_controls.normal_control.5.vial_number:
   5 NA's 
   1  260 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -687.0     5.0    23.0   100.7    58.0  3208.0       4 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.0190    0.1745    0.3223   81.3094   67.7875 1804.0000       181 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.6    15.0    20.0    22.9    21.0   185.0     181 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0    50.0    88.1   152.6   194.3   785.0     181 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
                   blood draw blood draw (preferred method) 
                          217                            22 
           surgical resection                          NA's 
                           21                             1 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
adjacent organ         muscle          other           NA's 
             3             12              6            240 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           21           240 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
  1   2 
260   1 

patient.biospecimen_cqcf.other_method_of_sample_procurement:
mrh/bso tah/bso    NA's 
      1       2     258 

patient.biospecimen_cqcf.other_vessel_used:
                              aluminum foil 
                                          1 
                                  cryosette 
                                          1 
        wrapped in foil; then stored in bag 
                                          1 
wrapped in foil; then stored in storage bag 
                                          1 
                                       NA's 
                                        257 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 no yes 
  3 258 

patient.biospecimen_cqcf.path_confirm_report_attached:
 no yes 
  1 260 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 no yes 
  1 260 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 no yes 
  2 259 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                        3                        16                       154 
                     NA's 
                       88 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
                                    macrodissection performed at tss to select for a region containing an acceptable tcga diagnosis 
                                                                                                                                  1 
  pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                  2 
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  1 
                                                                                                                               NA's 
                                                                                                                                257 

patient.biospecimen_cqcf.sample_prescreened:
yes 
261 


patient.biospecimen_cqcf.site_of_disease_description:
other  NA's 
    1   260 

patient.biospecimen_cqcf.submitted_for_lce:
  no NA's 
  41  220 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
   9   55  197 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0    2   50 NA's 
   3    1    1  256 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     50      60      70      75      95     100     256 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     40     200     280     270     360     470     256 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1 NA's 
   5  256 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   2.818   5.000  50.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  40.00   80.00   90.00   87.92   95.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     30     200     200     278     200    2985 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1 
261 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                     35                       6                     147 
               cryovial  other (please specify) 
                     71                       2 

patient.days_to_index:
   0 NA's 
 257    4 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and metastatic tumor(s)  primary tumor and recurrent tumor(s) 
                                    1                                     3 
                                 NA's 
                                  257 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     38     233     883    1313    2038    6784       4 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0   147.5   315.0   354.5   397.5   950.0     239 

patient.samples.sample.2.is_ffpe:
 no 
261 

patient.samples.sample.2.oct_embedded:
false  true 
  244    17 



patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.78  1.8 1.81 1.84 NA's 
   1    2    1    1  256 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118956130 132266844 145632881 147030366 164105557 174190420       256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
   2    3  256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.16 0.17 NA's 
   1    2    2  256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    25.0    26.0    22.8    28.0    30.0     256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   3    5   12 NA's 
   2    2    1  256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   7   10   11   12 NA's 
   2    1    1    1  256 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
2.08    3  3.4 NA's 
   2    1    2  256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   5  256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 NA's 
   2    3  256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 NA's 
   2    1    2  256 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118956039 145676663 164081770 156115571 167803987 174191670        19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    9   13 NA's 
   3  237    2   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.16 0.17  0.5 NA's 
   1    2    2  237   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    8.00   11.50   15.57   24.00   30.00      19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 242   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    7.00    5.64    7.00   12.00      19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   7.806  10.000  12.000      19 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
1.95 2.21  3.2   40 NA's 
   1    2    2  237   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
 242   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
   3    2  237   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
  30   81  131   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.70   16.58   17.66   25.83   21.72   63.50     256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 237                    5                   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   5  237   19 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
0.15 0.16 0.17 NA's 
   1    2    2  256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 237   24 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
 237   24 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             242 
                                            NA's 
                                              19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 242   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                        5                       237                        19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   1.700   1.720   1.754   1.920   1.930     256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
 7.6  7.9  9.2  9.3 NA's 
   1    2    1    1  256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      5     256 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
  15    4    2  240 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-dx-a3ly-01b-11w-a29a-09 tcga-k1-a3pn-02a-11w-a234-09 
                           1                            1 
tcga-k1-a42x-02a-11w-a26c-09 tcga-si-a71o-06a-11w-a44r-09 
                           1                            1 
                        NA's 
                         257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
1c96e2f4-831d-4567-888a-74b5313313f6 207e40d4-dd03-4f03-ad68-0af43f0bea57 
                                   1                                    1 
519ac7fb-fcac-42e5-8fd3-ccefedba16a3 88d76477-39b3-4180-a6ba-0494ab863a63 
                                   1                                    1 
                                NA's 
                                 257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
132270311 132270850 145673198 167803937      NA's 
        1         1         1         1       257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   9 NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   8   11   27 NA's 
   2    1    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   2    5    7 NA's 
   1    1    2  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   1    4    7   11 NA's 
   1    1    1    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a234 a26c a29a a44r NA's 
   1    1    1    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   c    e    f NA's 
   2    1    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
   1    2    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   4                  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-dx-a3ly-01b-11w tcga-k1-a3pn-02a-11w tcga-k1-a42x-02a-11w 
                   1                    1                    1 
tcga-si-a71o-06a-11w                 NA's 
                   1                  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
07432460-df70-4035-a712-4b35eb28c7b2 0f21f79b-dcbc-4bbf-bc62-e92f39d5fea9 
                                   1                                    1 
220684b2-c9fb-4daf-9f3e-ebaabb9a936b e09e27fd-f20b-4372-88ba-5326e51cfd51 
                                   1                                    1 
                                NA's 
                                 257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      4     257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
1.96 NA's 
   1  260 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
118961386 132270727 153889109 150951258 164081526 174190649 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  9 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
  99  139   23 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    5.0    16.0    26.0    22.2    28.0    30.0 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   5.261   6.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   6.000   7.000   7.686   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
 99 139  23 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 40 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 
  52   97  112 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118957152 132157266 132275880 140334492 150705832 174190096       139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5   26 NA's 
   5  117  139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 NA's 
  26   27   36   33  139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   12.00   25.00   20.37   28.00   30.00     139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 122  139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.861   9.000  12.000     139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   7.639   9.000  12.000     139 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.40    1.50    1.60    1.58    1.70    1.95     139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 122  139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10   13 NA's 
 117    5  139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2012 2013 2014 NA's 
  40   70   12  139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-dx-a3lt-10a-01d-a32v-09 tcga-dx-a3lw-10a-01d-a32v-09 
                           1                            1 
tcga-k1-a3pn-02a-11d-a22m-26 tcga-k1-a42x-02a-11d-a24w-05 
                           1                            1 
                        NA's 
                         257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
0b8a7a87-aeb5-47f8-b41c-a7452edb1e08 25c9b24c-5f2a-43ce-af2b-15b52d718f0f 
                                   1                                    1 
2d8fb05e-4fb9-4350-b197-a56f40089cb7 c7214163-07fc-488c-90f5-a10990ebaa09 
                                   1                                    1 
                                NA's 
                                 257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
122971445 132270668 150757032 150757055      NA's 
        1         1         1         1       257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    9   26 NA's 
   1    2    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.07 0.14 0.17 NA's 
   2    1    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   2    5   30 NA's 
   2    1    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   5   10   12 NA's 
   1    2    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    4    6 NA's 
   2    1    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a22m a24w a32v NA's 
   1    1    2  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   a    b    d    e NA's 
   1    1    1    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 1.4 2.21 NA's 
   3    1  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   4  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10   13   20 NA's 
   1    1    2  257 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2012 2013 NA's 
   2    2  257 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
118966670 132270749 153889091 150438155 164055447 174190692 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  64   63   80   54 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   12.00   26.00   21.77   28.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.713   6.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   6.000   7.000   7.686   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  64   63   80   54 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 2013 2014 
  52   84  125 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.62    8.87   13.90   20.58   24.78  120.09 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
261 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  64   63   80   54 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             261 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
261 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                              174                                87 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      260         1 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
  16    4    2  239 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    5.00   12.00   14.17   23.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no 
261 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.762   9.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   2.172   1.000  43.000 

patient.samples.sample.2.portions.portion.portion_sequence:
   1    2    3 NA's 
 171    4    1   85 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-qq-a5v9-11a-02-tsb                    NA's 
                      1                     260 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_uuid:
de991cb2-5865-43fd-82eb-9b8fd380cee3                                 NA's 
                                   1                                  260 

patient.samples.sample.2.portions.portion.slides.slide.2.image_file_name:
tcga-qq-a5v9-11a-02-tsb.de991cb2-5865-43fd-82eb-9b8fd380cee3.svs 
                                                               1 
                                                            NA's 
                                                             260 

patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   1 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
 100 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
 top NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.bcr_slide_barcode:
tcga-qq-a5v9-11a-03-tsc                    NA's 
                      1                     260 

patient.samples.sample.2.portions.portion.slides.slide.3.bcr_slide_uuid:
81f61633-c1a5-40f9-b953-d4e42882c069                                 NA's 
                                   1                                  260 

patient.samples.sample.2.portions.portion.slides.slide.3.image_file_name:
tcga-qq-a5v9-11a-03-tsc.81f61633-c1a5-40f9-b953-d4e42882c069.svs 
                                                               1 
                                                            NA's 
                                                             260 

patient.samples.sample.2.portions.portion.slides.slide.3.is_derived_from_ffpe:
  no NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_lymphocyte_infiltration:
   1 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_monocyte_infiltration:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_necrosis:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_neutrophil_infiltration:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_normal_cells:
 100 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_stromal_cells:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_tumor_cells:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_tumor_nuclei:
   0 NA's 
   1  260 

patient.samples.sample.2.portions.portion.slides.slide.3.section_location:
 top NA's 
   1  260 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  22  239 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    1    2    3 NA's 
  10    7    2    3  239 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1 NA's 
  17    5  239 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0    5 NA's 
  21    1  239 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    2 NA's 
  19    2    1  239 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0   50  100 NA's 
   7    1   14  239 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   12.73    0.00  100.00     239 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0   70   95  100 NA's 
  17    1    1    3  239 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0   88   97  100 NA's 
  17    1    2    2  239 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
  22  239 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    2.00   14.87    2.00  300.00      87 

patient.samples.sample.2.portions.portion.year_of_creation:
2011 2012 2013 2014 
  30   56   98   77 

patient.samples.sample.2.portions.shipment_portion.bcr_shipment_portion_uuid:
12b51ec3-e12d-4767-82d7-1da1605fdaa0 67d9aacb-87a3-42d5-9a32-fc1380aaf140 
                                   1                                    1 
ae5756bf-84c2-4769-ad8e-132c69828fcb bbecb2f3-2340-4019-a470-c78ef62e51a3 
                                   1                                    1 
                                NA's 
                                 257 

patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
   4  257 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
   4  257 

patient.samples.sample.2.portions.shipment_portion.plate_id:
a455 a456 NA's 
   3    1  257 

patient.samples.sample.2.portions.shipment_portion.portion_number:
  21 NA's 
   4  257 

patient.samples.sample.2.portions.shipment_portion.portion_sequence:
   2 NA's 
   3  258 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode:
tcga-dx-a3ly-01b-21-a455-20 tcga-k1-a3pn-02a-21-a455-20 
                          1                           1 
tcga-k1-a42x-02a-21-a455-20 tcga-vt-a80j-02a-21-a456-20 
                          1                           1 
                       NA's 
                        257 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
   9 NA's 
   4  257 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   7 NA's 
   4  257 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
   4  257 

patient.samples.sample.2.sample_type:
blood derived normal           metastatic        primary tumor 
                 238                    1                    1 
     recurrent tumor  solid tissue normal 
                   3                   18 

patient.samples.sample.2.sample_type_id:
  1   2   6  10  11 
  1   3   1 238  18 

patient.samples.sample.2.vial_number:
  a   b   d   e 
241  18   1   1 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   86.0   202.0   365.0   799.6   588.0  3178.0     252 

patient.samples.sample.3.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   122.5   200.0   286.7   292.5   850.0     255 

patient.samples.sample.3.is_ffpe:
  no NA's 
   9  252 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
    6     3   252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.75 1.84 NA's 
   1    1  259 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
122970493 132270305 145673201 142152180 145676652 167803451       252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    9 NA's 
   2    7  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.16  0.5 NA's 
   2    7  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   11.00   27.00   21.11   27.00   28.00     252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   2    3    5    7 NA's 
   4    2    1    2  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   5.000   6.000   6.556   7.000  11.000     252 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   b    c    d    h NA's 
   2    4    2    1  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
2.08   40 NA's 
   2    7  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   80 NA's 
   2    7  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
   3    4    2  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
9.56 29.2 NA's 
   1    1  259 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   7                    2                  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   2    7  252 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.16 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   7  254 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   7  254 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               9 
                                            NA's 
                                             252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                        2                         7                       252 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
 1.4  1.6 NA's 
   1    1  259 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   7    8 NA's 
   1    1  259 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      2     259 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
   3    3  255 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-fx-a2qs-11a-11w-a225-09 tcga-k1-a3po-11a-12w-a225-09 
                           1                            1 
                        NA's 
                         259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
51a39d86-fe4e-478e-b8f5-cc194ebcdb50 b82377b3-35cb-4829-a7c1-13f09f4577b7 
                                   1                                    1 
                                NA's 
                                 259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
122972381 122972394      NA's 
        1         1       259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   9 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  16 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   6 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a225 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   e    f NA's 
   1    1  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-fx-a2qs-11a-11w tcga-k1-a3po-11a-12w                 NA's 
                   1                    1                  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
3d3ee7b6-b5d5-41e7-9cdf-40700a6944fa ee284cfb-2de3-431c-a913-fa0a006a70ae 
                                   1                                    1 
                                NA's 
                                 259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     259 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
   1    1  259 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118965754 122971242 132254511 136141682 145632737 164081512       252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   9 NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 NA's 
   4    4    1  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      5       5      25      19      28      30     252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.778  12.000  12.000     252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   6.000   6.000   6.667   7.000  11.000     252 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 2.8  3.2  3.6 NA's 
   4    4    1  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  40 NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 NA's 
   6    2    1  252 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
122970504 122971066 132266322 132295407 132266342 160356208       254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5   26 NA's 
   2    5  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.15 0.16 0.17 NA's 
   4    2    1  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   5   16   28   30 NA's 
   3    1    2    1  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   7  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   3    5   10   12 NA's 
   2    1    1    3  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   4    6    7 NA's 
   2    4    1  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a21r a22m a24t a33m NA's 
   2    1    3    1  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   c    d    e    f NA's 
   2    3    1    1  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   1.500   1.600   1.690   1.825   2.080     254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   7  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10   13 NA's 
   5    2  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2012 2013 NA's 
   6    1  254 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-fx-a2qs-11a-11d-a21s-26 tcga-fx-a48g-11a-12d-a24w-05 
                           1                            1 
tcga-k1-a42x-11a-11d-a24w-05 tcga-kf-a41w-11a-11d-a24w-05 
                           1                            1 
                        NA's 
                         257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
34d27e94-2057-43dc-8f8e-f6f7ce337681 42eac056-842b-42fc-ac34-4391877ce15e 
                                   1                                    1 
47131a08-03d2-47ee-8cb0-291c1fd3c7b2 75bb0940-7828-4f6c-b6d6-46b037c3afa1 
                                   1                                    1 
                                NA's 
                                 257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
122970682 132266838 132270675 132270692      NA's 
        1         1         1         1       257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5   26 NA's 
   3    1  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.15 0.16 0.17 NA's 
   1    2    1  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   5   28 NA's 
   3    1  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   4  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   3   12 NA's 
   1    3  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   4    6 NA's 
   2    2  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a21s a24w NA's 
   1    3  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   c    d    f NA's 
   2    1    1  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 1.6 1.95 2.08 2.21 NA's 
   1    1    1    1  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   4  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10   13 NA's 
   1    3  257 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2012 NA's 
   4  257 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118966714 122970762 132270653 136150875 145671521 164081508       252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 NA's 
   1    4    2    2  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    5.00   25.00   19.56   28.00   30.00     252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.778  12.000  12.000     252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   6.000   6.000   6.667   7.000  11.000     252 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 NA's 
   1    4    2    2  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
   6    2    1  252 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.04    6.90   14.82   15.96   20.76   35.59     252 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   9  252 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 NA's 
   1    4    2    2  252 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               9 
                                            NA's 
                                             252 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   9  252 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    9                   252 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        9       252 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
   3    3  255 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   17.00   21.00   19.33   27.00   27.00     252 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
   9  252 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   8.000   6.889   9.000  11.000     252 

patient.samples.sample.3.portions.portion.portion_number:
   1   11   12   22 NA's 
   3    3    2    1  252 

patient.samples.sample.3.portions.portion.portion_sequence:
   1    2 NA's 
   8    1  252 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   6  255 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    1    3 NA's 
   4    1    1  255 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    6 NA's 
   5    1  255 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   6  255 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1 NA's 
   5    1  255 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
  80  100 NA's 
   1    5  255 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0   20 NA's 
   5    1  255 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   6  255 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   6  255 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
   6  255 

patient.samples.sample.3.portions.portion.weight:
   2   20   30 NA's 
   3    1    5  252 

patient.samples.sample.3.portions.portion.year_of_creation:
2011 2012 2013 NA's 
   1    6    2  252 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   3                    6                  252 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
   3    6  252 

patient.samples.sample.3.vial_number:
   a    b NA's 
   8    1  252 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     38     225     883    1303    2038    6931       4 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10:
49214666-c714-41b3-b562-a0c5b05ab722                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.11:
0440fc98-da55-4257-8c41-d85df933dc7f                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.12:
3598742c-cb50-4f43-bfa4-58117446f7b6                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.13:
40ba6624-ae9d-487a-ac1f-5b1cacc850ba                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.14:
0378bfd0-91ed-4eaa-a3ac-64d95fd76f44                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.15:
5e7e9be1-82a8-470d-b248-45fb9bc18910                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.16:
7e75bdcf-ca0e-466d-9ed4-ed87d47cf4ee                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.17:
4c927ccc-9572-457c-85e3-6ba5458fd983                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.18:
b49ef622-5a3a-4aad-a2af-b58150dd707c                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.19:
920d693b-8ff9-4784-aa87-4f7d73f651e1                                 NA's 
                                   1                                  260 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.20:
0729ab89-f7cf-4ddb-9735-87d2402de5b5                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.21:
8a27b115-5bbe-4e47-8c0f-33a0b2165300                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.22:
c04b72fb-815c-4520-8c95-ce2aa4ca9815                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.23:
4cf5ad3a-c432-4f9c-ad86-6bdbbb88b73b                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.24:
62349dce-ea2f-4da1-b671-df83ff7d58fc                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.25:
40980d79-8f3a-4cf2-91af-04c997b00eab                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.26:
f3f683dc-37f9-47c8-9aaa-766ee879ac46                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.27:
ee2225e6-ee58-40f1-ad24-b3f45bcc0029                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.28:
fcbdc34b-a467-46cc-883c-b19a66c124e9                                 NA's 
                                   1                                  260 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.29:
32c6abe8-b05e-418b-aa30-52bc5e1888c3                                 NA's 
                                   1                                  260 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.30:
95e300fb-73a2-463a-9a1c-069770c86f52                                 NA's 
                                   1                                  260 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
8ab57c98-a4b8-48d8-9107-23cded6c838e 9984fd31-e82e-44e7-9269-b6b90698b510 
                                   1                                    1 
aa4e7c84-518f-4f14-9dc7-3f28ffa6d95e                                 NA's 
                                   1                                  258 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
1d2a8ed5-5342-4b2d-9076-b10353563393 e2453e84-0c56-4ce1-8154-4b624f121b1d 
                                   1                                    1 
                                NA's 
                                 259 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.8:
99c4b5ef-3694-42df-9c3a-92acbcb29c87 e526559b-1371-43c7-879d-aa7c4c4617d4 
                                   1                                    1 
                                NA's 
                                 259 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
afe2b0a2-d1c1-4146-88c3-33927783f042 b00068fb-abe2-417f-86e6-c777438cebdb 
                                   1                                    1 
                                NA's 
                                 259 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   20.0   160.0   330.0   423.1   610.0  2150.0 

patient.samples.sample.is_ffpe:
 no 
261 

patient.samples.sample.oct_embedded:
false  true 
   44   217 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-dx-a240-01a-32w-a29a-09                         NA's 
                           1                          260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
3c74c616-7e25-41ca-9712-e1301c08c8d2                                 NA's 
                                   1                                  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
132270312      NA's 
        1       260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   9 NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
 0.5 NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
   8 NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   5 NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   1 NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
a29a NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   b NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
  40 NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  80 NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  260 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   w NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.bcr_analyte_barcode:
tcga-dx-a240-01a-32w                 NA's 
                   1                  260 

patient.samples.sample.portions.portion.2.analytes.analyte.bcr_analyte_uuid:
63a29a3f-bcca-40bb-90dd-6e8929b3f82b                                 NA's 
                                   1                                  260 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             260 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   1  260 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     260 

patient.samples.sample.portions.portion.2.analytes.analyte.subportion_sequence:
   2 NA's 
   1  260 

patient.samples.sample.portions.portion.2.bcr_portion_barcode:
tcga-dx-a240-01a-32                NA's 
                  1                 260 

patient.samples.sample.portions.portion.2.bcr_portion_uuid:
4b5b5994-c016-4581-b3b4-418b4381260f                                 NA's 
                                   1                                  260 

patient.samples.sample.portions.portion.2.day_of_creation:
  21 NA's 
   1  260 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
   1  260 

patient.samples.sample.portions.portion.2.month_of_creation:
   7 NA's 
   1  260 

patient.samples.sample.portions.portion.2.portion_number:
  32 NA's 
   1  260 

patient.samples.sample.portions.portion.2.portion_sequence:
   3 NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.bcr_slide_barcode:
tcga-dx-a240-01a-03-tsc                    NA's 
                      1                     260 

patient.samples.sample.portions.portion.2.slides.slide.bcr_slide_uuid:
f759dfe2-6410-494c-a5f3-a1e8e7be19a7                                 NA's 
                                   1                                  260 

patient.samples.sample.portions.portion.2.slides.slide.image_file_name:
tcga-dx-a240-01a-03-tsc.f759dfe2-6410-494c-a5f3-a1e8e7be19a7.svs 
                                                               1 
                                                            NA's 
                                                             260 

patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   0 NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   0 NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
  10 NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
  90 NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
  90 NA's 
   1  260 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
 top NA's 
   1  260 

patient.samples.sample.portions.portion.2.weight:
  20 NA's 
   1  260 

patient.samples.sample.portions.portion.2.year_of_creation:
2011 NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   1.810   1.860   1.857   1.900   2.260       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118956079 132275858 153888888 151821502 164104691 174192151         1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
 102  158    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.16 0.17 0.18 NA's 
  29   92   87   52    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   16.00   26.00   22.15   28.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.281   6.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.746   9.000  12.000       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.210   3.200   2.868   3.400   3.600       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 NA's 
 102  158    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 NA's 
  52   97  111    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-dx-a48v-01a-11r-a466-07 tcga-dx-a6bk-01a-11r-a466-07 
                           1                            1 
tcga-dx-a7eq-01a-11r-a466-07                         NA's 
                           1                          258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
3ab8aba7-9bde-42d3-b95b-e12e2779470b 55a3890b-6df6-49fb-9437-94ddddb4b32e 
                                   1                                    1 
f5fc7fbe-0a97-4f3e-a20d-b428f756960b                                 NA's 
                                   1                                  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
167812900 167812970 167812985      NA's 
        1         1         1       258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   3  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.16 0.18 NA's 
   1    2  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   9 NA's 
   3  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  12 NA's 
   3  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   3    5    7 NA's 
   1    1    1  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a466 NA's 
   3  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   a    b    h NA's 
   1    1    1  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
1.06 1.19 NA's 
   1    2  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   3  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6 NA's 
   3  258 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   3  258 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118956003 132269808 153889476 150384336 164104213 174191706         1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   13 NA's 
 158  102    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.16 0.17 0.18 NA's 
  29   92   87   52    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   16.00   26.00   22.15   28.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.281   6.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.746   9.000  12.000       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.080   2.340   2.618   3.200   3.600       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20 NA's 
 158  102    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
  52   97  111    1 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.14   16.95   24.95   30.23   40.83   89.43       1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   r NA's 
 260    1 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
0.15 0.16 0.17 0.18 NA's 
  29   92   87   52    1 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             260 
                                            NA's 
                                               1 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                      260                         1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.660   1.255   1.470   2.089   1.683 162.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.800   8.500   8.437   9.100  10.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    260       1 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 214   40    3    4 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
122972354 149446216 164081764 156000281 167803891 167804697        21 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   9 NA's 
 240   21 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 240   21 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00    8.00   11.00   15.49   24.00   29.00      21 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 240   21 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   5.625   7.000   7.000      21 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.671   6.000  11.000      21 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 240   21 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
 240   21 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 240   21 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
  29   80  131   21 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                 240                   21 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
 240   21 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 240   21 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
 240   21 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             240 
                                            NA's 
                                              21 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 240   21 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
    240      21 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 199   35    3   24 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
1.82 1.84 1.93 1.95 NA's 
   1    1    1    1  257 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118966245 132270781 153881046 150891953 164056120 174190644         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   9 NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 NA's 
  74  169   17    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   16.00   26.00   22.15   28.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.281   6.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.827   7.000  11.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 2.8  3.2  3.6 NA's 
  74  169   17    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  40 NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 NA's 
  52   97  111    1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118956950 132269141 160339810 149637301 164055450 174190388         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5   26 NA's 
 240   20    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.1555  0.1600  0.1800       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   16.00   26.00   22.15   28.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.281   6.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.827   7.000  11.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   1.950   1.950   1.985   2.080   2.340       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10   13 NA's 
  20  240    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2012 2013 2014 NA's 
  52   97  111    1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
122970432 128925360 145633068 140880690 150705835 167789194       139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   1    5   26 NA's 
   2   20  100  139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.1564  0.1600  0.1800     139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   12.00   25.00   20.27   28.00   30.00     139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 122  139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.861   9.000  12.000     139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.250   3.000   3.279   5.000   8.000     139 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   1.500   1.600   1.634   1.675   2.210     139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 122  139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
 6.7   10   13 NA's 
   2  100   20  139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2012 2013 2014 NA's 
  40   70   12  139 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-dx-a7er-01a-11d-a40c-01                         NA's 
                           1                          260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
2ebdcb6a-a3bf-41c0-9217-d95e17969fbf                                 NA's 
                                   1                                  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
167787827      NA's 
        1       260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1 NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.16 NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  14 NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   5 NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   2 NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a40c NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   b NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
1.07 NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
 6.7 NA's 
   1  260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2014 NA's 
   1  260 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118961093 132270769 153889398 150894292 164056116 174189497         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.1555  0.1600  0.1800       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   12.00   26.00   21.74   28.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.731   6.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.827   7.000  11.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.010   1.070   1.043   1.070   1.210       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
  52   84  124    1 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.36   15.67   23.84   26.77   36.84   66.93       1 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
 260    1 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.1555  0.1600  0.1800       1 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             260 
                                            NA's 
                                               1 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
 260    1 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                  260                     1 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
      256         4         1 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 214   40    3    4 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.75   13.00   14.28   21.00   31.00       1 

patient.samples.sample.portions.portion.is_ffpe:
  no NA's 
 260    1 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.515   9.000  12.000       1 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   11.00   12.59   11.00   42.00       1 

patient.samples.sample.portions.portion.portion_sequence:
   1    2    3    4 NA's 
 166   16    8    1   70 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
 260    1 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  1.0000  0.9346  2.0000  8.0000       1 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.2577  0.0000  5.0000       1 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.7885  0.0000 20.0000       1 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    2    3 NA's 
 230   17   10    3    1 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4192  0.0000 45.0000       1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   8.731  15.000  65.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   85.00   95.00   90.02  100.00  100.00       1 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   80.00   89.00   87.37   95.00  100.00       1 

patient.samples.sample.portions.portion.slides.slide.section_location:
 top NA's 
 260    1 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.00   30.00   29.77   30.00   80.00       1 

patient.samples.sample.portions.portion.year_of_creation:
2011 2012 2013 2014 NA's 
  24   55  117   64    1 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 223   38 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 223   38 

patient.samples.sample.portions.shipment_portion.plate_id:
a454 a455 a456 NA's 
  81   78   64   38 

patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   22.84   21.00   51.00      38 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.025   2.000   5.000     104 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   9 NA's 
 223   38 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   7 NA's 
 223   38 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
 223   38 

patient.samples.sample.sample_type:
primary tumor 
          261 

patient.samples.sample.sample_type_id:
  1 
261 

patient.samples.sample.vial_number:
  a 
261 

}}
\keyword{datasets}
