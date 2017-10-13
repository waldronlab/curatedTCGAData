\name{ KIRC }
\alias{ KIRC }
\docType{data}
\title{ Kidney renal clear cell carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( KIRC )
ExperimentList class object of length 12: 
 [1] CNASNP-20160128: RaggedExperiment with 488691 rows and 1059 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 85036 rows and 1059 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 528 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 528 columns 
 [5] Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 418 columns 
 [6] Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 480 columns 
 [7] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 326 columns 
 [8] mRNAArray-20160128: SummarizedExperiment with 17814 rows and 72 columns 
 [9] Mutation-20160128: RaggedExperiment with 26369 rows and 437 columns 
 [10] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 606 columns 
 [11] RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 537 columns 
 [12] RPPAArray-20160128: SummarizedExperiment with 217 rows and 478 columns 

> rownames( KIRC )
CharacterList of length 12
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation_methyl27-20160128"]] cg00000292 cg00002426 ... cg27665659
[["Methylation_methyl450-20160128"]] cg00000029 cg00000108 ... rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 PNMA1 ... SLC39A6 SNRPD2 AQP7 CTSC
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
...
<2 more elements>

> colnames( KIRC )
CharacterList of length 12
[["CNASNP-20160128"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ...
[["CNVSNP-20160128"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-3Z-A93Z-01A-11D-A36W-01 ...
[["Methylation_methyl27-20160128"]] TCGA-A3-3306-11A-01D-0859-05 ...
[["Methylation_methyl450-20160128"]] TCGA-3Z-A93Z-01A-11D-A36Y-05 ...
[["miRNASeqGene-20160128"]] TCGA-3Z-A93Z-01A-11R-A37P-13 ...
[["mRNAArray-20160128"]] TCGA-A3-3306-01A-01R-0864-07 ...
[["Mutation-20160128"]] TCGA-A3-3308-01A-01D-0966-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-3Z-A93Z-01A-11R-A37O-07 ...
...
<2 more elements>

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASNP-20160128 13.3 Mb
2                    CNVSNP-20160128  2.6 Mb
3          GISTIC_AllByGene-20160128  4.4 Mb
4  GISTIC_ThresholdedByGene-20160128  4.4 Mb
5      Methylation_methyl27-20160128  4.4 Mb
6     Methylation_methyl450-20160128 67.5 Mb
7              miRNASeqGene-20160128  0.1 Mb
8                 mRNAArray-20160128    1 Mb
9                  Mutation-20160128    8 Mb
10          RNASeq2GeneNorm-20160128  1.2 Mb
11               RNASeqGene-20160128  1.2 Mb
12                RPPAArray-20160128  0.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   360 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 177.00  177.00    2.24    1.77    2.61 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.00   52.00   61.00   60.57   70.00   90.00       1 

vital_status:
  0   1 
360 177 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   333.0   819.0   961.2  1432.0  3615.0     360 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   710.5  1454.5  1536.9  2172.0  4537.0     177 

tumor_tissue_site:
kidney 
   537 

pathologic_stage:
  stage i  stage ii stage iii  stage iv      NA's 
      269        57       125        84         2 


pathology_N_stage:
 n0  n1  nx 
240  17 280 

pathology_M_stage:
  m0   m1   mx NA's 
 426   79   30    2 

gender:
female   male 
   191    346 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1998    2004    2006    2006    2007    2013 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     191    1172    1117    1887    2799     510 

radiation_therapy:
  no  yes NA's 
 142    2  393 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00   90.00   85.56  100.00  100.00     483 

histological_type:
kidney clear cell renal carcinoma 
                              537 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   14.00   30.00   28.33   40.00   65.00     516 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1946    1966    1978    1979    1996    2001     525 

race:
                    asian black or african american                     white 
                        8                        56                       466 
                     NA's 
                        7 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    26                    359                    152 

admin.bcr:
nationwide children's hospital 
                           537 

admin.day_of_dcc_upload:
  1 
537 

admin.disease_code:
kirc 
 537 


admin.month_of_dcc_upload:
 10 
537 

admin.patient_withdrawal.withdrawn:
false 
  537 

admin.project_code:
tcga 
 537 

admin.year_of_dcc_upload:
2015 
 537 

patient.additional_studies.additional_study.disease_code:
fppp NA's 
   4  533 

patient.additional_studies.additional_study.project_code:
tcga NA's 
   4  533 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  26.00   52.00   61.00   60.59   70.00   90.00 


patient.clinical_cqcf.anatomic_neoplasm_subdivision:
 left right  NA's 
  243   268    26 

patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      424       113 

patient.clinical_cqcf.country:
united states          NA's 
           94           443 

patient.clinical_cqcf.days_to_consent:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-3371.00   -24.25    -7.00    73.03     0.00  3116.00      105 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   18.0   311.0   709.0   858.7  1317.0  2764.0     432 

patient.clinical_cqcf.histological_type:
kidney clear cell renal carcinoma 
                              537 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             524 
yes, pharmaceutical treatment prior to resection 
                                               9 
               yes, radiation prior to resection 
                                               4 

patient.clinical_cqcf.history_of_prior_malignancy:
                                                     no 
                                                    459 
                       yes, history of prior malignancy 
                                                     60 
yes, history of synchronous and or bilateral malignancy 
                                                      6 
                                                   NA's 
                                                     12 

patient.clinical_cqcf.normal_tissue_anatomic_site:
         kidney  kidney; cortex kidney; medulla   kidney; mixed            NA's 
            243              55              23             129              87 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
          404           133 

patient.clinical_cqcf.other_anatomic_site_normal_tissue:
  mm NA's 
   1  536 

patient.clinical_cqcf.tumor_type:
primary 
    537 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   14.00   16.52   24.00   31.00       6 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32873  -25616  -22322  -22302  -19022   -9714       1 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   322.2   731.0   886.6  1341.5  2830.0     375 

patient.days_to_initial_pathologic_diagnosis:
  0 
537 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -2     271    1114    1117    1794    3377     155 

patient.days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     191    1172    1117    1887    2799     510 

patient.drugs.drug.10.bcr_drug_barcode:
tcga-cj-4637-d9504               NA's 
                 1                536 

patient.drugs.drug.10.bcr_drug_uuid:
54419681-f557-4d4d-8a03-a2dc7482a7d0                                 NA's 
                                   1                                  536 

patient.drugs.drug.10.day_of_form_completion:
  25 NA's 
   1  536 

patient.drugs.drug.10.days_to_drug_therapy_end:
1883 NA's 
   1  536 

patient.drugs.drug.10.days_to_drug_therapy_start:
1868 NA's 
   1  536 

patient.drugs.drug.10.drug_name:
avastin    NA's 
      1     536 

patient.drugs.drug.10.month_of_form_completion:
   3 NA's 
   1  536 

patient.drugs.drug.10.regimen_indication:
progression        NA's 
          1         536 

patient.drugs.drug.10.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              536 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
   1  536 

patient.drugs.drug.10.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        536 

patient.drugs.drug.10.year_of_form_completion:
2011 NA's 
   1  536 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.25   18.50   18.03   27.50   31.00     503 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   238.0   482.0   703.2   920.2  2450.0     509 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    -92      70     235     503     767    1765     504 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease               stable disease 
                           3                            1 
                        NA's 
                         533 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   4.000   4.824   5.000  12.000     503 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.50    6.00   22.87   14.00  241.00     522 



patient.drugs.drug.2.regimen_indication:
               adjuvant other: specify in notes             progression 
                      2                       8                      16 
             recurrence                    NA's 
                      2                     509 


patient.drugs.drug.2.regimen_number:
   1    2    3    4 NA's 
   8    7    1    2  519 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
 intravenous (iv)              oral subcutaneous (sc)              NA's 
                9                17                 2               509 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  30    4  503 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                        11                          1 
            other, specify targeted molecular therapy 
                         2                         20 
                      NA's 
                       503 

patient.drugs.drug.2.therapy_types.therapy_type_notes:
sorafenib   vaccine      NA's 
        1         2       534 

patient.drugs.drug.2.total_dose:
   2  3.6 NA's 
   1    1  535 

patient.drugs.drug.2.total_dose_units:
g/day    ml  NA's 
    1     1   535 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
   4    2  531 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2011    2015     503 



patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.75   24.00   19.94   28.00   31.00     519 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   74.0   225.0   282.0   482.1   592.0  1888.0     522 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   27.0   107.8   222.0   460.9   407.0  2586.0     519 


patient.drugs.drug.3.measure_of_response:
stable disease           NA's 
             1            536 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   4.000   4.111   4.000   9.000     519 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.444   9.000  12.000     528 


patient.drugs.drug.3.prescribed_dose_units:
       mg     mg/m2 mg/m2/day        ml      NA's 
        4         1         2         1       529 

patient.drugs.drug.3.regimen_indication:
               adjuvant other: specify in notes             progression 
                      2                       2                      13 
                   NA's 
                    520 

patient.drugs.drug.3.regimen_indication_notes:
treatment of metastatic disease following cytoreductive nephrectomy 
                                                                  1 
                                                               NA's 
                                                                536 

patient.drugs.drug.3.regimen_number:
   1    2    3    4 NA's 
   4    1    5    1  526 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intramuscular (im)   intravenous (iv)               oral  subcutaneous (sc) 
                 1                  7                  5                  4 
              NA's 
               520 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  17    1  519 

patient.drugs.drug.3.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         9                          3 
            other, specify targeted molecular therapy 
                         1                          5 
                      NA's 
                       519 

patient.drugs.drug.3.therapy_types.therapy_type_notes:
vaccine    NA's 
      1     536 

patient.drugs.drug.3.total_dose:
       2.4 30 million       NA's 
         1          1        535 

patient.drugs.drug.3.total_dose_units:
  ml NA's 
   1  536 

patient.drugs.drug.3.tx_on_clinical_trial:
  no NA's 
   1  536 

patient.drugs.drug.3.year_of_form_completion:
2011 2014 NA's 
  17    1  519 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   16.25   24.00   21.14   28.00   31.00     523 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  155.0   231.8   378.5   608.2   606.5  2053.0     525 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   45.0   153.8   316.0   547.9   677.5  1962.0     523 


patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   3.500   4.214   4.000   9.000     523 

patient.drugs.drug.4.number_cycles:
   1    3 NA's 
   1    2  534 

patient.drugs.drug.4.prescribed_dose:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
     2.5     20.0     75.0  83385.4    137.5 500000.0      531 

patient.drugs.drug.4.prescribed_dose_units:
 g/day     mg mg/day  mg/kg   NA's 
     1      1      2      1    532 

patient.drugs.drug.4.regimen_indication:
other: specify in notes             progression                    NA's 
                      3                      11                     523 

patient.drugs.drug.4.regimen_indication_notes:
patient cannot tolerate sorafenib              re-started treatment 
                                1                                 1 
                             NA's 
                              535 

patient.drugs.drug.4.regimen_number:
   2    3    4    5 NA's 
   3    1    2    2  529 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
 intravenous (iv)              oral subcutaneous (sc)              NA's 
                4                 8                 2               523 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
  13    1  523 

patient.drugs.drug.4.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         5                          2 
targeted molecular therapy                       NA's 
                         7                        523 

patient.drugs.drug.4.year_of_form_completion:
2011 NA's 
  14  523 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   19.75   25.00   21.88   26.00   30.00     529 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  144.0   313.5   477.0   698.5   982.8  1625.0     529 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   81.0   246.0   388.0   427.6   529.8  1033.0     529 


patient.drugs.drug.5.month_of_form_completion:
   3    4    7    9 NA's 
   3    3    1    1  529 

patient.drugs.drug.5.number_cycles:
   1 NA's 
   2  535 

patient.drugs.drug.5.prescribed_dose:
1,000,000 units              10             400            NA's 
              1               1               2             533 

patient.drugs.drug.5.prescribed_dose_units:
    mg mg/day  mg/kg   NA's 
     1      1      1    534 

patient.drugs.drug.5.regimen_indication:
other: specify in notes             progression                    NA's 
                      1                       7                     529 

patient.drugs.drug.5.regimen_indication_notes:
metistatic disease               NA's 
                 1                536 

patient.drugs.drug.5.regimen_number:
   2    3 NA's 
   1    3  533 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
 intravenous (iv)              oral subcutaneous (sc)              NA's 
                2                 5                 1               529 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
   8  529 

patient.drugs.drug.5.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         3                          1 
targeted molecular therapy                       NA's 
                         4                        529 

patient.drugs.drug.5.year_of_form_completion:
2011 NA's 
   8  529 



patient.drugs.drug.6.day_of_form_completion:
   4   21   25   31 NA's 
   1    1    2    1  532 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    288     444     454     590     861     903     532 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   114.0   263.0   325.6   392.0   849.0     532 


patient.drugs.drug.6.month_of_form_completion:
   3    4    7 NA's 
   3    1    1  532 

patient.drugs.drug.6.number_cycles:
   2    6   10   11 NA's 
   1    1    1    1  533 

patient.drugs.drug.6.prescribed_dose:
 600 NA's 
   1  536 

patient.drugs.drug.6.prescribed_dose_units:
mg/m2  NA's 
    1   536 

patient.drugs.drug.6.regimen_indication:
other: specify in notes             progression                    NA's 
                      1                       4                     532 

patient.drugs.drug.6.regimen_indication_notes:
neoadjuvant        NA's 
          1         536 

patient.drugs.drug.6.regimen_number:
   1 NA's 
   1  536 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
oral NA's 
   5  532 

patient.drugs.drug.6.therapy_ongoing:
  no NA's 
   5  532 

patient.drugs.drug.6.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         1                          4 
                      NA's 
                       532 

patient.drugs.drug.6.year_of_form_completion:
2011 NA's 
   5  532 

patient.drugs.drug.7.bcr_drug_barcode:
tcga-cj-4637-d9500               NA's 
                 1                536 

patient.drugs.drug.7.bcr_drug_uuid:
afce5403-0ffb-4b19-82a9-c8132d2d406a                                 NA's 
                                   1                                  536 

patient.drugs.drug.7.day_of_form_completion:
  25 NA's 
   1  536 

patient.drugs.drug.7.days_to_drug_therapy_end:
 507 NA's 
   1  536 

patient.drugs.drug.7.days_to_drug_therapy_start:
 484 NA's 
   1  536 

patient.drugs.drug.7.drug_name:
intron a     NA's 
       1      536 

patient.drugs.drug.7.month_of_form_completion:
   3 NA's 
   1  536 

patient.drugs.drug.7.regimen_indication:
progression        NA's 
          1         536 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
subcutaneous (sc)              NA's 
                1               536 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   1  536 

patient.drugs.drug.7.therapy_types.therapy_type:
chemotherapy         NA's 
           1          536 

patient.drugs.drug.7.year_of_form_completion:
2011 NA's 
   1  536 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-cj-4637-d9503               NA's 
                 1                536 

patient.drugs.drug.8.bcr_drug_uuid:
43b4785b-3f0f-4184-be86-dacff571cf29                                 NA's 
                                   1                                  536 

patient.drugs.drug.8.day_of_form_completion:
  25 NA's 
   1  536 

patient.drugs.drug.8.days_to_drug_therapy_end:
1468 NA's 
   1  536 

patient.drugs.drug.8.days_to_drug_therapy_start:
1213 NA's 
   1  536 

patient.drugs.drug.8.drug_name:
nab-rapamycin          NA's 
            1           536 

patient.drugs.drug.8.month_of_form_completion:
   3 NA's 
   1  536 

patient.drugs.drug.8.regimen_indication:
progression        NA's 
          1         536 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
oral NA's 
   1  536 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   1  536 

patient.drugs.drug.8.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        536 

patient.drugs.drug.8.year_of_form_completion:
2011 NA's 
   1  536 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-cj-4637-d9499               NA's 
                 1                536 

patient.drugs.drug.9.bcr_drug_uuid:
191125ca-4520-483c-b644-25d9065e0a1c                                 NA's 
                                   1                                  536 

patient.drugs.drug.9.day_of_form_completion:
  25 NA's 
   1  536 

patient.drugs.drug.9.days_to_drug_therapy_end:
 476 NA's 
   1  536 

patient.drugs.drug.9.days_to_drug_therapy_start:
  51 NA's 
   1  536 

patient.drugs.drug.9.drug_name:
roferon-a      NA's 
        1       536 

patient.drugs.drug.9.month_of_form_completion:
   3 NA's 
   1  536 

patient.drugs.drug.9.regimen_indication:
progression        NA's 
          1         536 

patient.drugs.drug.9.route_of_administrations.route_of_administration:
subcutaneous (sc)              NA's 
                1               536 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   1  536 

patient.drugs.drug.9.therapy_types.therapy_type:
chemotherapy         NA's 
           1          536 

patient.drugs.drug.9.year_of_form_completion:
2011 NA's 
   1  536 



patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   16.00   17.06   28.00   31.00     452 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -28.0   133.5   354.5   503.6   643.8  2572.0     473 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -92.00   52.75  191.00  447.77  474.25 2752.00     453 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           8                            1 
              stable disease                         NA's 
                           2                          526 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.024   5.000  12.000     452 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    4.00    7.75    6.00   65.00     497 

patient.drugs.drug.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0      25      50  644002     800 9000000     505 




patient.drugs.drug.regimen_number:
   1    2    3    4 NA's 
  29    7    2    1  498 

patient.drugs.drug.route_of_administrations.route_of_administration:
 intravenous (iv)              oral subcutaneous (sc)              NA's 
               15                36                14               472 

patient.drugs.drug.route_of_administrations.route_of_administration.2:
oral NA's 
   1  536 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
  72    9  456 

patient.drugs.drug.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                        18                         24 
            other, specify targeted molecular therapy 
                         6                         36 
                      NA's 
                       453 

patient.drugs.drug.therapy_types.therapy_type_notes:
biological therapy     clinical trial           su011248               NA's 
                 3                  1                  1                532 


patient.drugs.drug.total_dose_units:
[iu]/kg      mg  mg/day   mg/wk    NA's 
      3       3       1       1     529 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  12    1  524 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2011    2015     452 

patient.eastern_cancer_oncology_group:
   0    1    2 NA's 
 109   26    3  399 

patient.erythrocyte_sedimentation_rate_result:
elevated   normal     NA's 
       2        8      527 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    26                    359                    152 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
  no  yes NA's 
  24   12  501 

patient.follow_ups.follow_up.2.additional_radiation_therapy:
  no  yes NA's 
  23   13  501 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure:
  no  yes NA's 
  25    2  510 

patient.follow_ups.follow_up.2.additional_surgery_metastatic_procedure:
  no  yes NA's 
  24    5  508 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   13.00   22.00   19.28   26.00   31.00     396 

patient.follow_ups.follow_up.2.days_to_additional_surgery_locoregional_procedure:
1646 2100 NA's 
   1    1  535 

patient.follow_ups.follow_up.2.days_to_additional_surgery_metastatic_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     29     277    1441    1520    1912    3939     532 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    162     651    1003    1326    1912    3615     518 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     16     963    1782    1978    2873    4537     416 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   23.0   396.0   700.0   919.2  1189.0  3766.0     500 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   0    1    3 NA's 
  13    4    1  519 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                            117 
                          NA's 
                           418 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response  partial remission/response 
                         68                           1 
        progressive disease              stable disease 
                         11                           6 
                       NA's 
                        451 

patient.follow_ups.follow_up.2.karnofsky_performance_score:
  80   90  100 NA's 
   1    3    5  528 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  84    5  448 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    3.00    4.95    7.00   11.00     396 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  67   18  452 

patient.follow_ups.follow_up.2.performance_status_scale_timing:
at recurrence/progression of disease                                other 
                                   4                                   11 
               post adjuvant therapy               post secondary therapy 
                                   1                                    1 
                                NA's 
                                 520 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        78         47        412 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         73                           2 
        progressive disease              stable disease 
                          7                           2 
                       NA's 
                        453 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  80    2  455 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
  77    6  454 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
  121    19   397 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2013    2015    2015     396 

patient.follow_ups.follow_up.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   9    2  526 

patient.follow_ups.follow_up.3.additional_radiation_therapy:
  no  yes NA's 
   5    6  526 

patient.follow_ups.follow_up.3.additional_surgery_locoregional_procedure:
  no NA's 
  12  525 

patient.follow_ups.follow_up.3.additional_surgery_metastatic_procedure:
  no  yes NA's 
  11    2  524 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   12.25   18.00   17.32   25.00   31.00     509 

patient.follow_ups.follow_up.3.days_to_additional_surgery_metastatic_procedure:
 684  905 NA's 
   1    1  535 

patient.follow_ups.follow_up.3.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  162.0   567.5   915.0  1246.3  1912.0  2564.0     527 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    511    1238    1522    1840    2671    3498     519 

patient.follow_ups.follow_up.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0   299.0   657.0   811.5  1227.0  2004.0     524 

patient.follow_ups.follow_up.3.eastern_cancer_oncology_group:
   0    1 NA's 
   4    2  531 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                             15 
                          NA's 
                           520 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response         progressive disease 
                          9                           1 
             stable disease                        NA's 
                          1                         526 

patient.follow_ups.follow_up.3.karnofsky_performance_score:
  90  100 NA's 
   3    2  532 

patient.follow_ups.follow_up.3.lost_follow_up:
  no  yes NA's 
   9    3  525 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   4.286   5.000  10.000     509 

patient.follow_ups.follow_up.3.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   9    2  526 

patient.follow_ups.follow_up.3.performance_status_scale_timing:
at recurrence/progression of disease                                other 
                                   1                                    2 
              post secondary therapy                                 NA's 
                                   1                                  533 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        10         17        510 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          8                         529 

patient.follow_ups.follow_up.3.radiation_therapy:
  no NA's 
   8  529 

patient.follow_ups.follow_up.3.targeted_molecular_therapy:
  no NA's 
   8  529 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
   18    10   509 

patient.follow_ups.follow_up.3.year_of_form_completion:
2011 2012 2014 2015 NA's 
  11    1   11    5  509 

patient.follow_ups.follow_up.4.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    1  534 

patient.follow_ups.follow_up.4.additional_radiation_therapy:
  no NA's 
   3  534 

patient.follow_ups.follow_up.4.additional_surgery_locoregional_procedure:
  no NA's 
   3  534 

patient.follow_ups.follow_up.4.additional_surgery_metastatic_procedure:
  no NA's 
   3  534 



patient.follow_ups.follow_up.4.day_of_form_completion:
   2   18   30   31 NA's 
   2    1    1    1  532 

patient.follow_ups.follow_up.4.days_to_death:
 475 1912 NA's 
   1    2  534 

patient.follow_ups.follow_up.4.days_to_last_followup:
1329 1398 NA's 
   1    1  535 

patient.follow_ups.follow_up.4.days_to_new_tumor_event_after_initial_treatment:
 454 1530 1709 NA's 
   1    1    1  534 

patient.follow_ups.follow_up.4.eastern_cancer_oncology_group:
   0 NA's 
   2  535 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
scheduled follow-up submission                           NA's 
                             1                            536 

patient.follow_ups.follow_up.4.karnofsky_performance_score:
 100 NA's 
   2  535 

patient.follow_ups.follow_up.4.month_of_form_completion:
   3    5    7 NA's 
   2    2    1  532 

patient.follow_ups.follow_up.4.performance_status_scale_timing:
other  NA's 
    2   535 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
with tumor       NA's 
         4        533 

patient.follow_ups.follow_up.4.vital_status:
alive  dead  NA's 
    2     3   532 

patient.follow_ups.follow_up.4.year_of_form_completion:
2011 NA's 
   5  532 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
  80   70  387 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
 112   39  386 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure:
  no  yes NA's 
 143   16  378 

patient.follow_ups.follow_up.additional_surgery_metastatic_procedure:
  no  yes NA's 
 125   30  382 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   15.00   15.51   22.00   31.00       8 

patient.follow_ups.follow_up.days_to_additional_surgery_locoregional_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  100.0   251.2   429.5   762.4   783.2  2764.0     523 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -43.0   162.0   425.0   560.5   745.5  2186.0     506 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   329.8   769.0   897.6  1350.0  2830.0     369 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     481    1280    1269    1874    3668     170 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -359.0   138.8   358.0   666.5   869.8  3250.0     423 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   0    1    2 NA's 
  53   18    6  460 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                            157 
                          NA's 
                           378 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response  partial remission/response 
                         33                           1 
        progressive disease              stable disease 
                          4                           4 
                       NA's 
                        495 

patient.follow_ups.follow_up.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00  100.00   80.37  100.00  100.00     510 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
  42   15  480 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.773   5.000  12.000       8 

patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  41    4  492 

patient.follow_ups.follow_up.performance_status_scale_timing:
at recurrence/progression of disease                                other 
                                  52                                   35 
               post adjuvant therapy               post secondary therapy 
                                   2                                    4 
                                NA's 
                                 444 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       340        152         45 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         33                           2 
        progressive disease              stable disease 
                          3                           3 
                       NA's 
                        496 

patient.follow_ups.follow_up.radiation_therapy:
  no NA's 
  52  485 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
  49    2  486 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  360   168     9 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2005    2011    2011    2011    2012    2015       8 

patient.gender:
female   male 
   191    346 

patient.hemoglobin_result:
elevated      low   normal     NA's 
       5      263      186       83 

patient.histological_type:
kidney clear cell renal carcinoma 
                              537 

patient.history_of_neoadjuvant_treatment:
 no yes 
519  18 

patient.icd_10:
c64.9 
  537 

patient.icd_o_3_histology:
8310/3 8312/3 
   523     14 

patient.icd_o_3_site:
c64.9 
  537 

patient.informed_consent_verified:
yes 
537 

patient.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    90.0    90.0    88.3   100.0   100.0     490 

patient.lactate_dehydrogenase_result:
elevated   normal     NA's 
      12       74      451 

patient.laterality:
bilateral      left     right 
        1       253       283 

patient.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   6.298   9.000  30.000     403 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    2.00    3.00    4.52    5.00   12.00 


patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no NA's 
   3  534 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   2    1  534 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 266 2008 NA's 
   1    1  535 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.additional_surgery_locoregional_procedure:
  no NA's 
   1  536 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.additional_surgery_metastatic_procedure:
  no NA's 
   1  536 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  32    2  503 

patient.number_of_lymphnodes_positive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3359  0.0000 15.0000     406 

patient.number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   14.00   30.00   28.33   40.00   65.00     516 


patient.performance_status_scale_timing:
        other pre-operative          NA's 
           13            96           428 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       364        145         28 

patient.platelet_qualitative_result:
elevated      low   normal     NA's 
      38       46      360       93 

patient.primary_lymph_node_presentation_assessment:
  no  yes NA's 
 395  135    7 

patient.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                         27                           2 
             stable disease                        NA's 
                          1                         507 

patient.prior_dx:
                                                     no 
                                                    459 
                                                    yes 
                                                     72 
                       yes, history of prior malignancy 
                                                      2 
yes, history of synchronous and or bilateral malignancy 
                                                      4 

patient.race:
                    asian black or african american                     white 
                        8                        56                       466 
                     NA's 
                        7 

patient.radiation_therapy:
  no NA's 
  31  506 

patient.radiations.radiation.2.anatomic_treatment_site:
distant recurrence       distant site               NA's 
                 3                 11                523 



patient.radiations.radiation.2.course_number:
   1    2    3    4 NA's 
   3    5    1    1  527 

patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    7.75   15.00   16.57   25.00   31.00     523 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -50.0   151.2   285.5   429.1   598.8  1787.0     523 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -72.0   147.8   282.0   422.1   576.2  1787.0     523 

patient.radiations.radiation.2.measure_of_response:
radiographic progressive disease                             NA's 
                               1                              536 

patient.radiations.radiation.2.month_of_form_completion:
   3    4    5   11 NA's 
   6    2    5    1  523 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   5.545   9.500  15.000     526 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     30    1950    2450    2314    3075    3750     525 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
  14  523 

patient.radiations.radiation.2.radiation_type:
               external           external beam other: specify in notes 
                      1                       7                       6 
                   NA's 
                    523 

patient.radiations.radiation.2.radiation_type_notes:
            not specified stereotactic radiosurgery stereotactic radiotherapy 
                        1                         3                         1 
    whole brain radiation                      NA's 
                        1                       531 

patient.radiations.radiation.2.regimen_indication:
other: specify in notes              palliative             progression 
                      1                       4                       7 
             recurrence                    NA's 
                      1                     524 

patient.radiations.radiation.2.regimen_indication_notes:
metastasis       NA's 
         1        536 

patient.radiations.radiation.2.units:
 cgy NA's 
  12  525 

patient.radiations.radiation.2.year_of_form_completion:
2010 2011 2012 2013 NA's 
   1   10    2    1  523 

patient.radiations.radiation.3.anatomic_treatment_site:
distant recurrence       distant site               NA's 
                 2                  1                534 

patient.radiations.radiation.3.bcr_radiation_barcode:
tcga-bp-4352-r11091  tcga-cj-4637-r9509  tcga-cj-4868-r9082                NA's 
                  1                   1                   1                 534 

patient.radiations.radiation.3.bcr_radiation_uuid:
0e294356-e8e1-4753-a8fa-147a165f223d 58fd569c-30ed-4a95-bf13-b8f9b37e1793 
                                   1                                    1 
ebcc2afd-0aae-4616-996b-93b3e6bfa750                                 NA's 
                                   1                                  534 

patient.radiations.radiation.3.course_number:
   2    3 NA's 
   1    2  534 

patient.radiations.radiation.3.day_of_form_completion:
   3   13   25 NA's 
   1    1    1  534 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
 188  277 1770 NA's 
   1    1    1  534 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
 183  271 1755 NA's 
   1    1    1  534 

patient.radiations.radiation.3.month_of_form_completion:
   3    5 NA's 
   2    1  534 

patient.radiations.radiation.3.numfractions:
   3    5 NA's 
   2    1  534 

patient.radiations.radiation.3.radiation_dosage:
 360 2700 3000 NA's 
   1    1    1  534 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   3  534 

patient.radiations.radiation.3.radiation_type:
          external beam other: specify in notes                    NA's 
                      1                       2                     534 

patient.radiations.radiation.3.radiation_type_notes:
            not specified stereotactic radiosurgery                      NA's 
                        1                         1                       535 

patient.radiations.radiation.3.regimen_indication:
palliative recurrence       NA's 
         2          1        534 

patient.radiations.radiation.3.units:
 cgy NA's 
   3  534 

patient.radiations.radiation.3.year_of_form_completion:
2011 NA's 
   3  534 

patient.radiations.radiation.4.anatomic_treatment_site:
distant recurrence               NA's 
                 1                536 

patient.radiations.radiation.4.bcr_radiation_barcode:
tcga-cj-4637-r9508               NA's 
                 1                536 

patient.radiations.radiation.4.bcr_radiation_uuid:
1e71eff3-84ec-469c-ab64-004c5f8d757c                                 NA's 
                                   1                                  536 

patient.radiations.radiation.4.course_number:
   2 NA's 
   1  536 

patient.radiations.radiation.4.day_of_form_completion:
  25 NA's 
   1  536 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
1185 NA's 
   1  536 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
1174 NA's 
   1  536 

patient.radiations.radiation.4.month_of_form_completion:
   3 NA's 
   1  536 

patient.radiations.radiation.4.numfractions:
  10 NA's 
   1  536 

patient.radiations.radiation.4.radiation_dosage:
 300 NA's 
   1  536 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   1  536 

patient.radiations.radiation.4.radiation_type:
external beam          NA's 
            1           536 

patient.radiations.radiation.4.regimen_indication:
palliative       NA's 
         1        536 

patient.radiations.radiation.4.units:
 cgy NA's 
   1  536 

patient.radiations.radiation.4.year_of_form_completion:
2011 NA's 
   1  536 

patient.radiations.radiation.5.anatomic_treatment_site:
distant recurrence               NA's 
                 1                536 

patient.radiations.radiation.5.bcr_radiation_barcode:
tcga-cj-4637-r9511               NA's 
                 1                536 

patient.radiations.radiation.5.bcr_radiation_uuid:
f89fed82-6f6b-4114-be5f-e2ed1c9ae196                                 NA's 
                                   1                                  536 

patient.radiations.radiation.5.course_number:
   5 NA's 
   1  536 

patient.radiations.radiation.5.day_of_form_completion:
  25 NA's 
   1  536 

patient.radiations.radiation.5.days_to_radiation_therapy_end:
1918 NA's 
   1  536 

patient.radiations.radiation.5.days_to_radiation_therapy_start:
1903 NA's 
   1  536 

patient.radiations.radiation.5.month_of_form_completion:
   3 NA's 
   1  536 

patient.radiations.radiation.5.numfractions:
  12 NA's 
   1  536 

patient.radiations.radiation.5.radiation_dosage:
3600 NA's 
   1  536 

patient.radiations.radiation.5.radiation_treatment_ongoing:
  no NA's 
   1  536 

patient.radiations.radiation.5.radiation_type:
external beam          NA's 
            1           536 

patient.radiations.radiation.5.regimen_indication:
palliative       NA's 
         1        536 

patient.radiations.radiation.5.units:
 cgy NA's 
   1  536 

patient.radiations.radiation.5.year_of_form_completion:
2011 NA's 
   1  536 

patient.radiations.radiation.anatomic_treatment_site:
 distant recurrence        distant site primary tumor field                NA's 
                  4                  47                   1                 485 



patient.radiations.radiation.course_number:
   1    2    3    6 NA's 
  24    3    1    1  508 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   20.00   17.04   25.00   30.00     484 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -156.0   115.2   271.0   490.2   711.5  2170.0     487 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -163.0   103.0   298.0   482.5   704.0  2166.0     484 

patient.radiations.radiation.measure_of_response:
                partial response radiographic progressive disease 
                               1                                2 
                            NA's 
                             534 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.057   5.000  11.000     484 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.522  10.000  15.000     514 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     20    1800    3000    2860    3500   20000     500 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  52  485 

patient.radiations.radiation.radiation_type:
               external           external beam other: specify in notes 
                      2                      29                      15 
                   NA's 
                    491 


patient.radiations.radiation.regimen_indication:
other: specify in notes              palliative             progression 
                      2                      19                      23 
             recurrence                    NA's 
                      3                     490 

patient.radiations.radiation.regimen_indication_notes:
                       metastatic to bone 
                                        1 
treatment of symptomatic brain metastases 
                                        1 
                                     NA's 
                                      535 

patient.radiations.radiation.units:
 cgy   gy NA's 
  37    1  499 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2012    2015     484 

patient.serum_calcium_result:
elevated      low   normal     NA's 
      10      204      151      172 

patient.stage_event.pathologic_stage:
  stage i  stage ii stage iii  stage iv 
      270        57       126        84 

patient.stage_event.system_version:
 5th  6th  7th NA's 
   5   73   92  367 

patient.stage_event.tnm_categories.clinical_categories.clinical_m:
  m0   m1   mx NA's 
  22    3    8  504 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
  m0   m1   mx NA's 
 426   79   30    2 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
 n0  n1  nx 
240  17 280 


patient.stopped_smoking_year:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1970    1983    1993    1995    2010    2012     524 

patient.targeted_molecular_therapy:
  no  yes NA's 
  28    3  506 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 465   52   20 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
  53  466   18 



patient.tumor_tissue_site:
kidney 
   537 

patient.vital_status:
alive  dead 
  375   162 

patient.white_cell_count_result:
elevated      low   normal     NA's 
     164        9      268       96 

patient.year_of_form_completion:
2010 2011 2013 2014 
 130  372   16   19 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1998    2004    2006    2006    2007    2013 

patient.year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1946    1966    1978    1979    1996    2001     525 


patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              72                                6 
                            NA's 
                             459 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                  7                 530 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus    non-canonical 
              94              442                1 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      424       113 

patient.biospecimen_cqcf.country:
united states          NA's 
           94           443 

patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1     944    1635    1565    2276    4541      36 

patient.biospecimen_cqcf.days_to_sample_procurement:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-246.000    0.000    0.000    8.493    0.000 1248.000 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
496  41 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     2                     31                    504 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
kidney 
   537 

patient.biospecimen_cqcf.histological_type:
kidney clear cell renal carcinoma 
                              537 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             524 
yes, pharmaceutical treatment prior to resection 
                                               9 
               yes, radiation prior to resection 
                                               4 

patient.biospecimen_cqcf.laterality:
 left right 
  255   282 

patient.biospecimen_cqcf.maximum_tumor_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.700   3.700   5.500   6.402   8.250  25.000       1 



patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-20.000   0.000   0.000   9.258   0.000 129.000     506 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0561  0.0945  0.2300  0.2152  0.3085  0.3910     531 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
nanodrop     NA's 
       6      531 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
 9.53 14.59    15  19.6  NA's 
    1     1     3     1   531 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  38.36   48.64   54.88  124.40  165.00  350.00     531 


patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
         kidney kidney; medulla   kidney; mixed            NA's 
              9               1               6             521 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           16           521 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
  18   14  505 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
3c4f15d4-dcf6-4d60-b86c-6006f9051e54                                 NA's 
                                   1                                  536 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
   0 NA's 
   1  536 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
laparoscopic radical nephrectomy                             NA's 
                               1                              536 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
kidney   NA's 
     1    536 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            1           536 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1 NA's 
   1  536 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-501.00    0.00    0.00   16.94    0.00 2097.00       4 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0379   0.1239   0.2190  16.9004   0.3000 717.7000      476 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.58   15.00   30.00   24.18   30.00   51.70     476 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.9   100.0   141.2   155.6   200.0   400.0     476 


patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
         kidney  kidney; cortex kidney; medulla   kidney; mixed            NA's 
            232              55              22             123             105 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
          386           151 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2 NA's 
 519   17    1 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
yes 
537 

patient.biospecimen_cqcf.path_confirm_report_attached:
  no  yes NA's 
  32  504    1 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
  no  yes NA's 
  10  508   19 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
yes 
537 

patient.biospecimen_cqcf.race:
black or african american                      NA's 
                       35                       502 

patient.biospecimen_cqcf.sample_prescreened:
yes 
537 

patient.biospecimen_cqcf.site_of_disease:
kidney 
   537 

patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
  45    1  491 

patient.biospecimen_cqcf.top_slide_submitted:
 no yes 
 77 460 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid:
06f27ecd-7877-4452-87a7-a4fbdfca570c 31bc9cec-2e1b-4797-bafb-9eb31cdee94d 
                                   1                                    1 
d7554387-d8bc-47b0-96cb-5ec27fa63f5d dd662d3e-e235-441c-af96-beb426beeda6 
                                   1                                    1 
                                NA's 
                                 533 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0 NA's 
   4  533 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  70   80 NA's 
   1    3  533 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   30    80 136.5   240  NA's 
    1     1     1     1   533 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   2 NA's 
   4  533 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   7.945  10.750  30.000      29 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   70.00   85.00   82.68   90.00  100.00       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0   215.8   275.0   347.8   400.0  2500.0      53 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1 NA's 
 536    1 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                      1                      70                      53 
               cryovial  other (please specify) 
                    159                     254 

patient.days_to_index:
  0 
537 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and additional primary tumor(s) 
                                            1 
                                         NA's 
                                          536 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   57.0   150.5   400.0   555.0   661.5  1874.0     498 

patient.samples.sample.2.diagnostic_slides.ffpe_slide_uuid:
04222a5c-a2b2-4206-8a54-66c7474982d8 48a5a3fc-18cb-4417-9638-a73d17b81085 
                                   1                                    1 
751b1107-fa18-4df5-89ae-42d06caf9b74 c55a29ca-468e-46de-9ff5-a51f46fe09fd 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.2.initial_weight:
 160 NA's 
   1  536 

patient.samples.sample.2.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.7000  0.8000  0.8494  1.0000  1.8000     116 

patient.samples.sample.2.is_ffpe:
 no yes 
533   4 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.200   1.500   1.683   2.000   4.000     116 

patient.samples.sample.2.oct_embedded:
false  true  NA's 
   38     1   498 



patient.samples.sample.2.portions.portion.2.analytes.analyte.a260_a280_ratio:
 1.8 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-cz-5456-11a-02r-1503-07                         NA's 
                           1                          536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
0164ecc1-16dd-42c8-814d-d787220f475e                                 NA's 
                                   1                                  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
1049985467       NA's 
         1        536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   7 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.13 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  12 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   1 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   8 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
1503 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   a NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
3.47 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
26.7 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-cz-5456-11a-02r-1502-13                         NA's 
                           1                          536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
50d6cf7e-9339-4567-af25-a68aaf450816                                 NA's 
                                   1                                  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
1049967535       NA's 
         1        536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
  13 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.13 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  12 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   1 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   8 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
1502 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   a NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
 2.6 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  20 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.amount:
34.8 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.analyte_type:
 rna NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.analyte_type_id:
   r NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.bcr_analyte_barcode:
tcga-cz-5456-11a-02r                 NA's 
                   1                  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.bcr_analyte_uuid:
ff8a3de4-ede6-4428-bc11-adae16b28155                                 NA's 
                                   1                                  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.concentration:
0.13 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        1                       536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
1.33 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.rna.rinvalue:
 8.3 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      1     536 

patient.samples.sample.2.portions.portion.2.bcr_portion_barcode:
tcga-cz-5456-11a-02                NA's 
                  1                 536 

patient.samples.sample.2.portions.portion.2.bcr_portion_uuid:
275b2ff3-efff-4ea0-ade6-4ee32ad3f260                                 NA's 
                                   1                                  536 

patient.samples.sample.2.portions.portion.2.day_of_creation:
  27 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.is_ffpe:
  no NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.month_of_creation:
  12 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.portion_number:
   2 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.slides.slide.bcr_slide_barcode:
tcga-cz-5456-11a-01-ts1                    NA's 
                      1                     536 

patient.samples.sample.2.portions.portion.2.slides.slide.bcr_slide_uuid:
54113269-f3e4-4c5a-862e-71ea7289ea40                                 NA's 
                                   1                                  536 

patient.samples.sample.2.portions.portion.2.slides.slide.image_file_name:
tcga-cz-5456-11a-01-ts1.54113269-f3e4-4c5a-862e-71ea7289ea40.svs 
                                                               1 
                                                            NA's 
                                                             536 

patient.samples.sample.2.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.slides.slide.section_location:
 top NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.weight:
  36 NA's 
   1  536 

patient.samples.sample.2.portions.portion.2.year_of_creation:
2010 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   1.800   1.800   1.785   1.800   1.880     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   4  533 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   4  533 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.649e+07 1.005e+08 4.293e+08 1.050e+09 1.050e+09       395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   10   13 NA's 
  64   74    4  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.150   0.500   0.333   0.500   0.500     395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    5.00   12.00   11.44   13.00   30.00     395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
 138    4  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.218   4.000   9.000     395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   7.000   9.000   8.648  10.000  12.000     395 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.08    4.01   40.00   22.31   40.00   40.00     395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
 137    5  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 26.7   80 NA's 
   1    6   63   72  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 NA's 
  72   63    2    5  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-b2-3924-01b-03r-a46x-36 tcga-b2-5633-01b-04r-a46x-36 
                           1                            1 
tcga-b2-5635-01b-04r-a46x-36                         NA's 
                           1                          534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
61eb1874-1e8e-4f02-a03d-71d727bb57fe e5629820-4944-4ec0-b158-f5ac416086dd 
                                   1                                    1 
e84e348f-a7a3-45b6-93a8-3cb6ad911782                                 NA's 
                                   1                                  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
167814209 167814210 167814222      NA's 
        1         1         1       534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
  36 NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 0.17 NA's 
   1    2  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  26 NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
 yes NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   6    7 NA's 
   1    2  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a46x NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   b    c NA's 
   1    2  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
0.75 0.85 NA's 
   1    2  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   5 NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2015 NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   4  533 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   4  533 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.039e+08 1.087e+08 4.984e+08 1.050e+09 1.054e+09         5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   10   13 NA's 
   5   98  365   64    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4548  0.5000  0.5000       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   20.00   15.32   25.00   31.00       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
 528    4    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000  10.000   7.611  11.000  12.000       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   9.000   7.902  10.000  12.000       5 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.86   40.00   40.00   35.19   40.00   40.00       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 493   39    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   80.00   80.00   72.14   80.00   80.00       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2013 2014 NA's 
 350  143   15   24    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    96.2   159.0   139.3   173.0   209.0      39 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                   452                     11                     69 
                  NA's 
                     5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w    x NA's 
  69  452   11    5 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4517  0.5000  0.5000      39 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 463   74 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
 429   34   74 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             532 
                                            NA's 
                                               5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no  yes NA's 
 528    4    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
highpure mirna (allprep dna) ffpe rna             mirvana (allprep dna) rna 
                                    4                                    65 
                              repli-g                             repli-g x 
                                  452                                    11 
                                 NA's 
                                    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.290   1.450   1.376   1.570   1.940     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   7.400   8.000   7.691   8.500   9.800     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    498      39 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   1  536 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100521584 100521767 100521790 102204504 100521812 128286814       504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
  10 NA's 
  33  504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
  33  504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  16   20 NA's 
   2   31  504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  33  504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   5    9 NA's 
   2   31  504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.000   7.000   9.000   8.424  10.000  11.000     504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1245 2279 NA's 
  31    2  504 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  10   40 NA's 
   2   31  504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
  33  504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  20   80 NA's 
   2   31  504 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2012 NA's 
  31    2  504 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.039e+08 1.039e+08 3.265e+08 1.040e+08 1.054e+09       265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8   10 NA's 
  46  226  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 272  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    8.00   12.64   25.00   25.00     265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 272  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   9.000  10.000   8.599  11.000  11.000     265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   9.000   8.018  11.000  12.000     265 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 272  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
 271    1  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 272  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2013 NA's 
 207   64    1  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   147.5   160.0   153.6   173.0   296.0     266 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                    67                    205                    265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
  67  205  265 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
 271  266 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 272  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
 271    1  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             272 
                                            NA's 
                                             265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 272  265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
       67       205       265 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
    271     266 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-ak-3440-10a-01x-1360-10 tcga-ak-3453-10a-01x-1360-10 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
8bf0b4c8-6d00-40d6-a8e3-0ac29f8ff83d b24c2fe3-37fc-49e8-8323-253953e42722 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
103950526 103950559      NA's 
        1         1       535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  25 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
  10 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   8 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
1360 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   a    d NA's 
   1    1  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.amount:
 153  161 NA's 
   1    1  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type:
repli-g x (qiagen) dna                   NA's 
                     2                    535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type_id:
   x NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.bcr_analyte_barcode:
tcga-ak-3440-10a-01x tcga-ak-3453-10a-01x                 NA's 
                   1                    1                  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.bcr_analyte_uuid:
65cb158d-0348-43db-b0fe-32d3c70447a1 6d9dca52-3b53-4c61-97fe-9a7947f1c0ed 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
        2       535 

patient.samples.sample.2.portions.portion.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      2     535 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   1.900   1.900   1.896   1.900   2.300      41 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   3  534 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.040e+08 1.040e+08 2.876e+08 1.322e+08 1.050e+09         3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2    5   10 NA's 
 399   59   31   45    3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1439  0.1500  0.1700       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0    11.0    17.0    17.6    25.0    30.0       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
 531    3    3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   9.000   6.878  10.000  12.000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   8.000   8.118  10.000  12.000       3 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.000   1.000   1.415   1.130   4.270       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 496   38    3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67   10.76    6.70   40.00       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2013 2014 NA's 
 353  143   24   14    3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   3  534 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   3  534 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.040e+08 1.040e+08 2.966e+08 1.070e+08 1.050e+09        37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8   10 NA's 
 324   69  107   37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1100  0.1500  0.1235  0.1500  0.1700      37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   11.00   20.00   17.67   25.00   30.00      37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
 497    3   37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.132  10.000  11.000      37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   8.000   8.236  10.000  12.000      37 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.000   3.740   3.147   4.010   4.540      37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 496    4   37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   26.70   26.70   27.89   30.02   40.00      37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2013 NA's 
 351  145    4   37 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.039e+08 1.040e+08 2.586e+08 1.210e+08 1.050e+09        42 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    5    8   10 NA's 
   2   68  143  282   42 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.05000 0.05000 0.08834 0.15000 0.17000      42 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   21.00   16.76   22.00   28.00      42 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no  yes NA's 
 492    3   42 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000  10.000   8.414  11.000  12.000      42 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   9.000   8.164  10.000  12.000      42 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.000   2.000   2.171   2.000   4.540      42 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
 492    3   42 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   20.00   40.00   30.84   40.00   60.00      42 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2012 2015 NA's 
 325  166    1    3   42 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.210e+08 1.210e+08 1.295e+08 1.210e+08 1.062e+09       126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    8   10 NA's 
   2   52  357  126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1344  0.1500  0.1700     126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   21.00   21.00   19.71   21.00   27.00     126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 411  126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   12.00   12.00   11.55   12.00   12.00     126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.645   9.000  12.000     126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1251    2094    2098    1982    2102    2279     126 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.000   2.000   1.958   2.000   3.200     126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
 411  126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   17.48   13.30   60.00     126 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 2012 NA's 
  69  330   12  126 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.210e+08 1.210e+08 1.420e+08 1.283e+08 1.062e+09       433 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   5    8   10 NA's 
   1    5   98  433 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1400  0.1153  0.1500  0.1700     433 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   16.00   21.00   19.13   21.00   25.00     433 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
 104  433 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000  12.000   9.471  12.000  12.000     433 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   7.000   5.779   9.000  11.000     433 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.860   2.000   2.135   2.130   6.000     433 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
 100    4  433 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   25.05   40.00   60.00     433 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2012    2015     433 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.210e+08 1.210e+08 3.091e+08 1.210e+08 1.062e+09       532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   8   10 NA's 
   1    4  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.13 0.14 0.15 0.16 NA's 
   1    1    2    1  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  16   21 NA's 
   1    4  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   5  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
  11   12 NA's 
   1    4  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   2    9   11   12 NA's 
   1    1    1    2  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
2048 2094 2095 2101 NA's 
   1    2    1    1  532 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
0.93 1.73    2 2.13 NA's 
   1    1    2    1  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   5  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
6.67 13.3 NA's 
   1    4  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   5  532 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-bp-4977-11a-01d-2100-10                         NA's 
                           1                          536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
045f9ad4-55d4-4d70-9f0d-f8d56ad28e62                                 NA's 
                                   1                                  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
121029378      NA's 
        1       536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
  10 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.14 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  21 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
  12 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   6 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
2100 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   c NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
1.86 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
13.3 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 NA's 
   1  536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   4  533 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   4  533 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.039e+08 1.040e+08 2.888e+08 1.322e+08 1.054e+09         1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    8   10 NA's 
 135  396    1    4    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1492  0.1500  0.5000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   11.00   17.00   17.46   25.00   30.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
 532    4    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   9.000   6.849  10.000  12.000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   8.000   8.116  10.000  12.000       1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.140   2.000   1.801   2.000  40.000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 497   39    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   13.30   11.81   13.30   80.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2013 2014 NA's 
 353  144   25   14    1 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.07   48.47   96.70  116.65  146.10  741.90       1 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
                 dna repli-g (qiagen) dna                 NA's 
                 535                    1                    1 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
   d    w NA's 
 535    1    1 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1498  0.1500  0.5000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 536    1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 429  107    1 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             536 
                                            NA's 
                                               1 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no  yes NA's 
 532    4    1 


patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       39       497         1 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   1  536 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   17.00   17.17   25.00   31.00       1 

patient.samples.sample.2.portions.portion.is_ffpe:
  no  yes NA's 
 532    4    1 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   6.653   9.000  12.000       1 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.069   1.000  11.000       1 

patient.samples.sample.2.portions.portion.portion_sequence:
   1    3    4   10 NA's 
  42    1    2    1  491 




patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no  yes NA's 
 114    4  419 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   1    2    3 NA's 
   1    1    2  533 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0    1 NA's 
   1    3  533 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
   0 NA's 
   4  533 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0    1 NA's 
   3    1  533 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
   0 NA's 
   4  533 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
  10   20   40 NA's 
   1    2    1  533 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
  60   80   90 NA's 
   1    2    1  533 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
  70   92 NA's 
   1    3  533 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
     3    115    419 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no  yes NA's 
 421    4  112 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0       1       2       2       3       4     532 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1 NA's 
   2    3  532 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   5  532 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1 NA's 
   3    2  532 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0 NA's 
   5  532 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
  15   18   22   30 NA's 
   2    1    1    1  532 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
  70   78   82   85 NA's 
   1    1    1    2  532 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
  75   90   95 NA's 
   1    3    1  532 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
bottom    top   NA's 
   115    310    112 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   32.00   36.00   45.71   39.00  400.00      96 

patient.samples.sample.2.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2010    2010    2010    2014       1 

patient.samples.sample.2.portions.shipment_portion.bcr_shipment_portion_uuid:
f7ad17e2-0024-4664-bc64-9273e9096366                                 NA's 
                                   1                                  536 

patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
   1  536 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
   1  536 

patient.samples.sample.2.portions.shipment_portion.plate_id:
1741 NA's 
   1  536 

patient.samples.sample.2.portions.shipment_portion.portion_number:
  21 NA's 
   1  536 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode:
tcga-cz-5459-01b-21-1741-20                        NA's 
                          1                         536 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
   4 NA's 
   1  536 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   5 NA's 
   1  536 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2011 NA's 
   1  536 

patient.samples.sample.2.sample_type:
additional - new primary     blood derived normal            primary tumor 
                       1                      111                        5 
     solid tissue normal 
                     420 

patient.samples.sample.2.sample_type_id:
  1   5  10  11 
  5   1 111 420 

patient.samples.sample.2.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.2000  0.3000  0.3618  0.4000  1.3000     116 

patient.samples.sample.2.vial_number:
  a   b 
530   7 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  3                 534 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  141.0   243.0   400.0   576.2   416.0  1681.0     532 

patient.samples.sample.3.initial_weight:
  50  410  980 NA's 
   2    1    1  533 

patient.samples.sample.3.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.5000  0.8000  0.8000  0.7929  0.9000  0.9000     523 

patient.samples.sample.3.is_ffpe:
  no NA's 
  23  514 

patient.samples.sample.3.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.075   1.450   1.536   1.925   2.600     523 

patient.samples.sample.3.oct_embedded:
false  NA's 
    5   532 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.8 NA's 
   7  530 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.087e+08 1.050e+09 7.357e+08 1.054e+09 1.054e+09       528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   10 NA's 
   7    2  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.2267  0.1600  0.5000     528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   9   11   13   20 NA's 
   3    3    1    2  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   9  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   2    4    5    9 NA's 
   3    1    3    2  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  5.000   6.000   7.000   7.556  10.000  10.000     528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
1244 1541 1672 1758 NA's 
   2    3    1    3  528 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.13    3.74   11.28    4.01   40.00     528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   9  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3 26.7   80 NA's 
   3    4    2  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   2    7  528 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.561e+08 1.050e+09 6.839e+08 1.050e+09 1.054e+09       517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   8   10   13 NA's 
   2   11    7  517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1575  0.5000  0.3770  0.5000  0.5000     517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    5.75    9.00   11.20   14.75   26.00     517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  20  517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     2.0     3.0     3.8     5.0     9.0     517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    6.00    6.25    7.25   12.00     517 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.60    3.15   40.00   27.04   40.00   40.00     517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
  15    5  517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20   80 NA's 
   7   13  517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2013 2014 NA's 
   2   13    2    3  517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  38.80   62.85  133.70  111.95  157.00  173.00     522 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                  13                    7                  517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   7   13  517 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.150   0.500   0.336   0.500   0.500     522 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  13  524 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
   8    5  524 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              20 
                                            NA's 
                                             517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  20  517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                        7                        13                       517 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.100   1.250   1.490   1.404   1.520   1.700     530 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.200   7.350   8.000   7.943   8.350   9.000     530 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     15     522 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   4  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-b2-4098-11a-01x-1245-10 tcga-b2-4099-11a-01x-1245-10 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
aa22c743-8832-4b67-ae8a-1deb52864ad7 cfc9ca86-4391-4664-bff8-6d6fe318e7fa 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
100521573 100521752      NA's 
        1         1       535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
  10 NA's 
   2  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   2  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  20 NA's 
   2  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
   2  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   7    8 NA's 
   1    1  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1245 NA's 
   2  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   a    g NA's 
   1    1  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
   2  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   2  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
   2  535 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   2  535 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 3.379e+08 1.050e+09 7.342e+08 1.050e+09 1.054e+09       531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8   10 NA's 
   3    3  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   6  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   2    9   20 NA's 
   3    1    2  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   6  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   3    5    9 NA's 
   3    1    2  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   1    7    8    9 NA's 
   3    1    1    1  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
1243 1585 1746 NA's 
   2    3    1  531 


patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   6  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   6  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   6  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    4  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   81.0    99.0   150.0   132.3   154.5   175.0     531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                     4                      2                    531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
   4    2  531 



patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
   6  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   6  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   6  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               6 
                                            NA's 
                                             531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   6  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
        4         2       531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      6     531 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
 1.9 NA's 
  18  519 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.204e+08 1.050e+09 6.072e+08 1.050e+09 1.050e+09       514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2   10 NA's 
  10    8    5  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1500  0.1500  0.1396  0.1600  0.1700     514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    9.00   12.00   13.83   16.00   30.00     514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  23  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   4.826   9.000  12.000     514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   6.000   6.348  10.500  12.000     514 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.070   2.000   1.855   2.130   3.200     514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
  18    5  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7 13.3   40 NA's 
   8    2    8    5  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2013 NA's 
   5   13    5  514 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.047e+08 1.050e+09 7.344e+08 1.050e+09 1.050e+09       519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8   10 NA's 
   7    3    8  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 0.15 0.16 NA's 
  11    2    5  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    9.00    9.00   11.67   13.75   25.00     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  18  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   3.278   2.000  12.000     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   6.500   6.278  11.000  12.000     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1186    1468    1534    1491    1535    1669     519 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   2 4.01 4.27 NA's 
  11    2    5  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
  18  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
26.7   40 NA's 
   7   11  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
   3   15  519 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.051e+08 1.050e+09 6.313e+08 1.050e+09 1.050e+09       519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8   10 NA's 
   7    6    5  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.05 0.15 0.16 NA's 
   7    5    6  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    9.00   10.50   12.44   14.00   21.00     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  18  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   3.944   3.500  12.000     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.500   6.889  11.000  12.000     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1250    1470    1536    1563    1553    2103     519 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.600   2.000   2.000   2.788   4.010   4.270     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
  18  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10 13.3 26.7   40 NA's 
   2    2    7    7  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
   2   16  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   4  533 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   4  533 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             533 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.210e+08 1.210e+08 2.751e+08 1.322e+08 1.050e+09       519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
  10 NA's 
  18  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0800  0.1500  0.1156  0.1575  0.1600     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   13.25   20.00   17.17   21.00   21.00     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  18  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2    3    9   12 NA's 
   4    4    2    8  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   7.000   7.111   8.000  10.000     519 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.070   1.650   2.000   1.841   2.000   2.130     519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  14    4  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
 6.7 13.3   20   40 NA's 
   2    8    4    4  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 2013 NA's 
   2   12    4  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   3  534 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
120998803 120998865 121030200 129136966 141328464 145675713       527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   1   10 NA's 
   3    7  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.05 0.15 0.16 NA's 
   1    2    7  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  14   16   21 NA's 
   3    1    6  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  10  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   3    5   12 NA's 
   3    1    6  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    5.25    8.00    7.10    8.00   10.00     527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
2095 2096 2279 a274 NA's 
   4    2    1    3  527 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   1.302   2.130   1.880   2.130   3.000     527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
   7    3  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
 6.7 13.3   60 NA's 
   3    6    1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 2012 2013 NA's 
   6    1    3  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-b2-3924-10a-01d-a46w-08 tcga-b2-5633-10a-01d-a46w-08 
                           1                            1 
tcga-b2-5635-10a-01d-a46w-08                         NA's 
                           1                          534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
1ba8a90f-b20e-4457-9e6e-5879934494cd 1e5aa7c1-197a-4452-921a-95216167282f 
                                   1                                    1 
5e5a9c17-1500-4fa1-848c-122e1252b0fe                                 NA's 
                                   1                                  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
132179141 145674240 145674241      NA's 
        1         1         1       534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   8 NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.08 NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   3 NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   8 NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   8    9 NA's 
   1    2  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
a46w NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   b    e    g NA's 
   1    1    1  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
 1.6 NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
  20 NA's 
   3  534 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2015 NA's 
   3  534 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.204e+08 1.050e+09 6.072e+08 1.050e+09 1.050e+09       514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2 NA's 
  13   10  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.15 0.16 0.17 NA's 
   9   12    2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    9.00   12.00   14.48   21.00   30.00     514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  23  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   4.826   9.000  12.000     514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   6.000   6.348  10.500  12.000     514 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.040   1.070   1.505   2.130   2.260     514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
  18    5  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7 13.3 NA's 
   8    5   10  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2013 NA's 
   5   13    5  514 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  16.37   41.85  107.40  166.89  212.30  487.40     514 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  23  514 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  23  514 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
0.15 0.16 0.17 NA's 
   9   12    2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  23  514 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
  16    7  514 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              23 
                                            NA's 
                                             514 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  23  514 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                               18                                 4 
pre-extracted dna received by tss                              NA's 
                                1                               514 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
        5        18       514 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   4  533 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   16.50   24.00   22.26   28.00   30.00     514 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  23  514 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000  11.000   8.522  12.000  12.000     514 

patient.samples.sample.3.portions.portion.portion_number:
   1   11 NA's 
  19    4  514 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
   8  529 




patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   6  531 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location:
 top NA's 
   6  531 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  18  519 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    2    3    5 NA's 
   1    1    1    1  533 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1 NA's 
   2    2  533 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   4  533 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    2 NA's 
   3    1  533 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
  70   80  100 NA's 
   2    1    1  533 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0   20   30 NA's 
   1    1    2  533 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   4  533 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   4  533 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     6     12    519 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   30.00   32.50   31.50   36.75   89.00     519 

patient.samples.sample.3.portions.portion.year_of_creation:
2010 2011 2012 2013 NA's 
  13    2    5    3  514 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   5                   18                  514 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
   5   18  514 

patient.samples.sample.3.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.2000  0.3500  0.4143  0.5750  0.9000     523 

patient.samples.sample.3.vial_number:
   a NA's 
  23  514 

patient.samples.sample.4.bcr_sample_barcode:
tcga-b2-3923-11a tcga-b2-3924-11a tcga-b2-5633-11a tcga-b2-5635-11a 
               1                1                1                1 
            NA's 
             533 

patient.samples.sample.4.bcr_sample_uuid:
354eacdb-c9b3-49d1-b0f6-c248ca599861 3c879277-3f05-4855-ad5f-5b4f7ea5e3c0 
                                   1                                    1 
42406d67-024d-48f0-b93a-6d4d7db10e95 eca45c05-9488-4cc8-9912-00e1208636dd 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.intermediate_dimension:
 0.4  0.7  0.9    1 NA's 
   1    1    1    1  533 

patient.samples.sample.4.is_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.longest_dimension:
 1.2    2  2.2  2.5 NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-b2-3923-11a-01w-1244-10 tcga-b2-3924-11a-01w-1244-10 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
10bbb834-dcc3-450a-abb0-a7bba25cc964 a80bf78a-6ce3-48f3-bebd-33f9ef4b1187 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
100511301 100511312      NA's 
        1         1       535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  10 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
 0.5 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  20 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   7 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
1244 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   e    f NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
  40 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  80 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-b2-3923-11a-01w-1242-08 tcga-b2-3924-11a-01w-1242-08 
                           1                            1 
tcga-b2-5633-11a-01w-1584-10 tcga-b2-5635-11a-01w-1584-10 
                           1                            1 
                        NA's 
                         533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
1a666ede-b2e1-42c6-8e0b-5e77c4efdc50 5dc821fd-0908-4fe0-adeb-254ae6c4599b 
                                   1                                    1 
7f7454d9-79ae-4144-826f-f19d933472f8 8ed04b2c-f8dc-4bb9-9c43-3ef8398a2f8a 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
 100515813  100515824 1049968945 1049968968       NA's 
         1          1          1          1        533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   8   10 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
 0.5 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   2   20 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   3    9 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   7   12 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
1242 1584 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   e    f    g NA's 
   1    2    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
  40 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  80 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.amount:
  82  121  138  151 NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                 NA's 
                   4                  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   w NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-b2-3923-11a-01w tcga-b2-3924-11a-01w tcga-b2-5633-11a-01w 
                   1                    1                    1 
tcga-b2-5635-11a-01w                 NA's 
                   1                  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
09255dda-0459-4470-a08a-fbfa3ce5e58e 3f7aca1e-f9a2-4c7e-80ef-bf5b304e2f3b 
                                   1                                    1 
41d298a8-e90f-4fa0-aa6c-4dbc7c96d05f 549057ce-057c-40c2-9fc7-9076ad6aa840 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.concentration:
 0.5 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      4     533 

patient.samples.sample.4.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      4     533 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-b2-3923-11a-01x-1245-10 tcga-b2-3924-11a-01x-1245-10 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
a55d08fe-a05c-45dd-ae45-7b9681b2684f e2053aca-519a-436f-a9c4-78491808f4c5 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
100521765 100521776      NA's 
        1         1       535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
  10 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  20 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   7 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1245 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   e    f NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-b2-3923-11a-01x-1243-08 tcga-b2-3924-11a-01x-1243-08 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
abd6130d-e304-4a7c-8e0a-e02bb38264dd be399bee-ce0d-49dc-826c-966e20ca01b9 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
100522053 100522064      NA's 
        1         1       535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  20 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   9 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   7 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
1243 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   e    f NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.amount:
 102  103 NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type:
repli-g x (qiagen) dna                   NA's 
                     2                    535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type_id:
   x NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-b2-3923-11a-01x tcga-b2-3924-11a-01x                 NA's 
                   1                    1                  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
6bbe5113-c5bb-48e0-a0eb-35fa0cd5d1c7 ce8f0d5f-fdf7-481b-8256-8c0406220b17 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
        2       535 

patient.samples.sample.4.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      2     535 

patient.samples.sample.4.portions.portion.analytes.analyte.a260_a280_ratio:
 1.9 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-b2-3923-11a-01d-1185-01 tcga-b2-3924-11a-01d-1185-01 
                           1                            1 
tcga-b2-5633-11a-01d-1532-02 tcga-b2-5635-11a-01d-1532-02 
                           1                            1 
                        NA's 
                         533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
0123b535-e834-48b2-8df0-b2dfed3db6cc 100ca67c-6216-4ef7-9937-3c110ccd2db9 
                                   1                                    1 
ab2b3dde-0a76-477e-8d8b-3aa705a69bcf b2d60fa6-5379-4a3e-9371-3d1a8445a58a 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
 100512537  100512548 1049967172 1049967193       NA's 
         1          1          1          1        533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.14 0.15 0.16 NA's 
   2    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   9   25 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   2    8 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   6   11 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
1185 1532 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   b    c    f    g NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
0.94 1.07 1.86    2 NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 6.7 13.3 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-b2-3923-11a-01d-1186-05 tcga-b2-3924-11a-01d-1186-05 
                           1                            1 
tcga-b2-5633-11a-01d-1534-10 tcga-b2-5635-11a-01d-1534-10 
                           1                            1 
                        NA's 
                         533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
246c0c37-c64d-4da9-920b-778adb283c06 72d43166-f6e6-45a4-9356-28e6c7c5e954 
                                   1                                    1 
a139965b-782e-4ef4-b2c6-c170ec263bc7 cc6d8e94-2376-4868-8746-6a5fac3ecbb9 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
 100512441  100512452 1049928047 1049928068       NA's 
         1          1          1          1        533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5   10 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 0.14 0.16 NA's 
   2    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   9   25 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   2    8 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   6   11 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
1186 1534 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   b    c    f    g NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   2 3.74 4.27 NA's 
   2    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
26.7   40 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-b2-3923-11a-01d-1250-08 tcga-b2-3924-11a-01d-1250-08 
                           1                            1 
tcga-b2-5633-11a-01d-1536-05 tcga-b2-5635-11a-01d-1536-05 
                           1                            1 
                        NA's 
                         533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
29944ef0-6c07-43c9-a02d-231547649ac1 a3e5b17c-cb5d-4ae9-aead-b850595e918b 
                                   1                                    1 
db3b71c9-3363-4f26-aa64-0d6cd727565f edc54104-dece-4b55-b2f6-33f3d1e4cd30 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
 100511577  100511588 1049927951 1049927972       NA's 
         1          1          1          1        533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.14 0.15 0.16 NA's 
   2    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   9   20 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   2    9 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   6   11 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
1250 1536 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   b    c    f    g NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 1.4  1.6 3.74 4.01 NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10 26.7 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-b2-3923-11a-01d-1251-10 tcga-b2-3924-11a-01d-1251-10 
                           1                            1 
tcga-b2-5633-11a-01d-2103-10 tcga-b2-5635-11a-01d-2103-10 
                           1                            1 
                        NA's 
                         533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
28aac82d-72ad-4956-a3c3-022718b48fdd 458290e2-8513-482c-bc50-9095a22c24d3 
                                   1                                    1 
56ca6c25-90ab-4108-bb15-4a992e011975 cfca4e91-156a-4c53-bfde-0a75085eb2a3 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
100511865 100511876 121030036 121030075      NA's 
        1         1         1         1       533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
  10 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.14 0.15 0.16 NA's 
   2    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  20   21 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   9   12 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   5    6 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
1251 2103 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   a    d    f    g NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
0.94 1.07 1.86    2 NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
 6.7 13.3 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-b2-3923-11a-01d-2048-08 tcga-b2-3924-11a-01d-2095-10 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
613bc534-7fbc-4a29-ad89-922e55fe8101 a529537a-f2b5-435e-ba56-018ad2bd7c93 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
 120998829 1061614950       NA's 
         1          1        535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8   10 NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.14 0.16 NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  16   21 NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
  11   12 NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   2    3 NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
2048 2095 NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   d    e NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
0.93 2.13 NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
6.67 13.3 NA's 
   1    1  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 NA's 
   2  535 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-b2-3923-11a-01d-2095-10                         NA's 
                           1                          536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
e610eab2-17d9-44ac-a182-c51ec64adb8f                                 NA's 
                                   1                                  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
120998858      NA's 
        1       536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
  10 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.14 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  21 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
  12 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   3 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
2095 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   b NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
1.86 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
13.3 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   1  536 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-b2-3923-11a-01d-1184-02 tcga-b2-3924-11a-01d-1184-02 
                           1                            1 
tcga-b2-5633-11a-01d-1530-01 tcga-b2-5635-11a-01d-1530-01 
                           1                            1 
                        NA's 
                         533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
4673770c-19a7-451a-a412-e272bea5b10a 62b189c2-2d6d-4a2a-a493-f5a025a11c62 
                                   1                                    1 
da40184e-b882-4a1a-84a0-5ce1293d4b56 eae353d2-6bb7-4a02-8b25-b67c95161819 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
 100512057  100512068 1049928812 1049928833       NA's 
         1          1          1          1        533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 NA's 
   2    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   9   25 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   2    8 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   6   11 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
1184 1530 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   b    c    f    g NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.93    1 1.86 2.13 NA's 
   1    1    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67 13.3 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
  39.4   83.4 537.64 618.97   NA's 
     1      1      1      1    533 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-b2-3923-11a-01d tcga-b2-3924-11a-01d tcga-b2-5633-11a-01d 
                   1                    1                    1 
tcga-b2-5635-11a-01d                 NA's 
                   1                  533 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid:
027e6451-853f-4f53-b27d-66b65276479c 3aa2a6a1-2871-4357-b968-8999d4419d8b 
                                   1                                    1 
a7319a11-bb08-4776-84cd-568c6a3c1733 ce5d8228-ee5a-4bed-89b9-989b56bbe9d7 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 NA's 
   2    1    1  533 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             533 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    4                   533 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      4     533 

patient.samples.sample.4.portions.portion.bcr_portion_barcode:
tcga-b2-3923-11a-01 tcga-b2-3924-11a-01 tcga-b2-5633-11a-01 tcga-b2-5635-11a-01 
                  1                   1                   1                   1 
               NA's 
                533 

patient.samples.sample.4.portions.portion.bcr_portion_uuid:
52653c73-2edc-4b84-9373-4d393aa47c44 55232416-8d43-4671-9efe-d8e24f5a52f8 
                                   1                                    1 
5ca8ba42-6078-4edb-88e1-860fb287d409 e4dbe698-3159-4245-997f-364c842cbb45 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.day_of_creation:
  29   30 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.month_of_creation:
  11   12 NA's 
   2    2  533 

patient.samples.sample.4.portions.portion.portion_number:
   1 NA's 
   4  533 

patient.samples.sample.4.portions.portion.portion_sequence:
   1 NA's 
   2  535 

patient.samples.sample.4.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-b2-3923-11a-01-ts1 tcga-b2-3924-11a-01-ts1                    NA's 
                      1                       1                     535 

patient.samples.sample.4.portions.portion.slides.slide.2.bcr_slide_uuid:
13df8c1e-ab10-4588-840b-3011dd3801e6 b7574432-f2bd-4902-bf09-47774f405f17 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.4.portions.portion.slides.slide.2.image_file_name:
tcga-b2-3923-11a-01-ts1.13df8c1e-ab10-4588-840b-3011dd3801e6.svs 
                                                               1 
tcga-b2-3924-11a-01-ts1.b7574432-f2bd-4902-bf09-47774f405f17.svs 
                                                               1 
                                                            NA's 
                                                             535 

patient.samples.sample.4.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.4.portions.portion.slides.slide.2.section_location:
 top NA's 
   2  535 

patient.samples.sample.4.portions.portion.slides.slide.bcr_slide_barcode:
tcga-b2-3923-11a-01-bs1 tcga-b2-3924-11a-01-bs1 tcga-b2-5633-11a-01-ts1 
                      1                       1                       1 
tcga-b2-5635-11a-01-ts1                    NA's 
                      1                     533 

patient.samples.sample.4.portions.portion.slides.slide.bcr_slide_uuid:
5a115116-d7af-41fc-ad9c-cdedd8098038 65fc51bf-9cc3-41a4-a149-8cf57a948b26 
                                   1                                    1 
7494bb32-bd4e-4f8d-a6f5-86284c970ed9 890eb76b-b487-43bd-bce1-c0db40ba1c61 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.4.portions.portion.slides.slide.image_file_name:
tcga-b2-3923-11a-01-bs1.5a115116-d7af-41fc-ad9c-cdedd8098038.svs 
                                                               1 
tcga-b2-3924-11a-01-bs1.65fc51bf-9cc3-41a4-a149-8cf57a948b26.svs 
                                                               1 
tcga-b2-5633-11a-01-ts1.7494bb32-bd4e-4f8d-a6f5-86284c970ed9.svs 
                                                               1 
tcga-b2-5635-11a-01-ts1.890eb76b-b487-43bd-bce1-c0db40ba1c61.svs 
                                                               1 
                                                            NA's 
                                                             533 

patient.samples.sample.4.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.4.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     2      2    533 

patient.samples.sample.4.portions.portion.weight:
   1   36   39 NA's 
   2    1    1  533 

patient.samples.sample.4.portions.portion.year_of_creation:
2010 2012 NA's 
   2    2  533 

patient.samples.sample.4.sample_type:
solid tissue normal                NA's 
                  4                 533 

patient.samples.sample.4.sample_type_id:
  11 NA's 
   4  533 

patient.samples.sample.4.shortest_dimension:
 0.4  0.7 NA's 
   2    2  533 

patient.samples.sample.4.vial_number:
   a NA's 
   4  533 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   57.0   139.5   313.0   545.3   694.5  1874.0     502 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
fc717d92-13bd-4968-8e82-0cc53eb6d5d1                                 NA's 
                                   1                                  536 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0    75.0   210.0   296.3   330.0  2080.0     502 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.7000  0.9000  0.9368  1.1000  2.0000      35 

patient.samples.sample.is_ffpe:
 no 
537 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   1.200   1.500   1.662   2.000   4.000      35 

patient.samples.sample.oct_embedded:
false  true  NA's 
   23    12   502 



patient.samples.sample.portions.portion.2.analytes.analyte.2.a260_a280_ratio:
 1.5  1.7  1.8 NA's 
   2   10   36  489 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103927675 103928608 103928636 103930042 103928659 103962949       489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1471  0.1500  0.1700     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  11   27 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   9   10 NA's 
   2   46  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.354   6.250   8.000     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_id:
1277 1325 NA's 
   2   46  489 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.47    3.74    4.01    3.93    4.01    4.54     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.volume:
26.7 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   1  536 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.244e+08 1.244e+08 1.062e+09 7.611e+08 1.062e+09 1.062e+09       509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7   13   26 NA's 
   1   19    8  509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1475  0.1500  0.1700     509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  12   14   24 NA's 
   8    1   19  509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  28  509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   3   10 NA's 
   9   19  509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.643   3.000   7.000     509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.plate_id:
2020 2206 a277 NA's 
  19    8    1  509 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.130   1.860   1.671   2.000   2.130     509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22   23 NA's 
  27    1  509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.volume:
6.67   13 13.3 NA's 
   8    1   19  509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 NA's 
  19    8    1  509 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   1  536 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
124428960 124429008 124429022 125074865 124429041 132179168       525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.center_id:
  13   26 NA's 
   1   11  525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.13 0.14 0.15 0.16 NA's 
   2    3    4    3  525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  12   13 NA's 
  11    1  525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  12  525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   3 NA's 
  12  525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   1    2    6 NA's 
   6    3    3  525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.plate_id:
2206 a27d NA's 
  11    1  525 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   0.930   1.000   1.145   1.070   3.000     525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22   23 NA's 
  11    1  525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.volume:
6.67   20 NA's 
  11    1  525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2012 2013 NA's 
  11    1  525 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-b2-3924-01a-02r-a277-07                         NA's 
                           1                          536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_uuid:
8a59f553-05f9-4f5e-bb4e-2b119e6af0e5                                 NA's 
                                   1                                  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
145632018      NA's 
        1       536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.center_id:
   7 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.16 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
  14 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   3 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   6 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.plate_id:
a277 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.plate_row:
   g NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.quantity:
2.08 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.source_center:
  23 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.volume:
  13 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2013 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-b2-3924-01a-02r-a27d-13                         NA's 
                           1                          536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.bcr_aliquot_uuid:
6becdc2c-5769-4c48-96e9-a362e3341b0b                                 NA's 
                                   1                                  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.biospecimen_barcode_bottom:
132179152      NA's 
        1       536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.center_id:
  13 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.concentration:
0.16 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.day_of_shipment:
  13 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.month_of_shipment:
   3 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.plate_column:
   6 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.plate_id:
a27d NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.plate_row:
   g NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.quantity:
 3.2 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.source_center:
  23 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.volume:
  20 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.year_of_shipment:
2013 NA's 
   1  536 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103927643 103928512 103928540 103929872 103928563 103961029       489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.center_id:
  13 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1471  0.1500  0.1700     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  11   27 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   9   10 NA's 
   2   46  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.354   6.250   8.000     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_id:
1276 1324 NA's 
   2   46  489 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.600   2.800   3.000   2.942   3.000   3.400     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.volume:
  20 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.30   36.30   55.81   72.19  102.80  209.33     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type:
 rna NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type_id:
   r NA's 
  48  489 



patient.samples.sample.portions.portion.2.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1471  0.1500  0.1700     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              48 
                                            NA's 
                                             489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                       48                       489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.400   1.500   1.471   1.600   2.000     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.600   7.800   8.500   8.346   8.900   9.500     489 

patient.samples.sample.portions.portion.2.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     48     489 



patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08      491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   8 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
  11 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.717   7.000   8.000     491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1383 NA's 
  46  491 


patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   40.00   40.00   39.03   40.00   40.00     491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   80.00   80.00   78.06   80.00   80.00     491 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  46  491 



patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08      489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   8   25 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
  10   11 NA's 
   2   46  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.583   7.000   8.000     489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_id:
1359 1382 NA's 
   2   46  489 


patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   40.00   40.00   39.28   40.00   40.00     489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   80.00   80.00   78.56   80.00   80.00     489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   87.50  101.50   97.42  110.50  140.00     489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  48                  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type_id:
   w NA's 
  48  489 



patient.samples.sample.portions.portion.2.analytes.analyte.3.concentration:
 0.5 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              48 
                                            NA's 
                                             489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     48     489 

patient.samples.sample.portions.portion.2.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
     48     489 



patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103947646 103948388 103948408 103948364 103948432 103948453       491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   8 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
  11 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.717   7.000   8.000     491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_id:
1385 NA's 
  46  491 


patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.quantity:
  40 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.volume:
  80 NA's 
  46  491 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  46  491 



patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08      489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   8   25 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
  10   11 NA's 
   2   46  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.583   7.000   8.000     489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_id:
1360 1384 NA's 
   2   46  489 


patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  56.00   80.75   85.00   85.73   90.00  149.00     489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type:
repli-g x (qiagen) dna                   NA's 
                    48                    489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type_id:
   x NA's 
  48  489 



patient.samples.sample.portions.portion.2.analytes.analyte.4.concentration:
 0.5 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              48 
                                            NA's 
                                             489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
       48       489 

patient.samples.sample.portions.portion.2.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
     48     489 

patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio:
 1.8  1.9 NA's 
   3   46  488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_barcode:
tcga-b2-3924-01a-02d-a46w-08 tcga-b8-4619-01a-02d-1554-10 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_uuid:
436c3b6b-a2a4-4e19-8f8b-8e7240a09a97 872e4e55-b271-47d4-9da3-fd2841fbe697 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
 132179207 1049985667       NA's 
         1          1        535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.center_id:
   8   10 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.concentration:
0.05 0.09 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
   3   14 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   2    8 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.plate_column:
   5    8 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.plate_id:
1554 a46w NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.plate_row:
   d    e NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.quantity:
 1.8    2 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.source_center:
  22   23 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.volume:
  20   40 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2011 2015 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_barcode:
tcga-b8-4619-01a-02d-2095-10                         NA's 
                           1                          536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_uuid:
c1ad70af-6764-48b5-9dd5-1a3c35e84651                                 NA's 
                                   1                                  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.biospecimen_barcode_bottom:
120998827      NA's 
        1       536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.center_id:
  10 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.concentration:
0.15 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.day_of_shipment:
  21 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.month_of_shipment:
  12 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.plate_column:
   5 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.plate_id:
2095 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.plate_row:
   e NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.quantity:
   2 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.source_center:
  22 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.volume:
13.3 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.11.year_of_shipment:
2011 NA's 
   1  536 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103925133 103927167 103927194 103928683 103927216 103965061       489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   1 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.14 0.15 0.16 0.17 NA's 
  10   22   12    4  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  11   27 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   9   10 NA's 
   2   46  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.354   6.250   8.000     489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
1274 1322 NA's 
   2   46  489 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
0.93    1 1.07 1.13 NA's 
  10   22   12    4  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
6.67 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  48  489 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103925143 103927263 103927290 103928636 103927312 103961797       489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
   5 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 NA's 
  10   22   12    4  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  11   27 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   9   10 NA's 
   2   46  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.354   6.250   8.000     489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
1275 1323 NA's 
   2   46  489 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
3.74 4.01 4.27 4.54 NA's 
  10   22   12    4  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
26.7 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 NA's 
  48  489 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103948935 103950682 103950710 103950558 103950736 103950758       489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id:
  10 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration:
0.05 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   8   25 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
  10   11 NA's 
   2   46  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.354   6.250   8.000     489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id:
1361 1386 NA's 
   2   46  489 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity:
   2 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume:
  40 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
  48  489 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103950821 103951071 103951098 104661402 103951123 120998677       489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.center_id:
   8   10 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.concentration:
0.05 0.14 0.15 NA's 
  46    1    1  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   8   21 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
  11   12 NA's 
  46    2  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.771   7.000  12.000     489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_id:
1387 2094 NA's 
  46    2  489 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.quantity:
1.86    2 NA's 
   1   47  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.volume:
13.3   40 NA's 
   2   46  489 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
  46    2  489 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.210e+08 1.210e+08 1.864e+08 1.210e+08 1.062e+09       494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.center_id:
   2    8   10 NA's 
   1    2   40  494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.concentration:
0.14 0.15 0.16 0.17 NA's 
   9   21    9    4  494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  14   16   21 NA's 
   1    2   40  494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  43  494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   2   11   12 NA's 
   1    2   40  494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   5.000   5.512   8.500  11.000     494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.plate_id:
1547 2048 2094 2095 NA's 
   1    2   17   23  494 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   2.000   2.000   1.977   2.130   2.260     494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
  43  494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.volume:
6.67 13.3 NA's 
   2   41  494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 NA's 
  43  494 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.disease_code:
fppp NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.project_code:
tcga NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-b2-3923-01a-02d-2095-10 tcga-b2-3924-01a-02d-a270-10 
                           1                            1 
tcga-b8-4619-01a-02d-1549-01 tcga-cj-4637-01a-02d-2095-10 
                           1                            1 
                        NA's 
                         533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
25c61f18-d7ca-432c-b4ec-9d4fa01d7453 8d8a31b2-718d-4c72-a147-e244f5e05a01 
                                   1                                    1 
a5020b81-edb1-4a2f-a31f-0582ee10be80 aab289f0-ba21-4966-9621-bfe07dc1c18a 
                                   1                                    1 
                                NA's 
                                 533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
 120998791  120998877  132158818 1049930075       NA's 
         1          1          1          1        533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.center_id:
   1   10 NA's 
   1    3  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.concentration:
0.09 0.14 0.15 NA's 
   1    1    2  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  13   14   21 NA's 
   1    1    2  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   4  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   2    3   12 NA's 
   1    1    2  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_column:
   3    7    8 NA's 
   2    1    1  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_id:
1549 2095 a270 NA's 
   1    2    1  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_row:
   a    c    d    h NA's 
   1    1    1    1  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.quantity:
   1  1.8 1.86    2 NA's 
   1    1    1    1  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
   3    1  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.volume:
6.67 13.3   20 NA's 
   1    2    1  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 2013 NA's 
   3    1  533 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.disease_code:
fppp NA's 
   2  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.project_code:
tcga NA's 
   2  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-b2-3923-01a-02d-a270-10 tcga-b2-3924-01a-02d-a274-01 
                           1                            1 
tcga-b8-4619-01a-02d-1551-05                         NA's 
                           1                          534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
0eec3cb3-1427-45f3-ac93-c2926955bd9b 7d6fbb34-0171-4e0a-a9b1-324ef3affa64 
                                   1                                    1 
ba2bd55c-459d-4018-8972-6c98e79bd2d5                                 NA's 
                                   1                                  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
 132160702  145675693 1049929979       NA's 
         1          1          1        534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.center_id:
   1    5   10 NA's 
   1    1    1  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.concentration:
0.08 0.15 0.17 NA's 
   1    1    1  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  13   14 NA's 
   1    2  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   3  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   2    3 NA's 
   1    2  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.plate_column:
   3    7 NA's 
   2    1  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.plate_id:
1551 a270 a274 NA's 
   1    1    1  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.plate_row:
   c    d    f NA's 
   1    1    1  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.quantity:
1.14  1.6 4.01 NA's 
   1    1    1  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
   1    2  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.volume:
 6.7   20 26.7 NA's 
   1    1    1  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 2013 NA's 
   1    2  534 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.additional_studies.additional_study.disease_code:
fppp NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.additional_studies.additional_study.project_code:
tcga NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-b2-3924-01a-02d-a27a-05 tcga-b8-4619-01a-02d-1553-08 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
bfa02d33-523e-4709-bf06-933c686d139c e16d8c96-e679-4c87-ad6d-5e58d6a9d267 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             536 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
 132159768 1049985622       NA's 
         1          1        535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.center_id:
   5    8 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.concentration:
0.05 0.17 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
  13   14 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   2    3 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.plate_column:
   2    3 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.plate_id:
1553 a27a NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.plate_row:
   d    h NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.quantity:
1.14    2 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.source_center:
  22   23 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.volume:
 6.7   40 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 2013 NA's 
   1    1  535 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103927695 103927744 103927773 103929788 103927796 103965253       488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   2   13 NA's 
  48    1  488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 NA's 
  10   23   12    4  488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  11   17   27 NA's 
  46    1    2  488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  49  488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   9   10   11 NA's 
   2   46    1  488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.408   7.000   8.000     488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
1273 1321 1409 NA's 
   2   46    1  488 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.860   2.000   2.000   2.075   2.130   4.500     488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
  49  488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
13.3   30 NA's 
  48    1  488 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 NA's 
  49  488 

patient.samples.sample.portions.portion.2.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  18.07   78.10  119.77  131.64  179.77  298.10     488 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
 dna  rna NA's 
  48    1  488 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   d    r NA's 
  48    1  488 



patient.samples.sample.portions.portion.2.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 NA's 
  10   23   12    4  488 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
  no NA's 
  48  489 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              49 
                                            NA's 
                                             488 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  49  488 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
    adna preparation type mirvana (allprep dna) rna                      NA's 
                       48                         1                       488 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
 1.7 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.rinvalue:
 8.1 NA's 
   1  536 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
     49     488 



patient.samples.sample.portions.portion.2.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   24.00   28.00   26.43   30.00   30.00     488 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
  49  488 

patient.samples.sample.portions.portion.2.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   7.000   7.939  11.000  11.000     488 

patient.samples.sample.portions.portion.2.portion_number:
   2 NA's 
  49  488 

patient.samples.sample.portions.portion.2.portion_sequence:
   2 NA's 
  17  520 




patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  49  488 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_lymphocyte_infiltration:
   1   13 NA's 
   1    1  535 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   1  536 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.8776  0.0000 15.0000     488 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   1  536 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.8367  0.0000 25.0000     488 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   5.000  10.000   9.959  10.000  55.000     488 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  45.00   82.75   90.00   88.04   95.00   99.00     489 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  70.00   80.00   85.00   83.53   85.00   98.00     488 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location:
bottom   NA's 
    49    488 




patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
  49  488 

patient.samples.sample.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   1    3 NA's 
   1    2  534 

patient.samples.sample.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   2  535 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.204   0.000  20.000     488 

patient.samples.sample.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   2  535 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.857   0.000  55.000     488 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   5.000   5.000   8.449  10.000  30.000     488 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   80.00   90.00   86.54   95.00  100.00     489 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  70.00   80.00   85.00   83.16   85.00   99.00     488 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
 top NA's 
  49  488 

patient.samples.sample.portions.portion.2.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.75   36.50   34.83   39.25   60.00     489 

patient.samples.sample.portions.portion.2.year_of_creation:
2010 2012 NA's 
  32   17  488 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.100   1.700   1.800   1.754   1.800   2.100       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.040e+08 1.040e+08 3.318e+08 1.641e+08 1.054e+09        51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13   26 NA's 
 469   13    4   51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1100  0.1400  0.1500  0.1486  0.1600  0.1800      51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   11.00   13.00   17.03   25.00   30.00      51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 486   51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   9.000   7.045  10.000  11.000      51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.767   5.000   9.000      51 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.800   3.740   3.740   3.763   4.010   4.540      51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
 450   36   51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   26.70   26.70   25.39   26.70   26.70      51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2010    2011    2011    2014      51 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   2  535 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.244e+08 1.244e+08 1.054e+09 6.018e+08 1.054e+09 1.062e+09       451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7   13   26 NA's 
   4   44   38  451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1477  0.1500  0.1700     451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   12.00   16.00   14.41   16.00   24.00     451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  86  451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   3    5   10   12 NA's 
  40   39    6    1  451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   3.907   5.000   8.000     451 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.000   2.000   1.969   2.800   3.400     451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22   23 NA's 
  82    4  451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.60    6.67   13.30   13.32   20.00   20.00     451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 NA's 
  44   38    3    1  451 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-b2-5633-01a-01r-a27d-13 tcga-b2-5635-01a-01r-a27d-13 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
44f75d53-c23f-4b1a-8bc7-79a2e91bbb89 74fac0ae-9809-4824-a034-43ea272aacf1 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
132179143 132179226      NA's 
        1         1       535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
  13 NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.16 0.17 NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  13 NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   3 NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   6    7 NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
a27d NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   a    h NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
 3.2  3.4 NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  23 NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
  20 NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2013 NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-b2-5635-01a-01r-a46x-36                         NA's 
                           1                          536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_uuid:
a35ed3d0-f79d-49c2-ae27-c753e90f925f                                 NA's 
                                   1                                  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
167814211      NA's 
        1       536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.center_id:
  36 NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.17 NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
  26 NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   8 NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   5 NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_id:
a46x NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_row:
   c NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.quantity:
0.85 NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.source_center:
  23 NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.volume:
   5 NA's 
   1  536 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2015 NA's 
   1  536 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 1.039e+08 1.040e+08 2.919e+08 1.289e+08 1.050e+09         1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   13 NA's 
  78  458    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1471  0.1525  0.1800       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   11.00   22.00   17.93   25.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 536    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   9.000   6.841  10.000  11.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    4.00    3.67    5.00    9.00       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.270   2.800   3.000   3.022   3.200   4.540       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 501   35    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   20.00   20.00   20.63   20.00   26.70       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2013 2014 NA's 
 355  146   21   14    1 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   27.30   45.30   63.89   75.30  623.30       1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
 536    1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   r NA's 
 536    1 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1471  0.1525  0.1800       1 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             536 
                                            NA's 
                                               1 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 536    1 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                      NA's 
                       65                       471                         1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.198   1.400   1.359   1.600   9.900       1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    7.80    8.40    8.32    8.80    9.60       1 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    536       1 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
  99    3    1  434 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100510584 100511299 100511330 107455533 107460025 128286766       521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
  10   13 NA's 
  15    1  521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.1  0.5 NA's 
   1   15  521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  16   20   25 NA's 
   4   11    1  521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  16  521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   5    8    9 NA's 
   4    1   11  521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   4    5    6   11 NA's 
   3    7    2    4  521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1192 1244 2279 NA's 
   1   11    4  521 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
 1.6   10   40 NA's 
   1    4   11  521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
  16  521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  16   20   80 NA's 
   1    4   11  521 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2012 NA's 
  12    4  521 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 1.039e+08 1.323e+08 5.294e+08 1.050e+09 1.054e+09         2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8   10   13 NA's 
  52  418   65    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1  0.5 NA's 
  65  470    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   17.00   13.71   20.00   26.00       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 535    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000  10.000   7.707  11.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.804   5.000   9.000       2 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.50   40.00   40.00   35.46   40.00   40.00       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
 500   35    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    80.0    80.0    73.5    80.0    80.0       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2013 2014 NA's 
 354  146   11   24    2 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0   149.0   161.0   151.2   173.0   709.0      37 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna            total rna                 NA's 
                 470                   65                    2 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   t    w NA's 
  65  470    2 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
 0.1  0.5 NA's 
  65  435   37 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 470   67 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
 435   35   67 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             535 
                                            NA's 
                                               2 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 535    2 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g total rna      NA's 
      470        65         2 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
 0.4  0.5  0.7 NA's 
   2    2    1  532 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    7.20    7.60    7.56    7.90    8.10     532 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
    500      37 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
  99    3    1  434 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96489064  96489749  96489816  98343233 100511341 100521811       461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
  10 NA's 
  76  461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
  76  461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   5   20 NA's 
  41   35  461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  76  461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   4    9 NA's 
  41   35  461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.368   5.000   6.000     461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 898 1244 1245 NA's 
  41   24   11  461 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
  40 NA's 
  76  461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
  76  461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  80 NA's 
  76  461 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  76  461 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96492698 100522042 103949052 102317750 103949369 103950565       276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8   10 NA's 
  76  185  276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   25.00   17.89   25.00   31.00     276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   3    9   10   11 NA's 
  41   35   91   94  276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.831   5.000   8.000     276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    886    1243    1364    1278    1371    1390     276 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   42.0    90.0   151.0   137.3   164.0   188.0     276 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                    65                    196                    276 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   w    x NA's 
  65  196  276 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             261 
                                            NA's 
                                             276 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 261  276 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
       65       196       276 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
    261     276 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100521548 100521759 100521782 100521762 100521804 100521828       513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id:
  10 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration:
 0.5 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment:
  20 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column:
   1    2    3    4 NA's 
   6    7    7    4  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id:
1245 NA's 
  24  513 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity:
  40 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center:
  22 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume:
  80 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  24  513 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100521586 100522046 100522070 100521994 100522091 100522098       513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   8 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
  20 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
   9 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   1    2    3    4 NA's 
   6    7    7    4  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_id:
1243 NA's 
  24  513 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
  40 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  22 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
  80 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2010 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  53.00   81.00   87.50   87.12   96.25  105.00     513 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type:
repli-g x (qiagen) dna                   NA's 
                    24                    513 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type_id:
   x NA's 
  24  513 



patient.samples.sample.portions.portion.analytes.analyte.5.concentration:
 0.5 NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful:
  no NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              24 
                                            NA's 
                                             513 

patient.samples.sample.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
  24  513 

patient.samples.sample.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
       24       513 

patient.samples.sample.portions.portion.analytes.analyte.5.spectrophotometer_method:
uv spec    NA's 
     24     513 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   1.900   1.900   1.901   1.900   2.000      36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_barcode:
tcga-b8-4620-01a-01d-1554-10 tcga-b8-4622-01a-02d-2096-10 
                           1                            1 
                        NA's 
                         535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_uuid:
03491783-8dfd-4481-a8c5-eb93caa1772c 12ab328c-8913-4d12-8685-d16924b37467 
                                   1                                    1 
                                NA's 
                                 535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
 121030209 1049985676       NA's 
         1          1        535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
  10 NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
0.05 0.15 NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
  14   21 NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   2   12 NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   5   10 NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_id:
1554 2096 NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_row:
   e    f NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
   2 NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  22 NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
13.3   40 NA's 
   1    1  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2011 NA's 
   2  535 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.040e+08 1.040e+08 2.919e+08 1.322e+08 1.050e+09         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2    5   10 NA's 
 399   61   41   35    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.070   0.140   0.150   0.144   0.150   0.170       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   11.00   17.00   17.47   25.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 536    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   9.000   6.841  10.000  11.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.349   5.000   8.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.000   1.000   1.472   1.860   4.540       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 501   35    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67   11.14   13.30   40.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2013 2014 NA's 
 355  146   21   14    1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.040e+08 1.040e+08 2.919e+08 1.289e+08 1.050e+09         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8   10 NA's 
 393   41  102    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1304  0.1500  0.1700       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   11.00   22.00   18.31   27.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 536    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   6.918  10.000  11.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.349   5.000   8.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.94    2.00    3.74    3.24    4.01    4.54       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 501   35    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
 6.7   13 26.7   40 NA's 
  41   35  358  102    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2013 2014 NA's 
 355  146   21   14    1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.039e+08 1.040e+08 3.017e+08 1.087e+08 1.050e+09        36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8   10 NA's 
 102  170  229   36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.05000 0.05000 0.08537 0.15000 0.17000      36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   20.00   16.42   25.00   28.00      36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 501   36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000  10.000   7.559  11.000  11.000      36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.447   5.000   8.000      36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    966    1366    1392    1396    1501    1670      36 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.000   2.000   2.288   2.000   4.540      36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
 501   36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
 6.7   10 26.7   40 NA's 
  41   35  102  323   36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
 355  146   36 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.210e+08 1.210e+08 1.332e+08 1.210e+08 1.062e+09        53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   2    8   10 NA's 
   1   53  430   53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1382  0.1500  0.1700      53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   21.00   21.00   19.89   21.00   21.00      53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 484   53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   12.00   12.00   11.65   12.00   12.00      53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.428   8.000  12.000      53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1251    2094    2098    1971    2102    2104      53 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.860   2.000   1.914   2.000   3.000      53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
 484   53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   15.84   13.30   60.00      53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
  81  403   53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   2  535 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.210e+08 1.210e+08 1.377e+08 1.283e+08 1.050e+09       414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000  10.000  10.000   9.569  10.000  10.000     414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1400  0.1165  0.1500  0.1700     414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   16.00   21.00   18.02   21.00   21.00     414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
 123  414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000  12.000   8.927  12.000  12.000     414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.50    5.00    5.52    7.00   12.00     414 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.860   2.000   2.249   2.260   6.800     414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
 118    5  414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   25.51   40.00   60.00     414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 2012 2013 2015 NA's 
  83   35    4    1  414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.disease_code:
fppp NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.project_code:
tcga NA's 
   2  535 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.082e+08 1.231e+08 3.204e+08 1.457e+08 1.062e+09       523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   1    5    8   10 NA's 
   3    1    1    9  523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.05 0.15 0.16 NA's 
   6    6    2  523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    8.00   14.00   13.57   16.00   21.00     523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  14  523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   3.000   5.286   9.500  12.000     523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.643   6.000  10.000     523 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.302   2.000   2.016   2.098   4.010     523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
  12    2  523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    8.35   20.00   26.67   40.00   60.00     523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 2012 2013 NA's 
  10    2    2  523 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.disease_code:
fppp NA's 
   2  535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.project_code:
tcga NA's 
   2  535 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             535 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.210e+08 1.210e+08 3.299e+08 1.322e+08 1.050e+09       528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   5    8   10 NA's 
   3    1    5  528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.05 0.15 0.16 NA's 
   1    6    2  528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  13   14   21 NA's 
   2    2    5  528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   9  528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   2    3   12 NA's 
   2    2    5  528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   3.000   3.778   4.000   7.000     528 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
1.01    2 2.13 4.01 NA's 
   2    4    2    1  528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
   7    2  528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
 6.7 13.3 26.7   40 NA's 
   2    5    1    1  528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 2013 NA's 
   7    2  528 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.322e+08 1.322e+08 1.678e+08 5.064e+08 1.050e+09 1.050e+09       532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   8   10 NA's 
   3    2  532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.05 0.08 NA's 
   2    3  532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   3   14   21 NA's 
   2    2    1  532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   5  532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   1    2    8 NA's 
   1    2    2  532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     5.0     5.2     8.0     9.0     532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
1553 1554 a469 a46w NA's 
   1    1    1    2  532 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
1.07  1.6    2 NA's 
   1    2    2  532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22   23 NA's 
   2    3  532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
13.4   20   40 NA's 
   1    2    2  532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 2015 NA's 
   2    3  532 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
9.651e+07 1.039e+08 1.040e+08 2.935e+08 1.322e+08 1.054e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   8 
137 399   1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1400  0.1500  0.1499  0.1500  0.5000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   11.00   17.00   17.55   25.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
537 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   9.000   6.838  10.000  11.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.352   5.000   8.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.140   2.000   1.802   2.000  40.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
502  35 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7 13.3   80 
  61   76  399    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2013 2014 
 355  147   21   14 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.9    62.4   127.0   163.0   193.4   864.6 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
                 dna repli-g (qiagen) dna 
                 536                    1 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d   w 
536   1 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1400  0.1500  0.1499  0.1500  0.5000 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
537 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
455  82 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             537 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
537 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type               repli-g 
                  536                     1 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
       35       502 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 100    3    1  433 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   13.00   15.98   22.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
537 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   8.000   6.933   9.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   3.128   2.000  21.000 

patient.samples.sample.portions.portion.portion_sequence:
   1    2 NA's 
  28   24  485 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 497   40 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    1.00    2.00    4.24    5.00   25.00     487 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.212   1.000  20.000     504 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.9799  0.0000 25.0000      40 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0    1 NA's 
  30    3  504 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.106   0.000  55.000      54 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   11.78   15.00   60.00      49 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   85.00   90.00   86.39   95.00  100.00      42 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  49.00   80.00   85.00   83.79   85.00   99.00      40 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    65    432     40 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
537 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   2.000   3.253   3.500  20.000     454 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.424   1.000 100.000     471 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.471   0.000  30.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    2 NA's 
  60    4    1  472 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.057   0.000  55.000      12 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   12.67   15.00   70.00       7 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  22.00   80.00   90.00   85.13   93.00  100.00       2 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   80.00   85.00   83.96   85.00  100.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
   432    105 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   32.00   36.00   46.48   39.00  180.00 

patient.samples.sample.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
 411   70   35   19    2 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 477   60 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 477   60 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00   21.00   14.62   21.00   41.00      60 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    3 NA's 
   1   18    1  517 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   4   30 NA's 
 453   24   60 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   5    7 NA's 
 453   24   60 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2011 2014 NA's 
 453   24   60 

patient.samples.sample.sample_type:
primary tumor 
          537 

patient.samples.sample.sample_type_id:
  1 
537 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.2000  0.3000  0.3759  0.5000  1.0000      35 

patient.samples.sample.vial_number:
  a   b 
533   4 

mRNA_cluster:
   1    2    3    4 NA's 
 147   90   94   86  120 

microRNA_cluster:
   1    2    3    4 NA's 
  82  115  144   73  123 

}}
\keyword{datasets}
