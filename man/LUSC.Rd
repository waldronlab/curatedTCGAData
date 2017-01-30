\name{ LUSC }
\alias{ LUSC }
\docType{data}
\title{ LUSC }

\details{
\preformatted{
> LUSC
A MultiAssayExperiment object of 12 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 12: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 240 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 552 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 387 columns 
 [4] CNASNP: RangedRaggedAssay with 543091 rows and 1035 columns 
 [5] CNVSNP: RangedRaggedAssay with 134864 rows and 1032 columns 
 [6] CNACGH: RangedRaggedAssay with 87417 rows and 407 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 412 columns 
 [8] mRNAArray: ExpressionSet with 17814 rows and 154 columns 
 [9] RPPAArray: ExpressionSet with 223 rows and 328 columns 
 [10] Mutations: RangedRaggedAssay with 65305 rows and 178 columns 
 [11] gistica: SummarizedExperiment with 24776 rows and 501 columns 
 [12] gistict: SummarizedExperiment with 24776 rows and 501 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( LUSC )
CharacterList of length 12
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-98588 1:98602-1510801 ... 24:15604025-59018259
[["CNVSNP"]] 1:3218610-17749207 1:17749504-17937997 ... 23:87455239-154905589
[["CNACGH"]] 1:554268-1640562 1:1643839-1654026 ... 24:57377728-57440809
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] AGRN AGRN GLTPD1 ACTRT2 CASP9 ... CNGA2 USP9Y CD9 TUBGCP5
...
<2 more elements>

> colnames( LUSC )
CharacterList of length 12
[["RNASeqGene"]] TCGA-18-3406-01A-01R-0980-07 ... TCGA-94-7033-01A-11R-1949-07
[["RNASeq2GeneNorm"]] TCGA-18-3406-01A-01R-0980-07 ...
[["miRNASeqGene"]] TCGA-18-5592-01A-01T-1634-13 ...
[["CNASNP"]] TCGA-18-3406-01A-01D-0978-01 ... TCGA-XC-AA0X-10A-01D-A400-01
[["CNVSNP"]] TCGA-18-3406-01A-01D-0978-01 ... TCGA-XC-AA0X-10A-01D-A400-01
[["CNACGH"]] TCGA-18-3406-01A-01D-1519-02 ... TCGA-85-6561-10A-01D-1815-02
[["Methylation"]] TCGA-18-3417-11A-01D-1440-05 ... TCGA-XC-AA0X-01A-32D-A408-05
[["mRNAArray"]] TCGA-18-3406-01A-01R-0980-07 ... TCGA-66-2800-01A-01R-1201-07
[["RPPAArray"]] TCGA-18-4721-01A-21-2193-20 ... TCGA-O2-A5IB-01A-21-A45E-20
[["Mutations"]] TCGA-18-3406-01A-01D-0983-08 ... TCGA-85-6561-01A-11D-1817-08
...
<2 more elements>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.2 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 40.7 Mb
5           CNVSNP 12.2 Mb
6           CNACGH  6.1 Mb
7      Methylation 67.4 Mb
8        mRNAArray    1 Mb
9        RPPAArray  0.1 Mb
10       Mutations 77.1 Mb
11         gistica  4.4 Mb
12         gistict  4.4 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   288 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 216.00  216.00    1.51    1.25    1.90 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  39.00   62.00   68.00   67.26   73.00   90.00      10 

vital_status:
  0   1 
284 220 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   282.0   551.0   871.9  1108.8  5287.0     288 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   272.8   663.5   945.0  1262.0  4765.0     112 

tumor_tissue_site:
lung 
 504 



pathology_N_stage:
 n0  n1  n2  n3  nx 
320 133  40   5   6 


gender:
female   male 
   131    373 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2005    2009    2008    2011    2013      25 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   298.8   706.0   904.8  1051.8  3724.0     436 

radiation_therapy:
  no  yes NA's 
 386   53   65 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    80.0    60.3    90.0   100.0     338 

histological_type:
                      lung basaloid squamous cell carcinoma 
                                                         15 
                     lung papillary squamous cell caricnoma 
                                                          6 
                    lung small cell squamous cell carcinoma 
                                                          1 
lung squamous cell carcinoma- not otherwise specified (nos) 
                                                        482 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   31.12   50.00   52.91   64.50  240.00      77 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1933    1952    1960    1960    1968    1997     183 

residual_tumor:
  r0   r1   r2   rx NA's 
 401   12    4   23   64 

race:
                    asian black or african american                     white 
                        9                        31                       351 
                     NA's 
                      113 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     8                    319                    177 

admin.bcr:
nationwide children's hospital 
                           504 

admin.day_of_dcc_upload:
  1  27 
503   1 

admin.disease_code:
lusc 
 504 


admin.month_of_dcc_upload:
  2  10 
  1 503 

admin.patient_withdrawal.withdrawn:
false 
  504 

admin.project_code:
tcga NA's 
 503    1 

admin.year_of_dcc_upload:
2014 2015 
   1  503 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  39.00   62.00   68.00   67.28   73.00   90.00       9 





patient.clinical_cqcf.consent_or_death_status:
consented  deceased      NA's 
      420        83         1 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -58.00   -1.00    0.00   64.95   30.00 4825.00     105 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0   282.5   669.0   938.6  1345.0  3600.0     425 


patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             499 
yes, pharmaceutical treatment prior to resection 
                                               4 
                                            NA's 
                                               1 

patient.clinical_cqcf.history_of_prior_malignancy:
                                                     no 
                                                    434 
                       yes, history of prior malignancy 
                                                     61 
yes, history of synchronous and or bilateral malignancy 
                                                      6 
                                                   NA's 
                                                      3 

patient.clinical_cqcf.normal_tissue_anatomic_site:
bronchus     lung    other     NA's 
       1      203       66      234 

patient.clinical_cqcf.normal_tissue_proximity:
adjacent (< or = 2cm)         distal (>2cm)                  NA's 
                    3                    63                   438 



patient.clinical_cqcf.tumor_type:
primary 
    504 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   13.00   14.05   20.00   31.00       6 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32872  -26974  -25060  -24754  -22801  -14345      10 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   212.8   517.5   848.0  1112.2  5287.0     346 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 479   25 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -10.0    37.0   211.0   591.2   889.0  4299.0     119 

patient.days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   298.8   706.0   904.8  1051.8  3724.0     436 

patient.diagnosis:
lung squamous cell carcinoma 
                         504 

patient.dlco_predictive_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   55.00   69.00   67.19   81.00  142.00     344 

patient.drugs.drug.10.bcr_drug_barcode:
tcga-39-5029-d8466               NA's 
                 1                503 

patient.drugs.drug.10.bcr_drug_uuid:
951e4ab1-1010-4a41-b6e0-933cd2388dd0                                 NA's 
                                   1                                  503 

patient.drugs.drug.10.day_of_form_completion:
  11 NA's 
   1  503 

patient.drugs.drug.10.days_to_drug_therapy_end:
 211 NA's 
   1  503 

patient.drugs.drug.10.days_to_drug_therapy_start:
 140 NA's 
   1  503 

patient.drugs.drug.10.drug_name:
docetaxel      NA's 
        1       503 

patient.drugs.drug.10.month_of_form_completion:
   2 NA's 
   1  503 

patient.drugs.drug.10.number_cycles:
   4 NA's 
   1  503 

patient.drugs.drug.10.prescribed_dose:
 150 NA's 
   1  503 

patient.drugs.drug.10.prescribed_dose_units:
  mg NA's 
   1  503 

patient.drugs.drug.10.regimen_indication:
adjuvant     NA's 
       1      503 

patient.drugs.drug.10.regimen_number:
   1 NA's 
   1  503 

patient.drugs.drug.10.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              503 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
   1  503 

patient.drugs.drug.10.therapy_types.therapy_type:
chemotherapy         NA's 
           1          503 

patient.drugs.drug.10.total_dose:
 600 NA's 
   1  503 

patient.drugs.drug.10.total_dose_units:
  mg NA's 
   1  503 

patient.drugs.drug.10.year_of_form_completion:
2011 NA's 
   1  503 

patient.drugs.drug.11.bcr_drug_barcode:
tcga-39-5029-d8467               NA's 
                 1                503 

patient.drugs.drug.11.bcr_drug_uuid:
ea26fa13-0c52-40b7-87b0-a485d962a0bf                                 NA's 
                                   1                                  503 

patient.drugs.drug.11.day_of_form_completion:
  11 NA's 
   1  503 

patient.drugs.drug.11.days_to_drug_therapy_end:
 535 NA's 
   1  503 

patient.drugs.drug.11.days_to_drug_therapy_start:
 514 NA's 
   1  503 

patient.drugs.drug.11.drug_name:
pemetrexed       NA's 
         1        503 

patient.drugs.drug.11.month_of_form_completion:
   2 NA's 
   1  503 

patient.drugs.drug.11.number_cycles:
   2 NA's 
   1  503 

patient.drugs.drug.11.prescribed_dose:
1000 NA's 
   1  503 

patient.drugs.drug.11.prescribed_dose_units:
  mg NA's 
   1  503 

patient.drugs.drug.11.regimen_indication:
recurrence       NA's 
         1        503 

patient.drugs.drug.11.regimen_number:
   1 NA's 
   1  503 

patient.drugs.drug.11.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              503 

patient.drugs.drug.11.therapy_ongoing:
  no NA's 
   1  503 

patient.drugs.drug.11.therapy_types.therapy_type:
chemotherapy         NA's 
           1          503 

patient.drugs.drug.11.total_dose:
2000 NA's 
   1  503 

patient.drugs.drug.11.total_dose_units:
  mg NA's 
   1  503 

patient.drugs.drug.11.year_of_form_completion:
2011 NA's 
   1  503 

patient.drugs.drug.12.bcr_drug_barcode:
tcga-39-5029-d8480               NA's 
                 1                503 

patient.drugs.drug.12.bcr_drug_uuid:
cd41f6fc-2fb2-4ca0-8b57-cbe1d5d6a05d                                 NA's 
                                   1                                  503 

patient.drugs.drug.12.day_of_form_completion:
  11 NA's 
   1  503 

patient.drugs.drug.12.days_to_drug_therapy_end:
 707 NA's 
   1  503 

patient.drugs.drug.12.days_to_drug_therapy_start:
 707 NA's 
   1  503 

patient.drugs.drug.12.drug_name:
carboplatin        NA's 
          1         503 

patient.drugs.drug.12.month_of_form_completion:
   2 NA's 
   1  503 

patient.drugs.drug.12.number_cycles:
   1 NA's 
   1  503 

patient.drugs.drug.12.prescribed_dose:
 180 NA's 
   1  503 

patient.drugs.drug.12.prescribed_dose_units:
  mg NA's 
   1  503 

patient.drugs.drug.12.regimen_indication:
palliative       NA's 
         1        503 

patient.drugs.drug.12.regimen_number:
   2 NA's 
   1  503 

patient.drugs.drug.12.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              503 

patient.drugs.drug.12.therapy_ongoing:
  no NA's 
   1  503 

patient.drugs.drug.12.therapy_types.therapy_type:
chemotherapy         NA's 
           1          503 

patient.drugs.drug.12.total_dose:
 180 NA's 
   1  503 

patient.drugs.drug.12.total_dose_units:
  mg NA's 
   1  503 

patient.drugs.drug.12.year_of_form_completion:
2011 NA's 
   1  503 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   16.00   15.26   23.50   31.00     385 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   42.0   120.0   153.0   278.1   212.0  2021.0     389 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0    49.0    82.0   201.5   129.0  1995.0     385 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                          10                           45 
            partial response               stable disease 
                           1                            3 
                        NA's 
                         445 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.967  10.000  12.000     384 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.500   3.368   4.000   9.000     466 


patient.drugs.drug.2.prescribed_dose_units:
   auc     mg mg/day  mg/m2   NA's 
     1     18      3      8    474 


patient.drugs.drug.2.regimen_indication_notes:
                      consolidative                        neo-adjuvant 
                                  1                                   3 
progression of small cell carcinoma                                NA's 
                                  1                                 499 

patient.drugs.drug.2.regimen_number:
   1    2    3    4 NA's 
  26    6    1    2  469 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              35                1              468 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
 117    2  385 

patient.drugs.drug.2.therapy_types.therapy_type:
chemotherapy      vaccine         NA's 
         119            1          384 

patient.drugs.drug.2.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   88.0   196.0   457.5  1606.1  1185.0 20800.0     480 

patient.drugs.drug.2.total_dose_units:
   mg mg/m2  NA's 
   21     4   479 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  74    3  427 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2015     384 



patient.drugs.drug.3.clinical_trail_drug_classification:
mage/a3    NA's 
      1     503 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    6.75   15.00   15.03   21.75   31.00     468 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   24.0   150.0   259.0   493.8   710.8  2062.0     470 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0    91.0   182.0   424.7   627.5  2036.0     469 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                          10                            2 
                        NA's 
                         492 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.750   6.000   6.556  10.000  12.000     468 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.882   4.000  10.000     487 

patient.drugs.drug.3.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.78   75.00   90.00  221.19  200.00 1178.00     489 

patient.drugs.drug.3.prescribed_dose_units:
    mg mg/day  mg/m2   NA's 
     8      4      3    489 


patient.drugs.drug.3.regimen_indication_notes:
                     consolidative                       neo-adjuvant 
                                 1                                  2 
recurrence of small cell carcinoma                               NA's 
                                 1                                500 

patient.drugs.drug.3.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.882   2.000   5.000     487 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              14                3              487 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  35    1  468 

patient.drugs.drug.3.therapy_types.therapy_type:
   ancillary chemotherapy      vaccine         NA's 
           1           32            1          470 

patient.drugs.drug.3.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.68  191.25  340.00  327.17  400.00  750.00     494 

patient.drugs.drug.3.total_dose_units:
   mg mg/m2  NA's 
    8     3   493 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  14    1  489 

patient.drugs.drug.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2014     468 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    6.50   13.00   13.55   17.75   31.00     482 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   47.0   160.5   491.0   451.3   677.0   986.0     482 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   107.0   349.0   384.4   588.0   915.0     483 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                           6                            1 
                        NA's 
                         497 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.500   5.591   7.750  12.000     482 

patient.drugs.drug.4.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.500   3.143   4.000   6.000     490 

patient.drugs.drug.4.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    75.0    90.0   141.3   175.0   500.0     495 

patient.drugs.drug.4.prescribed_dose_units:
 g/day     mg mg/day  mg/m2   NA's 
     1      4      1      3    495 


patient.drugs.drug.4.regimen_indication_notes:
                      neo-adjuvant recurrence of small cell carcinoma 
                                 1                                  1 
                              NA's 
                               502 

patient.drugs.drug.4.regimen_number:
   1    2 NA's 
   5    7  492 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              11                1              492 

patient.drugs.drug.4.therapy_ongoing:
  no NA's 
  22  482 

patient.drugs.drug.4.therapy_types.therapy_type:
chemotherapy         NA's 
          22          482 

patient.drugs.drug.4.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    150     180     360     390     430    1000     497 

patient.drugs.drug.4.total_dose_units:
   mg mg/m2  NA's 
    5     2   497 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
   5    2  497 

patient.drugs.drug.4.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     482 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    4.50    9.00   11.91   18.00   29.00     493 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   47.0   122.5   436.0   509.8   556.5  1929.0     494 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   116.5   349.0   448.2   548.0  1778.0     493 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease                         NA's 
                           2                          502 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.091   7.000  11.000     493 

patient.drugs.drug.5.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.750   3.500   3.375   4.000   6.000     496 

patient.drugs.drug.5.prescribed_dose:
   5  150  800 NA's 
   1    2    1  500 

patient.drugs.drug.5.prescribed_dose_units:
 auc   mg NA's 
   1    3  500 

patient.drugs.drug.5.regimen_indication:
               adjuvant other: specify in notes             progression 
                      3                       2                       1 
             recurrence                    NA's 
                      2                     496 

patient.drugs.drug.5.regimen_indication_notes:
                            neo-adjuvant 
                                       1 
progression of small cell lung carcinoma 
                                       1 
                                    NA's 
                                     502 

patient.drugs.drug.5.regimen_number:
   1    2    3    5 NA's 
   2    3    1    1  497 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               5                1              498 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
  10    1  493 

patient.drugs.drug.5.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                        10                          1 
                      NA's 
                       493 

patient.drugs.drug.5.total_dose:
  600  1600 19500  NA's 
    2     1     1   500 

patient.drugs.drug.5.total_dose_units:
  mg NA's 
   4  500 

patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
   3  501 

patient.drugs.drug.5.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2014     493 



patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    7.25   10.00   12.12   13.75   29.00     496 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   24.0   293.5   492.0   581.0   671.0  1778.0     496 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   24.0   234.0   447.5   514.6   600.0  1594.0     496 


patient.drugs.drug.6.measure_of_response:
clinical progressive disease                         NA's 
                           1                          503 

patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.75    4.00    4.50    6.50    8.00     496 

patient.drugs.drug.6.number_cycles:
   1    2    3    4 NA's 
   1    2    1    2  498 

patient.drugs.drug.6.prescribed_dose:
  75  200  551 1800 NA's 
   1    1    1    1  500 

patient.drugs.drug.6.prescribed_dose_units:
g/day    mg mg/m2  NA's 
    1     3     1   499 

patient.drugs.drug.6.regimen_indication:
               adjuvant other: specify in notes             progression 
                      2                       1                       2 
             recurrence                    NA's 
                      2                     497 

patient.drugs.drug.6.regimen_indication_notes:
progression of small cell carcinoma                                NA's 
                                  1                                 503 

patient.drugs.drug.6.regimen_number:
   1    2    3    4 NA's 
   3    2    1    1  497 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               5                1              498 

patient.drugs.drug.6.therapy_ongoing:
  no NA's 
   8  496 

patient.drugs.drug.6.therapy_types.therapy_type:
chemotherapy         NA's 
           7          497 

patient.drugs.drug.6.total_dose:
 160  600 2204 3600 NA's 
   1    1    1    1  500 

patient.drugs.drug.6.total_dose_units:
  mg NA's 
   4  500 

patient.drugs.drug.6.tx_on_clinical_trial:
 yes NA's 
   1  503 

patient.drugs.drug.6.year_of_form_completion:
2011 2012 2014 NA's 
   4    3    1  496 



patient.drugs.drug.7.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    7.25   10.00   12.12   13.75   29.00     496 

patient.drugs.drug.7.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  154.0   413.5   633.0   570.9   698.0   986.0     497 

patient.drugs.drug.7.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   93.0   423.2   609.0   552.8   687.2   915.0     496 


patient.drugs.drug.7.measure_of_response:
clinical progressive disease                         NA's 
                           1                          503 

patient.drugs.drug.7.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.75    4.00    4.50    6.50    8.00     496 

patient.drugs.drug.7.number_cycles:
   1    3    4 NA's 
   1    2    3  498 

patient.drugs.drug.7.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0   100.0   400.0   835.2  1770.0  1900.0     499 

patient.drugs.drug.7.prescribed_dose_units:
   mg mg/m2 mg/ml  NA's 
    3     1     1   499 


patient.drugs.drug.7.regimen_indication_notes:
recurrence of small cell carcinoma                               NA's 
                                 1                                503 

patient.drugs.drug.7.regimen_number:
   1    2    3    4 NA's 
   2    2    2    1  497 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               6                1              497 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   8  496 

patient.drugs.drug.7.therapy_types.therapy_type:
chemotherapy         NA's 
           8          496 

patient.drugs.drug.7.total_dose:
  300   850  1900 14000  NA's 
    1     1     1     1   500 

patient.drugs.drug.7.total_dose_units:
  mg NA's 
   4  500 

patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   1  503 

patient.drugs.drug.7.year_of_form_completion:
2011 2012 2014 NA's 
   4    3    1  496 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-22-4595-d14621 tcga-22-5472-d15157  tcga-39-5029-d8478 tcga-56-7579-d72569 
                  1                   1                   1                   1 
               NA's 
                500 

patient.drugs.drug.8.bcr_drug_uuid:
04a0b97f-9d27-4269-a5fa-b49c14900882 1cb0d459-7478-4201-812e-9faa07052a89 
                                   1                                    1 
516143b0-8dc2-47af-89fb-fa266890d325 b3ff04b9-6349-4655-a20e-6686a903e29d 
                                   1                                    1 
                                NA's 
                                 500 

patient.drugs.drug.8.day_of_form_completion:
   4   11   12   21 NA's 
   1    1    1    1  500 

patient.drugs.drug.8.days_to_drug_therapy_end:
 637  707  710 1307 NA's 
   1    1    1    1  500 

patient.drugs.drug.8.days_to_drug_therapy_start:
 585  609  707 1221 NA's 
   1    1    1    1  500 

patient.drugs.drug.8.drug_name:
    azd6244 carboplatin   cetuximab  ironotecan        NA's 
          1           1           1           1         500 

patient.drugs.drug.8.measure_of_response:
clinical progressive disease                         NA's 
                           1                          503 

patient.drugs.drug.8.month_of_form_completion:
   2    7    8 NA's 
   1    1    2  500 

patient.drugs.drug.8.number_cycles:
   1    2    4 NA's 
   1    1    1  501 

patient.drugs.drug.8.prescribed_dose:
   5   60  460 NA's 
   1    1    1  501 

patient.drugs.drug.8.prescribed_dose_units:
  auc    mg mg/m2  NA's 
    1     1     1   501 

patient.drugs.drug.8.regimen_indication:
other: specify in notes              palliative                    NA's 
                      1                       2                     501 

patient.drugs.drug.8.regimen_indication_notes:
recurrence of small cell carcinoma                               NA's 
                                 1                                503 

patient.drugs.drug.8.regimen_number:
   2    3 NA's 
   2    1  501 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               3              501 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   4  500 

patient.drugs.drug.8.therapy_types.therapy_type:
chemotherapy         NA's 
           3          501 

patient.drugs.drug.8.total_dose:
  10  460 NA's 
   1    1  502 

patient.drugs.drug.8.total_dose_units:
 auc   mg NA's 
   1    1  502 

patient.drugs.drug.8.tx_on_clinical_trial:
 yes NA's 
   1  503 

patient.drugs.drug.8.year_of_form_completion:
2011 2015 NA's 
   3    1  500 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-39-5029-d8475               NA's 
                 1                503 

patient.drugs.drug.9.bcr_drug_uuid:
9f10ef1f-7a1d-429e-b23c-e96c5c0370f8                                 NA's 
                                   1                                  503 

patient.drugs.drug.9.day_of_form_completion:
  11 NA's 
   1  503 

patient.drugs.drug.9.days_to_drug_therapy_end:
 672 NA's 
   1  503 

patient.drugs.drug.9.days_to_drug_therapy_start:
 672 NA's 
   1  503 

patient.drugs.drug.9.drug_name:
vinorelbine tartrate                 NA's 
                   1                  503 

patient.drugs.drug.9.month_of_form_completion:
   2 NA's 
   1  503 

patient.drugs.drug.9.number_cycles:
   1 NA's 
   1  503 

patient.drugs.drug.9.prescribed_dose:
  47 NA's 
   1  503 

patient.drugs.drug.9.prescribed_dose_units:
  mg NA's 
   1  503 

patient.drugs.drug.9.regimen_indication:
palliative       NA's 
         1        503 

patient.drugs.drug.9.regimen_number:
   1 NA's 
   1  503 

patient.drugs.drug.9.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              503 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   1  503 

patient.drugs.drug.9.therapy_types.therapy_type:
chemotherapy         NA's 
           1          503 

patient.drugs.drug.9.total_dose:
  47 NA's 
   1  503 

patient.drugs.drug.9.total_dose_units:
  mg NA's 
   1  503 

patient.drugs.drug.9.year_of_form_completion:
2011 NA's 
   1  503 



patient.drugs.drug.clinical_trail_drug_classification:
antigen specific cancer immunotherapeutic therapy 
                                                1 
                                             NA's 
                                              503 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   16.00   15.15   24.00   31.00     361 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   42.0   125.0   153.0   266.8   218.2  2016.0     376 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0    49.5    82.0   194.6   128.0  1903.0     365 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          13                           48 
            partial response               stable disease 
                           2                            3 
                        NA's 
                         438 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.938  10.000  12.000     360 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.489   4.000   7.000     457 




patient.drugs.drug.regimen_indication_notes:
                      neo-adjuvant recurrence of small cell carcinoma 
                                 2                                  1 
                              NA's 
                               501 

patient.drugs.drug.regimen_number:
   1    2    3 NA's 
  40    3    2  459 

patient.drugs.drug.route_of_administrations.route_of_administration:
intramuscular (im)   intravenous (iv)               oral               NA's 
                 2                 41                  1                460 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 131    8  365 

patient.drugs.drug.therapy_types.therapy_type:
  chemotherapy other, specify        vaccine           NA's 
           141              2              1            360 

patient.drugs.drug.therapy_types.therapy_type_notes:
mage3-as15-nsc-003            vaccine               NA's 
                 1                  1                502 

patient.drugs.drug.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   92.75  407.50 1277.96 1640.00 8950.00     480 


patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  85    3  416 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2003    2011    2012    2012    2013    2015     360 

patient.eastern_cancer_oncology_group:
   0    1    2    3 NA's 
  70   93   26    5  310 

patient.egfr_mutation_performed:
  no  yes NA's 
 278   21  205 

patient.egfr_mutation_result:
l858r other  NA's 
    1     1   502 

patient.eml4_alk_translocation_method:
fish NA's 
   4  500 

patient.eml4_alk_translocation_performed:
  no  yes NA's 
 278    9  217 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     8                    319                    177 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
  no  yes NA's 
  10   12  482 

patient.follow_ups.follow_up.2.additional_radiation_therapy:
  no  yes NA's 
  15    9  480 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure:
  no  yes NA's 
  13    4  487 

patient.follow_ups.follow_up.2.additional_surgery_metastatic_procedure:
  no NA's 
  10  494 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   16.00   14.97   22.00   31.00     391 

patient.follow_ups.follow_up.2.days_to_additional_surgery_locoregional_procedure:
 428  838 1433 1693 NA's 
   1    1    1    1  500 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  299.0   686.2   924.0  1303.0  1858.8  3924.0     480 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  122.0   845.5  1259.0  1542.1  1986.0  4765.0     413 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  159.0   546.0   830.5  1210.5  1570.8  4471.0     480 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   0    1    2    3 NA's 
  15    7    5    2  475 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             7                             91 
                          NA's 
                           406 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response         progressive disease 
                         56                          12 
             stable disease                        NA's 
                          2                         434 

patient.follow_ups.follow_up.2.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   72.50   90.00   77.33  100.00  100.00     474 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  84    8  412 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.442   8.000  12.000     391 

patient.follow_ups.follow_up.2.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      3                       6                       6 
                   NA's 
                    489 

patient.follow_ups.follow_up.2.new_neoplasm_event_types.new_neoplasm_event_type.2:
new primary tumor              NA's 
                1               503 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  68   24  412 

patient.follow_ups.follow_up.2.performance_status_scale_timing:
     adjuvant therapy                 other post-adjuvant therapy 
                    3                    10                    19 
        pre-operative                  NA's 
                    3                   469 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        69         17        418 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         90                           1 
        progressive disease              stable disease 
                          3                           1 
                       NA's 
                        409 

patient.follow_ups.follow_up.2.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                  6                                   4 
                               NA's 
                                494 

patient.follow_ups.follow_up.2.progression_determined_by_list.progression_determined_by.2:
convincing imaging               NA's 
                 2                502 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  88    9  407 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
  62   35  407 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   85    24   395 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2013    2013    2013    2015    2015     391 

patient.follow_ups.follow_up.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    2  500 

patient.follow_ups.follow_up.3.additional_radiation_therapy:
  no  yes NA's 
   3    1  500 

patient.follow_ups.follow_up.3.additional_surgery_locoregional_procedure:
  no NA's 
   2  502 

patient.follow_ups.follow_up.3.additional_surgery_metastatic_procedure:
  no NA's 
   3  501 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   17.00   23.00   20.56   25.00   31.00     495 

patient.follow_ups.follow_up.3.days_to_death:
 408  951 1190 NA's 
   1    1    1  501 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  559.0   840.5  1113.0  1088.6  1191.5  1884.0     497 

patient.follow_ups.follow_up.3.days_to_new_tumor_event_after_initial_treatment:
 237  556  632  898 NA's 
   1    1    1    1  500 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                              6 
                          NA's 
                           496 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response         progressive disease 
                          3                           4 
                       NA's 
                        497 

patient.follow_ups.follow_up.3.karnofsky_performance_score:
 100 NA's 
   1  503 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
   8  496 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   6.222  10.000  12.000     495 

patient.follow_ups.follow_up.3.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      2                       1                     501 

patient.follow_ups.follow_up.3.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   4    4  496 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         3          4        497 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                          6                           1 
                       NA's 
                        497 

patient.follow_ups.follow_up.3.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                  1                                   2 
                               NA's 
                                501 

patient.follow_ups.follow_up.3.progression_determined_by_list.progression_determined_by.2:
convincing imaging               NA's 
                 1                503 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
   6    1  497 

patient.follow_ups.follow_up.3.targeted_molecular_therapy:
  no  yes NA's 
   3    4  497 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
    6     3   495 

patient.follow_ups.follow_up.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2013    2014    2013    2015    2015     495 

patient.follow_ups.follow_up.4.additional_pharmaceutical_therapy:
  no NA's 
   1  503 

patient.follow_ups.follow_up.4.additional_radiation_therapy:
  no NA's 
   1  503 

patient.follow_ups.follow_up.4.additional_surgery_locoregional_procedure:
  no NA's 
   1  503 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-la-a7sw-f67139 tcga-nc-a5hp-f69354                NA's 
                  1                   1                 502 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
865fd90b-cbfb-4efc-b390-d42dbb4358e1 b9137785-c2f3-4344-881f-8f52eff6082a 
                                   1                                    1 
                                NA's 
                                 502 

patient.follow_ups.follow_up.4.day_of_form_completion:
   7   31 NA's 
   1    1  502 

patient.follow_ups.follow_up.4.days_to_death:
 408  770 NA's 
   1    1  502 

patient.follow_ups.follow_up.4.days_to_new_tumor_event_after_initial_treatment:
 311 NA's 
   1  503 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                              1 
                          NA's 
                           502 

patient.follow_ups.follow_up.4.followup_treatment_success:
progressive disease                NA's 
                  1                 503 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   2  502 

patient.follow_ups.follow_up.4.month_of_form_completion:
   1   10 NA's 
   1    1  502 

patient.follow_ups.follow_up.4.new_neoplasm_event_types.new_neoplasm_event_type:
locoregional recurrence                    NA's 
                      1                     503 

patient.follow_ups.follow_up.4.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   1    1  502 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
with tumor       NA's 
         1        503 

patient.follow_ups.follow_up.4.primary_therapy_outcome_success:
progressive disease                NA's 
                  1                 503 

patient.follow_ups.follow_up.4.progression_determined_by_list.progression_determined_by:
convincing imaging               NA's 
                 1                503 

patient.follow_ups.follow_up.4.radiation_therapy:
  no NA's 
   1  503 

patient.follow_ups.follow_up.4.targeted_molecular_therapy:
 yes NA's 
   1  503 

patient.follow_ups.follow_up.4.vital_status:
dead NA's 
   2  502 

patient.follow_ups.follow_up.4.year_of_form_completion:
2014 2015 NA's 
   1    1  502 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
  95   34  375 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
  93   34  377 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure:
  no  yes NA's 
  74   10  420 

patient.follow_ups.follow_up.additional_surgery_metastatic_procedure:
  no  yes NA's 
  57   11  436 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     8.5    15.0    15.7    23.0    31.0      61 

patient.follow_ups.follow_up.days_to_additional_surgery_locoregional_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  229.0   701.8  1175.5  1104.4  1605.5  1788.0     494 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  178.0   330.2   436.0   594.6   580.2  2215.0     494 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   295.2   541.0   859.4  1104.5  5287.0     346 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -9.0   395.0   642.0   920.4  1092.0  4694.0     191 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  103.0   263.0   427.0   631.2   760.0  2923.0     407 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  1.0000  0.8807  1.0000  4.0000     395 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                            337 
                          NA's 
                           165 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response  partial remission/response 
                        165                           2 
        progressive disease              stable disease 
                         59                          13 
                       NA's 
                        265 

patient.follow_ups.follow_up.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   80.00   54.17  100.00  100.00     396 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 237   37  230 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   7.151  11.000  12.000      60 

patient.follow_ups.follow_up.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     25                      18                       5 
                   NA's 
                    456 

patient.follow_ups.follow_up.new_neoplasm_event_types.new_neoplasm_event_type.2:
distant metastasis  new primary tumor               NA's 
                 1                  1                502 

patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 270  102  132 


patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       254         91        159 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        276                           5 
        progressive disease              stable disease 
                         25                          14 
                       NA's 
                        184 

patient.follow_ups.follow_up.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                 15                                  26 
                               NA's 
                                463 

patient.follow_ups.follow_up.progression_determined_by_list.progression_determined_by.2:
convincing imaging               NA's 
                 5                499 

patient.follow_ups.follow_up.progression_determined_by_list.progression_determined_by.3:
positive biomarker (s)                   NA's 
                     1                    503 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 349   47  108 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
 276  125  103 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  271   163    70 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2014    2015      60 

patient.gender:
female   male 
   131    373 

patient.histological_type:
                      lung basaloid squamous cell carcinoma 
                                                         15 
                     lung papillary squamous cell caricnoma 
                                                          6 
                    lung small cell squamous cell carcinoma 
                                                          1 
lung squamous cell carcinoma- not otherwise specified (nos) 
                                                        482 

patient.history_of_neoadjuvant_treatment:
                                              no 
                                             497 
                                             yes 
                                               6 
yes, pharmaceutical treatment prior to resection 
                                               1 




patient.informed_consent_verified:
yes 
504 

patient.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   80.00   57.09   90.00  100.00     401 

patient.kras_gene_analysis_performed:
  no  yes NA's 
 309   11  184 

patient.kras_mutation_found:
  no  yes NA's 
  14    1  489 

patient.kras_mutation_result:
other  NA's 
    1   503 

patient.location_in_lung_parenchyma:
   central lung peripheral lung            NA's 
            147              93             264 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.944   8.000  12.000 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  15   10  479 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  11   15  478 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   151.5   295.0   466.4   490.0  2197.0     474 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.additional_surgery_locoregional_procedure:
  no  yes NA's 
  14    1  489 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.days_to_additional_surgery_locoregional_procedure:
 168 NA's 
   1  503 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.additional_surgery_metastatic_procedure:
  no  yes NA's 
  11    1  492 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.days_to_additional_surgery_metastatic_procedure:
 453 NA's 
   1  503 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     14                      12                       4 
                   NA's 
                    474 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type.2:
distant metastasis               NA's 
                 2                502 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                  9                                  14 
                               NA's 
                                481 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by.2:
convincing imaging               NA's 
                 3                501 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 140   30  334 

patient.number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   31.12   50.00   52.91   64.50  240.00      77 


patient.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                   15                    12                    24 
        pre-operative                  NA's 
                  142                   311 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       313         82        109 

patient.post_bronchodilator_fev1_fvc_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   60.50   70.00   71.76   83.50  121.00     421 

patient.post_bronchodilator_fev1_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   62.00   76.00   74.61   91.00  140.00     411 

patient.pre_bronchodilator_fev1_fvc_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   61.00   70.00   69.69   78.00  117.00     341 

patient.pre_bronchodilator_fev1_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   60.00   72.00   70.94   86.00  132.00     316 

patient.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                        115                           9 
             stable disease                        NA's 
                          8                         372 

patient.prior_dx:
                                                     no 
                                                    433 
                                                    yes 
                                                     41 
                       yes, history of prior malignancy 
                                                     23 
yes, history of synchronous and or bilateral malignancy 
                                                      6 
                                                   NA's 
                                                      1 

patient.pulmonary_function_test_performed:
  no  yes NA's 
  93  210  201 

patient.race:
                    asian black or african american                     white 
                        9                        31                       351 
                     NA's 
                      113 

patient.radiation_therapy:
  no  yes NA's 
 139   15  350 

patient.radiations.radiation.2.anatomic_treatment_site:
       distant site primary tumor field                NA's 
                  3                   2                 499 



patient.radiations.radiation.2.course_number:
   1    2 NA's 
   3    1  500 

patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     7.0    12.5    14.0    18.0    31.0     498 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  130.0   224.8   305.5   345.2   347.2   771.0     498 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  111.0   173.0   200.0   229.2   325.0   337.0     499 

patient.radiations.radiation.2.measure_of_response:
radiographic progressive disease                             NA's 
                               1                              503 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.500   5.333   8.250  10.000     498 

patient.radiations.radiation.2.numfractions:
   6   10   14   33 NA's 
   1    2    1    1  499 

patient.radiations.radiation.2.radiation_dosage:
  30 2000 2100 3500 NA's 
   1    1    1    2  499 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   5  499 

patient.radiations.radiation.2.radiation_type:
     external external beam          NA's 
            1             4           499 

patient.radiations.radiation.2.regimen_indication:
   adjuvant progression  recurrence        NA's 
          1           2           1         500 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
   4    1  499 

patient.radiations.radiation.2.year_of_form_completion:
2011 2012 2013 2014 NA's 
   2    2    1    1  498 




patient.radiations.radiation.course_number:
   1    2 NA's 
  25    3  476 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.50   14.00   14.51   20.00   31.00     437 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   118.5   195.0   329.7   409.0  1929.0     441 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    63.0   160.0   292.6   356.0  1868.0     443 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              12                                2 
radiographic progressive disease                   stable disease 
                              10                                3 
                            NA's 
                             477 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   6.000   6.731  10.500  12.000     437 


patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      8      50    2700    2656    5000    6900     448 

patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
  62    2  440 

patient.radiations.radiation.radiation_type:
               external           external beam                internal 
                     29                      27                       1 
other: specify in notes                    NA's 
                      2                     445 

patient.radiations.radiation.radiation_type_notes:
stereotactic radio sugery                      NA's 
                        1                       503 


patient.radiations.radiation.regimen_indication_notes:
palliative for small cell carcinoma                                NA's 
                                  1                                 503 

patient.radiations.radiation.units:
 cgy   gy NA's 
  38   19  447 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     437 

patient.residual_tumor:
  r0   r1   r2   rx NA's 
 401   12    4   23   64 




patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
 n0  n1  n2  n3  nx 
320 133  40   5   6 


patient.stopped_smoking_year:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1952    1993    2002    1999    2008    2013     195 

patient.targeted_molecular_therapy:
  no  yes NA's 
 103   51  350 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 325  172    7 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
 172  325    7 



patient.tumor_tissue_site:
lung 
 504 

patient.vital_status:
alive  dead 
  343   161 

patient.year_of_form_completion:
2010 2011 2012 2013 2014 
  83  168  132  104   17 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2005    2009    2008    2011    2013      17 

patient.year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1933    1952    1960    1960    1968    1997     183 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
 case matched expression control case matched methylation control 
                              58                               72 
             germline comparison                             NA's 
                              25                              349 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              21                               33 
                            NA's 
                             450 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                 10                 494 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             349              155 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased      NA's 
      420        82         2 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   71.25  550.50 1062.89 1868.00 6501.00      26 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -92.00    0.00    0.00   19.65   30.00  792.00      26 

patient.biospecimen_cqcf.digital_image_submitted:
  no  yes NA's 
 459   44    1 

patient.biospecimen_cqcf.ethnicity:
not hispanic or latino                   NA's 
                    60                    444 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
bronchus     lung    other     NA's 
       7      461       33        3 


patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             499 
yes, pharmaceutical treatment prior to resection 
                                               4 
                                            NA's 
                                               1 

patient.biospecimen_cqcf.laterality:
 left right  NA's 
  209   252    43 

patient.biospecimen_cqcf.method_of_sample_procurement:
            excisional biopsy other method (please specify) 
                            1                            77 
              tumor resection                          NA's 
                          424                             2 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -22.00    0.00   15.00   22.23   35.00  127.00     407 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
0.238  NA's 
    1   503 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
nanodrop     NA's 
       1      503 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  20 NA's 
   1  503 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
  84 NA's 
   1  503 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
                   blood draw other method (please specify) 
                           36                             8 
              tumor resection                          NA's 
                           54                           406 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
 lung other  NA's 
   63     4   437 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
adjacent (< or = 2cm)         distal (>2cm)                  NA's 
                    1                    37                   466 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
  87   13  404 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
0f621065-6f8f-4f57-8676-4dd855f4b2ed 741080e8-f41a-46e5-827e-7ea5198049f8 
                                   1                                    1 
                                NA's 
                                 502 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
  35   37 NA's 
   1    1  502 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
     blood draw tumor resection            NA's 
              1               1             502 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
bronchus     NA's 
       1      503 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            1           503 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    3 NA's 
   1    1  502 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -92.0     0.0     1.0    22.4    31.0   792.0      27 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.040   0.134   0.301   8.013   0.574 100.000     429 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm:
        nano drop          nanodrop spectrophotometry              NA's 
                5                52                18               429 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0    20.0    20.0    22.7    26.5    30.0     429 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  8.356  46.500 100.000 189.990 300.000 600.000     429 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
                   blood draw other method (please specify) 
                          282                            37 
              tumor resection                          NA's 
                          144                            41 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
 lung other  NA's 
  138    62   304 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
adjacent (< or = 2cm)         distal (>2cm)                  NA's 
                    2                    24                   478 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2    3 NA's 
 469   31    3    1 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
  no  yes NA's 
   4  460   40 

patient.biospecimen_cqcf.path_confirm_report_attached:
  no  yes NA's 
   9  494    1 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
  no  yes NA's 
   7  457   40 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
  no  yes NA's 
  24  440   40 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                        3                        16                        57 
                     NA's 
                      428 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                4 
                                                                                                                             NA's 
                                                                                                                              500 

patient.biospecimen_cqcf.sample_prescreened:
 yes NA's 
 503    1 

patient.biospecimen_cqcf.site_of_disease:
bronchus     lung    other     NA's 
       7      461       33        3 

patient.biospecimen_cqcf.site_of_disease_description:
 lower middle  other  upper   NA's 
   183     18     33    260     10 


patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
 165   49  290 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
  61  442    1 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     5.0     7.5    12.7    17.5    50.0     494 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0    50.0    62.5    63.1    76.5    90.0     494 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0    63.0    90.5   108.6   175.5   200.0     494 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   2    8  494 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     2.0    10.0    11.5    20.0    70.0       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   65.50   70.00   72.76   80.00  100.00       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.0   138.5   200.0   326.3   300.0  3009.0      22 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1 NA's 
 503    1 


patient.days_to_index:
   0 NA's 
 479   25 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   103.2   327.0   909.3  1294.8  5727.0     390 

patient.samples.sample.2.diagnostic_slides.ffpe_slide_uuid:
692a8000-846e-4ed7-b790-b3281acb9dec 72c1a8db-146c-4408-8d31-43baba769fdf 
                                   1                                    1 
e0b0240c-9b6c-4da9-b9cb-bc634051fc45                                 NA's 
                                   1                                  501 

patient.samples.sample.2.initial_weight:
  80  130  150  160 NA's 
   3    1    1    1  498 

patient.samples.sample.2.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.6250  0.8000  0.8796  1.0000  2.2000     342 

patient.samples.sample.2.is_ffpe:
 no yes 
501   3 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   1.000   1.200   1.247   1.500   2.800     342 

patient.samples.sample.2.oct_embedded:
false  true  NA's 
  125     6   373 

patient.samples.sample.2.pathology_report_file_name:
tcga-68-7757.87fd1307-e3eb-45da-b0ec-2e3cb95e06d5.pdf 
                                                    1 
tcga-92-7340.ec11729e-f821-4f7b-91c5-0ed2026e5881.pdf 
                                                    1 
tcga-92-8063.517002a1-6e8c-400f-ae16-f23a061f6607.pdf 
                                                    1 
tcga-92-8064.38c3c16a-1a81-4812-9931-b5fb4b00615e.pdf 
                                                    1 
                                                 NA's 
                                                  500 

patient.samples.sample.2.pathology_report_uuid:
38c3c16a-1a81-4812-9931-b5fb4b00615e 517002a1-6e8c-400f-ae16-f23a061f6607 
                                   1                                    1 
87fd1307-e3eb-45da-b0ec-2e3cb95e06d5 ec11729e-f821-4f7b-91c5-0ed2026e5881 
                                   1                                    1 
                                NA's 
                                 500 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.7  1.8  1.9 NA's 
   1    8    1  494 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89575993  96491638  96491984  96632760  97513544 103959779       370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   8    9 NA's 
  22  112  370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
 0.5 NA's 
 134  370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   19.00   27.00   24.12   31.00   31.00     370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 134  370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   4.261   5.000   9.000     370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   7.209  10.000  12.000     370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  782.0   879.0   880.0   946.6   943.0  1262.0     370 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
  30   40 NA's 
  69   65  370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
 134  370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  60   80 NA's 
  69   65  370 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
 134  370 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89576377  89576436  96485711  95962602  96486884 153878584       433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   8    9   10 NA's 
   2   22   47  433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
 0.5 NA's 
  71  433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  13   15   31 NA's 
  22    2   47  433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  71  433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   1    3    7 NA's 
  22   47    2  433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       5       7       7      10      12     433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
0783 0881 0882 a315 NA's 
  22   36   11    2  433 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   5   30 NA's 
   2   69  433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22   23 NA's 
  69    2  433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
  10   60 NA's 
   2   69  433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2010 2013 NA's 
  69    2  433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-56-1622-11a-01w-a315-08 tcga-60-2713-11a-01w-a315-08 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
4a1025c4-405c-40fe-bfce-5a36cb5650b5 e7fcea98-5cd8-4952-ac9c-64ab831d777c 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
153878552 153878576      NA's 
        1         1       502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   8 NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
 0.5 NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  15 NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   7 NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   1 NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
a315 NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   b    d NA's 
   1    1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
   5 NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  23 NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
  10 NA's 
   2  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2013 NA's 
   2  502 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.079e+08 1.322e+08 3.335e+08 1.507e+08 1.076e+09        37 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   10   13 NA's 
   8  444   22   15   15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4832  0.5000  0.5000      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    8.00   12.00   16.89   27.00   31.00      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 489   15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.851   6.000  12.000      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   7.129   9.000  12.000      15 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.86   40.00   40.00   36.84   40.00   40.00      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 357  132   15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.30   80.00   80.00   74.33   80.00   80.00      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 NA's 
 134  113  110  132   15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    70.0   163.0   133.2   185.0   309.5     147 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 466                   23                   15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r    w NA's 
  15    8  466   15 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.500   0.500   0.477   0.500   0.500     147 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 466   38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
 334  132   38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             489 
                                            NA's 
                                              15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 489   15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                        8                        15                       451 
                repli-g x                      NA's 
                       15                        15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.90    1.16    1.28    1.36    1.56    1.88     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.450   8.200   7.987   8.400   9.100     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
      132       225       147 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   7  497 

patient.samples.sample.2.portions.portion.analytes.analyte.3.a260_a280_ratio:
 1.6  1.7 NA's 
   1    4  499 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97542305  97543270  97543318  99549109 103964699 103964771       439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   9 NA's 
  65  439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
  65  439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  19   27   29 NA's 
  36   18   11  439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  65  439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   5    6    9 NA's 
  36   11   18  439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   7.261  10.000  12.000     439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
 941  956 1070 1264 NA's 
  26   10   11   18  439 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
  65  439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
  65  439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
  65  439 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  65  439 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.754e+07 9.754e+07 9.889e+07 1.680e+08 1.070e+08 1.062e+09       416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7    8 NA's 
  14   74  416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4433  0.5000  0.5000     416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   19.00   19.00   21.32   27.00   30.00     416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  88  416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    5.00    6.00    8.25   12.00     416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.659   9.000  12.000     416 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.73   40.00   40.00   33.98   40.00   40.00     416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
  87    1  416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
13.3 26.7   80 NA's 
  12    2   74  416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 NA's 
  65   15    7    1  416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.2    61.5    73.0    82.9   101.5   195.0     417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                     9                     65                     14 
                  NA's 
                   416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   r    w    x NA's 
  14    9   65  416 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4426  0.5000  0.5000     417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  74  430 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
  73    1  430 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              88 
                                            NA's 
                                             416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  88  416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                       11                         3                         8 
                repli-g x                      NA's 
                       66                       416 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   1.130   1.305   1.289   1.423   1.500     490 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.200   7.300   7.550   7.793   8.275   9.000     490 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
        2        85       417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   4  500 



patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.210e+08 1.210e+08 3.202e+08 1.244e+08 1.062e+09       490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8 NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    9.50   21.00   18.71   29.25   30.00     490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.500   4.000   5.357   7.000  12.000     490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   1    5    6    8 NA's 
   1    6    5    2  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1712    1925    2117    2058    2163    2330     490 


patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2011 2012 NA's 
   7    7  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  153.0   173.5   179.0   183.2   187.0   245.0     490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                  14                  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
  14  490 



patient.samples.sample.2.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              14 
                                            NA's 
                                             490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
  14  490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     14     490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen   uv spec      NA's 
       10         4       490 

patient.samples.sample.2.portions.portion.analytes.analyte.4.subportion_sequence:
   1 NA's 
   4  500 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.900   1.900   1.882   1.900   2.000     284 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_barcode:
tcga-60-2722-11a-01d-2250-01                         NA's 
                           1                          503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_uuid:
1f700cde-dfea-4e03-9524-e54c586fa586                                 NA's 
                                   1                                  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
125245974      NA's 
        1       503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
   1 NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
0.15 NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
  29 NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   8 NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   6 NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_id:
2250 NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_row:
   b NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
 1.2 NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  22 NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
   8 NA's 
   1  503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2012 NA's 
   1  503 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 1.040e+08 1.252e+08 2.729e+08 1.457e+08 1.076e+09        25 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   5.565   8.000   8.000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0900  0.1062  0.1500  0.1700       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   10.00   15.00   17.43   27.00   30.00       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 501    3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.471   8.000  12.000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.852   9.000  12.000       3 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.860   3.000   2.668   3.200   4.540       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 373  128    3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   40.00   33.41   60.00   60.00       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2011    2011    2012    2014       3 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 9.751e+07 1.005e+08 4.053e+08 1.053e+09 1.054e+09       288 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   6.000   6.437   8.000   9.000     266 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1500  0.1167  0.1500  0.1700     266 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   10.00   15.00   16.68   30.00   30.00     266 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 238  266 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   6.000   6.382   8.000  11.000     266 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    6.00    6.58    8.00   12.00     266 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.000   2.000   2.316   3.000   4.540     266 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 230    8  266 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.30   20.00   26.49   40.00   60.00     266 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2010    2010    2011    2013     266 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 9.651e+07 9.889e+07 1.834e+08 1.040e+08 1.053e+09       344 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.824   8.000   9.000     322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1382  0.1500  0.1700     322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   14.00   15.96   27.00   30.00     322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 182  322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   6.000   6.445   9.000  11.000     322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   6.000   6.555   8.000  12.000     322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    688     847     984    1112    1441    1947     322 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.500   2.600   2.391   3.000   4.540     322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
 182  322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   10.00   20.00   19.06   20.00   40.00     322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2009 2010 2011 NA's 
  22  134   26  322 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.752e+07 9.752e+07 9.889e+07 2.237e+08 1.040e+08 1.050e+09       374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   4    6    8    9 NA's 
  27    9   76   18  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1365  0.1500  0.1700     374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     2.0    27.0    19.8    28.0    31.0     374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 130  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.138   6.000   9.000     374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   7.000   7.031   9.000  12.000     374 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.460   1.010   1.140   1.838   2.000   4.270     374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
 129    1  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.50    6.70    6.70   15.85   26.70   40.00     374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 2013 NA's 
 112   17    1  374 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.752e+07 9.752e+07 9.885e+07 1.771e+08 9.886e+07 1.050e+09       405 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   1    2    6    9 NA's 
   5    8   27   59  405 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1500  0.1500  0.1491  0.1500  0.1700     405 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   28.00   19.17   28.00   29.00     405 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  99  405 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   4.000   4.808   6.000   8.000     405 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   8.000   7.717  10.000  12.000     405 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.010   1.070   1.346   1.930   2.130     405 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
  98    1  405 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.300   6.700   6.700   9.058  13.300  13.400     405 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 2011 2012 2015 NA's 
  85    8    5    1  405 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.050e+09 1.050e+09 8.642e+08 1.050e+09 1.050e+09       423 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   8.000   5.938   8.000   9.000     423 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0906  0.1500  0.1600     423 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   26.00   29.00   27.44   31.00   31.00     423 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  81  423 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1    8    9   10 NA's 
  66    7    1    7  423 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   7.025   9.000  12.000     423 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.070   2.000   2.000   2.217   2.000   6.000     423 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
  80    1  423 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   13.30   40.00   30.39   40.00   40.00     423 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2010 2011 2012 2015 NA's 
  13   65    2    1  423 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.039e+08 1.252e+08 1.322e+08 3.714e+08 5.978e+08 1.050e+09       485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   1    8 NA's 
   6   13  485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0800  0.0926  0.1500  0.1600     485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  18   19   29   31 NA's 
   1    7    6    5  485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
  19  485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   1    8   10 NA's 
   5   13    1  485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.500   5.000   5.368   6.500  10.000     485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
1352 1522 2250 a320 NA's 
   1    5    6    7  485 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.200   1.210   1.390   1.518   2.000   2.000     485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
  12    7  485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
   8 17.4   40 NA's 
   6    7    6  485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2010 2011 2012 2013 NA's 
   1    5    6    7  485 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.252e+08 1.252e+08 1.252e+08 3.110e+08 1.252e+08 1.054e+09       499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   1    5 NA's 
   4    1  499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.14 0.15 NA's 
   1    4  499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   6   29 NA's 
   1    4  499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   5  499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   7    8 NA's 
   1    4  499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   5    6    7 NA's 
   1    3    1  499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
1871 2250 NA's 
   1    4  499 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
1.12  1.2 9.99 NA's 
   1    3    1  499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22 NA's 
   5  499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
   8 66.6 NA's 
   4    1  499 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 2012 NA's 
   1    4  499 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 1.040e+08 1.252e+08 2.742e+08 1.457e+08 1.076e+09        22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   4   8 
375  95  11  23 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1400  0.1500  0.1478  0.1600  0.1700 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   10.00   15.00   17.38   27.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no yes 
501   3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.462   8.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   6.853   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.000   1.070   1.304   1.400   4.010 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
373 131 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  6.670   6.670   6.700   9.461  10.000  40.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2009    2010    2012    2011    2012    2014 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   44.04  117.70  187.69  273.88 1385.00 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
504 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
504 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
  33   95  201  120   55 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
504 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
270 234 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             504 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no yes 
501   3 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type                  allprep ffpe dna 
                              272                                 3 
    chemical lysis dna extraction pre-extracted dna received by tss 
                              164                                65 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      283       221 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
  13  491 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    9.00   17.00   17.95   29.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no yes 
501   3 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   7.000   6.821  11.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
  1   2  11 
490   1  13 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
 182  322 




patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 111  393 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
 top NA's 
 111  393 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
 169  335 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.000   5.000   6.333   8.000  20.000     495 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   5.000   5.000   7.444  14.000  15.000     495 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0    8 NA's 
   6    1  497 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    2    3   20 NA's 
   4    2    2    1  495 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   27.50   60.00   53.57   80.00  100.00     497 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   12.50   30.00   34.29   52.50   80.00     497 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0   77 NA's 
   6    1  497 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0   65 NA's 
   6    1  497 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
bottom    top   NA's 
   111     58    335 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    2.00   40.96   66.50  300.00     214 

patient.samples.sample.2.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2009    2011    2012    2011    2012    2014 

patient.samples.sample.2.sample_type:
blood derived normal         ffpe scrolls        primary tumor 
                 314                    3                    1 
 solid tissue normal 
                 186 

patient.samples.sample.2.sample_type_id:
  1  10  11 
  4 314 186 

patient.samples.sample.2.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.4000  0.4074  0.5000  1.2000     342 

patient.samples.sample.2.vial_number:
  a   b   d   e   g 
497   4   1   1   1 



patient.samples.sample.3.days_to_collection:
 876  908  937 NA's 
   1    1    1  501 

patient.samples.sample.3.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   0.600   0.800   0.809   1.000   1.800     437 

patient.samples.sample.3.is_ffpe:
  no  yes NA's 
  68    3  433 

patient.samples.sample.3.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   0.850   1.100   1.239   1.400   4.400     437 

patient.samples.sample.3.oct_embedded:
false  NA's 
    3   501 

patient.samples.sample.3.pathology_report_file_name:
tcga-92-7340.ec11729e-f821-4f7b-91c5-0ed2026e5881.pdf 
                                                    1 
tcga-92-8063.517002a1-6e8c-400f-ae16-f23a061f6607.pdf 
                                                    1 
tcga-92-8064.38c3c16a-1a81-4812-9931-b5fb4b00615e.pdf 
                                                    1 
                                                 NA's 
                                                  501 

patient.samples.sample.3.pathology_report_uuid:
38c3c16a-1a81-4812-9931-b5fb4b00615e 517002a1-6e8c-400f-ae16-f23a061f6607 
                                   1                                    1 
ec11729e-f821-4f7b-91c5-0ed2026e5881                                 NA's 
                                   1                                  501 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.7  1.8 NA's 
   2    5  497 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-43-3394-11a-01r-1758-07 tcga-51-4079-11a-01r-1758-07 
                           1                            1 
tcga-51-4080-11a-01r-1758-07 tcga-51-4081-11a-01r-1758-07 
                           1                            1 
                        NA's 
                         500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
47bb130b-c5cc-40ba-b72f-7d337080c62f 80ee8813-997d-48b9-9853-520080e22e05 
                                   1                                    1 
d5da6fea-e0ee-4da5-8f4a-45d850fead0c e21484e9-d9c3-463f-89bd-82f09cc2579a 
                                   1                                    1 
                                NA's 
                                 500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
1053958183 1053958185 1053958206 1053958209       NA's 
         1          1          1          1        500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7 NA's 
   4  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 NA's 
   1    3  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  11 NA's 
   4  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   5 NA's 
   4  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    2 NA's 
   1    3  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
1758 NA's 
   4  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   d    e    f    g NA's 
   1    1    1    1  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
 1.5 1.86    2 NA's 
   1    1    2  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   4  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  10 13.3 NA's 
   1    3  500 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 NA's 
   4  500 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.210e+08 1.283e+08 5.162e+08 1.062e+09 1.076e+09       437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   1   32   34  437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1400  0.1700  0.3139  0.5000  0.5000     437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    9.00    9.00   14.04   22.00   30.00     437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  67  437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.657   7.500  12.000     437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   5.881   7.000  11.000     437 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.73    2.80    3.40   20.58   40.00   40.00     437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
  63    4  437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
13.3 13.5   20   80 NA's 
   1    1   33   32  437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 NA's 
  20   43    4  437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   45.35  100.50  117.59  181.50  256.00     441 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                  32                   35                  437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r    w NA's 
  30    5   32  437 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1400  0.1600  0.3021  0.5000  0.5000     441 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  32  472 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
  28    4  472 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              67 
                                            NA's 
                                             437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  67  437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                        5                        30                        28 
                repli-g x                      NA's 
                        4                       437 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.970   1.190   1.340   1.364   1.510   2.000     469 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    7.35    7.80    7.90    8.40    9.10     469 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
       26        37       441 

patient.samples.sample.3.portions.portion.analytes.analyte.3.a260_a280_ratio:
 1.6  1.7 NA's 
   1    4  499 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.210e+08 1.252e+08 3.648e+08 5.931e+08 1.076e+09       473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7    8 NA's 
  24    7  473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.2248  0.1600  0.5000     473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    9.00    9.00   12.32   16.50   30.00     473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  31  473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.903   8.000  12.000     473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   5.000   5.355   6.000  12.000     473 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.73    1.86    2.00   10.52    2.13   40.00     473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
  27    4  473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  13 13.3   80 NA's 
   1   23    7  473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 2013 NA's 
  12   15    4  473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.20   31.20   55.20   64.91   74.08  187.00     476 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   7                   24                  473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   r    w NA's 
  24    7  473 



patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1400  0.1954  0.1600  0.5000     476 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   7  497 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
   4    3  497 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              31 
                                            NA's 
                                             473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  31  473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g              repli-g x 
                    24                      4                      3 
                  NA's 
                   473 

patient.samples.sample.3.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.070   1.167   1.205   1.263   1.357   1.690     480 

patient.samples.sample.3.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.100   7.300   7.450   7.658   7.925   9.100     480 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
        3        25       476 



patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.210e+08 1.283e+08 4.489e+08 1.054e+09 1.076e+09       481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8 NA's 
  23  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
  23  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    7.00   11.00   16.65   29.00   30.00     481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  23  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   7.000   6.043   8.000  12.000     481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     5.5     6.0     6.0     7.0     9.0     481 



patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
  23  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22   23 NA's 
  22    1  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
  23  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2011 2012 2013 NA's 
   9   13    1  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  143.0   164.0   175.5   181.0   188.2   255.0     482 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                  23                  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
  23  481 



patient.samples.sample.3.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
  22  482 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
  23  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no  yes NA's 
  22    1  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              23 
                                            NA's 
                                             481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
  23  481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
       22         1       481 

patient.samples.sample.3.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen   uv spec      NA's 
       20         2       482 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9 NA's 
   1   14  489 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.210e+08 1.252e+08 4.939e+08 1.056e+09 1.076e+09       436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
  17   51  436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0751  0.0700  0.1700     436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0     9.0    10.0    13.1    18.0    30.0     436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  68  436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.265   9.000  12.000     436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   5.000   5.529   7.000   9.000     436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1450    2017    2122    2109    2293    2395     436 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.533   3.000   2.501   3.000   3.000     436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
  68  436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67   60 NA's 
  17   51  436 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 NA's 
   3   23   42  436 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.456e+08 1.054e+09 7.435e+08 1.054e+09 1.054e+09       483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8 NA's 
   5   16  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0786  0.0800  0.1500     483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   10.00   14.00   13.57   15.00   30.00     483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  21  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   8.000   7.381   8.000  12.000     483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   5.619   8.000   8.000     483 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.220   3.000   3.000   2.865   3.000   4.010     483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
  17    4  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
17.4 26.7   40   60 NA's 
   4    5    1   11  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2013 NA's 
   3   14    4  483 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.070e+08 1.136e+08 2.758e+08 1.136e+08 1.053e+09       487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8 NA's 
  12    5  487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1500  0.1218  0.1500  0.1700     487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   10.00   10.00   12.29   15.00   30.00     487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  17  487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   2    3    6    8 NA's 
   5    1    4    7  487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   5.000   5.353   7.000   8.000     487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1458    1553    1818    1766    1947    1947     487 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   2.000   2.000   2.139   2.000   4.270     487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
  17  487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
13.3 26.7   40 NA's 
  11    1    5  487 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 NA's 
  17  487 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.040e+08 1.040e+08 4.824e+08 1.050e+09 1.050e+09       499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   9 NA's 
   5  499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.05 NA's 
   5  499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   7   14 NA's 
   3    2  499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   5  499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2 NA's 
   5  499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    2    7 NA's 
   1    2    2  499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
1460 1555 NA's 
   3    2  499 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   2 NA's 
   5  499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   5  499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
  40 NA's 
   5  499 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 NA's 
   5  499 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.210e+08 1.252e+08 4.800e+08 1.054e+09 1.076e+09       433 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    8 NA's 
  51   17    3  433 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1400  0.1500  0.1458  0.1550  0.1700     433 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    9.00   10.00   12.93   18.00   30.00     433 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
  68    3  433 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.169   9.000  12.000     433 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.50    5.00    5.38    6.50    9.00     433 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   0.930   1.000   1.181   1.400   2.130     433 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
  68    3  433 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67   10 13.3   20 NA's 
  51   11    6    3  433 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2014 NA's 
   3   23   42    3  433 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.23   89.22  184.63  208.30  300.17  651.43     433 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  71  433 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  71  433 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.140   0.150   0.149   0.160   0.170     433 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  71  433 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
  68    3  433 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              71 
                                            NA's 
                                             433 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no  yes NA's 
  68    3  433 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
        adna preparation type              allprep ffpe dna 
                           67                             3 
chemical lysis dna extraction                          NA's 
                            1                           433 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       56        15       433 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00   11.00   11.39   16.00   31.00     433 

patient.samples.sample.3.portions.portion.is_ffpe:
  no  yes NA's 
  68    3  433 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.239   8.000  12.000     433 

patient.samples.sample.3.portions.portion.portion_number:
   1 NA's 
  71  433 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-43-3920-11b-01-ts1 tcga-51-4079-11a-01-ts1 tcga-51-4080-11a-01-ts1 
                      1                       1                       1 
tcga-51-4081-11a-01-ts1                    NA's 
                      1                     500 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_uuid:
10ecf62d-d270-4d74-a86d-dcc13ff09549 530df10b-7a9c-40a0-ae9d-bd2798e3b9dd 
                                   1                                    1 
7f5d50a8-d127-437d-a3ff-f4bd50498698 8a22d88d-afcc-4a61-b267-780d5d738f36 
                                   1                                    1 
                                NA's 
                                 500 

patient.samples.sample.3.portions.portion.slides.slide.2.image_file_name:
tcga-51-4079-11a-01-ts1.8a22d88d-afcc-4a61-b267-780d5d738f36.svs 
                                                               1 
tcga-51-4080-11a-01-ts1.530df10b-7a9c-40a0-ae9d-bd2798e3b9dd.svs 
                                                               1 
tcga-51-4081-11a-01-ts1.10ecf62d-d270-4d74-a86d-dcc13ff09549.svs 
                                                               1 
                                                            NA's 
                                                             501 

patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   4  500 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location:
 top NA's 
   4  500 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  67  437 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.25    5.00   10.79   14.25   35.00     490 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   10.93   17.50   25.00     490 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   1  503 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1   10   30 NA's 
   8    1    1    1  493 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
  18 NA's 
   1  503 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   6 NA's 
   1  503 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
  76 NA's 
   1  503 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
  65 NA's 
   1  503 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     4     63    437 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  29.00   60.00   63.00   65.36   70.00  136.00     438 

patient.samples.sample.3.portions.portion.year_of_creation:
2010 2011 2012 2014 NA's 
   5   32   31    3  433 

patient.samples.sample.3.sample_type:
blood derived normal         ffpe scrolls  solid tissue normal 
                   1                    3                   67 
                NA's 
                 433 

patient.samples.sample.3.sample_type_id:
   1   10   11 NA's 
   3    1   67  433 

patient.samples.sample.3.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.5000  0.5209  0.7000  1.0000     437 


patient.samples.sample.4.bcr_sample_barcode:
tcga-92-7340-10a tcga-92-8063-10a tcga-92-8064-10a             NA's 
               1                1                1              501 

patient.samples.sample.4.bcr_sample_uuid:
264e40a5-7514-43e9-ad8a-b141bfdeb5c6 7b686ccf-007b-40d3-a971-674c74cb5589 
                                   1                                    1 
b380d9b9-5562-400e-9a14-e7817d7c5ae4                                 NA's 
                                   1                                  501 

patient.samples.sample.4.is_ffpe:
  no NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-92-7340-10a-01w-2071-08 tcga-92-8063-10a-01w-2273-08 
                           1                            1 
tcga-92-8064-10a-01w-2273-08                         NA's 
                           1                          501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
4c714f7e-dd6e-4a32-8546-46b36e3f0f09 511de6c7-2ce3-4c9b-900c-8c6ad12323f6 
                                   1                                    1 
ba77fc69-d0e2-42e7-8b8b-afbf51ebe2d9                                 NA's 
                                   1                                  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
120997744 128287153 128287174      NA's 
        1         1         1       501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   8 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
 0.5 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   5    7 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   5   12 NA's 
   2    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   8   11 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
2071 2273 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   e    f    g NA's 
   1    1    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
  40 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  80 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.amount:
 175  211  219 NA's 
   1    1    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                 NA's 
                   3                  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   w NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-92-7340-10a-01w tcga-92-8063-10a-01w tcga-92-8064-10a-01w 
                   1                    1                    1 
                NA's 
                 501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
37e8a7c7-89bc-4298-9205-bb966ce4024a 60392cc4-674f-4848-9eac-f42e5b43b7d7 
                                   1                                    1 
a8243d0f-c83c-4920-8714-b4373bb196a2                                 NA's 
                                   1                                  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.concentration:
 0.5 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      3     501 

patient.samples.sample.4.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen      NA's 
        3       501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-92-7340-10a-01d-2042-08 tcga-92-8063-10a-01d-2244-08 
                           1                            1 
tcga-92-8064-10a-01d-2244-08                         NA's 
                           1                          501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
2abb7847-a772-4680-bbef-af095e97aa0b 3f377ed6-adf8-4874-8dc1-0a52df72f6d0 
                                   1                                    1 
58ddfe29-d764-4b83-ac40-7221a934a706                                 NA's 
                                   1                                  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
 125245790  125245809 1061605281       NA's 
         1          1          1        501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   8 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.05 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   9   18 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   4   11 NA's 
   2    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   7   10 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
2042 2244 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   b    c    g NA's 
   1    1    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   3 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  60 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-92-7340-10a-01d-2041-01 tcga-92-8063-10a-01d-2243-01 
                           1                            1 
tcga-92-8064-10a-01d-2243-01                         NA's 
                           1                          501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
02b393aa-53b0-4f5e-b099-bdbd9d491430 bf629958-9a7b-4c18-97af-6a51567e0602 
                                   1                                    1 
d5728d57-0380-4a40-9096-7142cf71adc0                                 NA's 
                                   1                                  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
 125245886  125245905 1061605185       NA's 
         1          1          1        501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.15 0.16 NA's 
   2    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   9   18 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   4   11 NA's 
   2    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   7   10 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
2041 2243 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   b    c    g NA's 
   1    1    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   1 1.07 NA's 
   2    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67 NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
145.63 245.63    337   NA's 
     1      1      1    501 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-92-7340-10a-01d tcga-92-8063-10a-01d tcga-92-8064-10a-01d 
                   1                    1                    1 
                NA's 
                 501 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid:
10c035d9-9183-4d69-b810-af873f7f5bac 26aabae5-e270-4a50-be95-12f7763b11fe 
                                   1                                    1 
712f7eea-ef84-45a7-a9ba-42ce45dfdb15                                 NA's 
                                   1                                  501 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
0.15 0.16 NA's 
   2    1  501 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             501 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   3  501 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
chemical lysis dna extraction                          NA's 
                            3                           501 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        3       501 

patient.samples.sample.4.portions.portion.bcr_portion_barcode:
tcga-92-7340-10a-01 tcga-92-8063-10a-01 tcga-92-8064-10a-01                NA's 
                  1                   1                   1                 501 

patient.samples.sample.4.portions.portion.bcr_portion_uuid:
0d392a3f-e078-44fc-874e-c26a399ca485 55541337-0de2-46cd-8dae-ee2037273d11 
                                   1                                    1 
d0c346fe-ef6a-4cfa-b736-e5c4db0da8f9                                 NA's 
                                   1                                  501 

patient.samples.sample.4.portions.portion.day_of_creation:
   6   16 NA's 
   1    2  501 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   3  501 

patient.samples.sample.4.portions.portion.month_of_creation:
   3   10 NA's 
   2    1  501 

patient.samples.sample.4.portions.portion.portion_number:
   1 NA's 
   3  501 

patient.samples.sample.4.portions.portion.year_of_creation:
2011 2012 NA's 
   1    2  501 

patient.samples.sample.4.sample_type:
blood derived normal                 NA's 
                   3                  501 

patient.samples.sample.4.sample_type_id:
  10 NA's 
   3  501 

patient.samples.sample.4.vial_number:
   a NA's 
   3  501 

patient.samples.sample.5.bcr_sample_barcode:
tcga-92-7340-11a             NA's 
               1              503 

patient.samples.sample.5.bcr_sample_uuid:
0050cf50-b69b-4806-966c-129ca63212ed                                 NA's 
                                   1                                  503 

patient.samples.sample.5.intermediate_dimension:
 1.1 NA's 
   1  503 

patient.samples.sample.5.is_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.longest_dimension:
 1.5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-92-7340-11a-01h-2044-13                         NA's 
                           1                          503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
f549929c-9d21-4c28-8478-31ecb00eb9e4                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
1061605884       NA's 
         1        503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  13 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.13 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   9 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
  11 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
2044 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   a NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
 2.6 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.amount:
45.5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.analyte_type_id:
   h NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-92-7340-11a-01h                 NA's 
                   1                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
4caa98e3-1a07-4969-86fb-4ca3d22331d4                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.concentration:
0.13 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        1                       503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
1.58 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.rna.rinvalue:
   9 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      1     503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-92-7340-11a-01r-2045-07                         NA's 
                           1                          503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
3a0a356b-ee3a-4084-81d7-04b74a62dd96                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
1061604934       NA's 
         1        503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
0.14 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   9 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
  11 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
2045 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   b NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
1.86 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
13.3 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.amount:
54.2 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.analyte_type:
 rna NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.analyte_type_id:
   r NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-92-7340-11a-01r                 NA's 
                   1                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
21f9150f-54f0-4eca-8705-8737dcd4c44e                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.concentration:
0.14 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                   NA's 
                     1                    503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
 1.5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.rna.rinvalue:
 8.8 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      1     503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-92-7340-11a-01w-2071-08                         NA's 
                           1                          503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
9a5e8fb5-49e5-4d98-a9a3-8b73e2c13aa3                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
120997757      NA's 
        1       503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
  12 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   6 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
2071 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   f NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.amount:
 191 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.bcr_analyte_barcode:
tcga-92-7340-11a-01w                 NA's 
                   1                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.bcr_analyte_uuid:
017084ac-d886-4399-aa31-714e880fa208                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     503 

patient.samples.sample.5.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen      NA's 
        1       503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-92-7340-11a-01d-2042-08                         NA's 
                           1                          503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
1f895c9b-ac4e-4aa9-905a-e7b36d8ad6f5                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
1061605294       NA's 
         1        503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   8 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.05 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   9 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
  11 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
2042 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   h NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   3 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  60 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-92-7340-11a-01d-2041-01                         NA's 
                           1                          503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
8c8a9742-6f82-4b5a-8410-d41ba1a55fe8                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
1061605198       NA's 
         1        503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.15 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   9 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
  11 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   5 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
2041 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   h NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   1 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.amount:
 261 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-92-7340-11a-01d                 NA's 
                   1                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.bcr_analyte_uuid:
d4063b3b-679b-43ba-99ee-4feb50bbfe3b                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.analytes.analyte.concentration:
0.15 NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             503 

patient.samples.sample.5.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    1                   503 

patient.samples.sample.5.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        1       503 

patient.samples.sample.5.portions.portion.bcr_portion_barcode:
tcga-92-7340-11a-01                NA's 
                  1                 503 

patient.samples.sample.5.portions.portion.bcr_portion_uuid:
f2d588c0-95f2-4013-ae53-ac13fb7cab63                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.day_of_creation:
   4 NA's 
   1  503 

patient.samples.sample.5.portions.portion.is_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.month_of_creation:
  10 NA's 
   1  503 

patient.samples.sample.5.portions.portion.portion_number:
   1 NA's 
   1  503 

patient.samples.sample.5.portions.portion.slides.slide.bcr_slide_barcode:
tcga-92-7340-11a-01-ts1                    NA's 
                      1                     503 

patient.samples.sample.5.portions.portion.slides.slide.bcr_slide_uuid:
51562ff0-25ee-4210-baf0-75543e5f1fb8                                 NA's 
                                   1                                  503 

patient.samples.sample.5.portions.portion.slides.slide.image_file_name:
tcga-92-7340-11a-01-ts1.51562ff0-25ee-4210-baf0-75543e5f1fb8.svs 
                                                               1 
                                                            NA's 
                                                             503 

patient.samples.sample.5.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.5.portions.portion.slides.slide.section_location:
 top NA's 
   1  503 

patient.samples.sample.5.portions.portion.weight:
  61 NA's 
   1  503 

patient.samples.sample.5.portions.portion.year_of_creation:
2011 NA's 
   1  503 

patient.samples.sample.5.sample_type:
solid tissue normal                NA's 
                  1                 503 

patient.samples.sample.5.sample_type_id:
  11 NA's 
   1  503 

patient.samples.sample.5.shortest_dimension:
 0.3 NA's 
   1  503 

patient.samples.sample.5.vial_number:
   a NA's 
   1  503 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   100.5   301.0   909.4  1331.5  5727.0     393 






patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
2bcc33c0-31f2-4354-b954-64bd646f40df 4eef2215-0a0e-478d-81be-207d566e8788 
                                   1                                    1 
631ec263-2df0-4d13-982b-14db382a2532                                 NA's 
                                   1                                  501 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
4a9f5939-4299-4648-8f62-2a0909e8d3bd 91be6f90-d9ab-4345-a3bd-91805d9761b9 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.8:
ec0d10ef-ec0e-4ad2-81fb-907754694e42                                 NA's 
                                   1                                  503 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
f05eb71e-b511-4326-9097-5ca191f0100e                                 NA's 
                                   1                                  503 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   100.0   165.0   280.3   295.0  2200.0     376 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.10    0.60    0.80    0.82    1.00    2.20     129 

patient.samples.sample.is_ffpe:
 no 
504 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.200   0.900   1.100   1.241   1.500   3.300     129 

patient.samples.sample.oct_embedded:
false  true  NA's 
   46    82   376 



patient.samples.sample.portions.portion.2.analytes.analyte.2.a260_a280_ratio:
 1.8 1.86 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-21-1076-01a-02r-0691-03                         NA's 
                           1                          503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
43c712dd-0e01-4399-b596-1072d3a25138                                 NA's 
                                   1                                  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.center_id:
   3 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  18 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
  11 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_id:
 691 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   f NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.quantity:
2.51 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.volume:
16.7 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2009 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-21-1076-01a-02r-0692-07                         NA's 
                           1                          503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
db2c54e8-2023-49de-a116-7bec77297dee                                 NA's 
                                   1                                  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  18 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  11 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.plate_id:
 692 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   f NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.quantity:
   2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.volume:
13.3 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2009 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-21-1076-01a-02r-0690-01 tcga-98-a53d-01a-32h-a263-13 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
154d17eb-4c3d-42be-b3da-6afd44a2368d cc27ae96-52c5-452a-9028-6c8df7f2eb19 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
128923924      NA's 
        1       503 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.center_id:
   1   13 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.16 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  18   30 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   1   11 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_column:
   1    5 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_id:
0690 a263 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_row:
   d    f NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.quantity:
   3  3.2 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.volume:
  20 NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2009 2013 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.amount:
14.16   310  NA's 
    1     1   502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type:
 rna NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type_id:
   h    r NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.bcr_analyte_barcode:
tcga-21-1076-01a-02r tcga-98-a53d-01a-32h                 NA's 
                   1                    1                  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.bcr_analyte_uuid:
a6281888-449e-4990-a8ff-2a504568ed62 cde14f25-c212-4244-b55b-b619d702e319 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.concentration:
0.15 0.16 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                      NA's 
                        1                         1                       502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.ratio_28s_18s:
1.16  1.8 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.rinvalue:
 7.2  8.5 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      2     502 

patient.samples.sample.portions.portion.2.analytes.analyte.2.subportion_sequence:
   2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.3.a260_a280_ratio:
 1.9 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-21-1076-01a-02t-0693-07 tcga-98-a53d-01a-32r-a262-07 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
32618f86-4f65-4370-8339-5faad4fa59d5 87e388aa-7db3-449e-b5d6-8f70f3eb6a5c 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
132159124      NA's 
        1       503 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.center_id:
   7 NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1 0.17 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  18   30 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   1   11 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_column:
   1    5 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_id:
0693 a262 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_row:
   d    f NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.quantity:
   1 2.21 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.volume:
  10   13 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2009 2013 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.amount:
12.48  76.5  NA's 
    1     1   502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type:
      rna total rna      NA's 
        1         1       502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type_id:
   r    t NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.bcr_analyte_barcode:
tcga-21-1076-01a-02t tcga-98-a53d-01a-32r                 NA's 
                   1                    1                  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.bcr_analyte_uuid:
19ae9619-973f-4f5c-8827-c653b5ba4a12 3df11d7e-0bdd-4757-b4e2-66bd5e8ef073 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.concentration:
 0.1 0.17 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction              total rna                   NA's 
                     1                      1                    502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.ratio_28s_18s:
0.96 1.33 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.rinvalue:
 6.9  7.1 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      2     502 

patient.samples.sample.portions.portion.2.analytes.analyte.3.subportion_sequence:
   2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-21-1076-01a-02w-0782-08                         NA's 
                           1                          503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_uuid:
3f5781a0-64dd-47f2-8d78-2758ba6c9887                                 NA's 
                                   1                                  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
89575998     NA's 
       1      503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  13 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 782 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_row:
   g NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.quantity:
  30 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.volume:
  60 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-21-1076-01a-02w-0783-09                         NA's 
                           1                          503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
711e9121-29d4-49fa-b9f6-88e458266c10                                 NA's 
                                   1                                  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.biospecimen_barcode_bottom:
89576382     NA's 
       1      503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  13 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 783 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   g NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.quantity:
  30 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.volume:
  60 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2010 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-21-1076-01a-02w-0781-10 tcga-98-a53d-01a-32w-a30p-08 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
517aa73c-b12f-43fb-afa1-9cdb37519251 f94959c7-d2a8-478d-9407-8aaea6c0217e 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
150704842      NA's 
        1       503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.center_id:
   8   10 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  12   13 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   1    6 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_column:
   1   11 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_id:
0781 a30p NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_row:
   g    h NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.quantity:
  30   40 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.source_center:
  22   23 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.volume:
  60   80 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 2013 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.amount:
  20 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type_id:
   w NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_barcode:
tcga-21-1076-01a-02w tcga-98-a53d-01a-32w                 NA's 
                   1                    1                  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_uuid:
4689f3ba-cd64-40ef-ae21-c2c4a82ffef6 516f4c60-f155-4045-9b2b-b7105cc7da8d 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.concentration:
 0.5 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.pcr_amplification_successful:
  no  yes NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     502 

patient.samples.sample.portions.portion.2.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      1     503 

patient.samples.sample.portions.portion.2.analytes.analyte.4.subportion_sequence:
   2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio:
 1.9 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-21-1076-01a-02d-0686-04 tcga-98-a53d-01a-32d-a26m-08 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
756ce59f-b325-4240-a48f-8880d0f9e167 cfaf3e78-a98b-435d-82a0-bc7ce8fb624e 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
145676170      NA's 
        1       503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   4    8 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.08 0.15 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  18   27 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   2   11 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   1   11 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
0686 a26m NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   f    h NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
 3.2 4.01 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
26.7   40 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2009 2013 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-21-1076-01a-02d-0687-06 tcga-98-a53d-01a-32d-a26n-05 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
6c7807d1-1344-4234-8ceb-b105c905ff9a f7e5bed6-9d82-48cb-9b8b-73cade6abd3a 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
145673290      NA's 
        1       503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
   5    6 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
0.15 0.16 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  18   27 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   2   11 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   1   11 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
0687 a26n NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_row:
   f    h NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
   2 2.08 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
  13 13.3 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2009 2013 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-21-1076-01a-02d-0688-02                         NA's 
                           1                          503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
6e302eeb-d744-49e5-88ce-958746c6cdfd                                 NA's 
                                   1                                  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id:
   2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration:
0.15 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  18 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
  11 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id:
 688 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_row:
   f NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity:
   3 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume:
  20 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2009 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-21-1076-01a-02d-0689-05                         NA's 
                           1                          503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
7b5acacf-c56b-4753-82f1-cbaf43b13ad4                                 NA's 
                                   1                                  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.center_id:
   5 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.concentration:
0.15 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  18 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
  11 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_column:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_id:
 689 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_row:
   f NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.quantity:
   2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.volume:
13.3 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2009 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-21-1076-01a-02d-1521-08                         NA's 
                           1                          503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
504d4cb0-d2dd-420d-82e6-9ec14434a0fc                                 NA's 
                                   1                                  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
1049950102       NA's 
         1        503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.center_id:
   8 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.concentration:
0.05 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  31 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.plate_column:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.plate_id:
1521 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.plate_row:
   d NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.quantity:
   2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.volume:
  40 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-21-1076-01a-02d-2250-01                         NA's 
                           1                          503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
f02b859d-022d-4df0-a5c7-58ebe45c1996                                 NA's 
                                   1                                  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
125245941      NA's 
        1       503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.center_id:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.concentration:
0.15 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  29 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   8 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_column:
   1 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_id:
2250 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_row:
   a NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.quantity:
 1.2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.volume:
   8 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2012 NA's 
   1  503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-21-1076-01a-02d-0685-01 tcga-98-a53d-01a-32d-a26l-01 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
0dce0e07-e8cb-424c-b5c5-f9249737135e ed150d83-2f82-4f89-b0d7-1500e471e4a4 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
145673482      NA's 
        1       503 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.15 0.16 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  18   27 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   2   11 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   1   11 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
0685 a26l NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   f    h NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
1.01 1.07 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2009 2013 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.amount:
19.67 423.7  NA's 
    1     1   502 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
 dna NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   d NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.bcr_analyte_barcode:
tcga-21-1076-01a-02d tcga-98-a53d-01a-32d                 NA's 
                   1                    1                  502 

patient.samples.sample.portions.portion.2.analytes.analyte.bcr_analyte_uuid:
924e8649-bbd1-449f-a342-856a57900cca dab4024e-a9a4-4972-b7c3-14302d957cca 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.analytes.analyte.concentration:
0.15 0.16 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             502 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    2                   502 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
        1         1       502 

patient.samples.sample.portions.portion.2.analytes.analyte.subportion_sequence:
   2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.bcr_portion_barcode:
tcga-21-1076-01a-02 tcga-98-a53d-01a-32                NA's 
                  1                   1                 502 

patient.samples.sample.portions.portion.2.bcr_portion_uuid:
bc4e974e-1557-4f01-a98e-c7a4ba634446 bf423cb4-794c-4b0e-b2b8-82bf7e92d927 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.day_of_creation:
   4    5 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.month_of_creation:
   2   12 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.portion_number:
   2   32 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.portion_sequence:
   3 NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_barcode:
tcga-21-1076-01a-02-bs2                    NA's 
                      1                     503 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_uuid:
c8953346-e558-46f7-aeef-792a4d2b448f                                 NA's 
                                   1                                  503 

patient.samples.sample.portions.portion.2.slides.slide.2.image_file_name:
tcga-21-1076-01a-02-bs2.c8953346-e558-46f7-aeef-792a4d2b448f.svs 
                                                               1 
                                                            NA's 
                                                             503 

patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location:
bottom   NA's 
     1    503 

patient.samples.sample.portions.portion.2.slides.slide.bcr_slide_barcode:
tcga-21-1076-01a-01-bs1 tcga-98-a53d-01a-03-ts3                    NA's 
                      1                       1                     502 

patient.samples.sample.portions.portion.2.slides.slide.bcr_slide_uuid:
7c3e12f1-2db9-464d-afc3-5a902abdaa00 aed40d68-da68-4be6-8170-02d7d5e3ea34 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.2.slides.slide.image_file_name:
tcga-21-1076-01a-01-bs1.7c3e12f1-2db9-464d-afc3-5a902abdaa00.svs 
                                                               1 
tcga-98-a53d-01a-03-ts3.aed40d68-da68-4be6-8170-02d7d5e3ea34.svs 
                                                               1 
                                                            NA's 
                                                             502 

patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   4 NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   2 NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   0 NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   3 NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   0 NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
  30 NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
  70 NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
  70 NA's 
   1  503 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
 top NA's 
   2  502 

patient.samples.sample.portions.portion.2.weight:
  20  111 NA's 
   1    1  502 

patient.samples.sample.portions.portion.2.year_of_creation:
2009 2012 NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.60    1.80    1.80    1.82    1.86    2.11     144 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96504404  96504823  98887256  98147378  98887408 100504261       392 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   3    7 NA's 
 123   11  370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0900  0.1400  0.1500  0.1481  0.1500  0.1700     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   10.00   12.18   18.00   30.00     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 134  370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   6.000   6.119   8.000  11.000     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.672   4.000   6.000     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  691.0   850.0   850.0   939.8  1030.0  1200.0     370 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   2.340   2.510   2.434   2.510   2.840     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
 134  370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3 16.7 NA's 
  11  123  370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2009 2010 NA's 
  22  112  370 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96504459  96504540  98884144  98149872  98889327 100518757       392 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   1    7 NA's 
  47   87  370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0900  0.1400  0.1500  0.1481  0.1500  0.1700     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   10.00   12.18   18.00   30.00     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 134  370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   6.000   6.119   8.000  11.000     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.672   4.000   6.000     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  692.0   851.0   851.0   940.8  1031.0  1201.0     370 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.790   2.000   2.000   2.307   2.800   3.400     370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
 134  370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 8.8 13.3   20 NA's 
   1   87   46  370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2009 2010 NA's 
  22  112  370 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-22-1000-01a-01r-a32z-07 tcga-43-2576-01a-01r-a32z-07 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
62710cd0-82b4-4746-ad7c-b320d933a542 ac4077ca-1cc5-4df5-b621-94e2cb8ecd0f 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
150757380 150757423      NA's 
        1         1       502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   7 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.14 0.15 NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
   2 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
  10 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   2    6 NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
a32z NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   c    f NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
1.82 1.95 NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  23 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
  13 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2013 NA's 
   2  502 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.040e+08 1.210e+08 1.978e+08 1.456e+08 1.076e+09        24 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   1    3    7   13 NA's 
  87   11   85  319    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0900  0.1500  0.1500  0.1534  0.1600  0.1800       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   10.00   15.00   17.55   27.00   30.00       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 502    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.482   8.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    3.59    5.00   12.00       2 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   3.200   3.215   3.400   4.540       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 375  127    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
13.3 16.7   20 26.7 NA's 
   2   11  406   83    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2011    2011    2012    2014       2 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    34.5   127.2   166.7   249.0   979.0       2 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
 502    2 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r NA's 
 319  183    2 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0900  0.1500  0.1500  0.1534  0.1600  0.1800       2 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             502 
                                            NA's 
                                               2 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 502    2 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                      NA's 
                      183                       319                         2 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.350   1.500   1.532   1.690   6.400       2 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   7.800   8.500   8.429   9.000  10.000       2 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    502       2 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
 305   14  185 

patient.samples.sample.portions.portion.analytes.analyte.3.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.440   1.800   1.840   1.829   1.890   2.130     327 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 9.885e+07 1.050e+09 7.499e+08 1.050e+09 1.050e+09       390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   7   13 NA's 
  36   78  390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.1 0.14 NA's 
 113    1  390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   2   14   26   30 NA's 
  35   61   17    1  390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 114  390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   1    2    4    6 NA's 
  17   61    1   35  390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.228   6.000  12.000     390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1032 1517 1557 a28v NA's 
  35   17   61    1  390 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.280   1.000   3.000   2.361   3.000   3.000     390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22   23 NA's 
 113    1  390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.80   10.00   30.00   23.56   30.00   30.00     390 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2011 2013 NA's 
  35   78    1  390 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.040e+08 1.210e+08 1.979e+08 1.456e+08 1.076e+09        24 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7   13 NA's 
 417   85    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.100   0.100   0.140   0.136   0.160   0.180       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   10.00   15.00   17.42   27.00   30.00       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 502    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.482   8.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.697   5.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.860   2.000   2.019   2.340   4.010       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
 375  127    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   13.00   13.30   15.42   13.30   30.00       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2011    2011    2012    2014       2 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   27.62   66.00   96.88  143.12  529.00       2 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
      rna total rna      NA's 
      319       183         2 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   r    t NA's 
 319  183    2 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.100   0.100   0.140   0.136   0.160   0.180       2 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             502 
                                            NA's 
                                               2 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 502    2 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction              total rna                   NA's 
                   319                    183                      2 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.250   1.420   1.402   1.600   2.380      34 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.600   7.600   8.200   8.153   8.700  10.000      34 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
    502       2 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
 305   14  185 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89575996  96491650  96491716  96556523  96492276 103960882       370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8    9 NA's 
  22  112  370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
 134  370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   19.00   27.00   24.12   31.00   31.00     370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 134  370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   4.261   5.000   9.000     370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.784   5.750   8.000     370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  782.0   879.0   879.0   946.2   939.0  1262.0     370 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
  30   40 NA's 
  69   65  370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
 134  370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  60   80 NA's 
  69   65  370 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
 134  370 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89576336  89576434  96485712  95961575  96485740 153878583       433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
   8    9   10 NA's 
   2   22   47  433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
  71  433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  13   15   31 NA's 
  22    2   47  433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  71  433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   1    3    7 NA's 
  22   47    2  433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.422   5.000   7.000     433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
0783 0881 a315 NA's 
  22   47    2  433 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
   5   30 NA's 
   2   69  433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22   23 NA's 
  69    2  433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
  10   60 NA's 
   2   69  433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2010 2013 NA's 
  69    2  433 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-56-1622-01a-01w-a315-08 tcga-60-2713-01a-01w-a315-08 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_uuid:
3fdccaf2-39c6-44d8-a71c-65cbe288c90e ece6a80c-da2c-452b-9746-584fb2ee6258 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.biospecimen_barcode_bottom:
153878559 153878582      NA's 
        1         1       502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.center_id:
   8 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
  15 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   7 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   1 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_id:
a315 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_row:
   a    c NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.quantity:
   5 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.source_center:
  23 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.volume:
  10 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2013 NA's 
   2  502 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.087e+08 1.322e+08 3.305e+08 1.507e+08 1.076e+09        35 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8   10 NA's 
 469   22   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
 491   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     8.0    13.0    17.1    27.0    31.0      13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 491   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.921   6.000  12.000      13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.916   5.000  12.000      13 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  30   40 NA's 
  69  422   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22   23 NA's 
 359  132   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  60   80 NA's 
  69  422   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 NA's 
 134  119  106  132   13 

patient.samples.sample.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   18.0    70.0   163.0   139.1   183.0   256.0     145 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                 491                   13 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
 491   13 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
 359  145 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
 491   13 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no  yes NA's 
 359  132   13 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             491 
                                            NA's 
                                              13 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 491   13 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
      475        16        13 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen   uv spec      NA's 
      147       212       145 

patient.samples.sample.portions.portion.analytes.analyte.4.subportion_sequence:
   1    2 NA's 
 298   10  196 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97543256  97543287  97543328  99549210 103964701 103964773       439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id:
   9 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration:
 0.5 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment:
  19   27   29 NA's 
  36   18   11  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment:
   5    6    9 NA's 
  36   11   18  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.108   7.000   8.000     439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id:
 941 1070 1264 NA's 
  36   11   18  439 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity:
  40 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center:
  22 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume:
  80 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  65  439 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97543859  97544150  97544191  99549789 103963550 103964594       439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   8 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
  19   27   29 NA's 
  36   18   11  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
   5    6    9 NA's 
  36   11   18  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.108   7.000   8.000     439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_id:
 940 1068 1263 NA's 
  36   11   18  439 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
  40 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  22 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
  80 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2010 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  44.00   63.00   76.00   77.72   94.00  120.00     439 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type:
repli-g x (qiagen) dna                   NA's 
                    65                    439 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type_id:
   x NA's 
  65  439 



patient.samples.sample.portions.portion.analytes.analyte.5.concentration:
 0.5 NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful:
  no NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              65 
                                            NA's 
                                             439 

patient.samples.sample.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
  65  439 

patient.samples.sample.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
       65       439 

patient.samples.sample.portions.portion.analytes.analyte.5.spectrophotometer_method:
uv spec    NA's 
     65     439 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9    2 NA's 
   7  212    1  284 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.252e+08 1.270e+08 5.978e+08 5.915e+08 1.053e+09 1.054e+09       494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
   1    5    8 NA's 
   4    4    2  494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.070   0.140   0.150   0.136   0.150   0.170     494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   14.00   19.00   18.70   27.25   29.00     494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
  10  494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   2    7    8 NA's 
   2    2    6  494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.25    3.00    3.00    3.75    6.00     494 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.070   1.200   1.305   3.626   3.942  11.320     494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  22   23 NA's 
   8    2  494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    8.00   17.40   25.21   26.70   66.60     494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2011 2012 2013 NA's 
   5    3    2  494 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_barcode:
tcga-43-3394-01a-01d-1553-08 tcga-43-3920-01a-01d-1553-08 
                           1                            1 
tcga-60-2695-01a-01d-2250-01 tcga-60-2722-01a-01d-2250-01 
                           1                            1 
                        NA's 
                         500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_uuid:
204fcc80-b946-4aa0-b519-9f590abad7d6 a762dd44-a8ac-4269-8163-bbf6d0f8d4b3 
                                   1                                    1 
efe4d5dd-fffb-41df-ab86-be06cc16646f fc4f2de1-fa57-4785-aa8e-ce52e1d21d52 
                                   1                                    1 
                                NA's 
                                 500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.biospecimen_barcode_bottom:
 125245958  125246004 1049985575 1049985576       NA's 
         1          1          1          1        500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.center_id:
   1    8 NA's 
   2    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.concentration:
0.05 0.15 0.17 NA's 
   2    1    1  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.day_of_shipment:
  14   29 NA's 
   2    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.is_derived_from_ffpe:
  no NA's 
   4  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.month_of_shipment:
   2    8 NA's 
   2    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_column:
   1    2    3 NA's 
   2    1    1  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_id:
1553 2250 NA's 
   2    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_row:
   a    d    e NA's 
   1    1    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.quantity:
 1.2 1.36    2 NA's 
   1    1    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.source_center:
  22 NA's 
   4  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.volume:
   8   40 NA's 
   2    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.year_of_shipment:
2011 2012 NA's 
   2    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.bcr_aliquot_barcode:
tcga-43-3394-01a-01d-1555-09 tcga-43-3920-01a-01d-1555-09 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.bcr_aliquot_uuid:
4b4b5d71-8625-4ef5-950f-2877bbd9a98f a91ba9a3-efc3-41e3-9074-90442e21e2cc 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.biospecimen_barcode_bottom:
1049985958 1049985959       NA's 
         1          1        502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.center_id:
   9 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.concentration:
0.05 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.day_of_shipment:
  14 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.month_of_shipment:
   2 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.plate_column:
   1 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.plate_id:
1555 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.plate_row:
   d    e NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.quantity:
   2 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.source_center:
  22 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.volume:
  40 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.year_of_shipment:
2011 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.bcr_aliquot_barcode:
tcga-43-3394-01a-01d-2250-01 tcga-43-3920-01a-01d-2250-01 
                           1                            1 
                        NA's 
                         502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.bcr_aliquot_uuid:
b3f94f83-6e19-4fdd-b004-a479f4aa93c0 f25b2cf4-55c3-4dd6-a05f-abc841db1585 
                                   1                                    1 
                                NA's 
                                 502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.biospecimen_barcode_bottom:
125245922 125245977      NA's 
        1         1       502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.center_id:
   1 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.concentration:
0.14 0.15 NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.day_of_shipment:
  29 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.is_derived_from_ffpe:
  no NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.month_of_shipment:
   8 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_column:
   1 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_id:
2250 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_row:
   f    g NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.quantity:
1.12  1.2 NA's 
   1    1  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.source_center:
  22 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.volume:
   8 NA's 
   2  502 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.year_of_shipment:
2012 NA's 
   2  502 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 1.040e+08 1.252e+08 2.765e+08 1.457e+08 1.076e+09        24 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    8.00    5.53    8.00    8.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0800  0.1047  0.1500  0.1700       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   10.00   15.00   17.41   27.00   30.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 502    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.484   8.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.438   5.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.000   3.000   2.693   3.200   4.540       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 375  127    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   40.00   33.92   60.00   60.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2011    2011    2012    2014       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.040e+08 1.252e+08 3.023e+08 1.457e+08 1.062e+09        24 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   2    5    6    8 NA's 
  29  299   69  105    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.140   0.150   0.137   0.160   0.170       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   10.00   15.00   17.54   27.00   30.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 502    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.484   8.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.655   5.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.950   2.000   2.178   2.210   4.270       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 375  127    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.00   13.30   19.33   13.30   60.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2011    2011    2012    2014       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 9.751e+07 1.005e+08 2.046e+08 1.070e+08 1.053e+09       289 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    5    8    9 NA's 
  69   98   34   36  267 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.140   0.150   0.136   0.150   0.170     267 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   10.00   15.00   16.74   30.00   30.00     267 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 237  267 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   6.000   6.384   8.000  11.000     267 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.413   3.000   6.000     267 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.860   2.000   2.312   3.000   4.540     267 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
 229    8  267 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.30   17.40   18.62   20.00   40.00     267 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2009 2010 2011 2013 NA's 
  22  138   69    8  267 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 9.651e+07 9.889e+07 1.075e+08 9.889e+07 1.054e+09       390 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    4    5    8 NA's 
  12   36   69   19  368 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1477  0.1500  0.1700     368 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     2.0    10.0    11.9    18.0    27.0     368 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 136  368 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   6.000   6.279   9.000  11.000     368 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.699   4.000   6.000     368 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.730   2.000   2.332   3.470   4.270     368 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
 135    1  368 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   15.86   26.70   26.70     368 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2009 2010 2011 2013 NA's 
  22  112    1    1  368 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.752e+07 9.752e+07 9.885e+07 2.244e+08 1.040e+08 1.050e+09       375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   6    8    9 NA's 
  36   75   18  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1355  0.1500  0.1700     375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   27.00   19.81   28.00   31.00     375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
 129  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.116   6.000   9.000     375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.698   4.000   6.000     375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    963     963    1029    1109    1268    1521     375 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.140   1.439   2.000   2.130     375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
 129  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70    6.70   13.21   13.30   40.00     375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 2011 NA's 
 112   17  375 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.752e+07 9.752e+07 9.888e+07 4.270e+08 1.050e+09 1.050e+09       408 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   1    2    9 NA's 
   4   33   59  408 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1472  0.1500  0.1700     408 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   26.00   28.00   25.68   28.00   29.00     408 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  96  408 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1    4    7    8 NA's 
  34   47   11    4  408 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   2.802   4.000   6.000     408 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.040   1.333   1.860   2.130     408 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
  95    1  408 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
 6.7    8 13.3 13.4 NA's 
  58    4   33    1  408 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2010 2011 2012 2015 NA's 
  58   33    4    1  408 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.477e+08 1.050e+09 7.700e+08 1.050e+09 1.076e+09       434 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     8.0     8.0     7.2     8.0     9.0     434 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0714  0.0700  0.1700     434 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   21.25   31.00   25.80   31.00   31.00     434 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
  70  434 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   1    2    8   10 NA's 
  41    5   16    8  434 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.571   4.000   6.000     434 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.000   2.000   2.201   2.000   6.800     434 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
  65    5  434 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   40.00   40.00   34.71   40.00   40.00     434 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2011    2015     434 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.252e+08 1.050e+09 7.192e+08 1.054e+09 1.054e+09       476 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   1    5    8    9 NA's 
  18    1    6    3  476 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1400  0.1189  0.1500  0.1700     476 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   21.25   22.00   22.25   29.00   31.00     476 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
  28  476 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   1    2    7    8 NA's 
   5    5    1   17  476 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.536   3.000   6.000     476 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.000   1.200   1.682   2.000  10.660     476 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22   23 NA's 
  27    1  476 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    8.00   19.00   40.00   66.60     476 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 2012 2013 NA's 
  21    6    1  476 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 1.040e+08 1.252e+08 2.762e+08 1.457e+08 1.076e+09        23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    4    8 NA's 
 377   95   11   20    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1486  0.1600  0.5000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0    10.0    15.0    17.4    27.0    30.0       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 503    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.483   8.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.441   5.000  12.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.000   1.010   1.377   1.220  40.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 375  128    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.670   6.670   6.700   9.477   8.350  80.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2011    2011    2012    2014       1 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   56.21  247.85  274.55  404.50 1020.63       2 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
                 dna repli-g (qiagen) dna                 NA's 
                 502                    1                    1 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
   d    w NA's 
 502    1    1 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1509  0.1600  0.1700       2 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 503    1 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 272  231    1 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             503 
                                            NA's 
                                               1 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
 503    1 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type               repli-g                  NA's 
                  502                     1                     1 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
      282       220         2 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
 306   14  184 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   17.00   16.12   25.00   31.00       1 

patient.samples.sample.portions.portion.is_ffpe:
  no NA's 
 503    1 

patient.samples.sample.portions.portion.lce:
 lce NA's 
   7  497 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.696  10.000  12.000       1 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000  11.000   9.489  11.000  51.000       1 

patient.samples.sample.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.408   1.250   5.000     384 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 265  239 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00   10.00   12.86   20.00   40.00     423 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     2.0     2.8     3.0    20.0     449 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   3.000   5.000   7.894  10.000  30.000     240 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   2.735   5.000  10.000     455 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   4.434   5.000  40.000     262 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   12.26   15.00   58.00     261 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  35.00   67.00   75.00   75.17   85.00  100.00     260 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  45.00   70.00   75.00   75.26   80.00   95.00     240 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    19    246    239 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
 503    1 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    5.00   10.55   15.00   70.00     250 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.867   3.000  50.000     286 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.000   5.000   8.036  10.000  80.000       6 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.532   3.000  35.000     288 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   5.474   5.000 100.000      44 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   13.39   20.00   60.00      44 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   65.00   75.00   72.67   85.00  100.00      43 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   65.00   75.00   73.81   80.00  100.00       6 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top   NA's 
   246    257      1 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   30.00   36.00   62.52  106.50  172.00       5 

patient.samples.sample.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2011    2011    2012    2014       1 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 328  176 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 328  176 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   13.00   21.00   19.32   21.00   61.00     176 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    5    6 NA's 
  16   95    2    1  390 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   5   30 NA's 
 195  133  176 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   3    7 NA's 
 195  133  176 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 2014 NA's 
 195  133  176 

patient.samples.sample.sample_type:
primary tumor 
          504 

patient.samples.sample.sample_type_id:
  1 
504 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.4000  0.3739  0.5000  1.0000     129 

patient.samples.sample.vial_number:
  a   b 
500   4 

Sex:
FEMALE   MALE   NA's 
    47    131    326 

Age.at.diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   63.00   68.00   67.63   73.00   85.00     329 


N.stage:
  N0   N1   N2   N3 NA's 
 117   39   17    5  326 

M.stage:
  M0   M1   MX  N/A NA's 
 165    3    6    4  326 



Nonsilent.Mutatios:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   161.2   227.5   273.5   316.5  2335.0     326 

Nonsilent.Mutatios.per.Mb:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0278  4.5658  6.4349  7.8169  8.8559 69.5970     326 

















Mutation_TNFAIP3:
p.G622C p.P467H p.R136H p.Y778C    NA's 
      1       1       1       1     500 


Mutation_SOX2:
p.M45L   NA's 
     1    503 

Mutation_FGFR1:
 p.P25Q p.R445W p.W471L    NA's 
      1       1       1     501 

Mutation_FGFR2:
p.E470Q p.K659N p.T786K p.W290C    NA's 
      1       1       1       1     500 

Mutation_FGFR3:
p.K715M p.R248C p.S249C    NA's 
      1       1       2     500 


Mutation_CCND1:
p.E162K    NA's 
      1     503 








Mutation_FOXP1:
 p.A645D p.G433fs  p.Q140H  p.Q280*     NA's 
       1        1        1        1      500 



Mutation_AKT3:
p.D395E p.E131K  p.W22C    NA's 
      1       1       1     501 



Mutation_KRAS:
p.C118S  p.Q61H    NA's 
      1       1     502 


Mutation_STK11:
p.G279fs  p.M51fs     NA's 
       2        1      501 



CNA_TP53:
 Del NA's 
   3  501 

CNA_HLA.A:
 Amp  Del NA's 
   1    1  502 

CNA_NFE2L2:
 Amp NA's 
   7  497 

CNA_NOTCH1:
 Del NA's 
   1  503 

CNA_NOTCH2:
 Amp  Del NA's 
   3    1  500 

CNA_SMAD4:
 Amp  Del NA's 
   1    3  500 

CNA_SOX2:
 Amp NA's 
  61  443 

CNA_FGFR1:
 Amp NA's 
  27  477 

CNA_FGFR3:
 Del NA's 
   5  499 

CNA_PIK3CA:
 Amp NA's 
  58  446 

CNA_CCND1:
 Amp NA's 
  17  487 

CNA_EGFR:
 Amp  Del NA's 
  11    1  492 

CNA_ERBB2:
 Amp  Del NA's 
   4    2  498 

CNA_PDGFRA:
 Amp NA's 
  10  494 

CNA_MYC:
 Amp NA's 
   6  498 

CNA_PTEN:
 Del NA's 
  11  493 

CNA_RB1:
 Del NA's 
   3  501 

CNA_CDKN2A:
 Del NA's 
  48  456 

CNA_CUL3:
 Del NA's 
   3  501 

CNA_FOXP1:
 Del NA's 
   6  498 

CNA_ASCL4:
 Amp NA's 
   1  503 

CNA_NF1:
 Amp  Del NA's 
   1    2  501 

CNA_AKT1:
 Amp  Del NA's 
   3    2  499 

CNA_AKT2:
 Amp NA's 
   5  499 

CNA_AKT3:
 Amp NA's 
   3  501 

CNA_BRAF:
 Amp  Del NA's 
   1    1  502 

CNA_HRAS:
 Del NA's 
   2  502 

CNA_NRAS:
 Amp  Del NA's 
   1    3  500 

CNA_RASA1:
 Del NA's 
   3  501 

CNA_TSC1:
 Del NA's 
   1  503 

Expression.Subtype:
    basal classical primitive secretory      NA's 
       43        65        27        43       326 

}}
\keyword{datasets}
