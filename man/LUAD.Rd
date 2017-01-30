\name{ LUAD }
\alias{ LUAD }
\docType{data}
\title{ LUAD }

\details{
\preformatted{
> LUAD
A MultiAssayExperiment object of 12 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 12: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 162 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 576 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 498 columns 
 [4] CNASNP: RangedRaggedAssay with 501475 rows and 1095 columns 
 [5] CNVSNP: RangedRaggedAssay with 115836 rows and 1095 columns 
 [6] CNAseq: RangedRaggedAssay with 399592 rows and 249 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 492 columns 
 [8] mRNAArray: ExpressionSet with 17814 rows and 32 columns 
 [9] RPPAArray: ExpressionSet with 223 rows and 365 columns 
 [10] Mutations: RangedRaggedAssay with 72541 rows and 230 columns 
 [11] gistica: SummarizedExperiment with 24776 rows and 516 columns 
 [12] gistict: SummarizedExperiment with 24776 rows and 516 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( LUAD )
CharacterList of length 12
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-625470 1:707099-8925112 ... 24:28438116-59018259
[["CNVSNP"]] 1:3218610-8925112 1:8931149-54019473 ... 23:55992309-154905589
[["CNAseq"]] 1:10208-3815866 1:3815867-4852674 ... 24:6618336-28783838
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] PLEKHN1 UBE2J2 Unknown C1orf200 ... C9orf131 FUBP3 FAM123B
...
<2 more elements>

> colnames( LUAD )
CharacterList of length 12
[["RNASeqGene"]] TCGA-05-4244-01A-01R-1107-07 ... TCGA-95-7043-01A-11R-1949-07
[["RNASeq2GeneNorm"]] TCGA-05-4244-01A-01R-1107-07 ...
[["miRNASeqGene"]] TCGA-05-4384-01A-01T-1754-13 ...
[["CNASNP"]] TCGA-05-4244-01A-01D-1877-01 ... TCGA-S2-AA1A-01A-12D-A396-01
[["CNVSNP"]] TCGA-05-4244-01A-01D-1877-01 ... TCGA-S2-AA1A-01A-12D-A396-01
[["CNAseq"]] TCGA-05-4249-01A-01D-1103-02 ... TCGA-91-6836-11A-01D-1853-02
[["Methylation"]] TCGA-05-4384-01A-01D-1756-05 ... TCGA-S2-AA1A-01A-12D-A398-05
[["mRNAArray"]] TCGA-05-4244-01A-01R-1107-07 ... TCGA-67-3774-01A-01R-0946-07
[["RPPAArray"]] TCGA-05-4244-01A-21-2190-20 ... TCGA-S2-AA1A-01A-21-A45B-20
[["Mutations"]] TCGA-05-4249-01A-01D-1105-08 ... TCGA-99-7458-01A-11D-2036-08
...
<2 more elements>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.2 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.2 Mb
4           CNASNP 35.9 Mb
5           CNVSNP 10.3 Mb
6           CNAseq 25.8 Mb
7      Methylation 67.5 Mb
8        mRNAArray    1 Mb
9        RPPAArray  0.1 Mb
10       Mutations 88.5 Mb
11         gistica  4.4 Mb
12         gistict  4.4 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   336 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 184.00  184.00    1.70    1.37    2.00 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  33.00   59.00   66.00   65.22   72.00   88.00      31 

vital_status:
  0   1 
332 188 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   297.8   619.0   791.4  1120.0  4961.0     336 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   365.2   620.5   868.7  1068.2  7248.0     126 

tumor_tissue_site:
lung 
 520 





gender:
female   male 
   279    241 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1991    2007    2010    2008    2011    2013      19 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    96.5   141.0   327.6   386.0  1178.0     513 

radiation_therapy:
  no  yes NA's 
 413   61   46 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   80.00   90.00   78.55  100.00  100.00     382 


number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   20.00   40.00   41.55   50.00  154.00     163 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1930    1956    1965    1965    1972    1999     241 

residual_tumor:
  r0   r1   r2   rx NA's 
 347   13    4   26  130 

race:
american indian or alaska native                            asian 
                               1                                8 
       black or african american                            white 
                              53                              392 
                            NA's 
                              66 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     7                    388                    125 

admin.bcr:
nationwide children's hospital 
                           520 

admin.day_of_dcc_upload:
  1 
520 

admin.disease_code:
luad 
 520 


admin.month_of_dcc_upload:
 10 
520 

admin.patient_withdrawal.withdrawn:
false 
  520 

admin.project_code:
tcga 
 520 

admin.year_of_dcc_upload:
2015 
 520 

patient.additional_studies.additional_study.disease_code:
fppp NA's 
  12  508 

patient.additional_studies.additional_study.project_code:
tcga NA's 
  12  508 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  33.00   59.00   66.00   65.32   73.00   88.00      19 


patient.anatomic_neoplasm_subdivision_other:
all 3 lobes of rt lung                lingula     right upper, lower 
                     1                      1                      1 
           rul and rml                   NA's 
                     1                    516 



patient.clinical_cqcf.consent_or_death_status:
consented  deceased    waiver 
      449        65         6 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -81.00   -1.00    0.00   71.27   29.00 6812.00      90 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   336.0   864.0   902.7  1235.0  3169.0     455 


patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             517 
yes, pharmaceutical treatment prior to resection 
                                               3 

patient.clinical_cqcf.history_of_prior_malignancy:
                                                     no 
                                                    426 
                       yes, history of prior malignancy 
                                                     75 
yes, history of synchronous and or bilateral malignancy 
                                                     16 
                                                   NA's 
                                                      3 

patient.clinical_cqcf.normal_tissue_anatomic_site:
bronchus     lung    other     NA's 
       1      232       12      275 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           96           424 



patient.clinical_cqcf.tumor_type:
primary 
    520 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   16.00   16.01   23.00   31.00       3 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32432  -26625  -24411  -24010  -21734  -12179      31 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     278     677     855    1182    4961     397 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 501   19 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    35.0   162.0   454.4   513.0  6812.0     133 

patient.days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    96.5   141.0   327.6   386.0  1178.0     513 

patient.diagnosis:
lung adenocarcinoma 
                520 

patient.dlco_predictive_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   57.00   73.00   73.34   87.25  178.00     320 

patient.drugs.drug.10.bcr_drug_barcode:
tcga-53-7624-d39739                NA's 
                  1                 519 

patient.drugs.drug.10.bcr_drug_uuid:
eac0a9fd-301b-48f7-afd2-499dfe2f0418                                 NA's 
                                   1                                  519 

patient.drugs.drug.10.day_of_form_completion:
  31 NA's 
   1  519 

patient.drugs.drug.10.days_to_drug_therapy_end:
 490 NA's 
   1  519 

patient.drugs.drug.10.days_to_drug_therapy_start:
 339 NA's 
   1  519 

patient.drugs.drug.10.drug_name:
pemetrexed       NA's 
         1        519 

patient.drugs.drug.10.measure_of_response:
clinical progressive disease                         NA's 
                           1                          519 

patient.drugs.drug.10.month_of_form_completion:
   1 NA's 
   1  519 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
   1  519 

patient.drugs.drug.10.therapy_types.therapy_type:
chemotherapy         NA's 
           1          519 

patient.drugs.drug.10.tx_on_clinical_trial:
  no NA's 
   1  519 

patient.drugs.drug.10.year_of_form_completion:
2013 NA's 
   1  519 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   17.00   16.75   25.00   31.00     367 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   25.0   110.0   148.0   221.5   193.0  1284.0     387 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    47.0    74.0   157.3   129.5  1147.0     373 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                          16                           51 
            partial response               stable disease 
                           6                           10 
                        NA's 
                         437 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.203   9.000  12.000     367 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    4.00    6.18    4.00  132.00     470 




patient.drugs.drug.2.regimen_indication_notes:
neo-adjuvant  neoadjuvant  new primary         NA's 
           1            1            1          517 


patient.drugs.drug.2.route_of_administrations.route_of_administration:
intramuscular (im)   intravenous (iv)               oral               NA's 
                 1                 47                  2                470 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
 143   10  367 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy             other, specify 
                       150                          1 
targeted molecular therapy                       NA's 
                         2                        367 

patient.drugs.drug.2.therapy_types.therapy_type_notes:
gsk mage vaccine study                   NA's 
                     1                    519 


patient.drugs.drug.2.total_dose_units:
 g/m2    mg mg/m2    ml  NA's 
    1    25     4     1   489 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  97    3  420 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2012    2012    2013    2015     367 



patient.drugs.drug.3.clinical_trail_drug_classification:
tyrosine kinase inhibitor                      NA's 
                        1                       519 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   15.50   16.37   25.00   31.00     458 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   62.0   130.0   186.0   355.1   569.0  1284.0     469 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  27.00   75.75  138.50  276.85  474.25 1135.00     460 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                          13                            9 
            partial response               stable disease 
                           2                            5 
                        NA's 
                         491 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.250   6.000   6.081   9.000  12.000     458 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   3.917   4.250   8.000     496 


patient.drugs.drug.3.prescribed_dose_units:
  auc  g/m2    mg mg/m2  NA's 
    2     1    12     3   502 

patient.drugs.drug.3.regimen_indication:
   adjuvant  palliative progression  recurrence        NA's 
         15           4           4           2         495 

patient.drugs.drug.3.regimen_number:
        1         2         4 lccc 9831      NA's 
       14         9         1         1       495 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
              25              495 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  54    8  458 

patient.drugs.drug.3.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         1                         53 
             immunotherapy targeted molecular therapy 
                         3                          4 
                      NA's 
                       459 

patient.drugs.drug.3.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   516.8  1684.0  1585.0  2365.0  3625.0     506 

patient.drugs.drug.3.total_dose_units:
   mg mg/m2  NA's 
   14     2   504 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  33    4  483 

patient.drugs.drug.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     458 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    5.75   12.50   13.78   19.25   31.00     488 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -40.0   133.0   396.0   407.9   581.0  1522.0     491 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -74.0    87.5   239.5   362.9   546.0  1284.0     488 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                           7                            5 
            partial response               stable disease 
                           2                            3 
                        NA's 
                         503 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.500   5.938   9.000  12.000     488 

patient.drugs.drug.4.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.454   3.500   5.000     509 


patient.drugs.drug.4.prescribed_dose_units:
   mg mg/m2  NA's 
    5     3   512 

patient.drugs.drug.4.regimen_indication:
               adjuvant other: specify in notes              palliative 
                      5                       1                       2 
            progression                    NA's 
                      4                     508 

patient.drugs.drug.4.regimen_indication_notes:
neo-adjuvant         NA's 
           1          519 

patient.drugs.drug.4.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    2.00    2.25    3.00    6.00     508 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intramuscular (im)   intravenous (iv)               NA's 
                 1                 11                508 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
  31    1  488 

patient.drugs.drug.4.therapy_types.therapy_type:
     ancillary   chemotherapy  immunotherapy other, specify           NA's 
             2             26              2              1            489 

patient.drugs.drug.4.therapy_types.therapy_type_notes:
phase ii clinical trial krw2170                            NA's 
                              1                             519 

patient.drugs.drug.4.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   281.2   384.0  1835.1  1866.0  8000.0     512 

patient.drugs.drug.4.total_dose_units:
   mg mg/m2  NA's 
    7     2   511 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
  19    1  500 

patient.drugs.drug.4.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     488 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.50    8.00   12.79   19.25   31.00     506 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   89.0   161.0   406.0   460.8   601.0  1649.0     507 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   28.0    78.5   277.0   387.4   562.5  1522.0     506 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease            complete response 
                           3                            3 
            partial response               stable disease 
                           1                            3 
                        NA's 
                         510 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.250   5.500   6.357   9.750  12.000     506 

patient.drugs.drug.5.number_cycles:
   1    5    6 NA's 
   1    1    1  517 

patient.drugs.drug.5.prescribed_dose:
  39  150 NA's 
   1    1  518 

patient.drugs.drug.5.prescribed_dose_units:
  mg NA's 
   2  518 

patient.drugs.drug.5.regimen_indication:
   adjuvant progression        NA's 
          2           1         517 

patient.drugs.drug.5.regimen_number:
   1    4 NA's 
   2    1  517 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               2                1              517 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
  14  506 

patient.drugs.drug.5.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         2                         11 
targeted molecular therapy                       NA's 
                         1                        506 

patient.drugs.drug.5.total_dose:
  39 9000 NA's 
   1    1  518 

patient.drugs.drug.5.total_dose_units:
  mg NA's 
   2  518 

patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
  11  509 

patient.drugs.drug.5.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2013    2013    2014    2015     506 



patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     4.0    13.0    14.5    24.5    31.0     510 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    132     186     187     288     452     483     515 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   66.0   190.2   434.5   519.1   695.2  1649.0     510 


patient.drugs.drug.6.measure_of_response:
clinical progressive disease            complete response 
                           2                            2 
            partial response               stable disease 
                           1                            1 
                        NA's 
                         514 

patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    4.50    4.80    5.75   12.00     510 

patient.drugs.drug.6.number_cycles:
   2 NA's 
   1  519 

patient.drugs.drug.6.prescribed_dose:
25mg/m2 = 50         NA's 
           1          519 

patient.drugs.drug.6.prescribed_dose_units:
mg/m2  NA's 
    1   519 

patient.drugs.drug.6.regimen_indication:
progression        NA's 
          1         519 

patient.drugs.drug.6.regimen_number:
   3 NA's 
   1  519 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              519 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
   7    3  510 

patient.drugs.drug.6.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         1                          8 
targeted molecular therapy                       NA's 
                         1                        510 

patient.drugs.drug.6.total_dose:
 300 NA's 
   1  519 

patient.drugs.drug.6.total_dose_units:
  mg NA's 
   1  519 

patient.drugs.drug.6.tx_on_clinical_trial:
  no NA's 
   9  511 

patient.drugs.drug.6.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2013    2013    2014    2015     510 



patient.drugs.drug.7.day_of_form_completion:
   3    9   17   31 NA's 
   2    1    1    1  515 

patient.drugs.drug.7.days_to_drug_therapy_end:
 186  195  581  676 NA's 
   1    1    1    1  516 

patient.drugs.drug.7.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   66.0   146.0   539.0   455.8   592.0   936.0     515 

patient.drugs.drug.7.drug_name:
carboplatin gemcitabine     tarceva vinorelbine        NA's 
          1           2           1           1         515 

patient.drugs.drug.7.measure_of_response:
clinical progressive disease            complete response 
                           2                            1 
                        NA's 
                         517 

patient.drugs.drug.7.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       3       4       5       5      12     515 

patient.drugs.drug.7.number_cycles:
   2 NA's 
   1  519 

patient.drugs.drug.7.prescribed_dose:
100mg/m2     NA's 
       1      519 

patient.drugs.drug.7.prescribed_dose_units:
mg/m2  NA's 
    1   519 

patient.drugs.drug.7.regimen_indication:
progression        NA's 
          1         519 

patient.drugs.drug.7.regimen_number:
   5 NA's 
   1  519 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              519 

patient.drugs.drug.7.therapy_ongoing:
  no  yes NA's 
   4    1  515 

patient.drugs.drug.7.therapy_types.therapy_type:
chemotherapy         NA's 
           5          515 

patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   4  516 

patient.drugs.drug.7.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2013    2013    2014    2015     515 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-53-7624-d39737 tcga-64-5815-d31484 tcga-73-4659-d12858 tcga-86-7701-d58408 
                  1                   1                   1                   1 
               NA's 
                516 

patient.drugs.drug.8.bcr_drug_uuid:
4878b403-b86b-448f-a98c-f33fb9f7ea01 87e2d964-3168-438c-9cb9-1f81d70122ae 
                                   1                                    1 
dc37e270-9e05-421e-a7f4-8b20df86dc92 e1e21cb8-57eb-41c0-bc61-810f106edb69 
                                   1                                    1 
                                NA's 
                                 516 

patient.drugs.drug.8.day_of_form_completion:
   3    9   31 NA's 
   2    1    1  516 

patient.drugs.drug.8.days_to_drug_therapy_end:
 195  490  609 NA's 
   1    1    1  517 

patient.drugs.drug.8.days_to_drug_therapy_start:
 146  339  602  936 NA's 
   1    1    1    1  516 

patient.drugs.drug.8.drug_name:
       almita carboplatinum     docetaxel         taxol          NA's 
            1             1             1             1           516 

patient.drugs.drug.8.measure_of_response:
clinical progressive disease            complete response 
                           1                            1 
                        NA's 
                         518 

patient.drugs.drug.8.month_of_form_completion:
   1    4    5   12 NA's 
   1    1    1    1  516 

patient.drugs.drug.8.number_cycles:
   2 NA's 
   1  519 

patient.drugs.drug.8.prescribed_dose:
50mg/m2 = 100mg            NA's 
              1             519 

patient.drugs.drug.8.prescribed_dose_units:
mg/m2  NA's 
    1   519 

patient.drugs.drug.8.regimen_indication:
progression        NA's 
          1         519 

patient.drugs.drug.8.regimen_number:
   7 NA's 
   1  519 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              519 

patient.drugs.drug.8.therapy_ongoing:
  no  yes NA's 
   3    1  516 

patient.drugs.drug.8.therapy_types.therapy_type:
chemotherapy         NA's 
           4          516 

patient.drugs.drug.8.total_dose:
 200 NA's 
   1  519 

patient.drugs.drug.8.total_dose_units:
  mg NA's 
   1  519 

patient.drugs.drug.8.tx_on_clinical_trial:
  no NA's 
   3  517 

patient.drugs.drug.8.year_of_form_completion:
2010 2012 2013 2014 NA's 
   1    1    1    1  516 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-53-7624-d39738                NA's 
                  1                 519 

patient.drugs.drug.9.bcr_drug_uuid:
0f25327c-c54d-4542-a956-47affd9d63b5                                 NA's 
                                   1                                  519 

patient.drugs.drug.9.day_of_form_completion:
  31 NA's 
   1  519 

patient.drugs.drug.9.days_to_drug_therapy_end:
 490 NA's 
   1  519 

patient.drugs.drug.9.days_to_drug_therapy_start:
 339 NA's 
   1  519 

patient.drugs.drug.9.drug_name:
bevacizumab        NA's 
          1         519 

patient.drugs.drug.9.measure_of_response:
clinical progressive disease                         NA's 
                           1                          519 

patient.drugs.drug.9.month_of_form_completion:
   1 NA's 
   1  519 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   1  519 

patient.drugs.drug.9.therapy_types.therapy_type:
chemotherapy         NA's 
           1          519 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   1  519 

patient.drugs.drug.9.year_of_form_completion:
2013 NA's 
   1  519 



patient.drugs.drug.clinical_trail_drug_classification:
asci NA's 
   1  519 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   16.00   16.46   25.00   31.00     341 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    -40     111     153     216     201    1238     370 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -74.0    43.0    72.0   154.8   118.5  1255.0     349 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          22                           57 
            partial response               stable disease 
                           7                           10 
                        NA's 
                         424 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    6.00    6.43    9.50   12.00     341 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   3.906   4.000  12.000     467 




patient.drugs.drug.regimen_indication_notes:
neo-adjuvant  neoadjuvant  new primary     protocol         NA's 
           2            1            1            1          515 

patient.drugs.drug.regimen_number:
   1    2    3    8 NA's 
  51    6    1    1  461 

patient.drugs.drug.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              55                4              461 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 164   13  343 


patient.drugs.drug.therapy_types.therapy_type_notes:
aurora kinase inhibitor - protocol therapy 
                                         1 
                                      NA's 
                                       519 

patient.drugs.drug.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     56     450    1360    3098    3817   15120     491 

patient.drugs.drug.total_dose_units:
 g/m2    mg mg/m2  NA's 
    1    27     3   489 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
 111    5  404 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     341 

patient.eastern_cancer_oncology_group:
   0    1    2    3 NA's 
  95  100   21    3  301 

patient.egfr_mutation_performed:
  no  yes NA's 
 196   80  244 


patient.eml4_alk_translocation_method:
  fish    ihc rt-pcr   NA's 
    19      3      3    495 

patient.eml4_alk_translocation_performed:
  no  yes NA's 
 212   34  274 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     7                    388                    125 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
  no  yes NA's 
  26   21  473 

patient.follow_ups.follow_up.2.additional_radiation_therapy:
  no  yes NA's 
  24   24  472 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure:
  no  yes NA's 
  38    5  477 

patient.follow_ups.follow_up.2.additional_surgery_metastatic_procedure:
  no  yes NA's 
  19   10  491 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    7.75   15.00   14.89   22.00   30.00     388 

patient.follow_ups.follow_up.2.days_to_additional_surgery_locoregional_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  437.0   453.0   639.0   980.8  1481.0  1894.0     515 

patient.follow_ups.follow_up.2.days_to_additional_surgery_metastatic_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  112.0   524.0   688.0   830.2   975.0  2092.0     511 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  281.0   696.2   974.0  1169.1  1288.0  4961.0     492 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0   705.8   945.5  1397.2  1699.8  7248.0     416 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   47.0   418.2   717.0   960.2  1188.2  4608.0     470 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  1.0000  0.8387  1.0000  4.0000     489 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                            19                            103 
                          NA's 
                           398 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response  partial remission/response 
                         60                           1 
        progressive disease              stable disease 
                         25                          12 
                       NA's 
                        422 

patient.follow_ups.follow_up.2.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    70.0    85.0    75.0    97.5   100.0     494 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
 104    6  410 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   5.083   8.000  12.000     388 

patient.follow_ups.follow_up.2.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     16                      18                       5 
                   NA's 
                    481 

patient.follow_ups.follow_up.2.new_neoplasm_event_types.new_neoplasm_event_type.2:
distant metastasis               NA's 
                 2                518 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  66   50  404 

patient.follow_ups.follow_up.2.performance_status_scale_timing:
     adjuvant therapy                 other post-adjuvant therapy 
                    1                    13                    20 
        pre-operative                  NA's 
                    5                   481 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        73         40        407 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                         90                          13 
             stable disease                        NA's 
                         11                         406 

patient.follow_ups.follow_up.2.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                  7                                  14 
                               NA's 
                                499 

patient.follow_ups.follow_up.2.progression_determined_by_list.progression_determined_by.2:
convincing imaging               NA's 
                 3                517 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
 106   11  403 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
  79   37  404 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
  104    28   388 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2013    2013    2013    2015    2015     388 

patient.follow_ups.follow_up.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   8    5  507 

patient.follow_ups.follow_up.3.additional_radiation_therapy:
  no  yes NA's 
   5    8  507 

patient.follow_ups.follow_up.3.additional_surgery_locoregional_procedure:
  no  yes NA's 
  11    1  508 

patient.follow_ups.follow_up.3.additional_surgery_metastatic_procedure:
  no NA's 
   8  512 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    9.50   15.00   15.63   20.50   31.00     501 

patient.follow_ups.follow_up.3.days_to_additional_surgery_locoregional_procedure:
1255 NA's 
   1  519 

patient.follow_ups.follow_up.3.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  503.0   676.8   804.0   845.0  1037.8  1265.0     512 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    788    1153    1272    1208    1303    1429     509 

patient.follow_ups.follow_up.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  316.0   568.0   836.0   847.3  1146.0  1258.0     507 

patient.follow_ups.follow_up.3.eastern_cancer_oncology_group:
   0    1    2 NA's 
   1    1    2  516 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             8                              9 
                          NA's 
                           503 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response         progressive disease 
                          7                           7 
             stable disease                        NA's 
                          2                         504 

patient.follow_ups.follow_up.3.karnofsky_performance_score:
   0   90 NA's 
   1    1  518 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
  15  505 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.474   9.000  12.000     501 

patient.follow_ups.follow_up.3.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      3                       4                       2 
                   NA's 
                    511 

patient.follow_ups.follow_up.3.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   5   13  502 

patient.follow_ups.follow_up.3.performance_status_scale_timing:
other  NA's 
    5   515 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         6         13        501 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                         12                           2 
             stable disease                        NA's 
                          2                         504 

patient.follow_ups.follow_up.3.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                  5                                   3 
                               NA's 
                                512 

patient.follow_ups.follow_up.3.progression_determined_by_list.progression_determined_by.2:
convincing imaging               NA's 
                 3                517 

patient.follow_ups.follow_up.3.progression_determined_by_list.progression_determined_by.3:
positive biomarker (s)                   NA's 
                     1                    519 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
  14    1  505 

patient.follow_ups.follow_up.3.targeted_molecular_therapy:
  no  yes NA's 
  10    6  504 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
   11     8   501 

patient.follow_ups.follow_up.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2013    2013    2013    2014     501 

patient.follow_ups.follow_up.4.additional_pharmaceutical_therapy:
  no NA's 
   1  519 

patient.follow_ups.follow_up.4.additional_radiation_therapy:
 yes NA's 
   1  519 

patient.follow_ups.follow_up.4.additional_surgery_locoregional_procedure:
  no NA's 
   1  519 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-44-2659-f40300 tcga-44-2664-f34138                NA's 
                  1                   1                 518 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
2386c68e-b1ca-41b8-a7e5-129d0ce2e2ca d19e4687-3387-4057-ba51-2a86d4ae43aa 
                                   1                                    1 
                                NA's 
                                 518 

patient.follow_ups.follow_up.4.day_of_form_completion:
  15   30 NA's 
   1    1  518 

patient.follow_ups.follow_up.4.days_to_last_followup:
1251 1367 NA's 
   1    1  518 

patient.follow_ups.follow_up.4.days_to_new_tumor_event_after_initial_treatment:
 832 NA's 
   1  519 

patient.follow_ups.follow_up.4.eastern_cancer_oncology_group:
   2 NA's 
   1  519 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                              1 
                          NA's 
                           518 

patient.follow_ups.follow_up.4.followup_treatment_success:
complete remission/response                        NA's 
                          2                         518 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   2  518 

patient.follow_ups.follow_up.4.month_of_form_completion:
   2    7 NA's 
   1    1  518 

patient.follow_ups.follow_up.4.new_neoplasm_event_types.new_neoplasm_event_type:
locoregional recurrence                    NA's 
                      1                     519 

patient.follow_ups.follow_up.4.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   1    1  518 

patient.follow_ups.follow_up.4.performance_status_scale_timing:
other  NA's 
    1   519 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
tumor free       NA's 
         2        518 

patient.follow_ups.follow_up.4.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          2                         518 

patient.follow_ups.follow_up.4.progression_determined_by_list.progression_determined_by:
convincing imaging               NA's 
                 1                519 

patient.follow_ups.follow_up.4.radiation_therapy:
  no NA's 
   2  518 

patient.follow_ups.follow_up.4.targeted_molecular_therapy:
  no  yes NA's 
   1    1  518 

patient.follow_ups.follow_up.4.vital_status:
alive  NA's 
    2   518 

patient.follow_ups.follow_up.4.year_of_form_completion:
2012 2013 NA's 
   1    1  518 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
  77   61  382 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
  76   65  379 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure:
  no  yes NA's 
 101   10  409 

patient.follow_ups.follow_up.additional_surgery_metastatic_procedure:
  no  yes NA's 
  85   20  415 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   17.00   16.98   26.00   31.00      60 

patient.follow_ups.follow_up.days_to_additional_surgery_locoregional_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   36.0   363.0   686.0   691.1   784.0  1577.0     509 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -43.0   183.0   390.0   421.8   598.0  1118.0     499 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   273.2   595.5   747.4   995.0  4961.0     396 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   407.5   555.0   785.1   826.2  6812.0     206 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0   231.8   405.5   517.2   538.2  4812.0     384 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  1.0000  0.8306  1.0000  4.0000     396 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                            10                            383 
                          NA's 
                           127 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response  partial remission/response 
                        205                           9 
        progressive disease              stable disease 
                         98                          33 
                       NA's 
                        175 

patient.follow_ups.follow_up.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    80.0    90.0    76.3   100.0   100.0     447 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 279   27  214 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.409   9.000  12.000      58 

patient.follow_ups.follow_up.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     50                      31                       4 
                   NA's 
                    435 

patient.follow_ups.follow_up.new_neoplasm_event_types.new_neoplasm_event_type.2:
distant metastasis  new primary tumor               NA's 
                 3                  1                516 

patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 279  145   96 


patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       250        142        128 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        279                           5 
        progressive disease              stable disease 
                         63                          33 
                       NA's 
                        140 

patient.follow_ups.follow_up.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                 27                                  50 
                               NA's 
                                443 

patient.follow_ups.follow_up.progression_determined_by_list.progression_determined_by.2:
convincing imaging               NA's 
                12                508 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 377   58   85 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
 286  146   88 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  327   128    65 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2013    2014    2015      58 

patient.gender:
female   male 
   279    241 


patient.history_of_neoadjuvant_treatment:
  no  yes NA's 
 516    3    1 




patient.informed_consent_verified:
yes 
520 

patient.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    80.0    90.0    84.7   100.0   100.0     420 

patient.kras_gene_analysis_performed:
  no  yes NA's 
 252   62  206 

patient.kras_mutation_found:
  no  yes NA's 
  39   23  458 


patient.location_in_lung_parenchyma:
   central lung peripheral lung            NA's 
             63             127             330 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.319   9.000  12.000 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  18   12  490 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  16   14  490 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   39.0   234.0   397.0   575.7   722.5  3044.0     489 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.additional_surgery_locoregional_procedure:
  no  yes NA's 
  14    4  502 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.days_to_additional_surgery_locoregional_procedure:
 226  392  489 3664 NA's 
   1    1    1    1  516 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.additional_surgery_metastatic_procedure:
  no  yes NA's 
  10    6  504 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.days_to_additional_surgery_metastatic_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  171.0   231.0   267.0   444.3   555.5  1099.0     513 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     10                      19                       2 
                   NA's 
                    489 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type.2:
distant metastasis               NA's 
                 7                513 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                 16                                  14 
                               NA's 
                                490 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by.2:
convincing imaging               NA's 
                 6                514 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 156   31  333 

patient.number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   20.00   40.00   41.55   50.00  154.00     163 


patient.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                   23                    11                    20 
        pre-operative                  NA's 
                  177                   289 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       313        114         93 

patient.post_bronchodilator_fev1_fvc_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   69.50   82.00   78.85   91.00  127.00     405 

patient.post_bronchodilator_fev1_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   69.00   82.50   80.01   95.00  151.00     378 

patient.pre_bronchodilator_fev1_fvc_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.72   70.00   81.00   80.16   92.00  125.00     323 

patient.pre_bronchodilator_fev1_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.95   66.00   83.00   80.76   96.00  156.00     283 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        130                           2 
        progressive disease              stable disease 
                          7                          12 
                       NA's 
                        369 

patient.prior_dx:
                                                     no 
                                                    428 
                                                    yes 
                                                     51 
                       yes, history of prior malignancy 
                                                     32 
yes, history of synchronous and or bilateral malignancy 
                                                      9 

patient.pulmonary_function_test_performed:
  no  yes NA's 
  92  247  181 

patient.race:
american indian or alaska native                            asian 
                               1                                8 
       black or african american                            white 
                              53                              392 
                            NA's 
                              66 

patient.radiation_therapy:
  no  yes NA's 
 144   13  363 




patient.radiations.radiation.2.course_number:
   1    2 NA's 
   6    2  512 

patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   16.00   16.16   25.00   30.00     501 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   70.0   265.0   549.5   588.9   671.0  1641.0     502 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   35.0   193.0   526.0   549.5   637.5  1641.0     501 

patient.radiations.radiation.2.measure_of_response:
               complete response                 partial response 
                               2                                3 
radiographic progressive disease                   stable disease 
                               4                                1 
                            NA's 
                             510 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    8.00    7.21   11.00   12.00     501 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.25   10.00   11.93   21.00   28.00     506 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      3     918    2200    2694    4250    7000     505 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no  yes NA's 
  18    1  501 

patient.radiations.radiation.2.radiation_type:
               external           external beam other: specify in notes 
                     10                       6                       2 
                   NA's 
                    502 

patient.radiations.radiation.2.radiation_type_notes:
 cyberknife gamma knife        NA's 
          1           1         518 

patient.radiations.radiation.2.regimen_indication:
   adjuvant  palliative progression  recurrence        NA's 
          2           4           1           1         512 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
  11    4  505 

patient.radiations.radiation.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2014    2015     501 

patient.radiations.radiation.3.anatomic_treatment_site:
distant recurrence       distant site   local recurrence               NA's 
                 3                  3                  1                513 



patient.radiations.radiation.3.course_number:
   2 NA's 
   4  516 

patient.radiations.radiation.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.50   16.00   13.29   16.50   22.00     513 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   64.0   570.2   680.5   899.5   841.0  2544.0     514 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   51.0   575.0   699.0   864.3   803.0  2544.0     513 

patient.radiations.radiation.3.measure_of_response:
partial response             NA's 
               2              518 

patient.radiations.radiation.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.500  11.000   7.714  11.000  12.000     513 

patient.radiations.radiation.3.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     5.0     5.6     9.0    10.0     515 

patient.radiations.radiation.3.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     30    2025    2175    2013    2588    3000     514 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no  yes NA's 
   6    1  513 

patient.radiations.radiation.3.radiation_type:
               external           external beam other: specify in notes 
                      2                       2                       2 
                   NA's 
                    514 

patient.radiations.radiation.3.radiation_type_notes:
cyber knife stereotactic radiosurgery                            cyberknife 
                                    1                                     1 
                                 NA's 
                                  518 

patient.radiations.radiation.3.regimen_indication:
palliative recurrence       NA's 
         3          1        516 

patient.radiations.radiation.3.units:
 cgy   gy NA's 
   5    1  514 

patient.radiations.radiation.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     513 

patient.radiations.radiation.4.anatomic_treatment_site:
distant recurrence               NA's 
                 1                519 

patient.radiations.radiation.4.bcr_radiation_barcode:
tcga-44-2668-r18707                NA's 
                  1                 519 

patient.radiations.radiation.4.bcr_radiation_uuid:
0b7f3a19-ed59-4ad2-bb47-860f1d7c29ed                                 NA's 
                                   1                                  519 

patient.radiations.radiation.4.course_number:
   2 NA's 
   1  519 

patient.radiations.radiation.4.day_of_form_completion:
  22 NA's 
   1  519 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
 587 NA's 
   1  519 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
 586 NA's 
   1  519 

patient.radiations.radiation.4.month_of_form_completion:
  11 NA's 
   1  519 

patient.radiations.radiation.4.numfractions:
   2 NA's 
   1  519 

patient.radiations.radiation.4.radiation_dosage:
1800 NA's 
   1  519 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   1  519 

patient.radiations.radiation.4.radiation_type:
other: specify in notes                    NA's 
                      1                     519 

patient.radiations.radiation.4.radiation_type_notes:
cyberknife       NA's 
         1        519 

patient.radiations.radiation.4.regimen_indication:
palliative       NA's 
         1        519 

patient.radiations.radiation.4.units:
 cgy NA's 
   1  519 

patient.radiations.radiation.4.year_of_form_completion:
2011 NA's 
   1  519 




patient.radiations.radiation.course_number:
   1    2    3 NA's 
  34    6    3  477 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   16.00   17.21   27.50   31.00     421 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   124.8   261.5   343.1   526.0  1255.0     432 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    91.0   240.0   339.4   516.0  1255.0     431 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              14                                3 
radiographic progressive disease                   stable disease 
                              24                                5 
                            NA's 
                             474 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.222   9.000  12.000     421 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   16.50   18.47   28.75   50.00     462 


patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
  94    3  423 




patient.radiations.radiation.regimen_indication_notes:
new primary        NA's 
          1         519 

patient.radiations.radiation.units:
 cgy   gy  mci NA's 
  52   21    1  446 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     421 

patient.residual_tumor:
  r0   r1   r2   rx NA's 
 347   13    4   26  130 






patient.stopped_smoking_year:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1948    1985    1997    1995    2007    2013     259 

patient.targeted_molecular_therapy:
  no  yes NA's 
 118   38  364 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 275  238    7 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
 238  275    7 



patient.tumor_tissue_site:
lung 
 520 

patient.vital_status:
alive  dead 
  393   127 

patient.year_of_form_completion:
2010 2011 2012 2013 2014 
  63  187  175   72   23 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1991    2007    2010    2008    2011    2013      10 

patient.year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1930    1956    1965    1965    1972    1999     241 


patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              53                               27 
                   heterogeneity                             NA's 
                               7                              433 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                 15                 505 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             325              195 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased    waiver 
      449        65         6 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    58.0   148.0   962.2  1421.0  7245.0      23 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -16.00    0.00    0.00   15.99   28.00  203.00      19 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
441  79 

patient.biospecimen_cqcf.ethnicity:
not hispanic or latino                   NA's 
                    44                    476 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
bronchus     lung    other 
       2      509        9 


patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             517 
yes, pharmaceutical treatment prior to resection 
                                               3 

patient.biospecimen_cqcf.laterality:
 left right  NA's 
  204   305    11 

patient.biospecimen_cqcf.method_of_sample_procurement:
            excisional biopsy fine needle aspiration biopsy 
                            2                             3 
other method (please specify)               tumor resection 
                          104                           411 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -6.0     0.0    11.5    22.5    35.0   203.0     362 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
0.121 0.136 0.139  NA's 
    1     1     1   517 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
nanodrop     NA's 
       3      517 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
12.1 13.6 13.9 NA's 
   1    1    1  517 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
 100 NA's 
   3  517 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
                   blood draw        fine needle aspiration 
                           75                             3 
other method (please specify)               tumor resection 
                           21                            67 
                         NA's 
                          354 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
 lung other  NA's 
   93     2   425 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           62           458 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2    3 NA's 
 118   51    1  350 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
0cdb8e18-bea8-4f10-9863-33346b44e43a                                 NA's 
                                   1                                  519 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
  38 NA's 
   1  519 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
tumor resection            NA's 
              1             519 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
lung NA's 
   1  519 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            1           519 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1 NA's 
   1  519 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-365.00    0.00    0.00   28.17   30.00 4614.00      28 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0282   0.2000   0.2400  10.8796   0.6007 100.0000      440 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
  15   17   20   30 NA's 
   3   18   48   11  440 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  21.00   64.75   85.00  148.29  143.00  709.20     440 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
                   blood draw             excisional biopsy 
                          362                             1 
other method (please specify)               tumor resection 
                           24                           127 
                         NA's 
                            6 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
bronchus     lung    other     NA's 
       1      138       10      371 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           34           486 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
  1   2   3 
483  36   1 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
  no  yes NA's 
  11  475   34 

patient.biospecimen_cqcf.path_confirm_report_attached:
 no yes 
 23 497 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
  no  yes NA's 
   1  485   34 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
  no  yes NA's 
   3  483   34 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                        1                        23                        31 
                     NA's 
                      465 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
  pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                 12 
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  2 
                                                                                                                               NA's 
                                                                                                                                506 

patient.biospecimen_cqcf.sample_prescreened:
yes 
520 

patient.biospecimen_cqcf.site_of_disease:
bronchus     lung    other 
       2      509        9 

patient.biospecimen_cqcf.site_of_disease_description:
 lower middle  other  upper   NA's 
   179     22      9    308      2 


patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
 183   40  297 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
  71  448    1 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0    5   10   30 NA's 
   1    1    1    1  516 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  60   70   75   80 NA's 
   1    1    1    1  516 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
  46  116  230  300 NA's 
   1    1    1    1  516 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   4    1  515 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   7.366  10.000  70.000       3 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  20.00   62.75   70.00   71.71   80.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0   100.0   200.0   266.6   280.0  2339.0       9 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1 
520 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                      9                      53                      38 
               cryovial  other (please specify) 
                    347                      73 

patient.days_to_index:
   0 NA's 
 501   19 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and recurrent tumor(s)                                 NA's 
                                   2                                  518 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      8     113     408    1039    1608    6271     395 


patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   80.0    95.0   130.0   144.5   195.0   250.0     509 

patient.samples.sample.2.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.6000  0.7000  0.8079  1.0000  2.2000     431 

patient.samples.sample.2.is_ffpe:
 no yes 
506  14 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   1.000   1.100   1.267   1.500   3.000     431 

patient.samples.sample.2.oct_embedded:
false  true  NA's 
  114    11   395 



patient.samples.sample.2.portions.portion.2.analytes.analyte.a260_a280_ratio:
1.81 NA's 
   1  519 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-44-4112-01b-06d-a273-01 tcga-44-6146-01b-04d-a273-01 
                           1                            1 
tcga-44-6147-01b-06r-a27d-13                         NA's 
                           1                          517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
1dc469df-f4a4-4f4e-b14f-805656c3de4f 7653845e-c755-46d5-bca6-7186bf6fd65c 
                                   1                                    1 
a41e8c4a-bc8e-4402-a189-33dd8ad7eac8                                 NA's 
                                   1                                  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
132158809 132158835 132179221      NA's 
        1         1         1       517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   1   13 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.14 0.15 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  13   14 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 yes NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   3 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   2 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
a273 a27d NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   c    f    h NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
0.94 1.01    3 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
 6.7   20 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2013 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-44-4112-01b-06d-a276-05 tcga-44-6146-01b-04d-a276-05 
                           1                            1 
tcga-44-6147-01b-06r-a46x-36                         NA's 
                           1                          517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
5a229ba3-7f55-4d14-889f-266fd5d0bdce abcae16f-a107-4062-be8a-79a7be4c53af 
                                   1                                    1 
fa0a8b44-09f7-464b-8895-81a70049e479                                 NA's 
                                   1                                  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
145675687 145675711 167814236      NA's 
        1         1         1       517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
   5   36 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  13   26 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 yes NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   3    8 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   2    4 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
a276 a46x NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_row:
   a    f    h NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
0.75 0.94 1.01 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
   5  6.7 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2013 2015 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-44-4112-01b-06d-a46w-08 tcga-44-6146-01b-04d-a46w-08 
                           1                            1 
                        NA's 
                         518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
55c0e17b-a9a5-46a5-a4ac-d26cbd83e7b1 6051356a-2f32-44f7-a36e-13ad6eef6f00 
                                   1                                    1 
                                NA's 
                                 518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
132157283 132179156      NA's 
        1         1       518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id:
   8 NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration:
0.07 0.08 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   3 NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
 yes NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   8 NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column:
   3 NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id:
a46w NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_row:
   a    g NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity:
 1.4  1.6 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume:
  20 NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2015 NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-44-4112-01b-06d-a271-08 tcga-44-6146-01b-04d-a271-08 
                           1                            1 
tcga-44-6147-01b-06r-a277-07                         NA's 
                           1                          517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
9bbfdad9-a417-49a6-ad5d-e865f63824bf df6f7086-9b48-46aa-ad7e-c0545d3af19f 
                                   1                                    1 
e6e89f3d-6c2b-46b3-9698-3d0ef5cbaed9                                 NA's 
                                   1                                  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
128923907 132158869 145632070      NA's 
        1         1         1       517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   7    8 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.07 0.08 0.15 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  14 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 yes NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   3 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   2 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
a271 a277 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   c    f    h NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
 1.4  1.6 1.95 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  13   20 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.amount:
 8.13  8.81 28.63  NA's 
    1     1     1   517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.analyte_type:
 dna  rna NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.analyte_type_id:
   d    r NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.bcr_analyte_barcode:
tcga-44-4112-01b-06d tcga-44-6146-01b-04d tcga-44-6147-01b-06r 
                   1                    1                    1 
                NA's 
                 517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.bcr_analyte_uuid:
11857252-ba3f-4eea-8bea-a415f84897ed 27bf8121-ff73-4e90-8684-161053f8cc3d 
                                   1                                    1 
bfbc6fcc-62ef-41c6-bf51-aa6359426d38                                 NA's 
                                   1                                  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.concentration:
0.14 0.15 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   2  518 

patient.samples.sample.2.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
 yes NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
                     allprep ffpe dna highpure mirna (allprep dna) ffpe rna 
                                    2                                     1 
                                 NA's 
                                  517 

patient.samples.sample.2.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
   0 NA's 
   1  519 

patient.samples.sample.2.portions.portion.2.analytes.analyte.rna.rinvalue:
 2.4 NA's 
   1  519 

patient.samples.sample.2.portions.portion.2.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
        2         1       517 

patient.samples.sample.2.portions.portion.2.bcr_portion_barcode:
tcga-44-4112-01b-06 tcga-44-6146-01b-04 tcga-44-6147-01b-06                NA's 
                  1                   1                   1                 517 

patient.samples.sample.2.portions.portion.2.bcr_portion_uuid:
b7175519-80fa-4538-9121-8c2116fdbbaa c0faaa32-d2f8-4ec8-a468-56c5a55090c0 
                                   1                                    1 
d747a77e-d225-4d9e-98fb-b4823293269d                                 NA's 
                                   1                                  517 

patient.samples.sample.2.portions.portion.2.day_of_creation:
   7   13   14 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.is_ffpe:
 yes NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.month_of_creation:
   2 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.portion_number:
   4    6 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.portion_sequence:
   4    6 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.bcr_slide_barcode:
tcga-44-4112-01b-06-bs6 tcga-44-6146-01b-04-bs4 tcga-44-6147-01b-06-bs6 
                      1                       1                       1 
                   NA's 
                    517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.bcr_slide_uuid:
74fac41a-3a9c-4821-9bb9-63985d835f30 a229c77a-9386-47fe-bded-15541c9a4d19 
                                   1                                    1 
a6f8a54c-8e91-4133-aab0-cde658ae6b1b                                 NA's 
                                   1                                  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.image_file_name:
tcga-44-4112-01b-06-bs6.a229c77a-9386-47fe-bded-15541c9a4d19.svs 
                                                               1 
tcga-44-6146-01b-04-bs4.a6f8a54c-8e91-4133-aab0-cde658ae6b1b.svs 
                                                               1 
tcga-44-6147-01b-06-bs6.74fac41a-3a9c-4821-9bb9-63985d835f30.svs 
                                                               1 
                                                            NA's 
                                                             517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
 yes NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_lymphocyte_infiltration:
   5   12   25 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_monocyte_infiltration:
   2    3 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_necrosis:
   0    1 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_neutrophil_infiltration:
   0    1    2 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_normal_cells:
   0    2 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_stromal_cells:
  20   27   30 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_tumor_cells:
  70   80 NA's 
   2    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
  70   80 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.slides.slide.2.section_location:
bottom   NA's 
     3    517 

patient.samples.sample.2.portions.portion.2.slides.slide.bcr_slide_barcode:
tcga-44-4112-01b-05-bs5 tcga-44-6146-01b-03-bs3 tcga-44-6147-01b-05-bs5 
                      1                       1                       1 
                   NA's 
                    517 

patient.samples.sample.2.portions.portion.2.slides.slide.bcr_slide_uuid:
21f5bbd4-ee97-4093-875a-47ec7abb9fbe b838e2dc-8869-4c72-9f1d-a066ff307579 
                                   1                                    1 
caa4a154-72d4-4a13-8999-674d5558bd09                                 NA's 
                                   1                                  517 

patient.samples.sample.2.portions.portion.2.slides.slide.image_file_name:
tcga-44-4112-01b-05-bs5.21f5bbd4-ee97-4093-875a-47ec7abb9fbe.svs 
                                                               1 
tcga-44-6146-01b-03-bs3.caa4a154-72d4-4a13-8999-674d5558bd09.svs 
                                                               1 
tcga-44-6147-01b-05-bs5.b838e2dc-8869-4c72-9f1d-a066ff307579.svs 
                                                               1 
                                                            NA's 
                                                             517 

patient.samples.sample.2.portions.portion.2.slides.slide.is_derived_from_ffpe:
 yes NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   5   10   20 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   2    3 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_necrosis:
   0    1    2 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0    1    2 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_normal_cells:
   0    2    5 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_stromal_cells:
  20   31   35 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_tumor_cells:
  60   65   79 NA's 
   1    1    1  517 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_tumor_nuclei:
  65   80 NA's 
   1    2  517 

patient.samples.sample.2.portions.portion.2.slides.slide.section_location:
 top NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.weight:
 400 NA's 
   3  517 

patient.samples.sample.2.portions.portion.2.year_of_creation:
2013 NA's 
   3  517 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.700   1.770   1.742   1.800   1.860     477 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96484633  98890500 103961063 107633929 103961102 167814238       453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   9   13   36 NA's 
  60    1    6  453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.16 0.17 0.18  0.5 NA's 
   3    1    3   60  453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0    20.5    27.0    24.1    27.0    29.0     453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
  60    7  453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   9.000   7.224   9.000   9.000     453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   6.000   6.403   8.000  11.000     453 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.80   40.00   40.00   35.95   40.00   40.00     453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
  60    7  453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   5   20   80 NA's 
   6    1   60  453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2013 2015 NA's 
  60    1    6  453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-38-4630-11a-01w-a315-08 tcga-44-6775-01c-02r-a46x-36 
                           1                            1 
                        NA's 
                         518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
3aa5c9bb-8270-4c9d-b16e-f7dfb4ec8c0f ddda418a-49fc-4658-a7d3-2617b474eb3b 
                                   1                                    1 
                                NA's 
                                 518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
153878502 167814219      NA's 
        1         1       518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   8   36 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.18  0.5 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  15   26 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   7    8 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1    4 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a315 a46x NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   b    h NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
 0.9    5 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   2  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   5   10 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2013 2015 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   9  511 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   9  511 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.136e+08 1.283e+08 2.715e+08 1.457e+08 1.076e+09        15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
  38  452   15   15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.5000  0.5000  0.4631  0.5000  0.5000      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     8.0    21.0    18.1    27.0    30.0      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
 496    9   15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.842   8.000  12.000      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.515  10.000  12.000      15 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.60   40.00   40.00   36.04   40.00   40.00      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 360  145   15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   80.00   80.00   73.25   80.00   80.00      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2014      15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    69.0   176.0   144.6   191.0   262.0     151 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                   451                      1                     53 
                  NA's 
                    15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r    w    x NA's 
  15   38  451    1   15 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.5000  0.5000  0.4495  0.5000  0.5000     151 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 452   68 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
 316  136   68 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             505 
                                            NA's 
                                              15 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no  yes NA's 
 496    9   15 


patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.010   1.110   1.008   1.300   1.700     467 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.300   7.000   7.400   6.859   8.100   9.400     467 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
      202       167       151 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   5  515 

patient.samples.sample.2.portions.portion.analytes.analyte.3.a260_a280_ratio:
 1.5  1.6  1.7 NA's 
   1    1    7  511 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96491266  96491548 103961622 101247981 103961660 103961696       461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   9 NA's 
  59  461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
  59  461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  15   27   29 NA's 
  16   35    8  461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  59  461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   4    6    9 NA's 
  16    8   35  461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   7.000   6.881   8.500  11.000     461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
 931 1070 1260 NA's 
  16    8   35  461 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
  59  461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
  59  461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
  59  461 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  59  461 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.751e+07 1.040e+08 1.040e+08 3.989e+08 1.054e+09 1.054e+09       417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7    8 NA's 
  11   92  417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
0.13 0.14 0.15  0.5 NA's 
   2    4    5   92  417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   22.00   27.00   24.11   29.00   29.00     417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 103  417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   8.000   7.291   9.000  12.000     417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   7.000   7.515   9.000  12.000     417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    930    1259    1259    1480    1906    2180     417 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
1.73 1.86    2   40 NA's 
   2    4    5   92  417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
 103  417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
13.3   80 NA's 
  11   92  417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2012 NA's 
  59   43    1  417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.2    59.0    78.0   101.0   158.0   236.0     417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                    33                     59                     11 
                  NA's 
                   417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   r    w    x NA's 
  11   33   59  417 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration:
0.13 0.14 0.15  0.5 NA's 
   2    4    5   92  417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  92  428 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
  92  428 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             103 
                                            NA's 
                                             417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 103  417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                        8                         3                        33 
                repli-g x                      NA's 
                       59                       417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.100   1.170   1.330   1.282   1.380   1.440     509 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.100   7.350   7.700   7.918   8.350   9.500     509 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
       27        76       417 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   2  518 



patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.140e+08 1.054e+09 6.261e+08 1.054e+09 1.054e+09       509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8 NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   8   25   30 NA's 
   3    6    2  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   1    6    7 NA's 
   2    3    6  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   8    9 NA's 
   9    2  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
1804 1906 2160 NA's 
   3    6    2  509 


patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2011 2012 NA's 
   9    2  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  164.0   182.0   186.0   189.4   196.5   231.0     509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                  11                  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
  11  509 



patient.samples.sample.2.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              11 
                                            NA's 
                                             509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     11     509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen   uv spec      NA's 
        8         3       509 

patient.samples.sample.2.portions.portion.analytes.analyte.4.subportion_sequence:
   1 NA's 
   2  518 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   1.900   1.900   1.919   1.900   3.200     349 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
  11  509 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
  11  509 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.752e+07 1.210e+08 1.323e+08 4.487e+08 1.054e+09 1.076e+09         2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   8.000   5.357   8.000  36.000       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.05000 0.08000 0.09913 0.15000 0.50000       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   18.00   18.22   29.00   30.00       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
 506   12    2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.741  11.000  12.000       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.986   9.000  12.000       2 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.750   1.010   3.000   2.258   3.000  40.000       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 395  123    2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    6.67   40.00   33.57   60.00   80.00       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   9  511 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   9  511 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.070e+08 1.678e+08 4.641e+08 1.054e+09 1.076e+09       243 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   8.000   8.271   9.000  36.000     243 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.05000 0.07000 0.08357 0.14000 0.17000     243 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   11.00   22.00   21.28   29.00   30.00     243 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
 267   10  243 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.552   8.000  12.000     243 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.455   9.000  12.000     243 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.750   1.960   2.240   2.374   3.000   4.270     243 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 200   77  243 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   26.70   28.00   37.14   60.00   60.00     243 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2013    2015     243 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.040e+08 1.136e+08 3.440e+08 1.678e+08 1.076e+09       375 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   6.469   9.000  10.000     375 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1300  0.1400  0.1285  0.1500  0.1700     375 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   10.00   22.00   19.36   27.00   30.00     375 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no  yes NA's 
 137    8  375 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   6.903   9.000  12.000     375 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.497   8.000  12.000     375 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.750   1.500   1.800   1.788   2.130   4.010     375 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
 122   23  375 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   10.00   13.30   16.18   20.00   60.00     375 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2015     375 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.040e+08 1.539e+08 2.869e+08 1.678e+08 1.076e+09       454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    8    9   10 NA's 
   6   29   17   14  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.070   0.135   0.110   0.150   0.170     454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   22.00   26.50   22.86   27.00   29.00     454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  66  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.273   9.000   9.000     454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.742   9.000  12.000     454 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.002   1.070   1.448   1.960   3.400     454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  41   25  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   13.40   16.88   28.00   40.00     454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2012    2012    2013    2015     454 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.194e+08 1.539e+08 2.305e+08 1.539e+08 1.076e+09       489 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   1    8   10 NA's 
   5   25    1  489 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0700  0.0800  0.1039  0.1500  0.1600     489 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   26.00   26.00   23.55   27.00   29.00     489 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  31  489 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   6.000   6.129   6.000   9.000     489 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.129   6.000   8.000     489 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.000   1.070   1.241   1.310   2.000     489 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
  12   19  489 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.670   7.335  13.400  14.160  13.400  40.000     489 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 2012 2013 2015 NA's 
   2   10   18    1  489 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
125245404 125245999 153879497 147818279 157362606 167813352       508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   1    8    9   10 NA's 
   4    5    1    2  508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0800  0.1025  0.1425  0.1700     508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  21   22   26   29 NA's 
   1    2    5    4  508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  12  508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1    4    6    8 NA's 
   1    2    5    4  508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.75    6.00    5.00    6.00    8.00     508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
2250 a313 a46a a46h NA's 
   4    5    1    2  508 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.070   1.095   1.281   1.240   2.240     508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
   4    8  508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   8 13.4   28 NA's 
   4    6    2  508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2012 2013 2015 NA's 
   4    5    3  508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-64-1680-10a-01d-a313-08                         NA's 
                           1                          519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
99cec84b-e353-4507-8f5e-0d3a14cd961c                                 NA's 
                                   1                                  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
153879511      NA's 
        1       519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   8 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.08 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  26 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   6 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   8 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
a313 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   d NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
1.07 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  23 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
13.4 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2013 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-64-1680-10a-01d-a46a-09                         NA's 
                           1                          519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
313d0c72-f165-450f-984f-6251c667d980                                 NA's 
                                   1                                  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
167811811      NA's 
        1       519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   9 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.08 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
  21 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   1 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   5 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
a46a NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   c NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
1.07 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  23 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
13.4 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2015 NA's 
   1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
  12  508 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
  12  508 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
9.649e+07 1.210e+08 1.323e+08 4.656e+08 1.054e+09 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   7   8 
310 174   2  34 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0600  0.1400  0.1500  0.1476  0.1600  0.5000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   18.00   18.03   29.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no yes 
506  14 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.727  11.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   6.977   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.000   1.130   1.453   1.730  40.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
395 125 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67    6.67    6.70   10.28   13.30   80.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
  62  194  207   33   24 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   39.52  124.35  185.91  268.27 1294.60 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
                 dna repli-g (qiagen) dna                  rna 
                 517                    1                    2 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d   r   w 
517   2   1 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1200  0.1400  0.1500  0.1524  0.1600  0.5000 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 518    2 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 372  146    2 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             520 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no yes 
506  14 


patient.samples.sample.2.portions.portion.analytes.analyte.rna.ratio_28s_18s:
   0 2.53 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.rna.rinvalue:
 2.4  2.5 NA's 
   1    1  518 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      348       172 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
  13  507 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   18.00   17.73   27.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no yes 
506  14 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.517  10.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   1.327   1.000  21.000 

patient.samples.sample.2.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.252   1.000   6.000     409 




patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no  yes NA's 
  28   12  480 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.25    5.00   11.36   15.00   50.00     506 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.250   2.000   5.786   4.500  50.000     506 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.500   4.286   8.000  15.000     506 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0    1    2    3 NA's 
   7    1    2    4  506 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.286   2.000   5.000     506 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   19.25   21.50   22.93   26.50   42.00     506 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   55.0    62.5    70.0    71.5    78.0    99.0     506 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  65.00   70.00   72.50   73.93   80.00   85.00     506 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    10     30    480 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no  yes NA's 
 100   12  408 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.000   4.000   8.345  10.000  50.000     491 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.000   5.000   7.862  10.000  40.000     491 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    2.88    3.00   20.00     495 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.793   2.000  30.000     491 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     5.0    30.8    60.0   100.0     495 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    15.0    25.0    25.8    40.0    50.0     495 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   55.00   40.52   75.00  100.00     495 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    65.0    41.2    75.0    80.0     495 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
bottom    top   NA's 
    30     82    408 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00   33.50   70.58   73.00  400.00     340 

patient.samples.sample.2.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2009    2011    2011    2011    2012    2014 

patient.samples.sample.2.sample_type:
blood derived normal         ffpe scrolls        primary tumor 
                 406                    2                   12 
     recurrent tumor  solid tissue normal 
                   2                   98 

patient.samples.sample.2.sample_type_id:
  1   2  10  11 
 14   2 406  98 

patient.samples.sample.2.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.4000  0.3775  0.5000  0.9000     431 

patient.samples.sample.2.vial_number:
  a   b   c   d 
503  14   2   1 



patient.samples.sample.3.days_to_collection:
 597  849 NA's 
   1    1  518 

patient.samples.sample.3.initial_weight:
  40 NA's 
   1  519 

patient.samples.sample.3.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.4000  0.5000  0.7000  0.7287  0.9000  1.5000     419 

patient.samples.sample.3.is_ffpe:
  no  yes NA's 
 115    2  403 

patient.samples.sample.3.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   0.800   1.000   1.186   1.500   2.700     419 

patient.samples.sample.3.oct_embedded:
false  NA's 
    2   518 

patient.samples.sample.3.pathology_report_file_name:
tcga-93-8067.24b499ff-c99b-4d57-ac96-8978d0a44538.pdf 
                                                    1 
tcga-93-a4jp.3afdf9a9-8d19-4060-86e0-b24ae8c83a82.pdf 
                                                    1 
                                                 NA's 
                                                  518 

patient.samples.sample.3.pathology_report_uuid:
24b499ff-c99b-4d57-ac96-8978d0a44538 3afdf9a9-8d19-4060-86e0-b24ae8c83a82 
                                   1                                    1 
                                NA's 
                                 518 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.5  1.7  1.8 NA's 
   1    3   11  505 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.648e+07 9.649e+07 1.054e+09 6.438e+08 1.054e+09 1.054e+09       506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    9 NA's 
   8    6  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 0.16  0.5 NA's 
   2    4    2    6  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  11   15   29 NA's 
   8    5    1  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  14  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   4    5    6 NA's 
   5    8    1  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    2    4    5 NA's 
   6    2    1    5  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
 929 1069 1758 NA's 
   5    1    8  506 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.320   1.895   2.065  18.204  40.000  40.000     506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
  14  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
 8.8 10.5 13.3   80 NA's 
   1    1    6    6  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   6    8  506 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.210e+08 1.283e+08 3.682e+08 1.054e+09 1.076e+09       409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   3   74   34  409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.160   0.500   0.382   0.500   0.500     409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     7.0     9.0    14.2    23.0    30.0     409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 111  409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   5.000   5.775   7.000  12.000     409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.631   7.000  11.000     409 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.73    3.00   40.00   27.60   40.00   40.00     409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
  95   16  409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
13.3   15   20   80 NA's 
   3    2   32   74  409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 NA's 
   6   38   51   16  409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    50.0   162.0   121.9   184.5   246.0     425 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                  74                   37                  409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r    w NA's 
  26   11   74  409 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.5000  0.3621  0.5000  0.5000     425 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  74  446 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
  58   16  446 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             111 
                                            NA's 
                                             409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 111  409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                       11                        26                        58 
                repli-g x                      NA's 
                       16                       409 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.900   1.110   1.290   1.323   1.480   2.030     483 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    7.50    7.80    7.84    8.10    8.90     483 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
       46        49       425 

patient.samples.sample.3.portions.portion.analytes.analyte.3.a260_a280_ratio:
 1.7  1.8 NA's 
   3    1  516 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
96491257 96491490 96491522 96890942 96491530 98888338      514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   9 NA's 
   6  514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   6  514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  15   29 NA's 
   5    1  514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   6  514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   4    6 NA's 
   5    1  514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   4    5 NA's 
   1    5  514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
 931 1070 NA's 
   5    1  514 


patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
   6  514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   6  514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
   6  514 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   6  514 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.751e+07 1.210e+08 1.210e+08 2.495e+08 1.210e+08 1.054e+09       485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7    8 NA's 
   4   31  485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
0.14 0.15  0.5 NA's 
   2    2   31  485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   15.00   27.00   21.74   28.00   29.00     485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  35  485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   6.000   7.771  12.000  12.000     485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0     5.0     5.0     5.6     6.0     9.0     485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    930    1831    2107    1863    2107    2180     485 


patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
1.86    2   40 NA's 
   2    2   31  485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
  35  485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
13.3   80 NA's 
   4   31  485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2012 NA's 
   6   23    6  485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.2    82.5   153.0   139.6   194.5   219.0     485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                    25                      6                      4 
                  NA's 
                   485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   r    w    x NA's 
   4   25    6  485 



patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration:
0.14 0.15  0.5 NA's 
   2    2   31  485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  31  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
  31  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              35 
                                            NA's 
                                             485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  35  485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                        3                         1                        25 
                repli-g x                      NA's 
                        6                       485 

patient.samples.sample.3.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
0.98 1.17 1.26 1.33 NA's 
   1    1    1    1  516 

patient.samples.sample.3.portions.portion.analytes.analyte.3.rna.rinvalue:
 7.1  7.3  7.9  8.3 NA's 
   1    1    1    1  516 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
       23        12       485 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-44-6144-11a-01w-1804-08 tcga-44-6776-11a-01w-1906-08 
                           1                            1 
tcga-44-6777-11a-01w-1906-08 tcga-44-6778-11a-01w-1906-08 
                           1                            1 
                        NA's 
                         516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
3e613163-8424-4e01-9bdb-ab5e5966057d 7e4c3802-1953-4ad8-8ef4-e3c92343b2d1 
                                   1                                    1 
88d43546-6e08-48a5-87eb-fac7c1e2fb4c b7b23920-f849-47cc-a08c-2003c85e4dbf 
                                   1                                    1 
                                NA's 
                                 516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
 107042109 1053936754 1053936765 1053936825       NA's 
         1          1          1          1        516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8 NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   8   25 NA's 
   1    3  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   6    7 NA's 
   1    3  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   6    7 NA's 
   1    3  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
1804 1906 NA's 
   1    3  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   a    b    d    g NA's 
   1    1    1    1  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2011 NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.amount:
 162  204  209  210 NA's 
   1    1    1    1  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   4                  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.bcr_analyte_barcode:
tcga-44-6144-11a-01w tcga-44-6776-11a-01w tcga-44-6777-11a-01w 
                   1                    1                    1 
tcga-44-6778-11a-01w                 NA's 
                   1                  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.bcr_analyte_uuid:
16157001-4dfd-4248-ac7f-8d4244a930c8 1d5cdecc-5937-4ace-9df1-bcb309477e9f 
                                   1                                    1 
6caeb7a7-e60c-4967-87d9-c4bfd6ede125 a8fe0446-3b6a-4f8b-a75c-25b18a56d313 
                                   1                                    1 
                                NA's 
                                 516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   4  516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      4     516 

patient.samples.sample.3.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen   uv spec      NA's 
        3         1       516 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9    2 NA's 
   1   32    2  485 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   2  518 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   2  518 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             518 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.210e+08 1.252e+08 5.458e+08 1.062e+09 1.076e+09       405 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    5    8 NA's 
  28    1   86  405 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.05    0.05    0.05    0.08    0.14    0.17     405 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   18.00   18.15   28.00   30.00     405 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 115  405 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.791   9.000  12.000     405 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.391   6.000  10.000     405 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.070   3.000   2.437   3.000   3.740     405 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 113    2  405 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7 26.7   60 NA's 
  26    7    1   81  405 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 NA's 
   7   43   63    2  405 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.161e+08 1.678e+08 4.550e+08 1.050e+09 1.076e+09       465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   8.000   8.164  10.000  10.000     465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0700  0.0896  0.1400  0.1700     465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   22.00   20.15   28.00   30.00     465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  55  465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   4.000   4.218   5.000   8.000     465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     4.0     6.0     6.4     9.0    12.0     465 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.900   1.960   2.240   2.459   3.000   4.270     465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
  34   21  465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   26.70   28.00   33.14   40.00   60.00     465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2012 2015 NA's 
   6   26    2   21  465 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   6  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   6  514 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              6             514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.070e+08 1.322e+08 5.685e+08 1.050e+09 1.076e+09       489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   1    5    8    9 NA's 
   2   10   17    2  489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0800  0.0961  0.1400  0.1700     489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   14.00   16.71   21.50   30.00     489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  31  489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.871   6.000   8.000     489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   5.000   4.968   6.500   8.000     489 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.730   2.000   1.983   2.000   3.740     489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
  25    6  489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.30   20.00   25.26   40.00   40.00     489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2012 2013 NA's 
   3   21    1    6  489 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   7  513 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   7  513 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              7             513 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.322e+08 1.050e+09 6.145e+08 1.050e+09 1.076e+09       497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    8    9   10 NA's 
   9    3    9    2  497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0800  0.0996  0.1550  0.1700     497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0    14.0    14.0    16.3    14.0    29.0     497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  23  497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   3.000   3.609   4.000   8.000     497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.500   7.000   6.391   8.000   9.000     497 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.070   1.600   1.562   2.000   2.240     497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  14    9  497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   28.00   23.23   40.00   40.00     497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 2012 2013 2015 NA's 
  11    3    7    2  497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   3  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   3  517 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.252e+08 1.322e+08 1.322e+08 3.843e+08 6.109e+08 1.076e+09       505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   1    5    8   10 NA's 
   5    1    8    1  505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.07    0.08    0.08    0.11    0.15    0.17     505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     7.0    14.0    14.6    24.0    29.0     505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  15  505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   7.000   6.267   8.000   8.000     505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     2.0     4.0     3.8     4.5     7.0     505 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.070   1.600   1.681   1.600   6.000     505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
   5   10  505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    7.35   20.00   16.63   20.00   40.00     505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 2012 2013 2015 NA's 
   3    2    5    5  505 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.disease_code:
fppp NA's 
   3  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.project_code:
tcga NA's 
   3  517 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
128925347 132159110 132160313 137566204 132174526 167813281       514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   1    8   10 NA's 
   2    3    1  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.08 0.09 0.15 0.16 NA's 
   3    1    1    1  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   3   14   22 NA's 
   2    3    1  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   6  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   3    4    8 NA's 
   3    1    2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   1    2    7    8 NA's 
   1    2    2    1  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
a271 a273 a46h a46w NA's 
   1    2    1    2  514 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   1.202   1.600   1.553   1.750   2.240     514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
   6  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
 6.7   20   28 NA's 
   2    3    1  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2013 2015 NA's 
   3    3  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-44-2665-10a-01d-a273-01 tcga-44-2666-10a-01d-a46w-08 
                           1                            1 
tcga-44-4112-10a-01d-a46h-10                         NA's 
                           1                          517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
d59902b3-f49d-4561-9a7c-6b2545588073 e6006192-08d8-4698-b8c2-d31c8d8fe442 
                                   1                                    1 
efcea0be-c3d0-49a5-9eec-b379afbd63f6                                 NA's 
                                   1                                  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
132159758 132179182 167813354      NA's 
        1         1         1       517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   1    8   10 NA's 
   1    1    1  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.08 0.17 NA's 
   2    1  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   3   14   22 NA's 
   1    1    1  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   3  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   3    4    8 NA's 
   1    1    1  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   1    3    8 NA's 
   1    1    1  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
a273 a46h a46w NA's 
   1    1    1  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   b    f    g NA's 
   1    1    1  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
1.14  1.6 2.24 NA's 
   1    1    1  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  23 NA's 
   3  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
 6.7   20   28 NA's 
   1    1    1  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2013 2015 NA's 
   1    2  517 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-44-4112-10a-01d-a46w-08                         NA's 
                           1                          519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
0a4ed026-3387-438e-8d61-82da7f187a84                                 NA's 
                                   1                                  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
132179159      NA's 
        1       519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   8 NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.08 NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   3 NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   8 NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   3 NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
a46w NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   b NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
 1.6 NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  23 NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
  20 NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2015 NA's 
   1  519 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   2  518 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   2  518 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             518 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.210e+08 1.252e+08 5.392e+08 1.062e+09 1.076e+09       403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    8 NA's 
  86   27    4  403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1447  0.1500  0.1700     403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   18.00   17.69   27.00   30.00     403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
 115    2  403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.761   9.000  12.000     403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.342   6.000  10.000     403 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   0.930   1.000   1.201   1.400   2.130     403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 113    4  403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67    8.46   10.00   20.00     403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2014     403 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.3   134.0   222.7   236.5   320.3   580.6     403 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
 117  403 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
 117  403 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1473  0.1500  0.1700     403 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 117  403 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 107   10  403 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             117 
                                            NA's 
                                             403 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no  yes NA's 
 115    2  403 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
        adna preparation type              allprep ffpe dna 
                          101                             2 
chemical lysis dna extraction                          NA's 
                           14                           403 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       82        35       403 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   13.00   12.81   21.00   30.00     403 

patient.samples.sample.3.portions.portion.is_ffpe:
  no  yes NA's 
 115    2  403 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.889  10.000  12.000     403 

patient.samples.sample.3.portions.portion.portion_number:
   1 NA's 
 117  403 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
   1  519 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-38-4625-11a-01-ts1 tcga-38-4626-11a-01-ts1 tcga-38-4627-11a-01-ts1 
                      1                       1                       1 
tcga-44-3919-11a-01-ts1                    NA's 
                      1                     516 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_uuid:
23f1d37a-1192-45ab-aae3-7404cd5c54df 30a90255-23fa-4666-89d2-e8f491c1fcc9 
                                   1                                    1 
8dfd2a41-54ff-44b2-9bb7-881cc4228d7f ec7af6b6-9a36-4f33-b2b2-1fcb7092ae5a 
                                   1                                    1 
                                NA's 
                                 516 

patient.samples.sample.3.portions.portion.slides.slide.2.image_file_name:
tcga-38-4625-11a-01-ts1.8dfd2a41-54ff-44b2-9bb7-881cc4228d7f.svs 
                                                               1 
tcga-38-4626-11a-01-ts1.ec7af6b6-9a36-4f33-b2b2-1fcb7092ae5a.svs 
                                                               1 
tcga-38-4627-11a-01-ts1.30a90255-23fa-4666-89d2-e8f491c1fcc9.svs 
                                                               1 
tcga-44-3919-11a-01-ts1.23f1d37a-1192-45ab-aae3-7404cd5c54df.svs 
                                                               1 
                                                            NA's 
                                                             516 

patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   4  516 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location:
 top NA's 
   4  516 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
 101  419 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   7.615  10.000  20.000     507 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   15.00   20.00   18.77   30.00   30.00     507 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   1  519 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   9  511 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
   0 NA's 
   1  519 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
  10 NA's 
   1  519 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
  90 NA's 
   1  519 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
  80 NA's 
   1  519 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     4     97    419 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   59.00   64.00   63.78   69.00  133.00     420 

patient.samples.sample.3.portions.portion.year_of_creation:
2010 2011 2012 2014 NA's 
  18   40   57    2  403 

patient.samples.sample.3.sample_type:
blood derived normal         ffpe scrolls  solid tissue normal 
                  14                    2                  101 
                NA's 
                 403 

patient.samples.sample.3.sample_type_id:
   1   10   11 NA's 
   2   14  101  403 

patient.samples.sample.3.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.4000  0.4733  0.6000  1.0000     419 

patient.samples.sample.3.vial_number:
   a    b    d    e NA's 
 114    1    1    1  403 



patient.samples.sample.4.days_to_collection:
  72 NA's 
   1  519 

patient.samples.sample.4.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  400.0   527.5   590.0   613.8   705.0   860.0     512 

patient.samples.sample.4.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.6000  0.8000  0.9000  0.8667  1.0000  1.0000     508 

patient.samples.sample.4.is_ffpe:
  no NA's 
  14  506 

patient.samples.sample.4.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.800   1.425   1.700   1.667   2.000   2.400     508 

patient.samples.sample.4.oct_embedded:
false  NA's 
    1   519 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   7  513 



patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
heterogeneity          NA's 
            7           513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
145631330 145632111 145632677 145632785 145633380 145634503       513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   8 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.07 0.08 NA's 
   2    5  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  25 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   3 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   2    4 NA's 
   6    1  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
a27t NA's 
   7  513 


patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
 2.8  3.2 NA's 
   2    5  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  40 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.74   10.17   26.86   21.08   29.45   34.68     513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.analyte_type:
 dna NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.analyte_type_id:
   d NA's 
   7  513 



patient.samples.sample.4.portions.portion.2.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 NA's 
   1    1    4    1  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    7                   513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        7       513 

patient.samples.sample.4.portions.portion.2.analytes.analyte.subportion_sequence:
   1 NA's 
   7  513 



patient.samples.sample.4.portions.portion.2.day_of_creation:
  14   25 NA's 
   6    1  513 

patient.samples.sample.4.portions.portion.2.is_ffpe:
  no NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.month_of_creation:
   2 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.portion_number:
  21   31 NA's 
   6    1  513 

patient.samples.sample.4.portions.portion.2.portion_sequence:
   2    3 NA's 
   6    1  513 




patient.samples.sample.4.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   1    2    5   20 NA's 
   1    4    1    1  513 

patient.samples.sample.4.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   2    5    7   20 NA's 
   3    2    1    1  513 

patient.samples.sample.4.portions.portion.2.slides.slide.percent_necrosis:
   0 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0   40 NA's 
   6    1  513 

patient.samples.sample.4.portions.portion.2.slides.slide.percent_normal_cells:
 100 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.slides.slide.percent_stromal_cells:
   0 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.slides.slide.percent_tumor_cells:
   0 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.slides.slide.percent_tumor_nuclei:
   0 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.slides.slide.section_location:
 top NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.weight:
  30 NA's 
   7  513 

patient.samples.sample.4.portions.portion.2.year_of_creation:
2013 NA's 
   7  513 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   6  514 



patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
heterogeneity          NA's 
            6           514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
145632540 145643766 145671476 145659635 145673291 145673892       514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.center_id:
   8 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.concentration:
0.07 0.08 NA's 
   2    4  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.day_of_shipment:
  25 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.month_of_shipment:
   3 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_column:
   4 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_id:
a27t NA's 
   6  514 


patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.quantity:
 2.8  3.2 NA's 
   2    4  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.volume:
  40 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.21   10.52   20.47   18.49   26.91   28.51     514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.analyte_type:
 dna NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.analyte_type_id:
   d NA's 
   6  514 



patient.samples.sample.4.portions.portion.3.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 NA's 
   2    1    2    1  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               6 
                                            NA's 
                                             514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    6                   514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        6       514 

patient.samples.sample.4.portions.portion.3.analytes.analyte.subportion_sequence:
   1 NA's 
   6  514 



patient.samples.sample.4.portions.portion.3.day_of_creation:
  25   26 NA's 
   5    1  514 

patient.samples.sample.4.portions.portion.3.is_ffpe:
  no NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.month_of_creation:
   2 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.portion_number:
  31   51 NA's 
   5    1  514 

patient.samples.sample.4.portions.portion.3.portion_sequence:
   3    5 NA's 
   5    1  514 




patient.samples.sample.4.portions.portion.3.slides.slide.is_derived_from_ffpe:
  no NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.slides.slide.percent_lymphocyte_infiltration:
   5   20   40 NA's 
   1    4    1  514 

patient.samples.sample.4.portions.portion.3.slides.slide.percent_monocyte_infiltration:
  10   20 NA's 
   1    5  514 

patient.samples.sample.4.portions.portion.3.slides.slide.percent_necrosis:
   0 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.slides.slide.percent_neutrophil_infiltration:
   1   20   40 NA's 
   1    1    4  514 

patient.samples.sample.4.portions.portion.3.slides.slide.percent_normal_cells:
 100 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.slides.slide.percent_stromal_cells:
   0 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.slides.slide.percent_tumor_cells:
   0 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.slides.slide.percent_tumor_nuclei:
   0 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.slides.slide.section_location:
 top NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.weight:
  30 NA's 
   6  514 

patient.samples.sample.4.portions.portion.3.year_of_creation:
2013 NA's 
   6  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.7  1.8 NA's 
   2    4  514 



patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 9.889e+07 5.764e+08 5.764e+08 1.054e+09 1.054e+09       514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    9 NA's 
   3    3  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 0.16  0.5 NA's 
   1    1    1    3  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  11   29 NA's 
   3    3  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   6  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   5    6 NA's 
   3    3  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    2    5 NA's 
   1    2    3  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
1069 1070 1758 NA's 
   2    1    3  514 


patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
1.47    2 2.13   40 NA's 
   1    1    1    3  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   6  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
10.5 13.3   80 NA's 
   1    2    3  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   3    3  514 



patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 1.050e+08 1.054e+09 6.612e+08 1.054e+09 1.062e+09       508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   3    6    3  508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.14 0.15 0.16  0.5 NA's 
   2    2    2    6  508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   12.50   28.00   22.75   29.00   30.00     508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  12  508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    5.00    6.00    5.25    6.00    7.00     508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   5.000   5.333   6.750  11.000     508 



patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.86    2.88   22.14   21.40   40.00   40.00     508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
  11    1  508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.30   19.50   53.35   49.27   80.00   80.00     508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 NA's 
   3    7    1    1  508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    9.00   47.20   63.51   80.00  205.00     509 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                     5                      1                      6 
                  NA's 
                   508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w    x NA's 
   6    5    1  508 



patient.samples.sample.4.portions.portion.analytes.analyte.2.concentration:
0.14 0.15 0.16  0.5 NA's 
   2    2    2    5  509 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   6  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
   5    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              12 
                                            NA's 
                                             508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  12  508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g              repli-g x 
                     6                      5                      1 
                  NA's 
                   508 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.120   1.407   1.665   1.580   1.810   1.850     514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.600   7.725   7.950   7.983   8.250   8.400     514 

patient.samples.sample.4.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
        2         9       509 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-44-3918-11a-01x-1070-09 tcga-44-4112-11a-01x-1070-09 
                           1                            1 
                        NA's 
                         518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
50fd623e-58be-49d7-9447-4f4935d95acd c55da949-9871-4f36-b7d2-dc8cc7a38f2b 
                                   1                                    1 
                                NA's 
                                 518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
98888323 98888347     NA's 
       1        1      518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   9 NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  29 NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   5 NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1070 NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   d    f NA's 
   1    1  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
   2  518 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   2  518 



patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 98891107  98891131 107042072 104110512 107042085 108686165       515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   8   27   29 NA's 
   2    1    2  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   4    6 NA's 
   1    4  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   5    6    7 NA's 
   2    1    2  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
1068 1711 1804 NA's 
   2    1    2  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   d    f    g NA's 
   2    2    1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    3  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   46.0    60.0   178.0   147.2   215.0   237.0     515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                     3                      2                    515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
   3    2  515 



patient.samples.sample.4.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   5  515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
        3         2       515 

patient.samples.sample.4.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      5     515 

patient.samples.sample.4.portions.portion.analytes.analyte.a260_a280_ratio:
 1.9 NA's 
  10  510 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 1.070e+08 1.050e+09 6.173e+08 1.050e+09 1.054e+09       507 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    5    8    9 NA's 
   8    2    2    1  507 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1600  0.1685  0.1700  0.5000     507 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   14.00   14.00   17.08   23.00   30.00     507 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  13  507 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   5.000   4.923   6.000  12.000     507 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   6.000   6.231   9.000  11.000     507 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.000   1.070   4.878   3.200  40.000     507 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
  12    1  507 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67   22.06   26.70   80.00     507 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 NA's 
   3    8    2  507 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.888e+07 3.428e+08 1.050e+09 7.665e+08 1.052e+09 1.054e+09       510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8 NA's 
   4    6  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 0.15 0.16 0.17 NA's 
   4    2    1    3  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  11   14   29   30 NA's 
   2    6    1    1  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  10  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    4.00    4.10    5.75    7.00     510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.00    6.00    6.30    8.75    9.00     510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1105    1551    1551    1540    1721    1855     510 


patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.700   2.250   3.000   3.123   4.010   4.540     510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
  10  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10 26.7   40   60 NA's 
   2    4    1    3  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
   2    8  510 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 3.428e+08 1.050e+09 7.665e+08 1.052e+09 1.054e+09       510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8    9 NA's 
   4    4    2  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.050   0.135   0.113   0.160   0.170     510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  11   14   29   30 NA's 
   2    6    1    1  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  10  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    4.00    4.10    5.75    7.00     510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   3    4    5    6 NA's 
   2    5    1    2  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1106    1553    1553    1542    1724    1856     510 


patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.140   1.895   2.000   1.987   2.098   3.470     510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
  10  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
 6.7 13.3 26.7   40 NA's 
   2    3    1    4  510 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
   2    8  510 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.050e+09 1.050e+09 1.050e+09 1.051e+09 1.053e+09 1.054e+09       514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    9 NA's 
   2    4  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.05 0.17 NA's 
   4    2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  11   14 NA's 
   2    4  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   6  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2    7 NA's 
   4    2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   2    3    8 NA's 
   1    1    4  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
1555 1877 NA's 
   4    2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   a    e    f    g NA's 
   2    1    1    2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
1.13    2 NA's 
   2    4  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   6  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
6.67   40 NA's 
   2    4  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 NA's 
   6  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-44-4112-11a-01d-a46h-10                         NA's 
                           1                          519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
8953d8f4-8959-4511-9fd4-975f35c8b99b                                 NA's 
                                   1                                  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
167813349      NA's 
        1       519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
  10 NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.09 NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  22 NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   4 NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   3 NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a46h NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   c NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
2.52 NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  28 NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2015 NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
heterogeneity          NA's 
            1           519 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 1.123e+08 5.978e+08 5.836e+08 1.050e+09 1.054e+09       506 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    8 NA's 
   2   10    2  506 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1500  0.1600  0.1764  0.1700  0.5000     506 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   14.00   14.00   17.64   24.50   30.00     506 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  14  506 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.250   5.000   4.786   6.000  12.000     506 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.250   5.500   6.143   8.750  11.000     506 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.130   1.798   2.130   4.669   2.260  40.000     506 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
  12    2  506 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   18.79   13.30   80.00     506 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 NA's 
   3    8    2    1  506 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.32   59.40   83.86  152.42  190.53  555.93     506 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
                 dna repli-g (qiagen) dna                 NA's 
                  13                    1                  506 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d    w NA's 
  13    1  506 



patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1814  0.1700  0.5000     506 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  14  506 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
  11    3  506 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              14 
                                            NA's 
                                             506 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  14  506 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
        adna preparation type chemical lysis dna extraction 
                           12                             1 
                      repli-g                          NA's 
                            1                           506 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
        3        11       506 

patient.samples.sample.4.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   1  519 



patient.samples.sample.4.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.25   16.00   16.14   21.25   30.00     506 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
  14  506 

patient.samples.sample.4.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000  11.000   7.786  11.750  12.000     506 

patient.samples.sample.4.portions.portion.portion_number:
   1   21 NA's 
  13    1  506 

patient.samples.sample.4.portions.portion.portion_sequence:
   1    2 NA's 
   4    1  515 

patient.samples.sample.4.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-44-3917-11a-01-ts1 tcga-44-3918-11a-01-ts1 tcga-44-4112-11a-01-ts1 
                      1                       1                       1 
                   NA's 
                    517 

patient.samples.sample.4.portions.portion.slides.slide.2.bcr_slide_uuid:
00b8ad90-0c01-4613-8347-7e476263fdfe 573e21b7-6d32-4c24-a09e-97fcbc5f2eaa 
                                   1                                    1 
a4fa986d-8039-4994-a1ce-b99403de19b5                                 NA's 
                                   1                                  517 

patient.samples.sample.4.portions.portion.slides.slide.2.image_file_name:
tcga-44-3917-11a-01-ts1.00b8ad90-0c01-4613-8347-7e476263fdfe.svs 
                                                               1 
tcga-44-3918-11a-01-ts1.a4fa986d-8039-4994-a1ce-b99403de19b5.svs 
                                                               1 
tcga-44-4112-11a-01-ts1.573e21b7-6d32-4c24-a09e-97fcbc5f2eaa.svs 
                                                               1 
                                                            NA's 
                                                             517 

patient.samples.sample.4.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   3  517 

patient.samples.sample.4.portions.portion.slides.slide.2.section_location:
 top NA's 
   3  517 




patient.samples.sample.4.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  12  508 

patient.samples.sample.4.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   4 NA's 
   1  519 

patient.samples.sample.4.portions.portion.slides.slide.percent_monocyte_infiltration:
  10 NA's 
   1  519 

patient.samples.sample.4.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   1  519 

patient.samples.sample.4.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   1  519 

patient.samples.sample.4.portions.portion.slides.slide.percent_normal_cells:
 100 NA's 
   1  519 

patient.samples.sample.4.portions.portion.slides.slide.percent_stromal_cells:
   0 NA's 
   1  519 

patient.samples.sample.4.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   1  519 

patient.samples.sample.4.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   1  519 

patient.samples.sample.4.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     3      9    508 

patient.samples.sample.4.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00   36.00   37.69   67.00   69.00     507 

patient.samples.sample.4.portions.portion.year_of_creation:
2010 2011 2012 2013 NA's 
   4    4    5    1  506 

patient.samples.sample.4.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   2                   12                  506 

patient.samples.sample.4.sample_type_id:
  10   11 NA's 
   2   12  506 

patient.samples.sample.4.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.4750  0.7500  0.6833  0.9000  1.0000     508 

patient.samples.sample.4.vial_number:
   a NA's 
  14  506 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   98.75  222.50 1073.76 1788.25 6271.00     408 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10:
930e7dc2-6073-4bf2-9b70-f17c0b26bdee                                 NA's 
                                   1                                  519 






patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
1f758560-85e2-4cf9-a81a-317536d96bcc 4bdde497-03d9-4e15-a00e-595baa3947b5 
                                   1                                    1 
9f722476-622e-4374-9fda-ef35184f01e0 a04b36c3-9cfd-4195-92c0-70046cd49d9a 
                                   1                                    1 
                                NA's 
                                 516 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.8:
b9fb358a-9618-4956-9544-c2ffe2678095 c54237a3-93f4-432f-90f1-b930b7341741 
                                   1                                    1 
                                NA's 
                                 518 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
bf1e9d28-3ca2-4a62-bf3e-3e735c6a2885                                 NA's 
                                   1                                  519 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     10      90     160     195     230     940     400 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.6000  0.7000  0.7667  0.9000  2.0000     115 

patient.samples.sample.is_ffpe:
 no 
520 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.100   0.900   1.100   1.183   1.500   3.200     115 

patient.samples.sample.oct_embedded:
false  true  NA's 
   28    84   408 



patient.samples.sample.portions.portion.2.analytes.analyte.2.a260_a280_ratio:
 1.8 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-05-4390-01a-02r-1755-07                         NA's 
                           1                          519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
6b6d7855-8621-45f8-98c9-5ad5b924b15e                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
1053958838       NA's 
         1        519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.center_id:
   7 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.concentration:
0.13 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  11 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   5 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_column:
   1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_id:
1755 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_row:
   b NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.quantity:
1.73 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.volume:
13.3 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.amount:
213.2  NA's 
    1   519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type:
 rna NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type_id:
   r NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.bcr_analyte_barcode:
tcga-05-4390-01a-02r                 NA's 
                   1                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.bcr_analyte_uuid:
806db4e7-8bd8-4330-a435-15142332571f                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.concentration:
0.13 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                   NA's 
                     1                    519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.ratio_28s_18s:
1.44 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.rinvalue:
 8.5 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      1     519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-05-4390-01a-02t-1754-13                         NA's 
                           1                          519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
7b6a78e4-ba6e-4e12-a12a-49b712229e44                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
1053958447       NA's 
         1        519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.center_id:
  13 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  11 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_column:
   1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_id:
1754 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_row:
   b NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.quantity:
   3 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.volume:
  30 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.amount:
347.5  NA's 
    1   519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type:
total rna      NA's 
        1       519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type_id:
   t NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.bcr_analyte_barcode:
tcga-05-4390-01a-02t                 NA's 
                   1                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.bcr_analyte_uuid:
031e0734-7c08-4b23-8789-6b4234ca4aa6                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.concentration:
 0.1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
total rna      NA's 
        1       519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.ratio_28s_18s:
1.19 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.rinvalue:
 7.1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      1     519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-05-4390-01a-02w-1804-08                         NA's 
                           1                          519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
7a36fec9-89cc-450c-8c14-c6279de5ba7e                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
107042126      NA's 
        1       519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.center_id:
   8 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   8 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   6 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_column:
   1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_id:
1804 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_row:
   c NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.amount:
 196 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_barcode:
tcga-05-4390-01a-02w                 NA's 
                   1                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_uuid:
494a397f-99aa-45be-be6d-61281130dda9                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.concentration:
 0.5 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     519 

patient.samples.sample.portions.portion.2.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      1     519 

patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio:
 1.9 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-05-4390-01a-02d-1752-01                         NA's 
                           1                          519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
1c3f76ca-a0f5-4b4c-8438-7eea1c5e161f                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
1053945527       NA's 
         1        519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.14 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  11 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   5 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
1752 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   b NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
0.93 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
6.67 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-05-4390-01a-02d-1753-08                         NA's 
                           1                          519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
d0854b5b-69be-4b84-aa37-ecdd0bc14de9                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
1053958255       NA's 
         1        519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
   8 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  11 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   5 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
1753 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_row:
   b NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
   3 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
  60 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-05-4390-01a-02d-1756-05                         NA's 
                           1                          519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
c511fed1-96ca-4c82-bf7d-b73c2b14e48a                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
1053958351       NA's 
         1        519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id:
   5 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration:
0.14 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  11 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   5 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column:
   1 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id:
1756 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_row:
   b NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity:
1.86 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume:
13.3 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 NA's 
   1  519 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   7  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   7  513 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
heterogeneity          NA's 
            7           513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.456e+08 1.456e+08 1.457e+08 2.592e+08 1.457e+08 1.054e+09       512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   2    8 NA's 
   1    7  512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.07 0.08 0.14 NA's 
   6    1    1  512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  11   25 NA's 
   1    7  512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   8  512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   3    5 NA's 
   7    1  512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   1 NA's 
   8  512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
1751 a27t NA's 
   1    7  512 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
1.86  2.8  3.2 NA's 
   1    6    1  512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
   1    7  512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
13.3   40 NA's 
   1    7  512 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2013 NA's 
   1    7  512 

patient.samples.sample.portions.portion.2.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.83   18.40   40.87  131.43   46.28  827.40     512 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
 dna NA's 
   8  512 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   d NA's 
   8  512 



patient.samples.sample.portions.portion.2.analytes.analyte.concentration:
0.14 0.15 0.16 NA's 
   5    2    1  512 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   8  512 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
   1    7  512 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               8 
                                            NA's 
                                             512 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   8  512 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    8                   512 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
        7         1       512 

patient.samples.sample.portions.portion.2.analytes.analyte.subportion_sequence:
   1    2 NA's 
   5    2  513 



patient.samples.sample.portions.portion.2.day_of_creation:
   2   15   30 NA's 
   1    4    3  512 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
   8  512 

patient.samples.sample.portions.portion.2.month_of_creation:
   2    7   11 NA's 
   4    1    3  512 

patient.samples.sample.portions.portion.2.portion_number:
   2   12   21   22 NA's 
   1    1    5    1  512 

patient.samples.sample.portions.portion.2.portion_sequence:
   1    2 NA's 
   1    6  513 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_barcode:
tcga-05-4390-01a-02-bs2                    NA's 
                      1                     519 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_uuid:
a85d2b2a-2e47-475f-8c98-318ae02e16c4                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.2.slides.slide.2.image_file_name:
tcga-05-4390-01a-02-bs2.a85d2b2a-2e47-475f-8c98-318ae02e16c4.svs 
                                                               1 
                                                            NA's 
                                                             519 

patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_lymphocyte_infiltration:
  12 NA's 
   1  519 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_necrosis:
  10 NA's 
   1  519 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_normal_cells:
   0 NA's 
   1  519 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_stromal_cells:
  25 NA's 
   1  519 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_cells:
  65 NA's 
   1  519 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
  75 NA's 
   1  519 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location:
bottom   NA's 
     1    519 




patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   8  512 

patient.samples.sample.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   1    2    5   10 NA's 
   1    1    1    3  514 

patient.samples.sample.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   0    2 NA's 
   2    1  517 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   0    1    5   10 NA's 
   3    2    2    1  512 

patient.samples.sample.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   3  517 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   0    5   29   50 NA's 
   5    1    1    1  512 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    4.50   11.75   21.25   40.00     512 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   66.25   70.00   75.00   91.25  100.00     512 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   69.50   74.00   74.25   82.50   90.00     512 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
 top NA's 
   8  512 

patient.samples.sample.portions.portion.2.weight:
  10   30   33  131 NA's 
   1    5    1    1  512 

patient.samples.sample.portions.portion.2.year_of_creation:
2010 2012 2013 NA's 
   1    3    4  512 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   7  513 



patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
heterogeneity          NA's 
            7           513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
145631750 145632144 145673860 145655942 145673880 145673939       513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.center_id:
   8 NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.concentration:
0.07 0.08 NA's 
   5    2  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.day_of_shipment:
  25 NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.month_of_shipment:
   3 NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_column:
   3 NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_id:
a27t NA's 
   7  513 


patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.quantity:
 2.8  3.2 NA's 
   5    2  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.volume:
  40 NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  18.46   23.43   34.84   37.41   50.21   61.25     513 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type:
 dna NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type_id:
   d NA's 
   7  513 



patient.samples.sample.portions.portion.3.analytes.analyte.concentration:
0.14 0.15 NA's 
   3    4  513 

patient.samples.sample.portions.portion.3.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             513 

patient.samples.sample.portions.portion.3.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   7  513 

patient.samples.sample.portions.portion.3.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    7                   513 

patient.samples.sample.portions.portion.3.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        7       513 

patient.samples.sample.portions.portion.3.analytes.analyte.subportion_sequence:
   1 NA's 
   7  513 



patient.samples.sample.portions.portion.3.day_of_creation:
   6    7 NA's 
   3    4  513 

patient.samples.sample.portions.portion.3.is_ffpe:
  no NA's 
   7  513 

patient.samples.sample.portions.portion.3.month_of_creation:
   3 NA's 
   7  513 

patient.samples.sample.portions.portion.3.portion_number:
  31   41   51 NA's 
   4    2    1  513 

patient.samples.sample.portions.portion.3.portion_sequence:
   3    4    5 NA's 
   4    2    1  513 




patient.samples.sample.portions.portion.3.slides.slide.is_derived_from_ffpe:
  no NA's 
   7  513 

patient.samples.sample.portions.portion.3.slides.slide.percent_lymphocyte_infiltration:
   3    4   20   40 NA's 
   1    2    3    1  513 

patient.samples.sample.portions.portion.3.slides.slide.percent_monocyte_infiltration:
   2    3   20 NA's 
   2    1    4  513 

patient.samples.sample.portions.portion.3.slides.slide.percent_necrosis:
   0    2    5   10 NA's 
   2    1    2    2  513 

patient.samples.sample.portions.portion.3.slides.slide.percent_neutrophil_infiltration:
   3    5   20   40 NA's 
   2    1    1    3  513 

patient.samples.sample.portions.portion.3.slides.slide.percent_normal_cells:
   0 NA's 
   7  513 

patient.samples.sample.portions.portion.3.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   10.00   20.00   16.86   24.00   25.00     513 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_cells:
  75   80   90 NA's 
   4    2    1  513 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_nuclei:
  75   80   90 NA's 
   4    2    1  513 

patient.samples.sample.portions.portion.3.slides.slide.section_location:
 top NA's 
   7  513 

patient.samples.sample.portions.portion.3.weight:
  20   30 NA's 
   2    5  513 

patient.samples.sample.portions.portion.3.year_of_creation:
2013 NA's 
   7  513 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.170   1.800   1.800   1.807   1.830   2.120     201 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
  11  509 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
  11  509 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
             11             509 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 98849031 132159756 132159830 131875136 132159998 167814215       506 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    9   13   36 NA's 
   9    1    3    1  506 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1325  0.1450  0.1664  0.1500  0.5000     506 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  13   14   26   29 NA's 
   3    9    1    1  506 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  14  506 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   3    6    7    8 NA's 
  11    1    1    1  506 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    2 NA's 
  12    2  506 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.700   1.722   1.950   4.819   3.000  40.000     506 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
   2   12  506 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   13.00   13.00   19.69   20.00   80.00     506 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2013 2015 NA's 
   2   11    1  506 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167814167 167814169 167814182 167814192 167814210 167814239       514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
  36 NA's 
   6  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.13 0.14 0.15 NA's 
   2    2    2  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  26 NA's 
   6  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   6  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   6  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1    5 NA's 
   5    1  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a46x NA's 
   6  514 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
0.65  0.7 0.75 NA's 
   2    2    2  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   6  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   5 NA's 
   6  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2015 NA's 
   6  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.752e+07 1.070e+08 1.252e+08 3.165e+08 1.500e+08 1.076e+09         1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
 102    1  416    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1526  0.1600  0.5000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   12.00   16.85   29.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 519    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.738  10.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.497   5.000  12.000       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.200   3.257   3.400  40.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 406  113    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   20.00   20.00   21.12   20.00   80.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2012    2014       1 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.50   31.62   81.50  116.14  157.00  730.30       1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
repli-g x (qiagen) dna                    rna                   NA's 
                     1                    518                      1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r    x NA's 
 416  102    1    1 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1526  0.1600  0.5000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             519 
                                            NA's 
                                               1 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 519    1 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                 repli-g x 
                      102                       416                         1 
                     NA's 
                        1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.660   1.250   1.400   1.415   1.597   2.140       2 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.900   7.700   8.300   8.278   8.800   9.900       2 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    519       1 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 385   30    1  104 

patient.samples.sample.portions.portion.analytes.analyte.3.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.180   1.700   1.800   1.777   1.855   2.110     285 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   3  517 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 98846631 145632059 148194718 145664262 167812910 167812963       510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   7    9   13 NA's 
   8    1    1  510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.100   0.150   0.160   0.189   0.170   0.500     510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   2    9   14   29 NA's 
   1    4    4    1  510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  10  510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.75    8.50    8.00   12.00   12.00     510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   1    2    5 NA's 
   3    5    2  510 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.120   1.570   5.342   1.950  40.000     510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22   23 NA's 
   2    8  510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
 6.6   10   13   80 NA's 
   4    1    4    1  510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2013 2014 NA's 
   2    4    4  510 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-44-6146-01a-11r-a46x-36 tcga-44-6147-01a-11r-a46x-36 
                           1                            1 
tcga-44-6775-01a-11r-a46x-36                         NA's 
                           1                          517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
7b1364ee-1b38-4ffe-9c56-7d4cf82c99db 7e7710ec-807d-4765-82a9-1520cf3da3ee 
                                   1                                    1 
de336a4c-e471-4cbb-80d7-9ec124eeb5be                                 NA's 
                                   1                                  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
167814144 167814218 167814237      NA's 
        1         1         1       517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
  36 NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
0.15 0.16 NA's 
   2    1  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
  26 NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   1    3 NA's 
   1    2  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
a46x NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   a    b    h NA's 
   1    1    1  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
0.75  0.8 NA's 
   2    1  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  23 NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
   5 NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2015 NA's 
   3  517 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.752e+07 1.210e+08 1.283e+08 3.848e+08 1.054e+09 1.076e+09         2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7    8   13 NA's 
 416    1  101    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1300  0.1500  0.1428  0.1600  0.5000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   18.00   18.17   29.00   30.00       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 518    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.745  10.500  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.529   4.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   1.860   2.000   2.289   2.340  40.000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
 406  112    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   13.30   13.30   16.68   13.30   80.00       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2012    2014       2 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   28.22   67.10   88.64  127.08  533.50       2 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                  rna            total rna 
                   1                  416                  101 
                NA's 
                   2 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   r    t    w NA's 
 416  101    1    2 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1300  0.1500  0.1428  0.1600  0.5000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             518 
                                            NA's 
                                               2 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 518    2 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g              total rna 
                   416                      1                    101 
                  NA's 
                     2 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.180   1.350   1.347   1.510   2.410      33 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.700   7.400   8.000   8.039   8.600   9.800      33 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
    518       2 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 385   30    1  104 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96484636  96485214 103960958 100757417 103961080 103961124       452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   9 NA's 
  68  452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
  68  452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  15   27   29 NA's 
  21   35   12  452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  68  452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   4    6    9 NA's 
  21   12   35  452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.500   2.676   3.250   6.000     452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 929 1069 1070 1258 NA's 
  21   11    1   35  452 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
  40 NA's 
  68  452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
  68  452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  80 NA's 
  68  452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  68  452 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-38-4630-01a-01w-a315-08                         NA's 
                           1                          519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
ec788331-45d9-4f2b-8ebd-cdc7f7fc18ab                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.biospecimen_barcode_bottom:
153878575      NA's 
        1       519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
   8 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  15 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   7 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   1 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
a315 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   g NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
   5 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  23 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
  10 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2013 NA's 
   1  519 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.087e+08 1.283e+08 3.080e+08 1.457e+08 1.076e+09        11 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8 NA's 
 509   11 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
 509   11 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.00   25.00   19.14   27.00   30.00      11 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 509   11 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.935   8.000  12.000      11 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.338   4.000  12.000      11 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
 509   11 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22   23 NA's 
 372  137   11 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
 509   11 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2013    2014      11 

patient.samples.sample.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   42.0   154.0   176.0   161.2   192.0   247.0     148 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                   508                      1                     11 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   w    x NA's 
 508    1   11 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
 372  148 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
 509   11 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no  yes NA's 
 372  137   11 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             509 
                                            NA's 
                                              11 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 509   11 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
      474        35        11 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen   uv spec      NA's 
      237       135       148 

patient.samples.sample.portions.portion.analytes.analyte.4.subportion_sequence:
   1    2    3 NA's 
 378   28    1  113 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96491241  96491534 103960914 100787311 103961654 103961701       453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id:
   9 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration:
 0.5 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment:
  15   27   29 NA's 
  21   35   11  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment:
   4    6    9 NA's 
  21   11   35  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   2.687   3.500   6.000     453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id:
 931 1070 1260 NA's 
  21   11   35  453 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity:
  40 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center:
  22 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume:
  80 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  67  453 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97512362  97512882 103959611 101106852 103959642 103959953       453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   8 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
  15   27   29 NA's 
  21   35   11  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
   4    6    9 NA's 
  21   11   35  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   2.687   3.500   6.000     453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_id:
 930 1068 1259 NA's 
  21   11   35  453 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
  40 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  22 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
  80 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2010 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  43.00   63.00   73.00   76.07   90.00  116.00     453 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type:
repli-g x (qiagen) dna                   NA's 
                    67                    453 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type_id:
   x NA's 
  67  453 



patient.samples.sample.portions.portion.analytes.analyte.5.concentration:
 0.5 NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful:
  no NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              67 
                                            NA's 
                                             453 

patient.samples.sample.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
  67  453 

patient.samples.sample.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
       67       453 

patient.samples.sample.portions.portion.analytes.analyte.5.spectrophotometer_method:
uv spec    NA's 
     67     453 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9    2 NA's 
   4  171    5  340 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.additional_studies.additional_study.disease_code:
fppp NA's 
   5  515 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.additional_studies.additional_study.project_code:
tcga NA's 
   5  515 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5             515 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.322e+08 1.457e+08 1.050e+09 6.878e+08 1.050e+09 1.054e+09       505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     5.0     8.0     6.6     9.0    10.0     505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0700  0.0887  0.1450  0.1500     505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
  11   13   14   22 NA's 
   3    2    9    1  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
  15  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   2    3    4    7 NA's 
   6    5    1    3  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   1    2    3 NA's 
   9    4    2  505 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.010   1.400   1.559   2.000   2.240     505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  22   23 NA's 
   9    6  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   20.00   24.09   40.00   40.00     505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2011 2013 2015 NA's 
   9    5    1  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.additional_studies.additional_study.disease_code:
fppp NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.additional_studies.additional_study.project_code:
tcga NA's 
   3  517 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             517 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.322e+08 1.322e+08 3.191e+08 1.678e+08 1.050e+09       510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.center_id:
   1    8    9   10 NA's 
   3    3    2    2  510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0725  0.1100  0.1050  0.1400  0.1500     510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.day_of_shipment:
   3   14   22   27 NA's 
   1    5    2    2  510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.is_derived_from_ffpe:
  no NA's 
  10  510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     3.0     3.5     4.1     5.5     8.0     510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_column:
   1    2    3 NA's 
   2    5    3  510 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_row:
   a    d    e    f NA's 
   3    3    2    2  510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   0.955   1.300   1.462   1.990   2.240     510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.source_center:
  22   23 NA's 
   4    6  510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   13.35   18.94   28.00   40.00     510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.year_of_shipment:
2011 2012 2013 2015 NA's 
   2    2    3    3  510 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.additional_studies.additional_study.disease_code:
fppp NA's 
   2  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.additional_studies.additional_study.project_code:
tcga NA's 
   2  518 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113627398 122893588 145675743 136932156 149777614 153879547       513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.center_id:
   5    8 NA's 
   2    5  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.concentration:
0.07 0.08 0.14 0.15 NA's 
   1    2    2    2  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.day_of_shipment:
   3   13   26   27 NA's 
   1    2    2    2  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.is_derived_from_ffpe:
  no NA's 
   7  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.month_of_shipment:
   3    6    8 NA's 
   2    4    1  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.plate_column:
   1    2    3    5 NA's 
   1    3    2    1  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.plate_id:
1931 a276 a313 a46w NA's 
   2    2    2    1  513 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.quantity:
0.94    1 1.07  1.6 NA's 
   3    2    1    1  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.source_center:
  22   23 NA's 
   2    5  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.volume:
6.67  6.7 13.4   20 NA's 
   2    2    2    1  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.year_of_shipment:
2012 2013 2015 NA's 
   2    4    1  513 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.bcr_aliquot_barcode:
tcga-44-2656-01a-02d-a313-08 tcga-44-2659-01a-01d-a313-08 
                           1                            1 
tcga-44-2665-01a-01d-2250-01 tcga-44-2668-01a-01d-a46w-08 
                           1                            1 
                        NA's 
                         516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.bcr_aliquot_uuid:
234a08d4-f797-442c-80d6-c133f96db37a 73f6eb52-7b85-49a2-ad10-9ad7314ed958 
                                   1                                    1 
9393e215-10ad-48d9-985c-2c3eea3f7766 fb64cda7-55e2-4c9c-b48d-80d870603c5e 
                                   1                                    1 
                                NA's 
                                 516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.biospecimen_barcode_bottom:
125246006 132159745 153879477 153879482      NA's 
        1         1         1         1       516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.center_id:
   1    8 NA's 
   1    3  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.concentration:
0.07 0.08 0.15 NA's 
   2    1    1  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.day_of_shipment:
   3   26   29 NA's 
   1    2    1  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.is_derived_from_ffpe:
  no NA's 
   4  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.month_of_shipment:
   6    8 NA's 
   2    2  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_column:
   1    3 NA's 
   2    2  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_id:
2250 a313 a46w NA's 
   1    2    1  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_row:
   f    g    h NA's 
   1    2    1  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.quantity:
0.94 1.07  1.2  1.4 NA's 
   1    1    1    1  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.source_center:
  22   23 NA's 
   1    3  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.volume:
   8 13.4   20 NA's 
   1    2    1  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.year_of_shipment:
2012 2013 2015 NA's 
   1    2    1  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.bcr_aliquot_barcode:
tcga-44-2656-01a-02d-a46w-08 tcga-44-2665-01a-01d-a271-08 
                           1                            1 
                        NA's 
                         518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.bcr_aliquot_uuid:
05aee344-6843-4d0d-9ad7-4c696b36f203 e80ca033-166a-450b-abdb-92d64f076780 
                                   1                                    1 
                                NA's 
                                 518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.biospecimen_barcode_bottom:
132159966 132160641      NA's 
        1         1       518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.center_id:
   8 NA's 
   2  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.concentration:
0.07 0.08 NA's 
   1    1  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.day_of_shipment:
   3   14 NA's 
   1    1  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.is_derived_from_ffpe:
  no NA's 
   2  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.month_of_shipment:
   3    8 NA's 
   1    1  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.plate_column:
   1    3 NA's 
   1    1  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.plate_id:
a271 a46w NA's 
   1    1  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.plate_row:
   a    g NA's 
   1    1  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.quantity:
 1.4  1.6 NA's 
   1    1  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.source_center:
  23 NA's 
   2  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.volume:
  20 NA's 
   2  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.year_of_shipment:
2013 2015 NA's 
   1    1  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.bcr_aliquot_barcode:
tcga-44-2665-01a-01d-a273-01                         NA's 
                           1                          519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.bcr_aliquot_uuid:
cee306c7-d850-4a8f-81ac-6ab2289c546e                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.biospecimen_barcode_bottom:
132158824      NA's 
        1       519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.center_id:
   1 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.concentration:
0.15 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.day_of_shipment:
  14 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.month_of_shipment:
   3 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.plate_column:
   3 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.plate_id:
a273 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.plate_row:
   g NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.quantity:
1.01 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.source_center:
  23 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.volume:
 6.7 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.year_of_shipment:
2013 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.bcr_aliquot_barcode:
tcga-44-2665-01a-01d-a276-05                         NA's 
                           1                          519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.bcr_aliquot_uuid:
0b8fd8b7-3d18-4825-b672-c05d18e490e5                                 NA's 
                                   1                                  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.biospecimen_barcode_bottom:
145675771      NA's 
        1       519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.center_id:
   5 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.concentration:
0.15 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.day_of_shipment:
  13 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.is_derived_from_ffpe:
  no NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.month_of_shipment:
   3 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.plate_column:
   3 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.plate_id:
a276 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.plate_row:
   g NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.quantity:
1.01 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.source_center:
  23 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.volume:
 6.7 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.year_of_shipment:
2013 NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.752e+07 1.210e+08 1.323e+08 4.746e+08 1.054e+09 1.076e+09         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    5    8    9 NA's 
 193   32  293    1    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.05000 0.08000 0.09906 0.15000 0.50000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   18.00   18.16   29.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 519    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.738  10.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.915   4.000  10.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   3.000   2.409   3.000  40.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 406  113    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67   40.00   34.65   60.00   80.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2012    2014       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.210e+08 1.317e+08 3.607e+08 1.053e+09 1.062e+09         2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8 NA's 
 350  168    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1500  0.1248  0.1600  0.1700       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   18.00   18.75   29.00   30.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 518    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.739  10.500  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.925   4.000  10.000       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.950   2.080   2.295   3.000   4.270       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 405  113    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.00   13.30   25.05   40.00   60.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2012    2014       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.070e+08 1.678e+08 3.676e+08 1.053e+09 1.076e+09       243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   7.101   9.000  10.000     243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0800  0.1400  0.1297  0.1500  0.1700     243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   11.00   22.00   21.45   28.00   30.00     243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 277  243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   5.000   5.621   8.000   9.000     243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.682   5.000  12.000     243 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.87    1.60    2.00    2.03    2.24    4.54     243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
 209   68  243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   17.24   28.00   30.00     243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2012 2015 NA's 
  67  136    6   68  243 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   4  516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   4  516 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             516 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.040e+08 1.539e+08 3.631e+08 6.089e+08 1.076e+09       413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   9.000   7.271   9.000  10.000     413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0800  0.1400  0.1131  0.1500  0.1600     413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   14.00   22.00   21.65   27.00   29.00     413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 107  413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   7.000   6.393   9.000   9.000     413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.421   5.000  11.000     413 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.500   1.679   2.240   6.000     413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  75   32  413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   20.00   17.86   28.00   40.00     413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2012    2013    2015     413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   5  515 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   5  515 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5             515 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 1.136e+08 1.539e+08 4.310e+08 1.050e+09 1.076e+09       458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   8.000   5.597   8.000  10.000     458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0800  0.1400  0.1194  0.1500  0.1700     458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   26.00   20.21   27.00   29.00     458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  62  458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   6.000   4.984   6.000   9.000     458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   2.032   3.000   7.000     458 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   0.940   1.070   1.587   2.000   6.400     458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
  39   23  458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67   13.00   15.64   13.40   40.00     458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     458 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.disease_code:
fppp NA's 
   5  515 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.project_code:
tcga NA's 
   5  515 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5             515 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.539e+08 1.539e+08 4.917e+08 1.050e+09 1.076e+09       478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   5.000   4.762   8.000  10.000     478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1450  0.1243  0.1500  0.1700     478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   14.00   16.00   19.74   26.00   29.00     478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  42  478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   6.000   4.667   6.000   9.000     478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.238   3.000   7.000     478 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.000   1.070   1.728   1.925   6.000     478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
  21   21  478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   13.40   14.14   13.40   40.00     478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 2012 2013 2015 NA's 
  15    6   19    2  478 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.disease_code:
fppp NA's 
   3  517 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.project_code:
tcga NA's 
   3  517 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             517 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.252e+08 1.322e+08 1.050e+09 6.327e+08 1.050e+09 1.050e+09       496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   1    5    8   10 NA's 
   3    5   15    1  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0800  0.0954  0.1500  0.1600     496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   14.00   14.08   14.00   29.00     496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
  24  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   2.000   3.792   6.000   8.000     496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.417   3.000   7.000     496 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.520   1.880   2.012   2.000   4.010     496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
  14   10  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   18.35   23.35   24.57   40.00   40.00     496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 2012 2013 2015 NA's 
  13    1    5    5  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.additional_studies.additional_study.disease_code:
fppp NA's 
   2  518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.additional_studies.additional_study.project_code:
tcga NA's 
   2  518 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             518 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.322e+08 1.050e+09 1.050e+09 8.361e+08 1.050e+09 1.050e+09       503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   1    5    8    9 NA's 
   2    2    5    8  503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.05 0.08 0.15 NA's 
  11    2    4  503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   3   14   21 NA's 
   1   15    1  503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
  17  503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   1    2    3    8 NA's 
   1   13    2    1  503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   1    2    5 NA's 
   8    8    1  503 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   2.000   2.000   2.042   2.000   4.010     503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22   23 NA's 
  13    4  503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   26.70   40.00   31.78   40.00   40.00     503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 2013 2015 NA's 
  13    2    2  503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   1  519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   1  519 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             519 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
9.649e+07 1.210e+08 1.323e+08 4.738e+08 1.054e+09 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   8 
315 182  23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1400  0.1500  0.1484  0.1600  0.5000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   18.00   18.17   29.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
520 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    4.00    6.00    6.74    9.50   12.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   2.912   4.000  10.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.000   1.070   1.451   1.730  40.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
407 113 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67    6.67    6.70   10.07   13.30   80.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
  69  198  209   22   22 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   64.86  212.50  235.88  325.25 1182.70 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
                 dna repli-g (qiagen) dna 
                 519                    1 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d   w 
519   1 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1400  0.1500  0.1517  0.1600  0.5000 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
520 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
383 137 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             520 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
520 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type               repli-g 
                  519                     1 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      339       181 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 385   30    1  104 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   14.00   15.66   24.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
520 

patient.samples.sample.portions.portion.lce:
 lce NA's 
  22  498 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.671  10.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   11.00   10.52   11.00   51.00 

patient.samples.sample.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.286   1.000   5.000     422 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 268  252 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     8.0    10.0    13.7    19.5    85.0     426 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    2.00    3.07    5.00   15.00     449 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   3.489   5.000  90.000     252 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    2.15    3.00   20.00     460 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    4.73    5.00   95.00     261 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   11.82   15.00   70.00     258 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   75.00   83.00   80.37   90.00  100.00     261 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    70.0    75.0    73.1    80.0    90.0     252 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    22    246    252 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
520 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   3.000   7.000   9.961  15.000  90.000     264 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   3.031   5.000  20.000     296 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   3.617   5.000  60.000       5 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.101   2.000  30.000     312 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   4.355   4.750  90.000      22 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   10.00   11.86   15.00   75.00      21 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   70.00   82.00   79.22   90.00  100.00      25 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    65.0    75.0    72.4    80.0   100.0       5 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
   246    274 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   30.00   33.00   47.99   40.00  174.00 

patient.samples.sample.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2009    2011    2011    2011    2012    2014 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 365  155 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 365  155 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   13.00   21.00   19.11   21.00   51.00     155 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    4    5 NA's 
  12   90    1    1  416 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   5   30 NA's 
 237  128  155 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   3    7 NA's 
 237  128  155 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 2014 NA's 
 237  128  155 

patient.samples.sample.sample_type:
primary tumor 
          520 

patient.samples.sample.sample_type_id:
  1 
520 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.4000  0.3756  0.5000  1.0000     115 

patient.samples.sample.vial_number:
  a   b 
517   3 

Sex:
[unknown]    FEMALE      MALE      NA's 
        1       130        99       290 

Age.at.diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   41.0    59.0    67.0    66.1    73.0    86.0     307 




Smoking.Status:
Current reformed smoker for < or = 15 years 
                                         73 
     Current reformed smoker for > 15 years 
                                         69 
                             Current smoker 
                                         45 
                        Lifelong Non-smoker 
                                         32 
                                       NA's 
                                        301 

Survival:
DECEASED   LIVING     NA's 
      68      161      291 

Transversion.High.Low:
High  Low NA's 
 142   88  290 

Nonsilent.Mutations:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   79.25  159.00  239.00  290.50 1339.00     290 

Nonsilent.Mutations.per.Mb:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.370   2.277   4.565   6.861   8.338  38.440     290 

Oncogene.Negative.or.Positive.Groups:
Oncogene Negative Oncogene Positive              NA's 
               87               143               290 










Mutation_U2AF1:
  none p.S34F   NA's 
   222      8    290 









Mutation_HRAS:
  none p.Q61L   NA's 
   229      1    290 

Mutation_NRAS:
  none p.Q61L   NA's 
   229      1    290 

Mutation_AKT1:
   none p.D323Y  p.E17K    NA's 
    228       1       1     290 


Mutation_MAP2K1:
   none p.C121S  p.F53L  p.K57N    NA's 
    227       1       1       1     290 


expression_subtype:
 prox.-inflam prox.-prolif.           TRU          NA's 
           78            63            89           290 


CNA_TP53:
  -2   -1    0    1 NA's 
   3  128   85   14  290 

CNA_KRAS:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -2.000   0.000   0.000   0.213   1.000   2.000     290 

CNA_EGFR:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-2.0000  0.0000  1.0000  0.5348  1.0000  2.0000     290 

CNA_STK11:
  -2   -1    0    1 NA's 
   3  118   96   13  290 

CNA_RBM10:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2.00000  0.00000  0.00000 -0.08696  0.00000  2.00000      290 

CNA_CDKN2A:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-2.0000 -1.0000 -1.0000 -0.7174  0.0000  2.0000     290 

CNA_KEAP1:
  -2   -1    0    1 NA's 
   4  115  101   10  290 

CNA_BRAF:
  -1    0    1    2 NA's 
  30  107   87    6  290 

CNA_U2AF1:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-2.0000 -1.0000  0.0000 -0.1304  0.0000  2.0000     290 

CNA_SMARCA4:
  -2   -1    0    1 NA's 
   4  115  102    9  290 

CNA_NF1:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-2.0000  0.0000  0.0000  0.2478  1.0000  2.0000     290 

CNA_MET:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-2.0000  0.0000  0.0000  0.2913  1.0000  2.0000     290 

CNA_ARID1A:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -2      -1       0       0       1       2     290 

CNA_SETD2:
  -1    0    1 NA's 
 106  110   14  290 

CNA_RB1:
  -2   -1    0    1 NA's 
   6  108   95   21  290 

CNA_PIK3CA:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2.00000 -1.00000  0.00000  0.01304  1.00000  2.00000      290 

CNA_MGA:
  -2   -1    0    1 NA's 
   7  119   87   17  290 

CNA_RIT1:
  -1    0    1    2 NA's 
   4   56  138   32  290 

CNA_NKX2.1:
  -1    0    1    2 NA's 
  33  100   65   32  290 

CNA_MDM2:
  -1    0    1    2 NA's 
  39  120   53   18  290 

CNA_TERT:
  -1    0    1    2 NA's 
   7   72  110   41  290 

CNA_TERC:
  -1    0    1    2 NA's 
  47  109   64   10  290 

CNA_MYC:
  -1    0    1    2 NA's 
  10   75  124   21  290 

CNA_CCND1:
  -1    0    1    2 NA's 
  25  131   64   10  290 

CNA_CCNE1:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2.00000  0.00000  0.00000  0.04783  0.00000  2.00000      290 

CNA_CDK4:
  -1    0    1    2 NA's 
  38  124   53   15  290 

CNA_ERBB2:
  -1    0    1    2 NA's 
  19  109   96    6  290 

CNA_HRAS:
  -2   -1    0    1 NA's 
   1   54  138   37  290 

CNA_NRAS:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2.00000 -1.00000  0.00000 -0.06522  0.00000  2.00000      290 

CNA_AKT1:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2.00000 -1.00000  0.00000 -0.06087  0.00000  2.00000      290 

FCNA_TP53:
   A    D    N NA's 
   2   13  215  290 

FCNA_KRAS:
   A    D    N NA's 
  26    2  202  290 

FCNA_EGFR:
   A    D    N NA's 
  19    1  210  290 

FCNA_STK11:
   A    D    N NA's 
   7   21  202  290 

FCNA_RBM10:
   A    D    N NA's 
   6    2  222  290 

FCNA_CDKN2A:
   A    D    N NA's 
   5   55  170  290 

FCNA_KEAP1:
   A    D    N NA's 
   5   14  211  290 

FCNA_BRAF:
   A    D    N NA's 
  11   10  209  290 

FCNA_U2AF1:
   A    D    N NA's 
   6    6  218  290 

FCNA_SMARCA4:
   A    D    N NA's 
   4   15  211  290 

FCNA_NF1:
   A    D    N NA's 
   9    5  216  290 

FCNA_MET:
   A    D    N NA's 
  12   11  207  290 

FCNA_ARID1A:
   A    D    N NA's 
  13   24  193  290 

FCNA_SETD2:
   A    D    N NA's 
   2   13  215  290 

FCNA_RB1:
   A    D    N NA's 
   1   19  210  290 

FCNA_PIK3CA:
   A    D    N NA's 
  22    8  200  290 

FCNA_MGA:
   A    D    N NA's 
   3   25  202  290 

FCNA_RIT1:
   A    D    N NA's 
  44    2  184  290 

FCNA_NKX2.1:
   A    D    N NA's 
  56    2  172  290 

FCNA_MDM2:
   A    D    N NA's 
  31    4  195  290 

FCNA_TERT:
   A    D    N NA's 
  48    1  181  290 

FCNA_TERC:
   A    D    N NA's 
  34    2  194  290 

FCNA_MYC:
   A    D    N NA's 
  52    1  177  290 

FCNA_CCND1:
   A    N NA's 
  25  205  290 

FCNA_CCNE1:
   A    D    N NA's 
  23    5  202  290 

FCNA_CDK4:
   A    D    N NA's 
  28    1  201  290 

FCNA_ERBB2:
   A    D    N NA's 
  13    3  214  290 

FCNA_HRAS:
   A    D    N NA's 
   5   14  211  290 

FCNA_NRAS:
   A    D    N NA's 
   3   33  194  290 

FCNA_AKT1:
   A    D    N NA's 
   3   20  207  290 

iCLUSTER.output:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   3.796   5.000   6.000     290 

methylation.signature:
        high intermediate          low         NA's 
          37           70           74          339 


ABSOLUTE.calls:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.390   2.010   2.735   2.861   3.435   7.200     290 

ABSOLUTE.calls.1:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1700  0.3300  0.4400  0.4637  0.6075  0.9100     290 

}}
\keyword{datasets}
