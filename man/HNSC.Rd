\name{ HNSC }
\alias{ HNSC }
\docType{data}
\title{ Head and Neck squamous cell carcinoma }

\details{
\preformatted{
> experiments( HNSC )
ExperimentList class object of length 11: 
 [1] CNASeq-20160128: RaggedExperiment with 32905 rows and 225 columns 
 [2] CNASNP-20160128: RaggedExperiment with 499142 rows and 1090 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 110289 rows and 1089 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 522 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 522 columns 
 [6] Methylation-20160128: SummarizedExperiment with 485577 rows and 580 columns 
 [7] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 532 columns 
 [8] Mutation-20160128: RaggedExperiment with 51799 rows and 279 columns 
 [9] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 566 columns 
 [10] RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 294 columns 
 [11] RPPAArray-20160128: SummarizedExperiment with 160 rows and 212 columns 

> rownames( HNSC )
CharacterList of length 11
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RNASeqGene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
...
<1 more element>

> colnames( HNSC )
CharacterList of length 11
[["CNASeq-20160128"]] TCGA-BA-4074-01A-01D-1431-02 ...
[["CNASNP-20160128"]] TCGA-4P-AA8J-01A-11D-A390-01 ...
[["CNVSNP-20160128"]] TCGA-4P-AA8J-01A-11D-A390-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-4P-AA8J-01A-11D-A390-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-4P-AA8J-01A-11D-A390-01 ...
[["Methylation-20160128"]] TCGA-4P-AA8J-01A-11D-A392-05 ...
[["miRNASeqGene-20160128"]] TCGA-4P-AA8J-01A-11R-A39B-13 ...
[["Mutation-20160128"]] TCGA-BA-4074-01A-01D-1434-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-4P-AA8J-01A-11R-A39I-07 ...
[["RNASeqGene-20160128"]] TCGA-BA-4074-01A-01R-1436-07 ...
...
<1 more element>

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128    1 Mb
2                    CNASNP-20160128 13.6 Mb
3                    CNVSNP-20160128  3.2 Mb
4          GISTIC_AllByGene-20160128  4.4 Mb
5  GISTIC_ThresholdedByGene-20160128  4.4 Mb
6               Methylation-20160128 67.5 Mb
7              miRNASeqGene-20160128  0.1 Mb
8                  Mutation-20160128 65.6 Mb
9           RNASeq2GeneNorm-20160128  1.2 Mb
10               RNASeqGene-20160128  1.1 Mb
11                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   305 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 223.00  223.00    1.18    1.06    1.35 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  19.00   53.00   61.00   60.91   69.00   89.00       1 

vital_status:
  0   1 
304 224 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   260.0   430.0   740.0   814.5  6417.0     305 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   529.5   851.0  1042.9  1404.0  5480.0     225 

tumor_tissue_site:
head and neck 
          528 




pathology_M_stage:
  m0   m1   mx NA's 
 191    1   65  271 

gender:
female   male 
   142    386 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2007    2010    2008    2011    2013       1 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   98.0   483.0   848.5  1226.9  1647.5  3930.0     506 

radiation_therapy:
  no  yes NA's 
 163  303   62 

histological_type:
                      head & neck squamous cell carcinoma 
                                                      517 
        head & neck squamous cell carcinoma basaloid type 
                                                       10 
head & neck squamous cell carcinoma, spindle cell variant 
                                                        1 

number_pack_years_smoked:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
  0.01685  25.00000  40.00000  45.75496  60.00000 300.00000       230 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1936    1959    1968    1967    1975    2001     246 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.186   3.000  44.000     115 

race:
american indian or alaska native                            asian 
                               2                               11 
       black or african american                            white 
                              48                              452 
                            NA's 
                              15 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    26                    465                     37 

admin.bcr:
nationwide children's hospital 
                           528 

admin.day_of_dcc_upload:
  1  17  24 
526   1   1 

admin.disease_code:
hnsc 
 528 


admin.month_of_dcc_upload:
  9  10 
  2 526 

admin.patient_withdrawal.withdrawn:
false 
  528 

admin.project_code:
tcga 
 528 

admin.year_of_dcc_upload:
2015 
 528 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  19.00   53.00   61.00   60.91   69.00   90.00       1 

patient.alcohol_history_documented:
  no  yes NA's 
 165  352   11 

patient.amount_of_alcohol_consumption_per_day:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   3.239   5.000  33.000     307 



patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      412       116 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-192.00   -1.00   10.00   59.57   34.00 1286.00     117 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   257.5   456.0   865.2  1011.0  6417.0     413 


patient.clinical_cqcf.histological_type:
                      head and neck squamous cell carcinoma 
                                                        517 
head and neck squamous cell carcinoma  spindle cell variant 
                                                          1 
        head and neck squamous cell carcinoma basaloid type 
                                                         10 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             518 
yes, pharmaceutical treatment prior to resection 
                                               1 
               yes, radiation prior to resection 
                                               9 

patient.clinical_cqcf.history_of_prior_malignancy:
                                                     no 
                                                    491 
                       yes, history of prior malignancy 
                                                     21 
yes, history of synchronous and or bilateral malignancy 
                                                     14 
                                                   NA's 
                                                      2 


patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           59           469 

patient.clinical_cqcf.other_anatomic_site_normal_tissue:
blood  NA's 
    1   527 

patient.clinical_cqcf.tumor_type:
primary 
    528 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   14.00   15.35   23.00   31.00      11 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32871  -25248  -22317  -22426  -19474   -7301       1 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   217.0   410.5   750.1   800.2  6417.0     358 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 527    1 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -2.0   173.0   369.0   623.5   851.0  5480.0     163 

patient.days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   98.0   483.0   848.5  1226.9  1647.5  3930.0     506 

patient.drugs.drug.10.bcr_drug_barcode:
tcga-cr-7404-d31335                NA's 
                  1                 527 

patient.drugs.drug.10.bcr_drug_uuid:
9ba61073-c6e7-43b8-b8a3-58776bd5447e                                 NA's 
                                   1                                  527 

patient.drugs.drug.10.clinical_trail_drug_classification:
phase i trial          NA's 
            1           527 

patient.drugs.drug.10.day_of_form_completion:
  30 NA's 
   1  527 

patient.drugs.drug.10.days_to_drug_therapy_end:
1318 NA's 
   1  527 

patient.drugs.drug.10.days_to_drug_therapy_start:
1297 NA's 
   1  527 

patient.drugs.drug.10.measure_of_response:
clinical progressive disease                         NA's 
                           1                          527 

patient.drugs.drug.10.month_of_form_completion:
   4 NA's 
   1  527 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
   1  527 

patient.drugs.drug.10.therapy_types.therapy_type:
chemotherapy         NA's 
           1          527 

patient.drugs.drug.10.tx_on_clinical_trial:
 yes NA's 
   1  527 

patient.drugs.drug.10.year_of_form_completion:
2012 NA's 
   1  527 

patient.drugs.drug.11.bcr_drug_barcode:
tcga-cr-7404-d31336                NA's 
                  1                 527 

patient.drugs.drug.11.bcr_drug_uuid:
5ca7e448-99a9-48ac-8237-bcaca6501f19                                 NA's 
                                   1                                  527 

patient.drugs.drug.11.day_of_form_completion:
  30 NA's 
   1  527 

patient.drugs.drug.11.days_to_drug_therapy_end:
1416 NA's 
   1  527 

patient.drugs.drug.11.days_to_drug_therapy_start:
1332 NA's 
   1  527 

patient.drugs.drug.11.drug_name:
pemetrexed       NA's 
         1        527 

patient.drugs.drug.11.month_of_form_completion:
   4 NA's 
   1  527 

patient.drugs.drug.11.therapy_ongoing:
  no NA's 
   1  527 

patient.drugs.drug.11.therapy_types.therapy_type:
chemotherapy         NA's 
           1          527 

patient.drugs.drug.11.tx_on_clinical_trial:
  no NA's 
   1  527 

patient.drugs.drug.11.year_of_form_completion:
2012 NA's 
   1  527 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   11.00   17.00   17.15   23.50   30.00     448 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0    84.0   113.0   281.4   292.0  3259.0     455 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   43.75   98.50  250.50  250.75 3246.00     452 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                           9                           20 
            partial response               stable disease 
                           1                            2 
                        NA's 
                         496 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.287   9.000  12.000     448 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.500   3.286   5.000   9.000     500 

patient.drugs.drug.2.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.25   30.00   92.80   87.50  692.00     505 


patient.drugs.drug.2.regimen_indication:
               adjuvant other: specify in notes              palliative 
                     12                      11                       6 
             recurrence                    NA's 
                      8                     491 

patient.drugs.drug.2.regimen_indication_notes:
          induction primary: concurrent  primary: induction                NA's 
                  1                   9                   1                 517 

patient.drugs.drug.2.regimen_number:
   1    2    3 NA's 
  23   10    1  494 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              33                1              494 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  75    5  448 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                        75                          1 
targeted molecular therapy                       NA's 
                         4                        448 

patient.drugs.drug.2.therapy_types.therapy_type.2:
targeted molecular therapy                       NA's 
                         1                        527 

patient.drugs.drug.2.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    50.0   300.0   387.4   519.0  1105.0     519 

patient.drugs.drug.2.total_dose_units:
  auc    mg mg/m2 mg/ml  NA's 
    1     5     1     7   514 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  35    7  486 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     448 



patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   14.00   21.00   19.03   24.00   30.00     489 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   34.0   103.8   181.0   404.2   428.0  2799.0     492 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    63.5   149.5   369.8   455.5  2786.0     492 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                           8                            7 
              stable disease                         NA's 
                           2                          511 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   7.128  10.000  12.000     489 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   3.500   4.643   6.750   9.000     514 

patient.drugs.drug.3.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    32.5    60.0   328.3   171.5  1831.0     518 

patient.drugs.drug.3.prescribed_dose_units:
  auc    mg mg/m2 mg/ml  NA's 
    1     3     7     1   516 

patient.drugs.drug.3.regimen_indication:
               adjuvant other: specify in notes              palliative 
                      1                       7                       6 
             recurrence                    NA's 
                      3                     511 

patient.drugs.drug.3.regimen_indication_notes:
            primary primary: concurrent  primary: induction                NA's 
                  1                   2                   4                 521 

patient.drugs.drug.3.regimen_number:
   1    2    3    5 NA's 
   4    8    2    1  513 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
              16              512 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  37    2  489 

patient.drugs.drug.3.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                        38                          1 
                      NA's 
                       489 

patient.drugs.drug.3.total_dose:
 455  577 1419 NA's 
   1    1    1  525 

patient.drugs.drug.3.total_dose_units:
   mg mg/m2 mg/ml  NA's 
    3     1     1   523 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  18    4  506 

patient.drugs.drug.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2014    2015     489 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.75   20.00   18.04   22.25   30.00     504 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   122.2   170.5   266.1   372.5   958.0     506 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   19.0    79.0   131.0   237.5   351.0   937.0     506 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                           6                            3 
              stable disease                         NA's 
                           1                          518 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.750   9.000   7.708   9.250  12.000     504 

patient.drugs.drug.4.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   4.500   4.917   7.250   9.000     516 

patient.drugs.drug.4.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   52.50   60.00  108.92   76.25  540.00     516 

patient.drugs.drug.4.prescribed_dose_units:
   auc     mg mg/day  mg/m2   NA's 
     2      2      1      7    516 

patient.drugs.drug.4.regimen_indication:
               adjuvant other: specify in notes              palliative 
                      1                       8                       3 
             recurrence                    NA's 
                      1                     515 

patient.drugs.drug.4.regimen_indication_notes:
            primary primary: concurrent  primary: induction                NA's 
                  1                   2                   5                 520 

patient.drugs.drug.4.regimen_number:
   1    2 NA's 
   6    7  515 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              11                1              516 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
  23    1  504 

patient.drugs.drug.4.therapy_types.therapy_type:
              chemotherapy             other, specify 
                        21                          1 
targeted molecular therapy                    vaccine 
                         1                          1 
                      NA's 
                       504 

patient.drugs.drug.4.therapy_types.therapy_type_notes:
ancillary agent            NA's 
              1             527 

patient.drugs.drug.4.total_dose:
 150  480  515 NA's 
   1    1    1  525 

patient.drugs.drug.4.total_dose_units:
   mg mg/m2  NA's 
    2     1   525 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
   9    2  517 

patient.drugs.drug.4.year_of_form_completion:
2010 2011 2012 2014 NA's 
   1   10    7    6  504 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   14.25   23.00   20.00   27.75   30.00     518 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  121.0   188.5   375.0   432.8   552.5  1152.0     518 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  114.0   158.0   298.0   412.6   548.0  1124.0     519 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease            complete response 
                           3                            1 
            partial response               stable disease 
                           1                            1 
                        NA's 
                         522 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    4.25    7.00    6.40    8.75   10.00     518 

patient.drugs.drug.5.number_cycles:
   1    2    6    8 NA's 
   1    1    1    1  524 

patient.drugs.drug.5.prescribed_dose:
   2   20   30 NA's 
   1    1    2  524 

patient.drugs.drug.5.prescribed_dose_units:
   auc mg/day  mg/m2   NA's 
     1      1      2    524 

patient.drugs.drug.5.regimen_indication:
other: specify in notes              palliative              recurrence 
                      2                       1                       1 
                   NA's 
                    524 

patient.drugs.drug.5.regimen_indication_notes:
primary: concurrent                NA's 
                  2                 526 

patient.drugs.drug.5.regimen_number:
   1    2    3 NA's 
   1    2    1  524 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               3                1              524 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
  10  518 

patient.drugs.drug.5.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         9                          1 
                      NA's 
                       518 

patient.drugs.drug.5.total_dose:
 100 NA's 
   1  527 

patient.drugs.drug.5.total_dose_units:
  mg NA's 
   1  527 

patient.drugs.drug.5.tx_on_clinical_trial:
  no  yes NA's 
   5    1  522 

patient.drugs.drug.5.year_of_form_completion:
2011 2012 2014 NA's 
   2    5    3  518 



patient.drugs.drug.6.day_of_form_completion:
   2   14   28   30 NA's 
   1    1    2    1  523 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   74.0   187.0   415.0   494.8   646.0  1152.0     523 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   74.0   187.0   375.0   475.6   618.0  1124.0     523 


patient.drugs.drug.6.measure_of_response:
clinical progressive disease                         NA's 
                           2                          526 

patient.drugs.drug.6.month_of_form_completion:
   4    5    8    9 NA's 
   1    1    1    2  523 

patient.drugs.drug.6.number_cycles:
   1    5 NA's 
   1    1  526 

patient.drugs.drug.6.prescribed_dose:
 1.5  400 NA's 
   1    1  526 

patient.drugs.drug.6.prescribed_dose_units:
  auc mg/m2  NA's 
    1     1   526 

patient.drugs.drug.6.regimen_indication:
other: specify in notes              recurrence                    NA's 
                      1                       1                     526 

patient.drugs.drug.6.regimen_indication_notes:
primary: induction               NA's 
                 1                527 

patient.drugs.drug.6.regimen_number:
   1    2 NA's 
   1    1  526 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              526 

patient.drugs.drug.6.therapy_ongoing:
  no NA's 
   5  523 

patient.drugs.drug.6.therapy_types.therapy_type:
chemotherapy         NA's 
           5          523 

patient.drugs.drug.6.tx_on_clinical_trial:
  no  yes NA's 
   2    1  525 

patient.drugs.drug.6.year_of_form_completion:
2011 2012 2014 NA's 
   2    2    1  523 

patient.drugs.drug.7.bcr_drug_barcode:
tcga-cr-5248-d31400 tcga-cr-6471-d18096 tcga-cr-6482-d18179 tcga-cr-7404-d31332 
                  1                   1                   1                   1 
               NA's 
                524 

patient.drugs.drug.7.bcr_drug_uuid:
0de72eb9-07cc-406f-a941-36f6e8cf4ece 5d20fe8a-0c34-41a9-b117-2cea2634c63d 
                                   1                                    1 
7f4949ac-e0b8-4e8e-a9c3-f749a60cce1f fc1b59d1-7ac2-4385-896c-020d6cdf8787 
                                   1                                    1 
                                NA's 
                                 524 

patient.drugs.drug.7.day_of_form_completion:
   2   14   28   30 NA's 
   1    1    1    1  524 

patient.drugs.drug.7.days_to_drug_therapy_end:
  74  599  646 1152 NA's 
   1    1    1    1  524 

patient.drugs.drug.7.days_to_drug_therapy_start:
  74  466  618 1124 NA's 
   1    1    1    1  524 

patient.drugs.drug.7.drug_name:
 cetuximab  cisplatin paclitaxel       NA's 
         1          1          2        524 

patient.drugs.drug.7.measure_of_response:
clinical progressive disease                         NA's 
                           1                          527 

patient.drugs.drug.7.month_of_form_completion:
   4    5    9 NA's 
   1    1    2  524 

patient.drugs.drug.7.number_cycles:
   1    5 NA's 
   1    1  526 

patient.drugs.drug.7.prescribed_dose:
  35   90 NA's 
   1    1  526 

patient.drugs.drug.7.prescribed_dose_units:
mg/m2  NA's 
    2   526 

patient.drugs.drug.7.regimen_indication:
other: specify in notes              recurrence                    NA's 
                      1                       1                     526 

patient.drugs.drug.7.regimen_indication_notes:
primary: induction               NA's 
                 1                527 

patient.drugs.drug.7.regimen_number:
   1    2 NA's 
   1    1  526 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              526 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   4  524 

patient.drugs.drug.7.therapy_types.therapy_type:
chemotherapy         NA's 
           4          524 

patient.drugs.drug.7.tx_on_clinical_trial:
  no  yes NA's 
   1    1  526 

patient.drugs.drug.7.year_of_form_completion:
2011 2012 NA's 
   2    2  524 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-cr-5248-d31401 tcga-cr-7404-d31333                NA's 
                  1                   1                 526 

patient.drugs.drug.8.bcr_drug_uuid:
13733a55-6052-4149-b293-3df87835ca00 ff9ed268-d746-4ee3-a453-67521f65380a 
                                   1                                    1 
                                NA's 
                                 526 

patient.drugs.drug.8.day_of_form_completion:
   2   30 NA's 
   1    1  526 

patient.drugs.drug.8.days_to_drug_therapy_end:
 634 1208 NA's 
   1    1  526 

patient.drugs.drug.8.days_to_drug_therapy_start:
 606 1194 NA's 
   1    1  526 

patient.drugs.drug.8.drug_name:
   imc-a12 paclitaxel       NA's 
         1          1        526 

patient.drugs.drug.8.measure_of_response:
stable disease           NA's 
             1            527 

patient.drugs.drug.8.month_of_form_completion:
   4    5 NA's 
   1    1  526 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   2  526 

patient.drugs.drug.8.therapy_types.therapy_type:
 chemotherapy immunotherapy          NA's 
            1             1           526 

patient.drugs.drug.8.tx_on_clinical_trial:
  no  yes NA's 
   1    1  526 

patient.drugs.drug.8.year_of_form_completion:
2012 NA's 
   2  526 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-cr-7404-d31334                NA's 
                  1                 527 

patient.drugs.drug.9.bcr_drug_uuid:
3fb10b09-e755-48b8-a5aa-af8ac116f134                                 NA's 
                                   1                                  527 

patient.drugs.drug.9.day_of_form_completion:
  30 NA's 
   1  527 

patient.drugs.drug.9.days_to_drug_therapy_end:
1080 NA's 
   1  527 

patient.drugs.drug.9.days_to_drug_therapy_start:
1052 NA's 
   1  527 

patient.drugs.drug.9.drug_name:
methotrexate         NA's 
           1          527 

patient.drugs.drug.9.month_of_form_completion:
   4 NA's 
   1  527 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   1  527 

patient.drugs.drug.9.therapy_types.therapy_type:
chemotherapy         NA's 
           1          527 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   1  527 

patient.drugs.drug.9.year_of_form_completion:
2012 NA's 
   1  527 



patient.drugs.drug.clinical_trail_drug_classification:
pi3k inhibitor           NA's 
             1            527 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   17.00   16.64   24.00   31.00     343 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   34.0    87.0   111.0   166.2   152.5  3259.0     353 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -303.0    44.0    71.0   122.4   108.0  3246.0     351 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           9                           72 
            partial response               stable disease 
                           4                            2 
                        NA's 
                         441 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.607   9.000  12.000     342 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.50    4.13    6.00    9.00     482 

patient.drugs.drug.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     9.0    40.0   116.3   100.0  2248.0     485 



patient.drugs.drug.regimen_indication_notes:
          induction             primary primary: concurrent  primary: induction 
                  1                   1                   4                   6 
               NA's 
                516 

patient.drugs.drug.regimen_number:
   1    2    3    4 NA's 
  73    1    2    1  451 

patient.drugs.drug.route_of_administrations.route_of_administration:
intravenous (iv)     intravesical             NA's 
              70                1              457 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 180    5  343 

patient.drugs.drug.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                       182                          3 
targeted molecular therapy                       NA's 
                         1                        342 

patient.drugs.drug.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    62.8   160.0   290.3   350.0  1746.0     503 


patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  92    6  430 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2014     342 

patient.egfr_amplication_status:
  amplified unamplified        NA's 
         11          16         501 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    26                    465                     37 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   8    9  511 

patient.follow_ups.follow_up.2.additional_radiation_therapy:
  no  yes NA's 
  15    2  511 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure:
  no  yes NA's 
   7    4  517 

patient.follow_ups.follow_up.2.additional_surgery_metastatic_procedure:
  no  yes NA's 
  10    1  517 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.50   19.00   16.69   22.00   31.00     368 

patient.follow_ups.follow_up.2.days_to_additional_surgery_locoregional_procedure:
 265  334  436  798 NA's 
   1    1    1    1  524 

patient.follow_ups.follow_up.2.days_to_additional_surgery_metastatic_procedure:
 409 NA's 
   1  527 

patient.follow_ups.follow_up.2.days_to_completion_of_curative_tx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    52.0    88.0   101.8   120.0  1220.0     439 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  108.0   374.0   580.0   749.7   888.5  2083.0     501 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   77.0   527.2   750.0   917.7  1232.2  3221.0     398 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  244.0   336.5   467.0   618.1   687.2  1855.0     510 

patient.follow_ups.follow_up.2.disease_after_curative_tx:
  no  yes NA's 
  81    5  442 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             9                            146 
                          NA's 
                           373 


patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
 119   15  394 

patient.follow_ups.follow_up.2.method_of_curative_tx:
concurrent chemotherapy               radiation                 surgery 
                     20                      12                      55 
                   NA's 
                    441 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.75    8.00    7.15   10.00   12.00     368 

patient.follow_ups.follow_up.2.new_neoplasm_event_occurrence_anatomic_site:
cervical lymph node  distant metastasis         oral cavity          oropharynx 
                  4                   6                   2                   3 
               NA's 
                513 


patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  20   18  490 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   4    8  516 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   9    4  515 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 152 1067 NA's 
   1    1  526 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   85.0   215.8   426.0   488.5   614.5  1369.0     514 


patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      7                       4                       2 
                   NA's 
                    515 


patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  12    3  513 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                 10                                   4 
             positive biomarker (s)                                NA's 
                                  1                                 513 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 101   15  412 

patient.follow_ups.follow_up.2.patient_death_reason:
        head and neck cancer other, non-malignant disease 
                           3                            4 
                        NA's 
                         521 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       116         30        382 


patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  44   96  388 

patient.follow_ups.follow_up.2.smokeless_tobacco_use_age_at_quit:
  36   48   58 NA's 
   1    1    1  525 

patient.follow_ups.follow_up.2.smokeless_tobacco_use_age_at_start:
  23   30 NA's 
   1    1  526 

patient.follow_ups.follow_up.2.smokeless_tobacco_use_at_diag:
  no  yes NA's 
  86    6  436 

patient.follow_ups.follow_up.2.smokeless_tobacco_use_per_day:
 1.5    5   10 NA's 
   1    1    1  525 

patient.follow_ups.follow_up.2.smokeless_tobacco_use_regularly:
  no  yes NA's 
  79   10  439 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
  80   61  387 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
  130    27   371 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2012    2013    2014    2015     368 

patient.follow_ups.follow_up.3.additional_pharmaceutical_therapy:
  no NA's 
   2  526 

patient.follow_ups.follow_up.3.additional_radiation_therapy:
  no  yes NA's 
   1    1  526 

patient.follow_ups.follow_up.3.additional_surgery_locoregional_procedure:
 yes NA's 
   2  526 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   15.00   18.00   17.65   21.00   26.00     477 

patient.follow_ups.follow_up.3.days_to_additional_surgery_locoregional_procedure:
 832  931 NA's 
   1    1  526 

patient.follow_ups.follow_up.3.days_to_completion_of_curative_tx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    77.0    98.0   106.3   128.0   467.0     487 

patient.follow_ups.follow_up.3.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  489.0   528.0   773.0   925.6  1020.5  2120.0     521 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    187     629    1224    1147    1442    2359     485 

patient.follow_ups.follow_up.3.days_to_new_tumor_event_after_initial_treatment:
 783  931 NA's 
   1    1  526 

patient.follow_ups.follow_up.3.disease_after_curative_tx:
  no  yes NA's 
  40    1  487 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             9                             42 
                          NA's 
                           477 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response         progressive disease 
                         37                           9 
             stable disease                        NA's 
                          2                         480 

patient.follow_ups.follow_up.3.lost_follow_up:
  no  yes NA's 
  43    5  480 

patient.follow_ups.follow_up.3.method_of_curative_tx:
concurrent chemotherapy               radiation                 surgery 
                     15                       2                      24 
                   NA's 
                    487 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   9.000   6.804   9.500  11.000     477 

patient.follow_ups.follow_up.3.new_neoplasm_event_occurrence_anatomic_site:
cervical lymph node         oral cavity                NA's 
                  1                   1                 526 

patient.follow_ups.follow_up.3.new_neoplasm_occurrence_anatomic_site_text:
buccal muco        NA's 
          1         527 

patient.follow_ups.follow_up.3.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   1    2  525 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   5    4  519 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   8    1  519 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    364     540     576     932    1085    2095     523 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  364.0   511.5   576.0   852.7  1116.0  2062.0     517 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
distant metastasis        oral cavity         oropharynx               NA's 
                 3                  2                  1                522 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      3                       3                       4 
                   NA's 
                    518 


patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   4    4  520 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                  6                                   2 
                               NA's 
                                520 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  36   11  481 

patient.follow_ups.follow_up.3.patient_death_reason:
      cancer related head and neck cancer                 NA's 
                   1                    5                  522 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        33         13        482 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         42                           1 
                       NA's 
                        485 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
   8   35  485 

patient.follow_ups.follow_up.3.smokeless_tobacco_use_age_at_quit:
  36   66 NA's 
   1    1  526 

patient.follow_ups.follow_up.3.smokeless_tobacco_use_age_at_start:
  23 NA's 
   1  527 

patient.follow_ups.follow_up.3.smokeless_tobacco_use_at_diag:
  no  yes NA's 
  40    2  486 

patient.follow_ups.follow_up.3.smokeless_tobacco_use_per_day:
 1.5    5 NA's 
   1    1  526 

patient.follow_ups.follow_up.3.smokeless_tobacco_use_regularly:
  no  yes NA's 
  40    4  484 

patient.follow_ups.follow_up.3.targeted_molecular_therapy:
  no  yes NA's 
  16   28  484 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
   43     7   478 

patient.follow_ups.follow_up.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2014    2014    2014    2015     477 

patient.follow_ups.follow_up.4.additional_pharmaceutical_therapy:
 yes NA's 
   1  527 

patient.follow_ups.follow_up.4.additional_radiation_therapy:
 yes NA's 
   1  527 

patient.follow_ups.follow_up.4.additional_surgery_locoregional_procedure:
 yes NA's 
   1  527 



patient.follow_ups.follow_up.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   15.25   16.00   17.00   17.00   31.00     510 

patient.follow_ups.follow_up.4.days_to_additional_surgery_locoregional_procedure:
 280 NA's 
   1  527 

patient.follow_ups.follow_up.4.days_to_completion_of_curative_tx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   97.25  115.00  116.86  146.75  207.00     514 

patient.follow_ups.follow_up.4.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  727.0   853.0   980.0   988.4   988.0  1394.0     523 

patient.follow_ups.follow_up.4.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    542    1038    1472    1447    1584    2239     515 

patient.follow_ups.follow_up.4.days_to_new_tumor_event_after_initial_treatment:
 234 NA's 
   1  527 

patient.follow_ups.follow_up.4.disease_after_curative_tx:
  no NA's 
  14  514 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             3                             15 
                          NA's 
                           510 

patient.follow_ups.follow_up.4.followup_treatment_success:
complete remission/response          persistent disease 
                         13                           1 
        progressive disease                        NA's 
                          4                         510 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
  17  511 

patient.follow_ups.follow_up.4.method_of_curative_tx:
chemotherapy (not given concurrently)                             radiation 
                                    1                                     2 
                              surgery                                  NA's 
                                   11                                   514 

patient.follow_ups.follow_up.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.389   4.750  10.000     510 

patient.follow_ups.follow_up.4.new_neoplasm_event_occurrence_anatomic_site:
oropharynx       NA's 
         1        527 

patient.follow_ups.follow_up.4.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  527 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   3  525 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   2    1  525 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
1730 NA's 
   1  527 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 781  876 1302 1716 NA's 
   1    1    1    1  524 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
cervical lymph node  distant metastasis          oropharynx                NA's 
                  1                   1                   1                 525 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      1                       2                     525 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
      lung oropharynx     r neck       NA's 
         1          1          1        525 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    1  525 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                  2                                   1 
                               NA's 
                                525 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  13    4  511 

patient.follow_ups.follow_up.4.patient_death_reason:
              cancer related         head and neck cancer 
                           1                            3 
other, non-malignant disease                         NA's 
                           1                          523 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        13          5        510 

patient.follow_ups.follow_up.4.primary_therapy_outcome_success:
complete remission/response                        NA's 
                         15                         513 

patient.follow_ups.follow_up.4.radiation_therapy:
  no  yes NA's 
   1   14  513 

patient.follow_ups.follow_up.4.smokeless_tobacco_use_age_at_quit:
  36   66 NA's 
   1    1  526 

patient.follow_ups.follow_up.4.smokeless_tobacco_use_at_diag:
  no  yes NA's 
  14    1  513 

patient.follow_ups.follow_up.4.smokeless_tobacco_use_regularly:
  no  yes NA's 
  13    2  513 

patient.follow_ups.follow_up.4.targeted_molecular_therapy:
  no  yes NA's 
   3   12  513 

patient.follow_ups.follow_up.4.vital_status:
alive  dead  NA's 
   13     5   510 

patient.follow_ups.follow_up.4.year_of_form_completion:
2012 2014 2015 NA's 
   2    3   13  510 

patient.follow_ups.follow_up.5.bcr_followup_barcode:
tcga-cn-4731-f69604 tcga-cn-4741-f69609 tcga-cn-6989-f69736                NA's 
                  1                   1                   1                 525 

patient.follow_ups.follow_up.5.bcr_followup_uuid:
8cd64e0e-ee30-4ef8-9b13-3ee0cea6d856 a3614061-a02a-4e1a-946d-526bbbcdc251 
                                   1                                    1 
bd22f7e1-8f6d-455d-8bb9-823bc1a37826                                 NA's 
                                   1                                  525 

patient.follow_ups.follow_up.5.day_of_form_completion:
  15   16 NA's 
   2    1  525 

patient.follow_ups.follow_up.5.days_to_completion_of_curative_tx:
  29 NA's 
   1  527 

patient.follow_ups.follow_up.5.days_to_death:
 980  998 NA's 
   1    1  526 

patient.follow_ups.follow_up.5.days_to_last_followup:
2239 NA's 
   1  527 

patient.follow_ups.follow_up.5.disease_after_curative_tx:
 yes NA's 
   1  527 

patient.follow_ups.follow_up.5.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                              1 
                          NA's 
                           525 

patient.follow_ups.follow_up.5.followup_treatment_success:
complete remission/response         progressive disease 
                          1                           2 
                       NA's 
                        525 

patient.follow_ups.follow_up.5.lost_follow_up:
  no NA's 
   3  525 

patient.follow_ups.follow_up.5.method_of_curative_tx:
surgery    NA's 
      1     527 

patient.follow_ups.follow_up.5.month_of_form_completion:
   1 NA's 
   3  525 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    1  526 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   2  526 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
2168 NA's 
   1  527 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 889 2168 NA's 
   1    1  526 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
distant metastasis               NA's 
                 2                526 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                526 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
lung NA's 
   2  526 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    1  526 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                  1                                   1 
                               NA's 
                                526 

patient.follow_ups.follow_up.5.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   1    2  525 

patient.follow_ups.follow_up.5.patient_death_reason:
head and neck cancer                 NA's 
                   2                  526 

patient.follow_ups.follow_up.5.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         1          2        525 

patient.follow_ups.follow_up.5.primary_therapy_outcome_success:
progressive disease                NA's 
                  1                 527 

patient.follow_ups.follow_up.5.radiation_therapy:
  no NA's 
   1  527 

patient.follow_ups.follow_up.5.smokeless_tobacco_use_at_diag:
  no NA's 
   1  527 

patient.follow_ups.follow_up.5.smokeless_tobacco_use_regularly:
  no NA's 
   1  527 

patient.follow_ups.follow_up.5.targeted_molecular_therapy:
  no NA's 
   1  527 

patient.follow_ups.follow_up.5.vital_status:
alive  dead  NA's 
    1     2   525 

patient.follow_ups.follow_up.5.year_of_form_completion:
2015 NA's 
   3  525 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
  30   27  471 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
  35   22  471 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure:
  no  yes NA's 
  37   11  480 

patient.follow_ups.follow_up.additional_surgery_metastatic_procedure:
  no  yes NA's 
  29    4  495 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   17.00   16.73   23.50   31.00      73 

patient.follow_ups.follow_up.days_to_additional_surgery_locoregional_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  106.0   152.0   209.0   427.6   640.0  1171.0     519 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure:
 364 1028 1797 NA's 
   1    1    1  525 

patient.follow_ups.follow_up.days_to_completion_of_curative_tx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-111.00   71.25  107.50  125.61  149.25 1220.00     390 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   217.2   395.0   702.2   620.2  5152.0     394 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0   360.0   558.0   814.3  1021.0  5480.0     215 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   175.0   236.0   434.3   561.0  1859.0     483 

patient.follow_ups.follow_up.disease_after_curative_tx:
  no  yes NA's 
 118   17  393 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                            11                            358 
                          NA's 
                           159 


patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 223   24  281 

patient.follow_ups.follow_up.method_of_curative_tx:
chemotherapy (not given concurrently)               concurrent chemotherapy 
                                    6                                    34 
                            radiation                               surgery 
                                   37                                    61 
                                 NA's 
                                  390 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.088   9.000  12.000      73 



patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 134   46  348 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no NA's 
   1  527 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   1  527 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 308 NA's 
   1  527 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 308  707 NA's 
   1    1  526 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
oral cavity        NA's 
          2         526 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
locoregional recurrence                    NA's 
                      2                     526 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
subcutaneous nodules on right side of jaw 
                                        1 
               tongue and floor of  mouth 
                                        1 
                                     NA's 
                                      526 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  527 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.progression_determined_by:
biopsy with histologic confirmation                                NA's 
                                  1                                 527 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  24   18  486 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  29   12  487 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   92.0   218.0   279.0   547.1   507.5  3329.0     513 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   46.0   177.5   339.0   415.3   496.5  3282.0     473 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     19                      32                       5 
                   NA's 
                    472 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  38   15  475 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                 28                                  20 
                               NA's 
                                480 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 183   58  287 

patient.follow_ups.follow_up.patient_death_reason:
        head and neck cancer other, non-malignant disease 
                          17                            4 
                        NA's 
                         507 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       297        108        123 


patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 136  265  127 

patient.follow_ups.follow_up.smokeless_tobacco_use_age_at_quit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  24.00   39.00   57.00   50.43   63.00   68.00     521 

patient.follow_ups.follow_up.smokeless_tobacco_use_age_at_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   16.75   20.00   21.88   22.50   38.00     520 

patient.follow_ups.follow_up.smokeless_tobacco_use_at_diag:
  no  yes NA's 
 170   10  348 

patient.follow_ups.follow_up.smokeless_tobacco_use_per_day:
   2   10   20 NA's 
   1    2    2  523 

patient.follow_ups.follow_up.smokeless_tobacco_use_regularly:
  no  yes NA's 
 151   16  361 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
 259  140  129 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  311   135    82 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2012    2013    2013    2014    2015      73 

patient.frequency_of_alcohol_consumption:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   6.000   4.114   7.000   7.000     304 

patient.gender:
female   male 
   142    386 

patient.histological_type:
                      head and neck squamous cell carcinoma 
                                                        517 
head and neck squamous cell carcinoma  spindle cell variant 
                                                          1 
        head and neck squamous cell carcinoma basaloid type 
                                                         10 

patient.history_of_neoadjuvant_treatment:
 no yes 
518  10 

patient.hpv_status_by_ish_testing:
negative positive     NA's 
      67       22      439 

patient.hpv_status_by_p16_testing:
negative positive     NA's 
      74       41      413 


patient.icd_o_3_histology:
8070/3 8071/3 8072/3 8074/3 8083/3 
   451     57     11      1      8 


patient.informed_consent_verified:
yes 
528 

patient.laterality:
   left midline   right    NA's 
    146      34     168     180 

patient.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   21.00   32.00   36.12   48.00  121.00     100 

patient.lymphnode_dissection_method_left:
functional (limited) neck dissection     modified radical neck dissection 
                                 186                                   86 
             radical neck dissection                                 NA's 
                                  20                                  236 

patient.lymphnode_dissection_method_right:
functional (limited) neck dissection     modified radical neck dissection 
                                 209                                   87 
             radical neck dissection                                 NA's 
                                  16                                  216 

patient.lymphnode_neck_dissection:
  no  yes NA's 
  97  428    3 

patient.lymphovascular_invasion_present:
  no  yes NA's 
 232  124  172 

patient.margin_status:
   close negative positive     NA's 
      50      357       60       61 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.277   9.000  12.000 


patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  32   11  485 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  29   14  485 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   42.0   211.0   246.0   411.1   365.0  1625.0     511 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   28.0   183.5   241.0   368.5   353.8  2318.0     480 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
  distant metastasis locoregional disease                 NA's 
                   3                   14                  511 


patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  32   17  479 

patient.new_tumor_events.new_tumor_event.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                 30                                  17 
                               NA's 
                                481 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 150   49  329 

patient.number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.186   3.000  44.000     115 

patient.number_of_lymphnodes_positive_by_ihc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.2706  0.0000 23.0000     310 

patient.number_pack_years_smoked:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
  0.01685  25.00000  40.00000  45.75496  60.00000 300.00000       230 


patient.perineural_invasion_present:
  no  yes NA's 
 198  172  158 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       341        141         46 

patient.postoperative_rx_tx:
  no  yes NA's 
 121   66  341 

patient.presence_of_pathological_nodal_extracapsular_spread:
        gross extension   microscopic extension no extranodal extension 
                     38                      77                     248 
                   NA's 
                    165 

patient.primary_lymph_node_presentation_assessment:
  no  yes NA's 
  54  431   43 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        147                           1 
        progressive disease              stable disease 
                         13                           4 
                       NA's 
                        363 

patient.prior_dx:
                                                     no 
                                                    495 
                                                    yes 
                                                     20 
                       yes, history of prior malignancy 
                                                      7 
yes, history of synchronous and or bilateral malignancy 
                                                      6 

patient.race:
american indian or alaska native                            asian 
                               2                               11 
       black or african american                            white 
                              48                              452 
                            NA's 
                              15 

patient.radiation_therapy:
  no  yes NA's 
  64  126  338 

patient.radiations.radiation.2.anatomic_treatment_site:
 distant recurrence    local recurrence primary tumor field       regional site 
                  4                   3                  19                  82 
               NA's 
                420 



patient.radiations.radiation.2.course_number:
   1    2    3 NA's 
  25    5    2  496 

patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   16.00   17.12   24.00   30.00     418 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   52.0    98.0   124.5   175.6   175.0  1780.0     418 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -291.0    49.0    77.5   130.6   132.5  1771.0     420 

patient.radiations.radiation.2.measure_of_response:
               complete response                 partial response 
                              61                                1 
radiographic progressive disease                   stable disease 
                               8                                1 
                            NA's 
                             457 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.618   9.000  12.000     418 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   24.00   25.00   24.54   32.00   50.00     437 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   59.82 5000.00 3274.30 5400.00 6930.00     420 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
 110  418 

patient.radiations.radiation.2.radiation_type:
               external           external beam other: specify in notes 
                     78                      28                       2 
                   NA's 
                    420 

patient.radiations.radiation.2.radiation_type_notes:
cyberknife       NA's 
         2        526 

patient.radiations.radiation.2.regimen_indication:
               adjuvant other: specify in notes              palliative 
                      9                      17                       2 
             recurrence                    NA's 
                      4                     496 

patient.radiations.radiation.2.regimen_indication_notes:
   back primary    NA's 
      1      17     510 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
  66   38  424 

patient.radiations.radiation.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2013    2013    2014    2015     418 

patient.radiations.radiation.3.anatomic_treatment_site:
 distant recurrence primary tumor field       regional site                NA's 
                  2                   6                  27                 493 



patient.radiations.radiation.3.course_number:
   1    2    4 NA's 
  15    2    1  510 

patient.radiations.radiation.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   12.50   15.00   17.46   24.00   28.00     493 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   63.0    96.5   115.0   170.4   151.5  1081.0     493 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -291.0    48.0    65.0   102.3   107.0  1081.0     495 

patient.radiations.radiation.3.measure_of_response:
               complete response radiographic progressive disease 
                              11                                2 
                            NA's 
                             515 

patient.radiations.radiation.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   6.771   9.000  11.000     493 

patient.radiations.radiation.3.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   27.00   33.00   29.32   33.00   38.00     497 

patient.radiations.radiation.3.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  42.00   54.76 4860.00 3350.29 6400.00 6930.00     495 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
  35  493 

patient.radiations.radiation.3.radiation_type:
               external           external beam other: specify in notes 
                     17                      17                       1 
                   NA's 
                    493 

patient.radiations.radiation.3.radiation_type_notes:
stereotactic radiation                   NA's 
                     1                    527 

patient.radiations.radiation.3.regimen_indication:
               adjuvant other: specify in notes              palliative 
                      3                      12                       2 
             recurrence                    NA's 
                      1                     510 

patient.radiations.radiation.3.regimen_indication_notes:
  chest primary    NA's 
      1      12     515 

patient.radiations.radiation.3.units:
 cgy   gy NA's 
  21   13  494 

patient.radiations.radiation.3.year_of_form_completion:
2011 2012 2013 2014 NA's 
  12    6   10    7  493 

patient.radiations.radiation.4.anatomic_treatment_site:
       distant site primary tumor field       regional site                NA's 
                  1                   1                   3                 523 



patient.radiations.radiation.4.course_number:
   1 NA's 
   2  526 

patient.radiations.radiation.4.day_of_form_completion:
   9   14   25 NA's 
   2    1    2  523 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  114.0   115.0   137.0   169.6   172.0   310.0     523 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   65.0    77.0    78.0   120.8   116.0   268.0     523 

patient.radiations.radiation.4.measure_of_response:
               complete response radiographic progressive disease 
                               1                                1 
                            NA's 
                             526 

patient.radiations.radiation.4.month_of_form_completion:
   4    9   11 NA's 
   1    3    1  523 

patient.radiations.radiation.4.numfractions:
  27   28   33   36 NA's 
   1    1    2    1  523 

patient.radiations.radiation.4.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.4  5280.0  5400.0  4378.1  5550.0  5610.0     523 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   5  523 

patient.radiations.radiation.4.radiation_type:
     external external beam          NA's 
            3             2           523 

patient.radiations.radiation.4.regimen_indication:
               adjuvant other: specify in notes                    NA's 
                      1                       1                     526 

patient.radiations.radiation.4.regimen_indication_notes:
primary    NA's 
      1     527 

patient.radiations.radiation.4.units:
 cgy   gy NA's 
   4    1  523 

patient.radiations.radiation.4.year_of_form_completion:
2011 2012 2013 2014 NA's 
   1    1    2    1  523 

patient.radiations.radiation.5.anatomic_treatment_site:
regional site          NA's 
            1           527 

patient.radiations.radiation.5.bcr_radiation_barcode:
tcga-cr-6471-r18086                NA's 
                  1                 527 

patient.radiations.radiation.5.bcr_radiation_uuid:
0b645599-8e25-453e-b8f5-450ec7e699d6                                 NA's 
                                   1                                  527 

patient.radiations.radiation.5.course_number:
   1 NA's 
   1  527 

patient.radiations.radiation.5.day_of_form_completion:
  14 NA's 
   1  527 

patient.radiations.radiation.5.days_to_radiation_therapy_end:
 114 NA's 
   1  527 

patient.radiations.radiation.5.days_to_radiation_therapy_start:
  77 NA's 
   1  527 

patient.radiations.radiation.5.month_of_form_completion:
   9 NA's 
   1  527 

patient.radiations.radiation.5.numfractions:
  27 NA's 
   1  527 

patient.radiations.radiation.5.radiation_dosage:
5400 NA's 
   1  527 

patient.radiations.radiation.5.radiation_treatment_ongoing:
  no NA's 
   1  527 

patient.radiations.radiation.5.radiation_type:
external beam          NA's 
            1           527 

patient.radiations.radiation.5.regimen_indication:
adjuvant     NA's 
       1      527 

patient.radiations.radiation.5.units:
 cgy NA's 
   1  527 

patient.radiations.radiation.5.year_of_form_completion:
2011 NA's 
   1  527 

patient.radiations.radiation.6.anatomic_treatment_site:
regional site          NA's 
            1           527 

patient.radiations.radiation.6.bcr_radiation_barcode:
tcga-cr-6471-r18090                NA's 
                  1                 527 

patient.radiations.radiation.6.bcr_radiation_uuid:
d0c21841-e729-4e83-ada6-dfb471ba6aab                                 NA's 
                                   1                                  527 

patient.radiations.radiation.6.course_number:
   2 NA's 
   1  527 

patient.radiations.radiation.6.day_of_form_completion:
  14 NA's 
   1  527 

patient.radiations.radiation.6.days_to_radiation_therapy_end:
 646 NA's 
   1  527 

patient.radiations.radiation.6.days_to_radiation_therapy_start:
 614 NA's 
   1  527 

patient.radiations.radiation.6.month_of_form_completion:
   9 NA's 
   1  527 

patient.radiations.radiation.6.numfractions:
  50 NA's 
   1  527 

patient.radiations.radiation.6.radiation_dosage:
6250 NA's 
   1  527 

patient.radiations.radiation.6.radiation_treatment_ongoing:
  no NA's 
   1  527 

patient.radiations.radiation.6.radiation_type:
external beam          NA's 
            1           527 

patient.radiations.radiation.6.regimen_indication:
recurrence       NA's 
         1        527 

patient.radiations.radiation.6.units:
 cgy NA's 
   1  527 

patient.radiations.radiation.6.year_of_form_completion:
2011 NA's 
   1  527 

patient.radiations.radiation.7.anatomic_treatment_site:
regional site          NA's 
            1           527 

patient.radiations.radiation.7.bcr_radiation_barcode:
tcga-cr-6471-r18091                NA's 
                  1                 527 

patient.radiations.radiation.7.bcr_radiation_uuid:
2ae99c0b-bc3f-48f3-83ff-25f41bae8462                                 NA's 
                                   1                                  527 

patient.radiations.radiation.7.course_number:
   3 NA's 
   1  527 

patient.radiations.radiation.7.day_of_form_completion:
  14 NA's 
   1  527 

patient.radiations.radiation.7.days_to_radiation_therapy_start:
 839 NA's 
   1  527 

patient.radiations.radiation.7.month_of_form_completion:
   9 NA's 
   1  527 

patient.radiations.radiation.7.radiation_treatment_ongoing:
  no NA's 
   1  527 

patient.radiations.radiation.7.radiation_type:
external beam          NA's 
            1           527 

patient.radiations.radiation.7.regimen_indication:
recurrence       NA's 
         1        527 

patient.radiations.radiation.7.units:
 cgy NA's 
   1  527 

patient.radiations.radiation.7.year_of_form_completion:
2011 NA's 
   1  527 




patient.radiations.radiation.course_number:
   1    2 NA's 
 102    7  419 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   17.00   16.89   24.00   31.00     214 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    91.0   117.0   138.7   151.0  1302.0     227 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   48.75   70.00   91.65  103.00 1248.00     232 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                             150                                4 
radiographic progressive disease                   stable disease 
                              16                                1 
                            NA's 
                             357 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.436   9.000  12.000     214 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   30.00   30.00   29.16   33.00   41.00     298 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      9      66    6000    4052    6600    7380     254 

patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
 308    2  218 

patient.radiations.radiation.radiation_type:
     external external beam      implants          NA's 
          174           118             1           235 


patient.radiations.radiation.regimen_indication_notes:
concurrent chemoradiation as definitive treatment 
                                                1 
                                      neoadjuvant 
                                                4 
                                          primary 
                                               19 
                                             NA's 
                                              504 

patient.radiations.radiation.units:
 cgy   gy NA's 
 191   84  253 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2011    2013    2013    2013    2015     214 



patient.stage_event.system_version:
4th 5th 6th 7th 
  1  10 125 392 

patient.stage_event.tnm_categories.clinical_categories.clinical_m:
  m0   m1   mx NA's 
 496    6   21    5 



patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
  m0   m1   mx NA's 
 191    1   65  271 



patient.stopped_smoking_year:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1955    1990    2000    1997    2009    2013     340 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 344  182    2 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
 182  344    2 



patient.tumor_tissue_site:
head and neck 
          528 

patient.vital_status:
alive  dead 
  358   170 

patient.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2010    2011    2012    2012    2013    2015 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1992    2007    2010    2008    2011    2013 

patient.year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1936    1959    1968    1967    1975    2001     246 


patient.hpv_test_results.hpv_test_result.hpv_call_2:
hpv33  NA's 
    1   527 

patient.hpv_test_results.hpv_test_result.hpv_status:
indeterminate      negative      positive 
            2           427            99 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
      case matched expression control      case matched methylation control 
                                   42                                    33 
                  germline comparison primary tumor and metastatic tumor(s) 
                                    7                                     2 
                                 NA's 
                                  444 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              28                               27 
                            NA's 
                             473 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                 28                 500 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             444               84 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      412       116 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    53.5   351.0  1279.9  1620.5  6835.0       5 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -12.00    0.00    4.00   25.53   35.00  718.00       1 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
274 254 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     7                    127                    394 


patient.biospecimen_cqcf.histological_type:
                      head and neck squamous cell carcinoma 
                                                        517 
head and neck squamous cell carcinoma  spindle cell variant 
                                                          1 
        head and neck squamous cell carcinoma basaloid type 
                                                         10 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             518 
yes, pharmaceutical treatment prior to resection 
                                               1 
               yes, radiation prior to resection 
                                               9 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -26.00    0.00    0.00   66.31   14.00 1158.00     439 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.1970   0.2118   0.2550  55.5772  40.7160 282.0000      520 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
 gentra puregene kit uv spectrophotometry                 NA's 
                   1                    7                  520 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.900   5.555   6.030   7.525   7.325  17.200     520 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
  20   25   30   50 NA's 
   1    5    1    1  520 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
tumor resection            NA's 
             11             517 


patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           26           502 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2    3 NA's 
  50   51    1  426 


patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
   0   53 NA's 
   5    1  522 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_conc:
0.216  NA's 
    1   527 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qm:
uv spectrophotometry                 NA's 
                   1                  527 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qty:
 5.4 NA's 
   1  527 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_vol:
  25 NA's 
   1  527 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
larynx   NA's 
     4    524 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            1           527 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    2 NA's 
   5    1  522 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-384.00    0.00    5.00   33.37   34.00 1614.00       5 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.0352    0.1240    0.3990  157.3650  260.0550 1107.1000       285 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.99   12.44   17.00   19.16   20.00  315.52     285 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   43.35   65.00  102.96  199.19  920.52     285 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
blood draw       NA's 
       207        321 


patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           30           498 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
  1   2 
502  26 

patient.biospecimen_cqcf.other_vessel_used:
        15 ml tube          15ml tube          cryosette polypropylene tube 
                 3                  3                  1                  3 
              NA's 
               518 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
  no  yes NA's 
   3  521    4 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
528 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 yes NA's 
 524    4 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
  no  yes NA's 
   4  520    4 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                        2                        18                       132 
                     NA's 
                      376 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
                                  macrodissection performed at tss to select for a region containing an acceptable tcga diagnosis 
                                                                                                                                1 
pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                3 
                                                                                                                             NA's 
                                                                                                                              524 

patient.biospecimen_cqcf.sample_prescreened:
 no yes 
  4 524 

patient.biospecimen_cqcf.site_of_disease:
head/neck 
      528 


patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
 178   47  303 

patient.biospecimen_cqcf.top_slide_submitted:
 no yes 
306 222 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    5.00   10.44   15.00   30.00     519 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   70.00   70.00   67.22   75.00   85.00     519 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0    82.5   200.0   157.1   202.0   300.0     521 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   3    7  518 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   15.00   14.89   30.00   50.00      16 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   70.00   70.00   72.49   80.00  100.00      16 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   104.2   199.0   188.8   200.0  2257.0       2 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1   2 
527   1 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                      7                      22                      45 
               cryovial  other (please specify) 
                    444                      10 

patient.days_to_index:
   0 NA's 
 527    1 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and metastatic tumor(s)                                  NA's 
                                    2                                   526 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0   120.0   371.0   834.6   903.0  6651.0     325 

patient.samples.sample.2.initial_weight:
 180  670 NA's 
   1    1  526 

patient.samples.sample.2.is_ffpe:
 no 
528 

patient.samples.sample.2.oct_embedded:
false  true  NA's 
  198     1   329 

patient.samples.sample.2.pathology_report_file_name:
tcga-ku-a6h7.010441cc-0047-41de-8416-565bcd270abb.pdf 
                                                    1 
tcga-uf-a71a.4e1d60ca-1c14-4c87-a590-96fa1d366bdc.pdf 
                                                    1 
                                                 NA's 
                                                  526 

patient.samples.sample.2.pathology_report_uuid:
010441cc-0047-41de-8416-565bcd270abb 4e1d60ca-1c14-4c87-a590-96fa1d366bdc 
                                   1                                    1 
                                NA's 
                                 526 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.89 1.94 NA's 
   1    1  526 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
150756749 153878527 153878536 156036038 153878577 167787800       523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    8   13 NA's 
   1    3    1  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.17 0.18  0.5 NA's 
   1    1    3  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  15   24   30 NA's 
   3    1    1  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   4    7 NA's 
   1    4  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   2    6    9 NA's 
   3    1    1  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a315 a31r a39i NA's 
   3    1    1  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   b    d    g    h NA's 
   1    2    1    1  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
2.21  3.6    5 NA's 
   1    1    3  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   5  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  10   13   20 NA's 
   3    1    1  523 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2013 2014 NA's 
   4    1  523 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.323e+08 1.678e+08 4.879e+08 1.054e+09 1.062e+09         3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   1  523    1    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.17 0.18  0.5 NA's 
   1    1  523    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     8.0    15.0    15.4    24.0    30.0       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 525    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   5.971   8.000  12.000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   7.048   9.000  12.000       3 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
2.34  3.4   40 NA's 
   1    1  523    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 312  213    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
   1    1  523    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
 241   71  143   70    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  58.31  168.00  177.00  179.04  188.75  260.00     214 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 523                    2                    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   2  523    3 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
0.17 0.18  0.5 NA's 
   1    1  312  214 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 523    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
 312  211    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             525 
                                            NA's 
                                               3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 525    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                 repli-g x 
                        2                       507                        16 
                     NA's 
                        3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
1.27 1.83 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
 8.6  8.8 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
      222        92       214 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-ku-a6h7-06a-21w-a333-08 tcga-uf-a71a-06a-11w-a44y-08 
                           1                            1 
                        NA's 
                         526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
ddb38cf0-b307-4796-ae26-f7a66d4c3a03 e75d960b-ae8e-4973-97e9-e5d7b57e1258 
                                   1                                    1 
                                NA's 
                                 526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
153887520 167788761      NA's 
        1         1       526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   8   21 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   7   10 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   1   11 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a333 a44y NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   d    h NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2013 2014 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-ku-a6h7-06a-21w tcga-uf-a71a-06a-11w                 NA's 
                   1                    1                  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
5f615d28-3a17-4ddc-951c-5aa90376569c 63b97922-7cb4-45af-92b6-86539737ca35 
                                   1                                    1 
                                NA's 
                                 526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     526 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   2  526 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.800   1.900   1.869   1.900   2.000     363 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.040e+08 1.210e+08 1.323e+08 3.864e+08 1.050e+09 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   2   8 
131  40 357 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05000 0.05000 0.08000 0.09121 0.14000 0.17000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   12.00   19.00   18.48   25.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   7.583  11.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.451   8.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.300   3.000   2.401   3.000   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 22  23 
328 200 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   10.00   40.00   35.37   60.00   60.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 2014 
  39  233   86  158   12 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.087e+08 1.283e+08 5.530e+08 1.054e+09 1.076e+09       301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8   10   26 NA's 
   8  169    5   45  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.05000 0.05000 0.07392 0.07500 0.17000     301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.00   20.00   18.65   27.00   30.00     301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 227  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.500   7.000   6.524   7.000  11.000     301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.767  10.000  12.000     301 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   2.000   3.000   2.585   3.000   4.200     301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 221    6  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   28.35   40.00   43.82   60.00   60.00     301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2012    2014     301 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.283e+08 1.283e+08 2.630e+08 1.508e+08 1.076e+09       479 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8   10 NA's 
  11   15   23  479 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0800  0.0939  0.1400  0.1700     479 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   16.00   11.98   20.00   30.00     479 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  49  479 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.755  10.000  10.000     479 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.918   6.000   7.000     479 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.530   1.600   2.000   2.651   3.740   4.800     479 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
  34   15  479 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.60   20.00   26.70   29.68   30.00   60.00     479 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 2013 NA's 
  17   17   15  479 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
128285338 128287358 150756972 147056737 150757041 167811805       519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8    9   10 NA's 
   1    1    7  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.05 0.07 0.08 0.15 NA's 
   2    3    3    1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   2   16   21   30 NA's 
   5    1    2    1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   9  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1    4    5   10 NA's 
   2    1    2    4  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    2    3    4 NA's 
   1    5    1    2  519 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.070   1.400   1.827   1.600   4.500     519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
   3    6  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4   20   30   60 NA's 
   2    5    1    1  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2012 2013 2015 NA's 
   3    4    2  519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-cv-5443-11a-01d-a46a-09                         NA's 
                           1                          527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
9ae993ad-ea03-4539-90d1-8d52ca61ff80                                 NA's 
                                   1                                  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
167811794      NA's 
        1       527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   9 NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.08 NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  21 NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1 NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   3 NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a46a NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   d NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
1.07 NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
13.4 NA's 
   1  527 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2015 NA's 
   1  527 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.040e+08 1.210e+08 1.323e+08 3.868e+08 1.050e+09 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   8 
397 130   1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.080   0.140   0.150   0.149   0.160   0.170 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   12.00   19.00   18.48   25.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   7.583  11.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.432   8.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.800   0.940   1.060   1.199   1.140   2.260 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
328 200 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  5.000   6.670   6.700   8.078   6.700  20.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
  39  233   86  158   12 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   14.00   36.31  103.70   91.25 1324.00 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
528 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
528 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
  40  166  160  121   41 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
528 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
329 199 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             528 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                              120                                76 
pre-extracted dna received by tss 
                              332 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      363       165 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   2  526 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   17.00   16.02   22.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no 
528 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   7.032   9.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
  1   2  11  21 
524   2   1   1 

patient.samples.sample.2.portions.portion.portion_sequence:
   1    2 NA's 
 184    3  341 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-cv-5439-11b-01-ts1                    NA's 
                      1                     527 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_uuid:
866024e7-85c6-4ecc-a121-ba1ea5a48bf7                                 NA's 
                                   1                                  527 

patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1  527 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
 top NA's 
   1  527 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  19  509 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   5   15 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    3 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0    5 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    3 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0 NA's 
   2  526 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
  25   40 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
  60   70 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
  75   80 NA's 
   1    1  526 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     1     18    509 

patient.samples.sample.2.portions.portion.weight:
   1    2   30 NA's 
   4  114    2  408 

patient.samples.sample.2.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
  74  223   81  139   11 

patient.samples.sample.2.sample_type:
blood derived normal           metastatic  solid tissue normal 
                 509                    2                   17 

patient.samples.sample.2.sample_type_id:
  6  10  11 
  2 509  17 

patient.samples.sample.2.vial_number:
  a   b   c   d 
504  16   2   6 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  2                 526 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   36.0   136.0   230.0   359.1   321.5  1333.0     521 

patient.samples.sample.3.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     20      60      80      74     100     110     523 

patient.samples.sample.3.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.7000  0.8000  0.8377  1.0000  1.6000     475 

patient.samples.sample.3.is_ffpe:
  no NA's 
  67  461 

patient.samples.sample.3.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.000   1.200   1.211   1.500   1.900     475 

patient.samples.sample.3.oct_embedded:
false  NA's 
    7   521 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.2  1.6  1.7  1.8 NA's 
   1    1    5   10  511 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.173e+08 1.244e+08 5.743e+08 1.054e+09 1.076e+09       489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7 NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1521  0.1600  0.1700     489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   12.00   12.00   16.38   27.00   29.00     489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000  10.000   8.333  10.000  12.000     489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   7.000   7.744  10.000  12.000     489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1915    1966    2016    2025    2081    2403     489 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   2.000   2.000   2.025   2.130   2.260     489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3 NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 NA's 
  33    6  489 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.210e+08 6.089e+08 5.880e+08 1.054e+09 1.076e+09       466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   8   13 NA's 
  23   39  466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.2811  0.5000  0.5000     466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   12.00   14.00   16.45   24.00   30.00     466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  62  466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   7.516  10.000  12.000     466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.500   7.403  10.000  12.000     466 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.60    3.00    3.20   16.75   40.00   40.00     466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
  60    2  466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20   80 NA's 
  39   23  466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
  49   11    1    1  466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   26.2   133.2   166.7   165.5   203.2   352.2     468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                  23                   39                  466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  39   23  466 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.2738  0.5000  0.5000     468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  23  505 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
  21    2  505 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              62 
                                            NA's 
                                             466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  62  466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                       39                        23                       466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.990   1.220   1.400   1.466   1.700   2.100     489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.800   8.300   8.361   9.000   9.700     489 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
       14        46       468 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.173e+08 1.210e+08 5.053e+08 1.062e+09 1.062e+09       489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    7.00   15.00   15.41   25.00   30.00     489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   7.564  11.000  12.000     489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.462   9.500  12.000     489 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
  38    1  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 2013 NA's 
  28   10    1  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  141.0   161.2   171.5   170.2   180.0   191.0     490 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  39                  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
  39  489 



patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
  38  490 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
  38    1  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              39 
                                            NA's 
                                             489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  39  489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
       38         1       489 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen      NA's 
       38       490 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   1.895   1.900   1.886   1.900   2.000     500 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              2                             526 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.136e+08 1.210e+08 4.842e+08 1.054e+09 1.076e+09       461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   8.000   6.298   8.000  13.000     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0888  0.1400  0.1800     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   12.00   12.00   16.94   24.00   27.00     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  67  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.500   7.000   7.597  10.000  12.000     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   7.000   7.522  10.000  12.000     461 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.500   3.000   2.469   3.000   3.600     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
  60    7  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   10.00   60.00   40.29   60.00   60.00     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 NA's 
  51    9    7  461 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.136e+08 1.062e+09 6.865e+08 1.062e+09 1.076e+09       475 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    7    8   26 NA's 
  24    1   21    7  475 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1400  0.1091  0.1500  0.1700     475 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   12.00   13.00   17.02   27.00   27.00     475 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  53  475 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   7.811  10.000  12.000     475 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.887  11.000  12.000     475 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.060   2.000   2.800   2.494   3.000   3.740     475 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
  52    1  475 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.60   13.30   20.00   32.25   60.00   60.00     475 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2014 NA's 
  44    8    1  475 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.136e+08 1.136e+08 5.079e+08 1.062e+09 1.062e+09       492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8   10   26 NA's 
  20    1    1   14  492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1400  0.1411  0.1500  0.1700     492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   13.00   20.00   19.86   27.00   27.00     492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  36  492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   6.000   5.889   7.000   7.000     492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.500   4.194   6.000   7.000     492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1512    1913    1913    2031    2318    2318     492 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.965   2.800   2.587   3.050   3.740     492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
  36  492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.30   13.30   20.00   18.79   20.00   40.00     492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 NA's 
  21   15  492 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-cv-6961-11a-01d-2268-08                         NA's 
                           1                          527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
26f6321f-f079-430f-8b87-5f06b92c07af                                 NA's 
                                   1                                  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
128287377      NA's 
        1       527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8 NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.16 NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   2 NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   5 NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   3 NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
2268 NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   g NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
 4.8 NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
  30 NA's 
   1  527 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2012 NA's 
   1  527 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              2                             526 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.136e+08 1.210e+08 4.697e+08 1.054e+09 1.076e+09       461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    7   13 NA's 
  55    7    4    1  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1473  0.1500  0.1800     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   12.00   12.00   16.94   24.00   27.00     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  67  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.500   7.000   7.597  10.000  12.000     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   7.000   7.522  10.000  12.000     461 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   0.930   1.000   1.176   1.070   3.600     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
  60    7  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67    7.94    6.67   20.00     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 NA's 
  51    9    7  461 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.22   52.70   99.70  117.88  180.50  374.00     461 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna  rna NA's 
  62    5  461 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d    r NA's 
  62    5  461 



patient.samples.sample.3.portions.portion.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             527 

patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1473  0.1500  0.1800     461 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  62  466 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
  60    2  466 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              67 
                                            NA's 
                                             461 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  67  461 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type         mirvana (allprep dna) rna 
                               55                                 5 
pre-extracted dna received by tss                              NA's 
                                7                               461 

patient.samples.sample.3.portions.portion.analytes.analyte.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.130   1.280   1.350   1.366   1.380   1.690     523 

patient.samples.sample.3.portions.portion.analytes.analyte.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    7.30    7.70    7.66    7.80    8.50     523 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       39        28       461 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   5  523 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00   22.00   16.43   24.50   26.00     461 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  67  461 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   7.000   7.194   8.500  11.000     461 

patient.samples.sample.3.portions.portion.portion_number:
   1   11   21 NA's 
  62    4    1  461 

patient.samples.sample.3.portions.portion.portion_sequence:
   1    2 NA's 
   6    1  521 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  61  467 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.75    4.00   12.08   16.25   40.00     516 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1    3   20 NA's 
   4    2    1    2  519 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0   20 NA's 
   4    1  523 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    3   20 NA's 
   5    2    1    2  518 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
  40   70  100 NA's 
   1    1    3  523 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0   30   40 NA's 
   3    1    1  523 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   5  523 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   5  523 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
  61  467 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   57.00   62.00   57.20   64.25   74.00     468 

patient.samples.sample.3.portions.portion.year_of_creation:
2010 2011 2012 2013 NA's 
   1   58    1    7  461 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   2                   65                  461 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
   2   65  461 

patient.samples.sample.3.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.4000  0.5000  0.5151  0.6000  1.0000     475 

patient.samples.sample.3.vial_number:
   a    b NA's 
  63    4  461 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0   117.0   344.0   827.9   866.0  6651.0     331 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
4c6d6f13-08e5-41dd-be8f-d89087d00f22 9117aba6-d184-4ea1-b7ba-6006427dc59d 
                                   1                                    1 
e58a0e28-d773-493b-8119-5883f1aa9b79                                 NA's 
                                   1                                  525 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.4:
370068fc-e205-494a-a5c1-0cbafe2a28f2                                 NA's 
                                   1                                  527 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     10      70     130     167     210     870     331 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.400   0.600   0.617   0.800   1.800     223 

patient.samples.sample.is_ffpe:
 no 
528 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.7000  0.9000  0.9334  1.2000  3.2000     223 

patient.samples.sample.oct_embedded:
false  true  NA's 
  101    97   330 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.640   1.800   1.800   1.827   1.870   2.230     130 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.210e+08 1.507e+08 3.792e+08 1.050e+09 1.076e+09         5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
 409  114    5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1568  0.1600  0.1800       5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   12.00   19.00   18.69   25.00   30.00       5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 523    5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   7.518  11.000  12.000       5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.038   5.000  12.000       5 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   2.000   2.210   2.673   3.400   4.540       5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
 323  200    5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13 13.3   20 26.7 NA's 
  86  231  114   92    5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2013    2014       5 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167812919 167812924 167812968 167812960 167812987 167812989       519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   9  519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 0.16 0.17 0.18 NA's 
   3    1    3    2  519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   9 NA's 
   9  519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   9  519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  12 NA's 
   9  519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   3.556   5.000   6.000     519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a466 NA's 
   9  519 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
0.99 1.06 1.12 1.19 NA's 
   3    1    3    2  519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   9  519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6 NA's 
   9  519 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   9  519 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.040e+08 1.210e+08 1.323e+08 3.754e+08 1.050e+09 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  7  13 
119 409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1500  0.1568  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   12.00   19.00   18.65   25.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   7.551  11.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.881   5.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   2.800   3.000   2.895   3.200   4.540 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
 22  23 
328 200 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13 18.1   20 26.7 
 114    1  408    5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
  39  233   86  159   11 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    3.2    50.2    83.2   137.4   209.2   661.2 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
528 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
  r 
528 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1500  0.1568  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             528 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna 
                       11                       517 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.470   1.630   1.605   1.790   2.610 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.900   8.600   8.529   9.100   9.900 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
    528 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 402   38    2   86 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-cn-4735-01a-01w-a315-08 tcga-cr-6482-01a-11w-a315-08 
                           1                            1 
tcga-cr-6487-01a-11w-a315-08                         NA's 
                           1                          525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
218dec27-87c2-4c8a-b6c0-aa478ac75af6 da1d8c86-4361-474a-9741-89dcf3b00721 
                                   1                                    1 
de339b1c-33cf-4903-970c-b315b27c5ec9                                 NA's 
                                   1                                  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
153878551 153878558 153878560      NA's 
        1         1         1       525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8 NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  15 NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   7 NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   2 NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
a315 NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   c    e    f NA's 
   1    1    1  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
   5 NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  23 NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  10 NA's 
   3  525 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2013 NA's 
   3  525 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.040e+08 1.322e+08 1.608e+08 4.771e+08 1.054e+09 1.062e+09 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  8  13 
523   5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
0.1 0.5 
  5 523 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    8.00   15.00   15.56   24.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   7.000   6.051   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.398   6.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  3  40 
  5 523 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
 22  23 
312 216 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
 30  80 
  5 523 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
   5  236   71  146   70 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   18.5   165.0   175.0   174.8   187.0   242.0     216 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna            total rna 
                 523                    5 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
  t   w 
  5 523 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
 0.1  0.5 NA's 
   5  307  216 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 523    5 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
 307  216    5 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             528 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x total rna 
      507        16         5 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.520   0.630   0.980   0.848   1.050   1.060     523 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.90    7.00    7.20    7.26    7.50    7.70     523 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
      215        97       216 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 402   38    2   86 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
1.05e+09 1.05e+09 1.05e+09 1.05e+09 1.05e+09 1.05e+09      523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   3 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   1 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
  11 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
1487 NA's 
   5  523 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2011 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  134.0   158.0   163.0   161.6   176.0   177.0     523 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   5                  523 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
   5  523 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             523 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   5  523 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      5     523 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      5     523 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9    2 NA's 
   1  164    4  359 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.040e+08 1.150e+08 1.323e+08 3.767e+08 1.050e+09 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   2   5   8 
131  40  15 342 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05000 0.05000 0.08000 0.09415 0.14000 0.17000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   12.00   19.00   18.65   25.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   7.551  11.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.114   5.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.300   3.000   2.392   3.000   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 22  23 
328 200 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   10.00   40.00   34.68   60.00   60.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 2014 
  39  233   86  159   11 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.040e+08 1.150e+08 1.323e+08 3.485e+08 1.678e+08 1.062e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  5   8 
401 127 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0500  0.1300  0.1500  0.1284  0.1600  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   12.00   19.00   17.96   25.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   7.589  11.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.246   6.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.730   1.950   2.080   2.471   3.000   4.540 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
 22  23 
328 200 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  13 13.3 26.7   40   60 
 184  158   59   16  111 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2012 2013 2014 
  39  233   86  159   11 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.087e+08 1.136e+08 5.301e+08 1.054e+09 1.076e+09       300 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.500   9.912   8.000  26.000     300 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1400  0.1179  0.1500  0.1700     300 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.00   20.00   19.14   27.00   30.00     300 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 228  300 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.447   7.000  11.000     300 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   2.943   4.000  10.000     300 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.000   2.000   2.466   3.000   4.540     300 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
 224    4  300 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   20.00   24.65   40.00   60.00     300 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2012 2013 NA's 
  39  133   52    4  300 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.283e+08 1.283e+08 1.395e+08 2.310e+08 1.508e+08 1.076e+09       488 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8   10   26 NA's 
  13   26    1  488 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0800  0.0878  0.1400  0.1600     488 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    2.00    9.45   16.00   30.00     488 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  40  488 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   5.000   5.500   7.225  10.000  11.000     488 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   2.275   3.000  10.000     488 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.600   3.000   2.732   3.450   4.500     488 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  24   16  488 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
  20   30   60 NA's 
  17   11   12  488 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 2012 2013 NA's 
   2   22   16  488 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
121031875 135894428 150757009 149573303 167811784 167811814       521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   5    9   10 NA's 
   2    2    3  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 0.08 0.14 NA's 
   1    4    2  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   2   21   30 NA's 
   2    3    2  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   7  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1   10   11 NA's 
   3    2    2  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   1    2    3    5 NA's 
   2    1    2    2  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
2064 a32x a469 a46a NA's 
   2    2    1    2  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   a    b    c    f NA's 
   1    1    3    2  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
1.07  1.4  1.6 1.86 NA's 
   3    1    1    2  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
   2    5  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
13.3 13.4   20 NA's 
   2    3    2  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 2013 2015 NA's 
   2    2    3  521 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-cv-6955-01a-11d-2317-26 tcga-cv-6956-01a-21d-2317-26 
                           1                            1 
                        NA's 
                         526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
080c55b2-1a8c-4a68-942f-eef7e126473c 118f9dcf-edc1-46fa-aa9d-c0a22b494c89 
                                   1                                    1 
                                NA's 
                                 526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
1061615481 1061615502       NA's 
         1          1        526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
  26 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.14 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  13 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   6 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   2 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
2317 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   a    b NA's 
   1    1  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
 2.8 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
  20 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2012 NA's 
   2  526 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-cv-6956-01a-21d-a32x-10                         NA's 
                           1                          527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
5046f1b4-9d95-4841-ba30-691fa44719a2                                 NA's 
                                   1                                  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
150756997      NA's 
        1       527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
  10 NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.07 NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   2 NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
  10 NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   3 NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
a32x NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   d NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
 1.4 NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  23 NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
  20 NA's 
   1  527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2013 NA's 
   1  527 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.040e+08 1.210e+08 1.323e+08 3.761e+08 1.050e+09 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   8 
397 130   1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1400  0.1500  0.1508  0.1600  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   12.00   19.00   18.65   25.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   7.551  11.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.106   5.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.000   1.070   1.224   1.140   2.800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
328 200 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7   10 13.3   40 
 198  199   33   97    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
  39  233   86  159   11 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.03   31.04   72.70  139.07  226.20  747.33 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
528 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
528 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
  16  124  224  120   44 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
528 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
329 199 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             528 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  528 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      359       169 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 402   38    2   86 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   20.00   18.09   26.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
528 

patient.samples.sample.portions.portion.lce:
 lce NA's 
   3  525 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   8.000   7.159   9.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   11.00   10.98   11.00   51.00 

patient.samples.sample.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.249   1.000   5.000     335 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 212  316 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00   10.00   11.25   15.50   50.00     460 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0    1    2 NA's 
  40    3    2  483 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.118   2.250  21.000     316 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.217   2.500  22.000     468 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.473   0.000  50.000     321 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    6.00   15.00   17.08   25.00   75.00     322 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   75.00   85.00   79.97   90.00  100.00     318 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   70.00   75.00   75.45   80.00   98.00     316 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    11    201    316 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
528 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.00    8.00   11.62   15.00  100.00     216 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    1.39    0.00   20.00     256 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   2.157   2.000  30.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.266   1.000  50.000     246 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.752   0.000  70.000      12 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   15.00   17.99   25.00   75.00      10 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   70.00   80.00   77.32   90.00  100.00       2 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   67.75   75.00   74.45   80.00  100.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
   201    327 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   29.00   30.00   30.75   33.00  163.00      26 

patient.samples.sample.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
  76  227   75  145    5 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 357  171 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 357  171 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   13.00   21.00   18.01   21.00   61.00     171 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    3    6 NA's 
  22  109    1    1  395 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   7   30 NA's 
 212  145  171 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   7   12 NA's 
 145  212  171 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2011 2014 NA's 
 212  145  171 

patient.samples.sample.sample_type:
primary tumor 
          528 

patient.samples.sample.sample_type_id:
  1 
528 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.2000  0.3000  0.3351  0.5000  0.9000     223 

patient.samples.sample.vial_number:
  a   b 
523   5 

}}
\keyword{datasets}
