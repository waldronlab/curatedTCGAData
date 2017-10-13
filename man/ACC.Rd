\name{ ACC }
\alias{ ACC }
\docType{data}
\title{ Adrenocortical carcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( ACC )
ExperimentList class object of length 9: 
 [1] CNASNP-20160128: RaggedExperiment with 79861 rows and 180 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 21052 rows and 180 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 90 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 90 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 80 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 80 columns 
 [7] Mutation-20160128: RaggedExperiment with 20166 rows and 90 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 79 columns 
 [9] RPPAArray-20160128: SummarizedExperiment with 192 rows and 46 columns 

> rownames( ACC )
CharacterList of length 9
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363

> colnames( ACC )
CharacterList of length 9
[["CNASNP-20160128"]] TCGA-OR-A5J1-01A-11D-A29H-01 ...
[["CNVSNP-20160128"]] TCGA-OR-A5J1-01A-11D-A29H-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-OR-A5J1-01A-11D-A29H-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-OR-A5J1-01A-11D-A29H-01 ...
[["Methylation-20160128"]] TCGA-OR-A5J1-01A-11D-A29J-05 ...
[["miRNASeqGene-20160128"]] TCGA-OR-A5J1-01A-11R-A29W-13 ...
[["Mutation-20160128"]] TCGA-OR-A5J1-01A-11D-A29I-10 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-OR-A5J1-01A-11R-A29S-07 ...
[["RPPAArray-20160128"]] TCGA-OR-A5J2-01A-21-A39K-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  2.2 Mb
2                   CNVSNP-20160128  0.6 Mb
3         GISTIC_AllByGene-20160128  4.3 Mb
4 GISTIC_ThresholdedByGene-20160128  4.3 Mb
5              Methylation-20160128 67.4 Mb
6             miRNASeqGene-20160128  0.1 Mb
7                 Mutation-20160128 11.9 Mb
8          RNASeq2GeneNorm-20160128  1.1 Mb
9                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   58 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  34.00   34.00    1.51    1.34    3.03 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.00   35.50   48.50   47.16   60.00   83.00 

vital_status:
 0  1 
58 34 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   399.0   551.5   854.6  1202.2  2405.0      58 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  383.0   917.8  1453.5  1781.6  2339.2  4673.0      34 

tumor_tissue_site:
adrenal 
     92 

pathologic_stage:
  stage i  stage ii stage iii  stage iv      NA's 
        9        44        19        18         2 

pathology_T_stage:
  t1   t2   t3   t4 NA's 
   9   49   11   21    2 

pathology_N_stage:
  n0   n1 NA's 
  80   10    2 

gender:
female   male 
    60     32 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1997    2005    2008    2007    2011    2012 

radiation_therapy:
  no  yes NA's 
  71   18    3 

histological_type:
   adrenocortical carcinoma- myxoid type 
                                       1 
adrenocortical carcinoma- oncocytic type 
                                       4 
    adrenocortical carcinoma- usual type 
                                      87 

residual_tumor:
  r0   r1   r2   rx NA's 
  64    7   12    6    3 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.548   0.500  52.000      61 

race:
                    asian black or african american                     white 
                        2                         1                        78 
                     NA's 
                       11 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     8                     40                     44 

admin.bcr:
nationwide children's hospital 
                            92 

admin.day_of_dcc_upload:
 1 
92 

admin.disease_code:
acc 
 92 


admin.month_of_dcc_upload:
10 
92 

admin.patient_withdrawal.withdrawn:
false 
   92 

admin.project_code:
tcga 
  92 

admin.year_of_dcc_upload:
2015 
  92 


patient.clinical_cqcf.consent_or_death_status:
consented  deceased    waiver 
       75         6        11 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -294.0     0.0     0.0   169.5    22.5  3682.0      17 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   245.0   423.0   571.7   706.0  1677.0      85 

patient.clinical_cqcf.histological_type:
   adrenocortical carcinoma- myxoid type 
                                       1 
adrenocortical carcinoma- oncocytic type 
                                       4 
    adrenocortical carcinoma- usual type 
                                      87 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                              91 
yes, pharmaceutical treatment prior to resection 
                                               1 

patient.clinical_cqcf.normal_tissue_anatomic_site:
kidney  other   NA's 
     3      1     88 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
            4            88 

patient.clinical_cqcf.tumor_type:
primary 
     92 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    5.00    9.00   11.74   17.00   27.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -30535  -22097  -18016  -17422  -13032   -5383 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   387.0   552.0   827.8  1200.5  2385.0      61 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    149     645    1183    1436    1942    4673      31 

patient.drugs.drug.10.bcr_drug_barcode:
tcga-ou-a5pi-d65719                NA's 
                  1                  91 

patient.drugs.drug.10.bcr_drug_uuid:
63516d8e-6fd0-445d-87c1-a324961e772c                                 NA's 
                                   1                                   91 

patient.drugs.drug.10.day_of_form_completion:
  29 NA's 
   1   91 

patient.drugs.drug.10.days_to_drug_therapy_end:
 878 NA's 
   1   91 

patient.drugs.drug.10.days_to_drug_therapy_start:
 725 NA's 
   1   91 

patient.drugs.drug.10.drug_name:
avastin    NA's 
      1      91 

patient.drugs.drug.10.measure_of_response:
stable disease           NA's 
             1             91 

patient.drugs.drug.10.month_of_form_completion:
   9 NA's 
   1   91 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
   1   91 

patient.drugs.drug.10.therapy_types.therapy_type:
immunotherapy          NA's 
            1            91 

patient.drugs.drug.10.tx_on_clinical_trial:
  no NA's 
   1   91 

patient.drugs.drug.10.year_of_form_completion:
2014 NA's 
   1   91 

patient.drugs.drug.2.bcr_drug_barcode:
tcga-or-a5jm-d49540 tcga-or-a5k2-d48216 tcga-ou-a5pi-d45487                NA's 
                  1                   1                   1                  89 

patient.drugs.drug.2.bcr_drug_uuid:
2b5dd32a-1f38-4899-a90f-77c16b1f900e 3511fdb0-aa74-42f8-a96f-22043a6186d2 
                                   1                                    1 
529c2145-589c-4daf-b2de-b256c181efc6                                 NA's 
                                   1                                   89 

patient.drugs.drug.2.day_of_form_completion:
   3   10   16 NA's 
   1    1    1   89 

patient.drugs.drug.2.days_to_drug_therapy_end:
 239  298  439 NA's 
   1    1    1   89 

patient.drugs.drug.2.days_to_drug_therapy_start:
  69  118  378 NA's 
   1    1    1   89 

patient.drugs.drug.2.drug_name:
   cisplatin  doxorubicin ketoconazole         NA's 
           1            1            1           89 

patient.drugs.drug.2.measure_of_response:
clinical progressive disease               stable disease 
                           1                            1 
                        NA's 
                          90 

patient.drugs.drug.2.month_of_form_completion:
   7    9   10 NA's 
   1    1    1   89 

patient.drugs.drug.2.therapy_ongoing:
  no NA's 
   3   89 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         2                          1 
                      NA's 
                        89 

patient.drugs.drug.2.tx_on_clinical_trial:
  no NA's 
   3   89 

patient.drugs.drug.2.year_of_form_completion:
2013 NA's 
   3   89 

patient.drugs.drug.3.bcr_drug_barcode:
tcga-or-a5k2-d48217 tcga-ou-a5pi-d45488                NA's 
                  1                   1                  90 

patient.drugs.drug.3.bcr_drug_uuid:
30e49257-af92-4c80-9da6-2ea6444dcb07 c4f9820c-381e-4ea5-86e2-c6be026baf87 
                                   1                                    1 
                                NA's 
                                  90 

patient.drugs.drug.3.day_of_form_completion:
  10   16 NA's 
   1    1   90 

patient.drugs.drug.3.days_to_drug_therapy_end:
 239  299 NA's 
   1    1   90 

patient.drugs.drug.3.days_to_drug_therapy_start:
  55  118 NA's 
   1    1   90 

patient.drugs.drug.3.drug_name:
carboplatin   cisplatin        NA's 
          1           1          90 

patient.drugs.drug.3.measure_of_response:
stable disease           NA's 
             1             91 

patient.drugs.drug.3.month_of_form_completion:
   7    9 NA's 
   1    1   90 

patient.drugs.drug.3.therapy_ongoing:
  no NA's 
   2   90 

patient.drugs.drug.3.therapy_types.therapy_type:
chemotherapy         NA's 
           2           90 

patient.drugs.drug.3.tx_on_clinical_trial:
  no NA's 
   2   90 

patient.drugs.drug.3.year_of_form_completion:
2013 NA's 
   2   90 

patient.drugs.drug.4.bcr_drug_barcode:
tcga-or-a5k2-d48219 tcga-ou-a5pi-d45489                NA's 
                  1                   1                  90 

patient.drugs.drug.4.bcr_drug_uuid:
d2547fc3-e37d-4bb0-bed0-c4a5989cd685 fac3df36-8c2f-4de8-aa0b-36eac3a6a9d0 
                                   1                                    1 
                                NA's 
                                  90 

patient.drugs.drug.4.day_of_form_completion:
  10   16 NA's 
   1    1   90 

patient.drugs.drug.4.days_to_drug_therapy_end:
 239  298 NA's 
   1    1   90 

patient.drugs.drug.4.days_to_drug_therapy_start:
  69  118 NA's 
   1    1   90 

patient.drugs.drug.4.drug_name:
etoposide  mitotane      NA's 
        1         1        90 

patient.drugs.drug.4.measure_of_response:
stable disease           NA's 
             1             91 

patient.drugs.drug.4.month_of_form_completion:
   7    9 NA's 
   1    1   90 

patient.drugs.drug.4.therapy_ongoing:
  no NA's 
   2   90 

patient.drugs.drug.4.therapy_types.therapy_type:
chemotherapy         NA's 
           2           90 

patient.drugs.drug.4.tx_on_clinical_trial:
  no NA's 
   2   90 

patient.drugs.drug.4.year_of_form_completion:
2013 NA's 
   2   90 

patient.drugs.drug.5.bcr_drug_barcode:
tcga-ou-a5pi-d65713                NA's 
                  1                  91 

patient.drugs.drug.5.bcr_drug_uuid:
24e5dd42-a517-41eb-96b5-31b361e509af                                 NA's 
                                   1                                   91 

patient.drugs.drug.5.day_of_form_completion:
  29 NA's 
   1   91 

patient.drugs.drug.5.days_to_drug_therapy_end:
 694 NA's 
   1   91 

patient.drugs.drug.5.days_to_drug_therapy_start:
 605 NA's 
   1   91 

patient.drugs.drug.5.drug_name:
carboplatin        NA's 
          1          91 

patient.drugs.drug.5.measure_of_response:
stable disease           NA's 
             1             91 

patient.drugs.drug.5.month_of_form_completion:
   9 NA's 
   1   91 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
   1   91 

patient.drugs.drug.5.therapy_types.therapy_type:
chemotherapy         NA's 
           1           91 

patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
   1   91 

patient.drugs.drug.5.year_of_form_completion:
2014 NA's 
   1   91 

patient.drugs.drug.6.bcr_drug_barcode:
tcga-ou-a5pi-d65714                NA's 
                  1                  91 

patient.drugs.drug.6.bcr_drug_uuid:
ede41fc6-31ac-48cc-90d7-34982230c4e6                                 NA's 
                                   1                                   91 

patient.drugs.drug.6.day_of_form_completion:
  29 NA's 
   1   91 

patient.drugs.drug.6.days_to_drug_therapy_end:
 694 NA's 
   1   91 

patient.drugs.drug.6.days_to_drug_therapy_start:
 605 NA's 
   1   91 

patient.drugs.drug.6.drug_name:
etoposide      NA's 
        1        91 

patient.drugs.drug.6.measure_of_response:
stable disease           NA's 
             1             91 

patient.drugs.drug.6.month_of_form_completion:
   9 NA's 
   1   91 

patient.drugs.drug.6.therapy_ongoing:
  no NA's 
   1   91 

patient.drugs.drug.6.therapy_types.therapy_type:
chemotherapy         NA's 
           1           91 

patient.drugs.drug.6.tx_on_clinical_trial:
  no NA's 
   1   91 

patient.drugs.drug.6.year_of_form_completion:
2014 NA's 
   1   91 

patient.drugs.drug.7.bcr_drug_barcode:
tcga-ou-a5pi-d65715                NA's 
                  1                  91 

patient.drugs.drug.7.bcr_drug_uuid:
5210e184-ab88-43fb-aa3e-075b069c7641                                 NA's 
                                   1                                   91 

patient.drugs.drug.7.day_of_form_completion:
  29 NA's 
   1   91 

patient.drugs.drug.7.days_to_drug_therapy_end:
 694 NA's 
   1   91 

patient.drugs.drug.7.days_to_drug_therapy_start:
 605 NA's 
   1   91 

patient.drugs.drug.7.drug_name:
doxorubicin        NA's 
          1          91 

patient.drugs.drug.7.measure_of_response:
stable disease           NA's 
             1             91 

patient.drugs.drug.7.month_of_form_completion:
   9 NA's 
   1   91 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   1   91 

patient.drugs.drug.7.therapy_types.therapy_type:
chemotherapy         NA's 
           1           91 

patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   1   91 

patient.drugs.drug.7.year_of_form_completion:
2014 NA's 
   1   91 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-ou-a5pi-d65717                NA's 
                  1                  91 

patient.drugs.drug.8.bcr_drug_uuid:
c580d8a5-b9ba-408b-b48c-475ec3b9b71d                                 NA's 
                                   1                                   91 

patient.drugs.drug.8.day_of_form_completion:
  29 NA's 
   1   91 

patient.drugs.drug.8.days_to_drug_therapy_end:
 817 NA's 
   1   91 

patient.drugs.drug.8.days_to_drug_therapy_start:
 725 NA's 
   1   91 

patient.drugs.drug.8.drug_name:
carboplatin        NA's 
          1          91 

patient.drugs.drug.8.measure_of_response:
stable disease           NA's 
             1             91 

patient.drugs.drug.8.month_of_form_completion:
   9 NA's 
   1   91 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   1   91 

patient.drugs.drug.8.therapy_types.therapy_type:
chemotherapy         NA's 
           1           91 

patient.drugs.drug.8.tx_on_clinical_trial:
  no NA's 
   1   91 

patient.drugs.drug.8.year_of_form_completion:
2014 NA's 
   1   91 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-ou-a5pi-d65718                NA's 
                  1                  91 

patient.drugs.drug.9.bcr_drug_uuid:
db218b3e-4521-4679-b6ec-6007015ddcb0                                 NA's 
                                   1                                   91 

patient.drugs.drug.9.day_of_form_completion:
  29 NA's 
   1   91 

patient.drugs.drug.9.days_to_drug_therapy_end:
 817 NA's 
   1   91 

patient.drugs.drug.9.days_to_drug_therapy_start:
 725 NA's 
   1   91 

patient.drugs.drug.9.drug_name:
streptozocin         NA's 
           1           91 

patient.drugs.drug.9.measure_of_response:
stable disease           NA's 
             1             91 

patient.drugs.drug.9.month_of_form_completion:
   9 NA's 
   1   91 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   1   91 

patient.drugs.drug.9.therapy_types.therapy_type:
chemotherapy         NA's 
           1           91 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   1   91 

patient.drugs.drug.9.year_of_form_completion:
2014 NA's 
   1   91 



patient.drugs.drug.day_of_form_completion:
   3   10   11   16 NA's 
   1    1    1    2   87 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   93.0   239.0   298.0   419.6   439.0  1029.0      87 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   69.0    78.0   118.0   322.2   378.0   968.0      87 


patient.drugs.drug.measure_of_response:
clinical progressive disease               stable disease 
                           2                            1 
                        NA's 
                          89 

patient.drugs.drug.month_of_form_completion:
   7    9   10 NA's 
   1    3    1   87 

patient.drugs.drug.therapy_ongoing:
  no NA's 
   5   87 

patient.drugs.drug.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         3                          2 
                      NA's 
                        87 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
   4    1   87 

patient.drugs.drug.year_of_form_completion:
2013 NA's 
   5   87 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     8                     40                     44 



patient.follow_ups.follow_up.2.day_of_form_completion:
  25   30 NA's 
   3    8   81 

patient.follow_ups.follow_up.2.days_to_death:
2405 NA's 
   1   91 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1082    1908    2336    2340    2964    3465      82 

patient.follow_ups.follow_up.2.lost_follow_up:
  no NA's 
  11   81 

patient.follow_ups.follow_up.2.mitotane_therapy_adjuvant_setting:
  no  yes NA's 
   6    5   81 

patient.follow_ups.follow_up.2.month_of_form_completion:
   3 NA's 
  11   81 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   1   91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
 yes NA's 
   1   91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
1309 NA's 
   1   91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 153 NA's 
   1   91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.histologic_disease_progression_present_indicator:
 yes NA's 
   1   91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_confirmed_diagnosis_method_name:
imaging    NA's 
      1      91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
lymph node(s)          NA's 
            1            91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                 91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1   91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    1   90 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
 yes NA's 
   2   90 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 883 1309 NA's 
   1    1   90 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 153  826 NA's 
   1    1   90 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.histologic_disease_progression_present_indicator:
 yes NA's 
   1   91 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name:
imaging    NA's 
      2      90 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
               lung peritoneal surfaces                NA's 
                  1                   1                  90 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      1                       1                      90 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   2   90 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   7    2   83 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        10          1         81 

patient.follow_ups.follow_up.2.post_surgical_procedure_assessment_thyroid_gland_carcinoma_status:
no imaging evidence of disease                           NA's 
                            11                             81 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
   6    5   81 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response                        NA's 
                         11                          81 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  10    1   81 

patient.follow_ups.follow_up.2.therapeutic_mitotane_levels_achieved:
  no  yes NA's 
   1    4   87 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   10     1    81 

patient.follow_ups.follow_up.2.year_of_form_completion:
2015 NA's 
  11   81 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   11.00   11.17   15.00   31.00      32 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  436.0   490.0   498.0   926.2  1355.0  1852.0      87 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    464     878    1424    1747    2212    4673      41 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
  56    4   32 

patient.follow_ups.follow_up.mitotane_therapy_adjuvant_setting:
  no  yes NA's 
  25   31   36 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  5.000   5.000   6.000   5.867   6.000  12.000      32 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   7   85 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   5    2   85 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     69     561    1003     905    1309    1583      87 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0    96.5   200.0   339.0   428.5  1149.0      84 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.histologic_disease_progression_present_indicator:
  no  yes NA's 
   2    4   86 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_confirmed_diagnosis_method_name:
  imaging pathology      NA's 
        5         3        84 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
distant metastasis               NA's 
                 8                 84 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
soft tissue of pancreatic tail                           NA's 
                             1                             91 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   3    5   84 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
 yes NA's 
   2   90 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
 yes NA's 
   2   90 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
  69 NA's 
   1   91 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
  61   62 NA's 
   1    1   90 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.histologic_disease_progression_present_indicator:
  no  yes NA's 
   1    1   90 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_confirmed_diagnosis_method_name:
  imaging pathology      NA's 
        1         1        90 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
        liver lymph node(s)          NA's 
            1             1            90 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                 90 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    1   90 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   5   12   75 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  12    5   75 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   69.0   641.5  1061.5  1029.1  1298.5  2112.0      82 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0   166.0   391.0   582.5   826.0  2099.0      75 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.histologic_disease_progression_present_indicator:
  no  yes NA's 
   3    9   80 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name:
  imaging pathology      NA's 
       11         6        75 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                     16                       1                      75 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
pancreas     NA's 
       1       91 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   7   10   75 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  34   17   41 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        42         14         36 

patient.follow_ups.follow_up.post_surgical_procedure_assessment_thyroid_gland_carcinoma_status:
no imaging evidence of disease  persistent distant metastases 
                            49                              6 
                          NA's 
                            37 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  24   32   36 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         47                           1 
        progressive disease              stable disease 
                          6                           2 
                       NA's 
                         36 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
  47    9   36 

patient.follow_ups.follow_up.therapeutic_mitotane_levels_achieved:
  no  yes NA's 
   7   22   63 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
   51     5    36 

patient.follow_ups.follow_up.year_of_form_completion:
2014 NA's 
  60   32 

patient.gender:
female   male 
    60     32 

patient.history_of_neoadjuvant_treatment:
 no yes 
 91   1 

patient.icd_10:
c74.0 
   92 

patient.icd_o_3_histology:
8370/1 8370/3 
     5     87 

patient.icd_o_3_site:
c74.0 
   92 

patient.informed_consent_verified:
yes 
 92 

patient.mitotane_therapy:
  no  yes NA's 
  32   55    5 

patient.mitotane_therapy_adjuvant_setting:
  no  yes NA's 
   5   43   44 

patient.mitotane_therapy_for_macroscopic_residual_disease:
  no  yes NA's 
   4   10   78 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   9.000   9.000   9.098  11.000  12.000 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   4    7   81 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
  10    1   81 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
1003 1583 NA's 
   1    1   90 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0    64.5   134.5   180.9   179.0   783.0      78 

patient.new_tumor_events.new_tumor_event.2.histologic_disease_progression_present_indicator:
  no  yes NA's 
   2    1   89 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_confirmed_diagnosis_method_name:
imaging    NA's 
     12      80 


patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
biochemical evidence of disease              distant metastasis 
                              1                              10 
        locoregional recurrence                            NA's 
                              3                              78 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
brain  NA's 
    1    91 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   9    2   81 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    2   89 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no  yes NA's 
   2    1   89 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
  61  126  162  236 NA's 
   1    1    1    1   88 

patient.new_tumor_events.new_tumor_event.3.histologic_disease_progression_present_indicator:
  no NA's 
   1   91 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_confirmed_diagnosis_method_name:
imaging    NA's 
      4      88 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
liver  lung  NA's 
    3     1    88 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 4                 88 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   3   89 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 236 NA's 
   1   91 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_confirmed_diagnosis_method_name:
imaging    NA's 
      1      91 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    1    91 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                 91 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  14   25   53 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  30    9   53 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   69.0   371.8   895.5   851.9  1156.8  2112.0      76 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   108.8   213.5   393.3   516.8  2099.0      50 

patient.new_tumor_events.new_tumor_event.histologic_disease_progression_present_indicator:
  no  yes NA's 
   7   10   75 

patient.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name:
  imaging pathology      NA's 
       36         6        50 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     30                      11                       1 
                   NA's 
                     50 

patient.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
           abdomen            ovarian           pancreas vena cava thrombus 
                 1                  1                  1                  1 
              NA's 
                88 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  25   16   51 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  43   42    7 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        46         44          2 

patient.post_surgical_procedure_assessment_thyroid_gland_carcinoma_status:
biochemical evidence of disease  no imaging evidence of disease 
                              1                              63 
  persistent distant metastases persistent locoregional disease 
                             17                               2 
                           NA's 
                              9 

patient.postoperative_rx_tx:
  no  yes NA's 
  33   55    4 

patient.primary_pathology.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.00   35.50   48.50   47.16   60.00   83.00 

patient.primary_pathology.ct_scan:
 yes NA's 
  84    8 


patient.primary_pathology.days_to_initial_pathologic_diagnosis:
 0 
92 

patient.primary_pathology.excess_adrenal_hormone_history_types.excess_adrenal_hormone_diagnosis_method_type:
                   biochemical assessment 
                                       12 
both clinical and biochemical assessments 
                                       41 
                                     NA's 
                                       39 


patient.primary_pathology.excess_adrenal_hormone_history_types.excess_adrenal_hormone_history_type.2:
          cortisol           estrogen mineralocorticoids               NA's 
                21                  2                  1                 68 

patient.primary_pathology.germline_genotype_testing_report.germline_genotype_test_categories.ret:
ret negative         NA's 
           1           91 

patient.primary_pathology.germline_genotype_testing_report.molecular_analysis_performed_indicator:
  no  yes NA's 
  66   15   11 

patient.primary_pathology.histological_type:
   adrenocortical carcinoma- myxoid type 
                                       1 
adrenocortical carcinoma- oncocytic type 
                                       4 
    adrenocortical carcinoma- usual type 
                                      87 

patient.primary_pathology.laterality:
 left right 
   52    40 

patient.primary_pathology.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    3.00    8.25   10.00   52.00      60 

patient.primary_pathology.metastatic_neoplasm_confirmed_diagnosis_method_names.metastatic_neoplasm_confirmed_diagnosis_method_name:
    biopsy proven imaging suspected    other, specify              NA's 
                5                 7                 5                75 

patient.primary_pathology.metastatic_neoplasm_confirmed_diagnosis_method_names.metastatic_neoplasm_confirmed_diagnosis_method_name.2:
imaging suspected              NA's 
                2                90 

patient.primary_pathology.metastatic_neoplasm_confirmed_diagnosis_method_names.metastatic_neoplasm_confirmed_diagnosis_method_text:
excision of brain met             operation             resection 
                    1                     1                     1 
   surgical resection                  NA's 
                    1                    88 


patient.primary_pathology.metastatic_neoplasm_initial_diagnosis_anatomic_sites.metastatic_neoplasm_initial_diagnosis_anatomic_site:
          bone          liver           lung other, specify           NA's 
             2              7              9              2             72 

patient.primary_pathology.metastatic_neoplasm_initial_diagnosis_anatomic_sites.metastatic_neoplasm_initial_diagnosis_anatomic_site.2:
         liver           lung other, specify           NA's 
             2              1              2             87 

patient.primary_pathology.metastatic_neoplasm_initial_diagnosis_anatomic_sites.metastatic_neoplasm_initial_diagnosis_anatomic_site.3:
other, specify     peritoneum           NA's 
             1              1             90 

patient.primary_pathology.mitoses_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    4.00    7.00   19.03   18.00  129.00      31 

patient.primary_pathology.number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.548   0.500  52.000      61 

patient.primary_pathology.primary_lymph_node_presentation_assessment:
  no  yes NA's 
  40   35   17 

patient.primary_pathology.residual_tumor:
  r0   r1   r2   rx NA's 
  64    7   12    6    3 

patient.primary_pathology.tumor_tissue_site:
adrenal 
     92 

patient.primary_pathology.weiss_assessment_report.weiss_assessment_categories.atypical_mitotic_figures:
 atypical mitotic figures absent atypical mitotic figures present 
                              35                               33 
                            NA's 
                              24 

patient.primary_pathology.weiss_assessment_report.weiss_assessment_categories.cytoplasm_presence_less_than_equal_25_percent:
 cytoplasm presence <= 25% absent cytoplasm presence <= 25% present 
                               12                                57 
                             NA's 
                               23 

patient.primary_pathology.weiss_assessment_report.weiss_assessment_categories.diffuse_architecture:
 diffuse architecture absent diffuse architecture present 
                          20                           50 
                        NA's 
                          22 

patient.primary_pathology.weiss_assessment_report.weiss_assessment_categories.invasion_of_tumor_capsule:
 invasion of tumor capsule absent invasion of tumor capsule present 
                               35                                48 
                             NA's 
                                9 

patient.primary_pathology.weiss_assessment_report.weiss_assessment_categories.mitotic_rate:
 mitotic rate > 5/50 hpf absent mitotic rate > 5/50 hpf present 
                             32                              48 
                           NA's 
                             12 

patient.primary_pathology.weiss_assessment_report.weiss_assessment_categories.necrosis:
 necrosis absent necrosis present             NA's 
              20               67                5 

patient.primary_pathology.weiss_assessment_report.weiss_assessment_categories.nuclear_grade_iii_iv:
 nuclear grade iii or iv absent nuclear grade iii or iv present 
                             15                              56 
                           NA's 
                             21 

patient.primary_pathology.weiss_assessment_report.weiss_assessment_categories.sinusoid_invasion:
 sinusoid invasion absent sinusoid invasion present                      NA's 
                       39                        29                        24 

patient.primary_pathology.weiss_assessment_report.weiss_assessment_categories.weiss_venous_invasion:
 venous invasion absent venous invasion present                    NA's 
                     43                      36                      13 

patient.primary_pathology.weiss_assessment_report.weiss_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   6.000   5.543   7.000   9.000      22 

patient.primary_pathology.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1997    2005    2008    2007    2011    2012 

patient.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                         42                          20 
             stable disease                        NA's 
                          1                          29 

patient.prior_dx:
 no yes 
 86   6 

patient.race:
                    asian black or african american                     white 
                        2                         1                        78 
                     NA's 
                       11 

patient.radiation_therapy:
  no  yes NA's 
  70   17    5 

patient.radiations.radiation.10.anatomic_treatment_site:
distant recurrence               NA's 
                 1                 91 

patient.radiations.radiation.10.bcr_radiation_barcode:
tcga-pk-a5hb-r50633                NA's 
                  1                  91 

patient.radiations.radiation.10.bcr_radiation_uuid:
0d175898-2cda-4d50-b5b7-5f0ded9f0ee7                                 NA's 
                                   1                                   91 

patient.radiations.radiation.10.day_of_form_completion:
  29 NA's 
   1   91 

patient.radiations.radiation.10.days_to_radiation_therapy_end:
1192 NA's 
   1   91 

patient.radiations.radiation.10.days_to_radiation_therapy_start:
1172 NA's 
   1   91 

patient.radiations.radiation.10.month_of_form_completion:
  10 NA's 
   1   91 

patient.radiations.radiation.10.numfractions:
  15 NA's 
   1   91 

patient.radiations.radiation.10.radiation_dosage:
  40 NA's 
   1   91 

patient.radiations.radiation.10.radiation_treatment_ongoing:
  no NA's 
   1   91 

patient.radiations.radiation.10.radiation_type:
external     NA's 
       1       91 

patient.radiations.radiation.10.units:
  gy NA's 
   1   91 

patient.radiations.radiation.10.year_of_form_completion:
2013 NA's 
   1   91 

patient.radiations.radiation.2.anatomic_treatment_site:
 distant site regional site          NA's 
            1             5            86 



patient.radiations.radiation.2.day_of_form_completion:
   6    9   10   30 NA's 
   1    2    2    1   86 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  70.00   76.25   85.50  253.17   90.25 1113.00      86 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  36.00   41.25   48.50  223.00   52.00 1113.00      86 

patient.radiations.radiation.2.measure_of_response:
                partial response radiographic progressive disease 
                               1                                1 
                            NA's 
                              90 

patient.radiations.radiation.2.month_of_form_completion:
   2    9 NA's 
   1    5   86 

patient.radiations.radiation.2.numfractions:
   1   25 NA's 
   1    2   89 

patient.radiations.radiation.2.radiation_dosage:
  10 42.8 43.2   45 NA's 
   1    1    1    3   86 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   6   86 

patient.radiations.radiation.2.radiation_type:
external     NA's 
       6       86 

patient.radiations.radiation.2.units:
  gy NA's 
   6   86 

patient.radiations.radiation.2.year_of_form_completion:
2013 2014 NA's 
   5    1   86 

patient.radiations.radiation.3.anatomic_treatment_site:
distant site         NA's 
           1           91 

patient.radiations.radiation.3.bcr_radiation_barcode:
tcga-pk-a5hb-r48535                NA's 
                  1                  91 

patient.radiations.radiation.3.bcr_radiation_uuid:
b05f7ed0-99b4-4c26-b0ac-75e1c16afd8a                                 NA's 
                                   1                                   91 

patient.radiations.radiation.3.day_of_form_completion:
  29 NA's 
   1   91 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
1072 NA's 
   1   91 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
1058 NA's 
   1   91 

patient.radiations.radiation.3.measure_of_response:
partial response             NA's 
               1               91 

patient.radiations.radiation.3.month_of_form_completion:
  10 NA's 
   1   91 

patient.radiations.radiation.3.numfractions:
  10 NA's 
   1   91 

patient.radiations.radiation.3.radiation_dosage:
  30 NA's 
   1   91 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   1   91 

patient.radiations.radiation.3.radiation_type:
external     NA's 
       1       91 

patient.radiations.radiation.3.units:
  gy NA's 
   1   91 

patient.radiations.radiation.3.year_of_form_completion:
2013 NA's 
   1   91 

patient.radiations.radiation.4.anatomic_treatment_site:
distant site         NA's 
           1           91 

patient.radiations.radiation.4.bcr_radiation_barcode:
tcga-pk-a5hb-r50639                NA's 
                  1                  91 

patient.radiations.radiation.4.bcr_radiation_uuid:
90d87c53-17f4-4749-b39c-3b25cb8c7206                                 NA's 
                                   1                                   91 

patient.radiations.radiation.4.day_of_form_completion:
  29 NA's 
   1   91 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
1303 NA's 
   1   91 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
1289 NA's 
   1   91 

patient.radiations.radiation.4.month_of_form_completion:
  10 NA's 
   1   91 

patient.radiations.radiation.4.numfractions:
  10 NA's 
   1   91 

patient.radiations.radiation.4.radiation_dosage:
  30 NA's 
   1   91 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   1   91 

patient.radiations.radiation.4.radiation_type:
external     NA's 
       1       91 

patient.radiations.radiation.4.units:
  gy NA's 
   1   91 

patient.radiations.radiation.4.year_of_form_completion:
2013 NA's 
   1   91 

patient.radiations.radiation.5.anatomic_treatment_site:
distant site         NA's 
           1           91 

patient.radiations.radiation.5.bcr_radiation_barcode:
tcga-pk-a5hb-r50638                NA's 
                  1                  91 

patient.radiations.radiation.5.bcr_radiation_uuid:
4282fef7-b0be-4d96-8d81-c3279ad4dfa5                                 NA's 
                                   1                                   91 

patient.radiations.radiation.5.day_of_form_completion:
  29 NA's 
   1   91 

patient.radiations.radiation.5.days_to_radiation_therapy_end:
1303 NA's 
   1   91 

patient.radiations.radiation.5.days_to_radiation_therapy_start:
1289 NA's 
   1   91 

patient.radiations.radiation.5.month_of_form_completion:
  10 NA's 
   1   91 

patient.radiations.radiation.5.numfractions:
  10 NA's 
   1   91 

patient.radiations.radiation.5.radiation_dosage:
  30 NA's 
   1   91 

patient.radiations.radiation.5.radiation_treatment_ongoing:
  no NA's 
   1   91 

patient.radiations.radiation.5.radiation_type:
external     NA's 
       1       91 

patient.radiations.radiation.5.units:
  gy NA's 
   1   91 

patient.radiations.radiation.5.year_of_form_completion:
2013 NA's 
   1   91 

patient.radiations.radiation.6.anatomic_treatment_site:
distant recurrence               NA's 
                 1                 91 

patient.radiations.radiation.6.bcr_radiation_barcode:
tcga-pk-a5hb-r50637                NA's 
                  1                  91 

patient.radiations.radiation.6.bcr_radiation_uuid:
a509fba2-7e82-43f2-b257-a4b28487bc7a                                 NA's 
                                   1                                   91 

patient.radiations.radiation.6.day_of_form_completion:
  29 NA's 
   1   91 

patient.radiations.radiation.6.days_to_radiation_therapy_end:
1303 NA's 
   1   91 

patient.radiations.radiation.6.days_to_radiation_therapy_start:
1289 NA's 
   1   91 

patient.radiations.radiation.6.month_of_form_completion:
  10 NA's 
   1   91 

patient.radiations.radiation.6.numfractions:
  10 NA's 
   1   91 

patient.radiations.radiation.6.radiation_dosage:
  30 NA's 
   1   91 

patient.radiations.radiation.6.radiation_treatment_ongoing:
  no NA's 
   1   91 

patient.radiations.radiation.6.radiation_type:
external     NA's 
       1       91 

patient.radiations.radiation.6.units:
  gy NA's 
   1   91 

patient.radiations.radiation.6.year_of_form_completion:
2013 NA's 
   1   91 

patient.radiations.radiation.7.anatomic_treatment_site:
distant site         NA's 
           1           91 

patient.radiations.radiation.7.bcr_radiation_barcode:
tcga-pk-a5hb-r50636                NA's 
                  1                  91 

patient.radiations.radiation.7.bcr_radiation_uuid:
6f22f0ba-2a6d-493c-9233-2a364c42d265                                 NA's 
                                   1                                   91 

patient.radiations.radiation.7.day_of_form_completion:
  29 NA's 
   1   91 

patient.radiations.radiation.7.days_to_radiation_therapy_end:
1303 NA's 
   1   91 

patient.radiations.radiation.7.days_to_radiation_therapy_start:
1289 NA's 
   1   91 

patient.radiations.radiation.7.month_of_form_completion:
  10 NA's 
   1   91 

patient.radiations.radiation.7.numfractions:
  10 NA's 
   1   91 

patient.radiations.radiation.7.radiation_dosage:
  30 NA's 
   1   91 

patient.radiations.radiation.7.radiation_treatment_ongoing:
  no NA's 
   1   91 

patient.radiations.radiation.7.radiation_type:
external     NA's 
       1       91 

patient.radiations.radiation.7.units:
  gy NA's 
   1   91 

patient.radiations.radiation.7.year_of_form_completion:
2013 NA's 
   1   91 

patient.radiations.radiation.8.anatomic_treatment_site:
distant site         NA's 
           1           91 

patient.radiations.radiation.8.bcr_radiation_barcode:
tcga-pk-a5hb-r50635                NA's 
                  1                  91 

patient.radiations.radiation.8.bcr_radiation_uuid:
5092687f-5e4e-4fb4-add7-91d628e80c86                                 NA's 
                                   1                                   91 

patient.radiations.radiation.8.day_of_form_completion:
  29 NA's 
   1   91 

patient.radiations.radiation.8.days_to_radiation_therapy_end:
1246 NA's 
   1   91 

patient.radiations.radiation.8.days_to_radiation_therapy_start:
1233 NA's 
   1   91 

patient.radiations.radiation.8.month_of_form_completion:
  10 NA's 
   1   91 

patient.radiations.radiation.8.numfractions:
  10 NA's 
   1   91 

patient.radiations.radiation.8.radiation_dosage:
  30 NA's 
   1   91 

patient.radiations.radiation.8.radiation_treatment_ongoing:
  no NA's 
   1   91 

patient.radiations.radiation.8.radiation_type:
external     NA's 
       1       91 

patient.radiations.radiation.8.units:
  gy NA's 
   1   91 

patient.radiations.radiation.8.year_of_form_completion:
2013 NA's 
   1   91 

patient.radiations.radiation.9.anatomic_treatment_site:
distant site         NA's 
           1           91 

patient.radiations.radiation.9.bcr_radiation_barcode:
tcga-pk-a5hb-r50634                NA's 
                  1                  91 

patient.radiations.radiation.9.bcr_radiation_uuid:
f71faf4a-6b2f-4213-bc66-494650bf7d67                                 NA's 
                                   1                                   91 

patient.radiations.radiation.9.day_of_form_completion:
  29 NA's 
   1   91 

patient.radiations.radiation.9.days_to_radiation_therapy_end:
1246 NA's 
   1   91 

patient.radiations.radiation.9.days_to_radiation_therapy_start:
1233 NA's 
   1   91 

patient.radiations.radiation.9.month_of_form_completion:
  10 NA's 
   1   91 

patient.radiations.radiation.9.numfractions:
  10 NA's 
   1   91 

patient.radiations.radiation.9.radiation_dosage:
  30 NA's 
   1   91 

patient.radiations.radiation.9.radiation_treatment_ongoing:
  no NA's 
   1   91 

patient.radiations.radiation.9.radiation_type:
external     NA's 
       1       91 

patient.radiations.radiation.9.units:
  gy NA's 
   1   91 

patient.radiations.radiation.9.year_of_form_completion:
2013 NA's 
   1   91 

patient.radiations.radiation.anatomic_treatment_site:
 distant recurrence        distant site primary tumor field                NA's 
                  1                   1                  12                  78 



patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    8.25   10.00   11.38   11.00   30.00      76 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   70.0    84.5    95.0   187.1   117.8  1099.0      76 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  16.00   39.00   50.50  146.00   85.25 1099.00      76 

patient.radiations.radiation.measure_of_response:
               complete response radiographic progressive disease 
                               2                                4 
                  stable disease                             NA's 
                               2                               84 

patient.radiations.radiation.month_of_form_completion:
   2    9   10   12 NA's 
   1   11    3    1   76 

patient.radiations.radiation.numfractions:
   1    4   25   28 NA's 
   1    1    3    1   86 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0    49.8    52.8   417.7    55.0  4500.0      80 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  16   76 

patient.radiations.radiation.radiation_type:
external internal     NA's 
      14        1       77 

patient.radiations.radiation.units:
 cgy   gy NA's 
   1   11   80 

patient.radiations.radiation.year_of_form_completion:
2013 2014 NA's 
  14    2   76 

patient.stage_event.pathologic_stage:
  stage i  stage ii stage iii  stage iv      NA's 
        9        44        19        18         2 

patient.stage_event.tnm_categories.clinical_categories.clinical_m:
  m0   m1 NA's 
  72   18    2 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
  n0   n1 NA's 
  80   10    2 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_t:
  t1   t2   t3   t4 NA's 
   9   49   11   21    2 

patient.therapeutic_mitotane_levels_achieved:
  no  yes NA's 
   8   22   62 

patient.therapeutic_mitotane_lvl_macroscopic_residual:
  no  yes NA's 
   2    4   86 

patient.therapeutic_mitotane_lvl_progression:
  no  yes NA's 
   1    3   88 

patient.therapeutic_mitotane_lvl_recurrence:
  no  yes NA's 
  16    4   72 

patient.tissue_prospective_collection_indicator:
 no yes 
 89   3 

patient.tissue_retrospective_collection_indicator:
 no yes 
  3  89 

patient.tissue_source_site:
or ou p6 pa pk 
82  1  3  1  5 

patient.vital_status:
alive  dead 
   61    31 

patient.year_of_form_completion:
2013 2014 
  75   17 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
extension set          NA's 
           12            80 

patient.bcr_canonical_check.bcr_patient_canonical_status:
    canonical non-canonical 
           80            12 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased    waiver 
       75         6        11 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   10.0   615.2  1427.5  1809.4  2596.5  5550.0 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-16.000  -1.250   0.000   2.739   0.000 168.000 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     8                     39                     45 

patient.biospecimen_cqcf.histological_type:
   adrenocortical carcinoma- myxoid type 
                                       1 
adrenocortical carcinoma- oncocytic type 
                                       4 
    adrenocortical carcinoma- usual type 
                                      87 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                              91 
yes, pharmaceutical treatment prior to resection 
                                               1 

patient.biospecimen_cqcf.laterality:
 left right 
   52    40 

patient.biospecimen_cqcf.maximum_tumor_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.20    7.35    9.80   10.35   13.25   20.00       1 

patient.biospecimen_cqcf.method_of_sample_procurement:
surgical resection 
                92 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -6.0   436.5   620.0   754.4   824.0  2146.0      85 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
0.0753 0.1134  0.174   NA's 
     1      1      1     89 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
nanodrop     NA's 
       3       89 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  16 NA's 
   3   89 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
  92  142  213 NA's 
   1    1    1   89 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
blood draw       NA's 
         7         85 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1 NA's 
   7   85 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1776.0    -3.0     0.0   161.4    35.5  2335.0       2 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.0594    0.1127    0.2026   68.8023    0.3180 2467.0000        56 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm:
            cdrop          nanodrop spectrophotometry              NA's 
                1                27                 8                56 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   15.00   16.00   18.28   19.25   50.00      56 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  18.60   53.25   92.00  102.83  156.55  256.00      56 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
                87                  3                  2 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
kidney  other   NA's 
     3      1     88 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            4            88 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
 1 
92 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 no yes 
  1  91 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
 92 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 no yes 
  1  91 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
yes 
 92 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                        1                         1                        78 
                     NA's 
                       12 

patient.biospecimen_cqcf.sample_prescreened:
yes 
 92 

patient.biospecimen_cqcf.site_of_disease:
adrenal gland 
           92 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   4.728  10.000  60.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   60.0    80.0    80.0    83.2    90.0   100.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  12.00   93.75  136.00  160.67  200.00  828.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
 1 
92 

patient.biospecimen_cqcf.vessel_used:
cassette cryomold cryovial 
      65        1       26 

patient.days_to_index:
 0 
92 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   34.0   718.5  1541.5  1890.3  2689.5  5587.0 

patient.samples.sample.2.initial_weight:
  50  140 NA's 
   2    2   88 

patient.samples.sample.2.is_ffpe:
no 
92 

patient.samples.sample.2.oct_embedded:
false  true 
   91     1 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
153884479 153887364 153887388 153887537 153887411 153889238 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
10 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.5 
 92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
24 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
no 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
 7 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.859   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a31a a31e 
  80   12 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
40 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
23 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
80 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2013 
  92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna 
                  92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
 w 
92 



patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
yes 
 92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
yes 
 92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
no 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
repli-g 
     92 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   4   88 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
132266233 145634419 145634444 143890887 145634469 145634494 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
10 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
  33   53    6 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
 8 12 
80 12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 5  6 
80 12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.859   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a29l a30a 
  80   12 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
 33  53   6 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
23 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
40 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2013 
  92 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
132266234 145630007 145630032 143887091 145630055 145630078 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  19   30   26   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
 8 12 
80 12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 5  6 
80 12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.859   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a29k a309 
  80   12 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  19   30   26   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
23 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
 92 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 
  92 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  4.940   9.912  14.960  20.091  21.970  86.130 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
 92 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
 d 
92 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  19   30   26   17 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 92 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
 92 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              92 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
92 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                               56                                36 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       92 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   4   88 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    6.0    12.0    17.0    18.7    22.0    28.0 

patient.samples.sample.2.portions.portion.is_ffpe:
no 
92 

patient.samples.sample.2.portions.portion.month_of_creation:
 1  2  3  4 
33 48  1 10 

patient.samples.sample.2.portions.portion.portion_number:
 1 11 
88  4 

patient.samples.sample.2.portions.portion.portion_sequence:
 1 
92 

patient.samples.sample.2.portions.portion.slides.slide.bcr_slide_barcode:
tcga-or-a5k9-11a-01-ts1 tcga-or-a5kb-11a-01-tsa tcga-pk-a5hb-11a-01-ts1 
                      1                       1                       1 
tcga-pk-a5hc-11a-01-ts1                    NA's 
                      1                      88 

patient.samples.sample.2.portions.portion.slides.slide.bcr_slide_uuid:
1a2443ec-274d-4dbb-bd6b-4d7e1b4c6f47 4200b785-70dc-46e5-9bed-69915138685d 
                                   1                                    1 
d6ae8fbc-2b4b-4140-9e8e-6e0b1fa63439 e5691cf0-c485-4c84-9170-fafd25929621 
                                   1                                    1 
                                NA's 
                                  88 

patient.samples.sample.2.portions.portion.slides.slide.image_file_name:
tcga-or-a5k9-11a-01-ts1.d6ae8fbc-2b4b-4140-9e8e-6e0b1fa63439.svs 
                                                               1 
tcga-or-a5kb-11a-01-tsa.4200b785-70dc-46e5-9bed-69915138685d.svs 
                                                               1 
tcga-pk-a5hb-11a-01-ts1.1a2443ec-274d-4dbb-bd6b-4d7e1b4c6f47.svs 
                                                               1 
tcga-pk-a5hc-11a-01-ts1.e5691cf0-c485-4c84-9170-fafd25929621.svs 
                                                               1 
                                                            NA's 
                                                              88 

patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   4   88 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    1 NA's 
   2    2   88 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   4   88 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   4   88 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   4   88 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
 100 NA's 
   4   88 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   0 NA's 
   4   88 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   4   88 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   4   88 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
   4   88 

patient.samples.sample.2.portions.portion.weight:
 1.1  1.3    2   30 NA's 
   1    1   50    4   36 

patient.samples.sample.2.portions.portion.year_of_creation:
2013 
  92 

patient.samples.sample.2.sample_type:
blood derived normal  solid tissue normal 
                  87                    5 

patient.samples.sample.2.sample_type_id:
10 11 
87  5 

patient.samples.sample.2.vial_number:
 a  b  c 
85  6  1 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   34.0   688.5  1541.5  1878.9  2660.2  5587.0 






patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
bbada09f-0a15-466b-b0ea-1266e799746f                                 NA's 
                                   1                                   91 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   10.0    60.0   115.0   122.4   160.0   590.0 

patient.samples.sample.is_ffpe:
no 
92 

patient.samples.sample.oct_embedded:
false  true 
   26    66 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.780   1.830   1.855   1.858   1.890   2.000      12 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132270079 132270556 132270580 132270658 132270604 132271717        12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  13 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.16 0.17 0.18 NA's 
   1   28   38   13   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   8 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   5 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     5.5     5.5     8.0    10.0      12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a29w NA's 
  80   12 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   3  3.2  3.4  3.6 NA's 
   1   28   38   13   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  20 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2013 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-or-a5j4-01a-11r-a466-07                         NA's 
                           1                           91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
2f5ad134-04cc-460a-a515-29f982d03ff7                                 NA's 
                                   1                                   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
167812971      NA's 
        1        91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.17 NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   9 NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  12 NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   4 NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a466 NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   b NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
1.12 NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6 NA's 
   1   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   1   91 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
145631380 145632215 145632242 146709142 145632270 153889237 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
 7 10 
80 12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.16 0.17 0.18  0.5 
   1   28   38   13   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
 8 24 
80 12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
no 
92 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
 5  7 
80 12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   4.967   8.000  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a29s a31e 
  80   12 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
1.95 2.08 2.21 2.34   40 
   1   28   38   13   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
23 
92 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
13 80 
80 12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2013 
  92 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.42   37.10   44.71   45.26   50.53   89.15      12 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna 
                  12                   80 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
 r  w 
80 12 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
0.15 0.16 0.17 0.18 NA's 
   1   28   38   13   12 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  12   80 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
  12   80 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              92 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
no 
92 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g 
                       80                        12 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.720   1.240   1.430   1.444   1.673   2.050      12 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   8.000   8.500   8.482   9.000   9.500      12 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     80      12 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
 1  2 
90  2 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
153888013 153888033 153888054 153888054 153888075 153888095        12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  24 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   7 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.225   9.000  12.000      12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a319 NA's 
  80   12 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2013 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  80                   12 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
  80   12 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              80 
                                            NA's 
                                              12 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     80      12 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
  78    2   12 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
132266253 145633171 145633196 143889687 145633219 145633245 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
10 
92 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
  29   61    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
 8 12 
80 12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
92 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 5  6 
80 12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.598   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a29i a30a 
  80   12 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
 29  61   2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
23 
92 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
40 
92 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2013 
  92 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
145633487 145633661 145633682 145633676 145633705 145633726        12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 NA's 
  11   29   29   11   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   8 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   5 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.225   9.000  12.000      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a29j NA's 
  80   12 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.82 1.95 2.08 2.21 NA's 
  11   29   29   11   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  13 NA's 
  80   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2013 NA's 
  80   12 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
132266256 145634805 145634830 143891361 145634854 145634878 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
92 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  14   34   33   11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
 8 12 
80 12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
92 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 5  6 
80 12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.598   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a29h a309 
  80   12 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  14   34   33   11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
23 
92 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
 92 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 
  92 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.50   23.93   32.85   33.60   41.64   63.56 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
 92 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
 d 
92 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  14   34   33   11 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 92 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
 92 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              92 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
92 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                   92 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       92 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
 1  2 
90  2 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    4.0     6.0    15.0    16.9    28.0    30.0 

patient.samples.sample.portions.portion.is_ffpe:
no 
92 

patient.samples.sample.portions.portion.month_of_creation:
 1  2  3 
43 47  2 

patient.samples.sample.portions.portion.portion_number:
11 12 22 31 
88  1  1  2 

patient.samples.sample.portions.portion.portion_sequence:
 1  2  3 
89  1  2 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
no 
92 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   3.717   2.000  40.000 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
 0  1  5 20 
82  1  1  8 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   2.587   2.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
 0  8 20 40 
83  1  6  2 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
 0  5 40 
90  1  1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   1.000   3.946   5.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   90.00   95.00   92.98  100.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  65.00   85.00   90.00   90.92   97.00  100.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
top 
 92 

patient.samples.sample.portions.portion.weight:
10 20 30 
 3  4 85 

patient.samples.sample.portions.portion.year_of_creation:
2013 
  92 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
  46   46 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
  46   46 

patient.samples.sample.portions.shipment_portion.plate_id:
a39k NA's 
  46   46 

patient.samples.sample.portions.shipment_portion.portion_number:
  11   21   41 NA's 
   1   44    1   46 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2 NA's 
   1   41   50 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   7 NA's 
  46   46 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   5 NA's 
  46   46 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
  46   46 

patient.samples.sample.sample_type:
primary tumor 
           92 

patient.samples.sample.sample_type_id:
 1 
92 

patient.samples.sample.vial_number:
 a 
92 

Histology:
     Myxoid Type   Oncocytic Type Sarcomatoid Type       Usual Type 
               1                4                2               84 
            NA's 
               1 

C1A.C1B:
 C1A  C1B NA's 
  43   35   14 

mRNA_K4:
              steroid-phenotype-high steroid-phenotype-high+proliferation 
                                  25                                   22 
               steroid-phenotype-low  steroid-phenotype-low+proliferation 
                                  27                                    4 
                                NA's 
                                  14 

MethyLevel:
        CIMP-high CIMP-intermediate          CIMP-low              NA's 
               20                27                32                13 


SCNA.cluster:
Chromosomal       Noisy       Quiet        NA's 
         54          27           8           3 

protein.cluster:
   1    2    3 NA's 
  11   19   15   47 

COC:
COC1 COC2 COC3 NA's 
  33   19   24   16 


purity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.7500  0.8500  0.8171  0.9200  1.0000       8 

ploidy:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.190   1.505   2.350   2.424   3.018   5.650       8 

genome_doublings:
   0    1    2 NA's 
  41   37    6    8 

ADS:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
-3.370000 -0.610000  0.070000 -0.000128  0.737500  1.590000        14 

}}
\keyword{datasets}
