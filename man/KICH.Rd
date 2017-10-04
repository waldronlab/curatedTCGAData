\name{ KICH }
\alias{ KICH }
\docType{data}
\title{ Kidney Chromophobe }

\details{
\preformatted{
> experiments( KICH )
ExperimentList class object of length 9: 
 [1] CNASNP-20160128: RaggedExperiment with 57729 rows and 132 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 10164 rows and 132 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 66 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 66 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 66 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 91 columns 
 [7] Mutation-20160128: RaggedExperiment with 7559 rows and 66 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 91 columns 
 [9] RPPAArray-20160128: SummarizedExperiment with 193 rows and 63 columns 

> rownames( KICH )
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

> colnames( KICH )
CharacterList of length 9
[["CNASNP-20160128"]] TCGA-KL-8323-01A-21D-2308-01 ...
[["CNVSNP-20160128"]] TCGA-KL-8323-01A-21D-2308-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-KL-8323-01A-21D-2308-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-KL-8323-01A-21D-2308-01 ...
[["Methylation-20160128"]] TCGA-KL-8323-01A-21D-2312-05 ...
[["miRNASeqGene-20160128"]] TCGA-KL-8323-01A-21R-2314-13 ...
[["Mutation-20160128"]] TCGA-KL-8323-01A-21D-2310-10 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-KL-8323-01A-21R-2315-07 ...
[["RPPAArray-20160128"]] TCGA-KL-8324-01A-21-A421-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  1.6 Mb
2                   CNVSNP-20160128  0.3 Mb
3         GISTIC_AllByGene-20160128  4.3 Mb
4 GISTIC_ThresholdedByGene-20160128  4.3 Mb
5              Methylation-20160128 67.4 Mb
6             miRNASeqGene-20160128  0.1 Mb
7                 Mutation-20160128  2.5 Mb
8          RNASeq2GeneNorm-20160128  1.1 Mb
9                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   57 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
   9.00    9.00    2.34    1.99      NA 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  17.00   42.00   50.00   51.52   61.75   86.00 

vital_status:
 0  1 
56 10 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    325     725     855    1001    1158    2172      57 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     30    1366    2713    2320    3138    4676      10 

tumor_tissue_site:
kidney 
    66 

pathologic_stage:
  stage i  stage ii stage iii  stage iv 
       21        25        14         6 


pathology_N_stage:
n0 n1 n2 nx 
40  3  2 21 

pathology_M_stage:
  m0   m1   mx NA's 
  34    2    9   21 

gender:
female   male 
    27     39 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2000    2004    2005    2005    2007    2011 

radiation_therapy:
no 
66 

karnofsky_performance_score:
  90  100 NA's 
   3   10   53 

histological_type:
kidney chromophobe 
                66 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   24.00   25.09   31.00   75.00      55 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1949    1964    1974    1974    1984    1997      58 

race:
                    asian black or african american                     white 
                        2                         4                        58 
                     NA's 
                        2 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     4                     32                     30 

admin.bcr:
nationwide children's hospital 
                            66 

admin.day_of_dcc_upload:
 1 
66 

admin.disease_code:
kich 
  66 


admin.month_of_dcc_upload:
10 
66 

admin.patient_withdrawal.withdrawn:
false 
   66 

admin.project_code:
tcga 
  66 

admin.year_of_dcc_upload:
2015 
  66 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  17.00   42.00   50.00   51.52   61.75   86.00 


patient.clinical_cqcf.anatomic_neoplasm_subdivision:
 left right 
   30    36 

patient.clinical_cqcf.consent_or_death_status:
consented 
       66 

patient.clinical_cqcf.country:
united states 
           66 

patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-122.00  -15.00   -6.00  -12.05   -2.00   35.00 

patient.clinical_cqcf.histological_type:
kidney chromophobe 
                66 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
no 
66 

patient.clinical_cqcf.history_of_prior_malignancy:
                                                     no 
                                                     59 
                       yes, history of prior malignancy 
                                                      5 
yes, history of synchronous and or bilateral malignancy 
                                                      2 

patient.clinical_cqcf.normal_tissue_anatomic_site:
         kidney  kidney; cortex kidney; medulla   kidney; mixed            NA's 
             24               3               5              25               9 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           57             9 

patient.clinical_cqcf.tumor_type:
primary 
     66 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   13.00   14.00   17.44   24.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -31591  -22564  -18424  -18996  -15424   -6556 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  325.0   670.5   804.5   854.2   979.5  1590.0      58 

patient.days_to_initial_pathologic_diagnosis:
 0 
66 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -2     994    1983    1891    2619    4335       9 



patient.drugs.drug.2.day_of_form_completion:
  14   16   24 NA's 
   1    1    3   61 

patient.drugs.drug.2.days_to_drug_therapy_end:
 366  817  942 4436 NA's 
   1    1    1    1   62 

patient.drugs.drug.2.days_to_drug_therapy_start:
 196  558  911 2731 NA's 
   1    1    1    1   62 

patient.drugs.drug.2.drug_name:
 bevacizumab       ptk-zk    sorafenib temsirolimus         NA's 
           1            1            1            2           61 

patient.drugs.drug.2.month_of_form_completion:
   1    6    7 NA's 
   1    1    3   61 

patient.drugs.drug.2.therapy_ongoing:
  no NA's 
   5   61 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         2                          3 
                      NA's 
                        61 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
   3    2   61 

patient.drugs.drug.2.year_of_form_completion:
2012 2013 NA's 
   4    1   61 

patient.drugs.drug.3.bcr_drug_barcode:
tcga-kl-8336-d33941 tcga-kl-8341-d33923 tcga-km-8438-d32861                NA's 
                  1                   1                   1                  63 

patient.drugs.drug.3.bcr_drug_uuid:
6442c5a4-a38c-487d-95a2-9bd3eb249787 74b0a250-2c27-49bb-9ed5-9e2994c6cf9a 
                                   1                                    1 
ac4d913b-061e-41b9-a504-8e63c9a93d00                                 NA's 
                                   1                                   63 

patient.drugs.drug.3.day_of_form_completion:
  14   24 NA's 
   1    2   63 

patient.drugs.drug.3.days_to_drug_therapy_end:
 457  838 3858 NA's 
   1    1    1   63 

patient.drugs.drug.3.days_to_drug_therapy_start:
 397  824 3554 NA's 
   1    1    1   63 

patient.drugs.drug.3.drug_name:
bevacizumab   sunitinib        NA's 
          2           1          63 

patient.drugs.drug.3.month_of_form_completion:
   6    7 NA's 
   1    2   63 

patient.drugs.drug.3.therapy_ongoing:
  no NA's 
   3   63 

patient.drugs.drug.3.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         1                          2 
                      NA's 
                        63 

patient.drugs.drug.3.tx_on_clinical_trial:
  no NA's 
   3   63 

patient.drugs.drug.3.year_of_form_completion:
2012 NA's 
   3   63 

patient.drugs.drug.4.bcr_drug_barcode:
tcga-kl-8341-d33924                NA's 
                  1                  65 

patient.drugs.drug.4.bcr_drug_uuid:
e922ebc8-97a1-4b90-8d38-5cdd480be0d5                                 NA's 
                                   1                                   65 

patient.drugs.drug.4.day_of_form_completion:
  24 NA's 
   1   65 

patient.drugs.drug.4.days_to_drug_therapy_end:
 609 NA's 
   1   65 

patient.drugs.drug.4.days_to_drug_therapy_start:
 567 NA's 
   1   65 

patient.drugs.drug.4.drug_name:
temsirolimus         NA's 
           1           65 

patient.drugs.drug.4.month_of_form_completion:
   7 NA's 
   1   65 

patient.drugs.drug.4.therapy_ongoing:
  no NA's 
   1   65 

patient.drugs.drug.4.therapy_types.therapy_type:
chemotherapy         NA's 
           1           65 

patient.drugs.drug.4.tx_on_clinical_trial:
  no NA's 
   1   65 

patient.drugs.drug.4.year_of_form_completion:
2012 NA's 
   1   65 

patient.drugs.drug.5.bcr_drug_barcode:
tcga-kl-8341-d33925                NA's 
                  1                  65 

patient.drugs.drug.5.bcr_drug_uuid:
6c137653-e683-420b-b63b-f2c57b064041                                 NA's 
                                   1                                   65 

patient.drugs.drug.5.day_of_form_completion:
  24 NA's 
   1   65 

patient.drugs.drug.5.days_to_drug_therapy_start:
 624 NA's 
   1   65 

patient.drugs.drug.5.drug_name:
sorafenib      NA's 
        1        65 

patient.drugs.drug.5.month_of_form_completion:
   7 NA's 
   1   65 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
   1   65 

patient.drugs.drug.5.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                         65 

patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
   1   65 

patient.drugs.drug.5.year_of_form_completion:
2012 NA's 
   1   65 



patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    9.75   15.50   16.12   24.00   24.00      58 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  139.0   279.8   462.5   780.7   618.2  2731.0      60 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  111.0   127.5   196.0   613.4   695.0  2342.0      59 


patient.drugs.drug.measure_of_response:
clinical progressive disease                         NA's 
                           2                           64 

patient.drugs.drug.month_of_form_completion:
   5    6    7    8 NA's 
   1    2    3    2   58 

patient.drugs.drug.therapy_ongoing:
  no NA's 
   8   58 

patient.drugs.drug.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         1                          1 
targeted molecular therapy                       NA's 
                         6                         58 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
   6    2   58 

patient.drugs.drug.year_of_form_completion:
2012 2013 NA's 
   7    1   58 

patient.eastern_cancer_oncology_group:
   0    1    2 NA's 
   7    7    1   51 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     4                     32                     30 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    7.00   24.00   17.23   24.00   25.00      53 

patient.follow_ups.follow_up.2.days_to_death:
 754  855  920 1158 NA's 
   1    1    1    1   62 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  400.0   891.8  1224.0  1763.6  2076.2  4335.0      58 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   0    1 NA's 
   3    2   61 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             4                              8 
                          NA's 
                            54 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response  partial remission/response 
                          5                           1 
                       NA's 
                         60 

patient.follow_ups.follow_up.2.karnofsky_performance_score:
  80   90  100 NA's 
   1    1    3   61 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
   8    2   56 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   7.000   6.385  10.000  12.000      53 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    4   61 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   5   61 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_surgery_locoregional_procedure:
  no NA's 
   2   64 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_surgery_metastatic_procedure:
  no  yes NA's 
   3    1   62 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_additional_surgery_metastatic_procedure:
 118 NA's 
   1   65 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    103     159     464    1194     911    4335      61 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   6    5   55 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         5          6         55 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                          5                           2 
                       NA's 
                         59 

patient.follow_ups.follow_up.2.radiation_therapy:
  no NA's 
  10   56 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
   6    4   56 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
    8     4    54 

patient.follow_ups.follow_up.2.year_of_form_completion:
2012 2013 NA's 
   6    7   53 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    6.50   20.50   17.75   25.50   30.00      58 

patient.follow_ups.follow_up.3.days_to_death:
 754  855  920 NA's 
   1    1    1   63 

patient.follow_ups.follow_up.3.days_to_last_followup:
 876 1437 3745 4622 NA's 
   1    1    1    1   62 

patient.follow_ups.follow_up.3.eastern_cancer_oncology_group:
   0    1 NA's 
   1    1   64 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             3                              4 
                          NA's 
                            59 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response  partial remission/response 
                          1                           1 
             stable disease                        NA's 
                          1                          63 

patient.follow_ups.follow_up.3.karnofsky_performance_score:
  90 NA's 
   2   64 

patient.follow_ups.follow_up.3.lost_follow_up:
  no  yes NA's 
   3    3   60 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.750   7.000   6.875   9.750  12.000      58 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   3    2   61 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   5   61 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_surgery_locoregional_procedure:
  no NA's 
   3   63 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_surgery_metastatic_procedure:
  no NA's 
   4   62 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    363     565     616    1408     876    4622      61 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   2    5   59 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         1          5         60 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                          2                           1 
                       NA's 
                         63 

patient.follow_ups.follow_up.3.radiation_therapy:
  no NA's 
   6   60 

patient.follow_ups.follow_up.3.targeted_molecular_therapy:
  no  yes NA's 
   4    2   60 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
    4     3    59 

patient.follow_ups.follow_up.3.year_of_form_completion:
2012 2013 2014 NA's 
   3    4    1   58 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-kl-8336-f33937                NA's 
                  1                  65 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
e29fd202-5683-4857-a0b6-2a4f43931f96                                 NA's 
                                   1                                   65 

patient.follow_ups.follow_up.4.day_of_form_completion:
  24 NA's 
   1   65 

patient.follow_ups.follow_up.4.days_to_death:
 920 NA's 
   1   65 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
additional new tumor event                       NA's 
                         1                         65 

patient.follow_ups.follow_up.4.month_of_form_completion:
   7 NA's 
   1   65 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   1   65 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   1   65 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_surgery_metastatic_procedure:
  no NA's 
   1   65 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 817 NA's 
   1   65 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
 yes NA's 
   1   65 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
with tumor       NA's 
         1         65 

patient.follow_ups.follow_up.4.radiation_therapy:
  no NA's 
   1   65 

patient.follow_ups.follow_up.4.targeted_molecular_therapy:
 yes NA's 
   1   65 

patient.follow_ups.follow_up.4.vital_status:
dead NA's 
   1   65 

patient.follow_ups.follow_up.4.year_of_form_completion:
2012 NA's 
   1   65 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   18.25   24.00   22.23   31.00   31.00       4 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  725.0   779.2   887.5  1097.3  1098.5  2172.0      60 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     30    1061    2633    2198    3098    4676      11 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   0    1 NA's 
   8    3   55 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             5                             57 
                          NA's 
                             4 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response                        NA's 
                         17                          49 

patient.follow_ups.follow_up.karnofsky_performance_score:
  90  100 NA's 
   3    8   55 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
  31   14   21 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   7.000   6.371   7.000  10.000       4 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    5   59 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   7   59 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_surgery_locoregional_procedure:
  no NA's 
   2   64 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_surgery_metastatic_procedure:
  no  yes NA's 
   3    2   61 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_additional_surgery_metastatic_procedure:
  82 2023 NA's 
   1    1   64 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  47.00   91.75  466.50  897.33 1706.75 2317.00      60 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  44    6   16 

patient.follow_ups.follow_up.performance_status_scale_timing:
at recurrence/progression of disease                                 NA's 
                                   1                                   65 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        42          8         16 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         17                           2 
                       NA's 
                         47 

patient.follow_ups.follow_up.radiation_therapy:
  no NA's 
  49   17 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
  45    4   17 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
   55     6     5 

patient.follow_ups.follow_up.year_of_form_completion:
2012 2014 2015 NA's 
  33   17   12    4 

patient.gender:
female   male 
    27     39 

patient.hemoglobin_result:
   low normal   NA's 
    29     23     14 

patient.histological_type:
kidney chromophobe 
                66 

patient.history_of_neoadjuvant_treatment:
no 
66 

patient.icd_10:
c64.9 
   66 

patient.icd_o_3_histology:
8317/3 
    66 

patient.icd_o_3_site:
c64.9 
   66 

patient.informed_consent_verified:
yes 
 66 

patient.karnofsky_performance_score:
  70   80   90  100 NA's 
   1    2    5    3   55 

patient.lactate_dehydrogenase_result:
   low normal   NA's 
     1      5     60 

patient.laterality:
 left right 
   30    36 

patient.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     2.0     6.0     7.8     9.5    33.0      31 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   6.000   7.000   6.318   8.000  10.000 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    7   57 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   9   57 

patient.new_tumor_events.new_tumor_event.additional_surgery_locoregional_procedure:
  no  yes NA's 
   6    1   59 

patient.new_tumor_events.new_tumor_event.additional_surgery_metastatic_procedure:
  no  yes NA's 
   5    2   59 

patient.new_tumor_events.new_tumor_event.days_to_additional_surgery_locoregional_procedure:
 452 NA's 
   1   65 

patient.new_tumor_events.new_tumor_event.days_to_additional_surgery_metastatic_procedure:
  82 2020 NA's 
   1    1   64 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   46.0    64.0   175.0   669.1   758.0  2317.0      57 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  55    9    2 

patient.number_of_lymphnodes_positive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3143  0.0000  4.0000      31 

patient.number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   24.00   25.09   31.00   75.00      55 


patient.percent_tumor_sarcomatoid:
  90 NA's 
   1   65 

patient.performance_status_scale_timing:
other  NA's 
   15    51 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        52         13          1 

patient.platelet_qualitative_result:
elevated      low   normal     NA's 
       3        3       46       14 

patient.presence_of_sarcomatoid_features:
  no  yes NA's 
  22    3   41 

patient.primary_lymph_node_presentation_assessment:
 no yes 
 31  35 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         29                           1 
        progressive disease              stable disease 
                          2                           1 
                       NA's 
                         33 

patient.prior_dx:
                                                     no 
                                                     59 
                       yes, history of prior malignancy 
                                                      5 
yes, history of synchronous and or bilateral malignancy 
                                                      2 

patient.race:
                    asian black or african american                     white 
                        2                         4                        58 
                     NA's 
                        2 

patient.radiation_therapy:
no 
66 

patient.serum_calcium_result:
elevated      low   normal     NA's 
       1       36       10       19 

patient.stage_event.pathologic_stage:
  stage i  stage ii stage iii  stage iv 
       21        25        14         6 

patient.stage_event.system_version:
5th 6th 7th 
  8  41  17 

patient.stage_event.tnm_categories.clinical_categories.clinical_m:
  m0   mx NA's 
  28   13   25 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
  m0   m1   mx NA's 
  34    2    9   21 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
n0 n1 n2 nx 
40  3  2 21 


patient.stopped_smoking_year:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1955    1982    2000    1990    2004    2006      59 

patient.targeted_molecular_therapy:
 no yes 
 60   6 

patient.tissue_prospective_collection_indicator:
no 
66 

patient.tissue_retrospective_collection_indicator:
yes 
 66 

patient.tissue_source_site:
kl km kn ko 
24  9 19 14 


patient.tumor_tissue_site:
kidney 
    66 

patient.vital_status:
alive  dead 
   57     9 

patient.white_cell_count_result:
elevated   normal     NA's 
      30       22       14 

patient.year_of_form_completion:
2012 2013 
  52   14 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2000    2004    2005    2005    2007    2011 

patient.year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1949    1964    1974    1974    1984    1997      58 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
 case matched expression control case matched methylation control 
                              25                               32 
                            NA's 
                               9 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control                             NA's 
                              25                               41 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
               9               57 

patient.biospecimen_cqcf.consent_or_death_status:
consented 
       66 

patient.biospecimen_cqcf.country:
united states 
           66 

patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    128    1908    2357    2353    2795    4441 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-16.000  -2.750   0.000  -1.015   0.000  37.000 

patient.biospecimen_cqcf.digital_image_submitted:
no 
66 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
kidney 
    66 

patient.biospecimen_cqcf.histological_type:
kidney chromophobe 
                66 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
no 
66 

patient.biospecimen_cqcf.laterality:
 left right 
   30    36 

patient.biospecimen_cqcf.maximum_tumor_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.100   5.275   9.600   9.550  12.500  20.600 

patient.biospecimen_cqcf.method_of_sample_procurement:
hand-assisted laparoscopic radical nephrectomy 
                                             2 
              laparoscopic radical nephrectomy 
                                            19 
                      open partial nephrectomy 
                                             4 
                      open radical nephrectomy 
                                            41 

patient.biospecimen_cqcf.normal_controls.normal_control.2.bcr_sample_uuid:
ab7d2d26-60ac-42be-aa84-a06194dbd9f7                                 NA's 
                                   1                                   65 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
kidney; medulla            NA's 
              1              65 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            1            65 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   2 NA's 
   1   65 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -365.0    -3.0     0.0   202.7     0.0  3591.0 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
0.1349 0.1948   NA's 
     1      1     64 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm:
nanodrop     NA's 
       2       64 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
 27.3 30.68  NA's 
    1     1    64 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
 140  220 NA's 
   1    1   64 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
                                    blood draw 
                                             9 
hand-assisted laparoscopic radical nephrectomy 
                                             2 
              laparoscopic radical nephrectomy 
                                            15 
                      open partial nephrectomy 
                                             1 
                      open radical nephrectomy 
                                            39 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
         kidney  kidney; cortex kidney; medulla   kidney; mixed            NA's 
             24               3               5              25               9 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           57             9 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
 1 
66 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
yes 
 66 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
 66 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
yes 
 66 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
yes 
 66 

patient.biospecimen_cqcf.sample_prescreened:
yes 
 66 

patient.biospecimen_cqcf.site_of_disease:
kidney 
    66 

patient.biospecimen_cqcf.submitted_for_lce:
no 
66 

patient.biospecimen_cqcf.top_slide_submitted:
yes 
 66 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
 0  1  5 10 15 
55  2  4  4  1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  70.00   90.00   95.00   91.55   95.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   70.0   248.0   343.0   369.1   500.0  1072.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
 1 
66 

patient.biospecimen_cqcf.vessel_used:
cassette cryomold cryovial 
      19       33       14 

patient.days_to_index:
 0 
66 



patient.samples.sample.2.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.4000  0.7000  1.0000  0.9263  1.1000  1.7000       9 

patient.samples.sample.2.is_ffpe:
no 
66 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.200   1.500   1.754   2.200   3.800       9 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
1.21e+08 1.21e+08 1.21e+08 1.21e+08 1.21e+08 1.21e+08       41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 0.16 0.17 NA's 
   3   15    5    2   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  11 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   9   10   11   12 NA's 
   7    8    8    2   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
2315 NA's 
  25   41 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
1.86    2 2.13 2.26 NA's 
   3   15    5    2   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 NA's 
  25   41 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.210e+08 1.210e+08 1.062e+09 6.939e+08 1.062e+09 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
10 13 
41 25 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17  0.5 
   3   15    5    2   41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
 8  9 11 
 1 40 25 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
no 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
 5  6  7 
 1 25 40 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   6.000   9.000   7.924  11.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
2314 2331 2332 a29f 
  25   12   28    1 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
2.8   3 3.2 3.4  40 
  3  15   5   2  41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
22 23 
65  1 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
20 80 
25 41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 
  65    1 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.2   132.2   185.0   164.7   198.0   219.2       1 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna 
                  41                   25 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
 r  w 
25 41 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.5000  0.3663  0.5000  0.5000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  41   25 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
  40    1   25 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              66 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
no 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                 repli-g x 
                       25                        40                         1 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.130   1.300   1.440   1.464   1.580   1.820      41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.100   7.700   8.300   8.192   8.800   9.300      41 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
       40        25         1 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.062e+09 1.062e+09 1.062e+09 1.065e+09 1.062e+09 1.076e+09        41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   9 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   7 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00    4.00    5.08    6.00   12.00      41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
2331 2332 NA's 
   6   19   41 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  171.0   189.0   194.0   196.2   206.0   222.0      41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  25                   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
  25   41 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              25 
                                            NA's 
                                              41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  25   41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     25      41 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen      NA's 
       25        41 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
121012610 121012663 121013496 123163610 125247110 131701896 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
10 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.05 
  66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
10 11 
 1 65 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 6  9 
65  1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.250   4.500   6.333  10.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
2310 2311 2397 
  31   34    1 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 3 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
22 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
60 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 
  66 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
121013514 121013712 121014250 121692313 121014297 160356297         8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5   10 NA's 
  57    1    8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1400  0.1426  0.1500  0.1600       8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  11   25 NA's 
  57    1    8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  58    8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   6   11 NA's 
  57    1    8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   9.000   6.621  11.000  12.000       8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
2312 2313 a34n NA's 
  31   26    1    8 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.860   1.860   1.898   2.000   2.130       8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
  57    1    8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
13.3 13.4 NA's 
  57    1    8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2012 2013 NA's 
  57    1    8 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
160338853 160355367 160356298 162838345 165947884 167811818        60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
  10 NA's 
   6   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.07 NA's 
   6   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   8   21   25 NA's 
   2    2    2   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   6   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1   11 NA's 
   4    2   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    2    3 NA's 
   4    1    1   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a34n a35s a469 NA's 
   2    2    2   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   a    b    c    h NA's 
   1    3    1    1   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
0.46 0.94 NA's 
   2    4   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   6   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
 6.6 13.4 NA's 
   2    4   60 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2013 2014 2015 NA's 
   2    2    2   60 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.252e+08 1.252e+08 1.252e+08 1.397e+08 1.252e+08 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.13 0.14 0.15 0.16 0.17 
   6   31   24    4    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
10 11 
 1 65 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 6  9 
65  1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   6.485  10.750  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
2308 2309 2389 
  31   34    1 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.87 0.93    1 1.07 1.13 
   6   31   24    4    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
22 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67 
  66 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 
  66 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   11.3   181.1   217.3   214.2   245.8   358.3 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
 66 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
 d 
66 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
   6   31   24    4    1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 66 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
no 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              66 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
66 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                               57                                 7 
pre-extracted dna received by tss 
                                2 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       66 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   17.00   15.79   21.00   23.00 

patient.samples.sample.2.portions.portion.is_ffpe:
no 
66 

patient.samples.sample.2.portions.portion.month_of_creation:
 5  6  7 
63  2  1 

patient.samples.sample.2.portions.portion.portion_number:
 1 
66 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  57    9 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.121   1.000   7.000      33 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    5 NA's 
  32    1   33 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
  33   33 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
  57    9 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  51.00   61.00   64.00   64.35   70.00   74.00       9 

patient.samples.sample.2.portions.portion.year_of_creation:
2012 
  66 

patient.samples.sample.2.sample_type:
blood derived normal  solid tissue normal 
                   9                   57 

patient.samples.sample.2.sample_type_id:
10 11 
 9 57 

patient.samples.sample.2.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.4000  0.4596  0.6000  1.0000       9 

patient.samples.sample.2.vial_number:
 a 
66 




patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.500   0.800   1.000   1.085   1.200   2.200 

patient.samples.sample.is_ffpe:
no 
66 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.700   1.500   1.800   1.827   2.000   3.200 





patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.210e+08 1.210e+08 1.210e+08 1.355e+08 1.210e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
 7 
66 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.13 0.14 0.15 0.16 
   5   16   31   14 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
10 11 
 1 65 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
 6  9 
65  1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.250   4.500   4.515   6.750   9.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
2315 2403 
  65    1 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
1.73 1.86    2 2.13 
   5   16   31   14 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
22 
66 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3 
  66 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 
  66 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.210e+08 1.210e+08 1.210e+08 1.355e+08 1.210e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
13 
66 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.13 0.14 0.15 0.16 
   5   16   31   14 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
10 11 
 1 65 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
 6  9 
65  1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.250   4.500   4.515   6.750   9.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
2314 2401 
  65    1 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
2.6 2.8   3 3.2 
  5  16  31  14 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
22 
66 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
20 
66 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 
  66 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.20   58.77   86.70  108.56  134.45  403.20 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
 66 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
 r 
66 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
0.13 0.14 0.15 0.16 
   5   16   31   14 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              66 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                       66 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.790   1.212   1.415   1.381   1.542   1.980 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    7.4     8.4     8.8     8.7     9.1     9.5 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
     66 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
 1 
66 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.323e+08 1.076e+09 1.076e+09 1.062e+09 1.076e+09 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
10 
66 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
0.5 
 66 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
 8  9 
 1 65 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
 5  7 
 1 65 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.303   7.750  10.000 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
2331 a29f 
  65    1 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
40 
66 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
22 23 
65  1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
80 
66 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2013 
  65    1 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  176.0   186.0   191.0   192.9   200.0   212.0       1 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna 
                  66 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
 w 
66 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
  65    1 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
yes 
 66 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 no yes 
 65   1 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              66 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x 
       65         1 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen      NA's 
       65         1 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
 1 
66 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
125246617 125247093 125247120 125344871 125247144 131700980 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
10 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.05 
  66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
10 11 
 1 65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 6  9 
65  1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.250   4.500   4.515   6.750   9.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
2310 2397 
  65    1 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 3 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
22 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
60 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 
  66 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
121013538 121014264 121014288 121176208 121014313 131702399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
 5 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.13 0.14 0.15 0.16 0.17 
   1   26   27   11    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
10 11 
 1 65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
 6  9 
65  1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.250   4.500   4.515   6.750   9.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
2312 2398 
  65    1 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.73 1.86    2 2.13 2.26 
   1   26   27   11    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
22 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
13.3 
  66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2012 
  66 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 16405359 160355809 160356294 133057119 160356296 167811837        61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
  10 NA's 
   5   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.07 0.08 NA's 
   1    4   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   8   21   25   29 NA's 
   1    1    2    1   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   5   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1   11 NA's 
   3    2   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    3    5 NA's 
   3    1    1   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a34n a35s a35z a469 NA's 
   2    1    1    1   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   a    b    c    e NA's 
   2    1    1    1   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
0.46 0.53 1.07 NA's 
   1    1    3   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   5   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
 6.6 13.4 NA's 
   2    3   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2013 2014 2015 NA's 
   2    2    1   61 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-ko-8416-01a-11d-a469-10                         NA's 
                           1                           65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
b7750ed4-b64e-4fe0-91e4-5a50eee43c2b                                 NA's 
                                   1                                   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
167811766      NA's 
        1        65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
  10 NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.08 NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  21 NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1 NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   2 NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a469 NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   g NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
1.07 NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4 NA's 
   1   65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2015 NA's 
   1   65 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.252e+08 1.252e+08 1.252e+08 1.397e+08 1.252e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.13 0.14 0.15 0.16 0.17 
   1   26   27   11    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
10 11 
 1 65 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 6  9 
65  1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   4.667   7.000  11.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
2308 2389 
  65    1 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.87 0.93    1 1.07 1.13 
   1   26   27   11    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
22 
66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67 
  66 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 
  66 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.80   71.22   93.75  107.43  150.95  260.30 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
 66 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
 d 
66 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
   1   26   27   11    1 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 66 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              66 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                   66 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       66 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
 1 
66 



patient.samples.sample.portions.portion.day_of_creation:
10 12 17 21 30 
24  1 20 19  2 

patient.samples.sample.portions.portion.is_ffpe:
no 
66 

patient.samples.sample.portions.portion.month_of_creation:
 5  7 
65  1 

patient.samples.sample.portions.portion.portion_number:
11 21 
65  1 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  56   10 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.406   2.000  10.000      34 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0    5   10 NA's 
  28    2    1   35 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4107  0.0000 10.0000      10 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
  31   35 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   0 NA's 
  56   10 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   4.643  10.000  15.000      10 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  85.00   90.00   95.00   94.95  100.00  100.00      10 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
  80   85   90   95 NA's 
   2    5   28   21   10 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
 top NA's 
  56   10 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
no 
66 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.353   1.750  10.000      32 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    2    5   15 NA's 
  30    1    1    1   33 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
 0  1  4  5 15 
60  1  1  3  1 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
  33   33 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
 0  3 
65  1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   5.000   5.333  10.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  75.00   90.00   95.00   94.09  100.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
80 85 90 92 95 
 2 11 36  1 16 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
    56     10 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  27.00   32.00   36.00   35.74   39.00   42.00 

patient.samples.sample.portions.portion.year_of_creation:
2012 
  66 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
  63    3 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
  63    3 

patient.samples.sample.portions.shipment_portion.plate_id:
a421 NA's 
  63    3 

patient.samples.sample.portions.shipment_portion.portion_number:
  21 NA's 
  63    3 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   2 NA's 
  63    3 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   4 NA's 
  63    3 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   6 NA's 
  63    3 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
  63    3 

patient.samples.sample.sample_type:
primary tumor 
           66 

patient.samples.sample.sample_type_id:
 1 
66 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1000  0.3000  0.5000  0.4879  0.6000  1.0000 

patient.samples.sample.vial_number:
 a 
66 

Disease.Abbreviation:
KICH 
  66 

mtDNA.data:
 NO YES 
  5  61 

WGS.data:
 NO YES 
 16  50 

Primary.Site.Of.Disease:
Kidney 
    66 

Gender:
FEMALE   MALE 
    27     39 

Age.at.Diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  17.00   42.00   50.00   51.52   61.75   86.00 

Race:
          [Not Evaluated]                 [Unknown]                     ASIAN 
                        1                         1                         2 
BLACK OR AFRICAN AMERICAN                     WHITE 
                        4                        58 

Ethnicity:
       [Not Evaluated]              [Unknown]     HISPANIC OR LATINO 
                    27                      3                      4 
NOT HISPANIC OR LATINO 
                    32 

Year.of.Initial.Pathologic.Diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2000    2004    2005    2005    2007    2011 

Prior.Diagnosis..Y.N.:
                                                     No 
                                                     59 
                       Yes, History of Prior Malignancy 
                                                      5 
Yes, History of Synchronous and or Bilateral Malignancy 
                                                      2 

Prior.Therapy..Y.N.:
No 
66 

Histological.Subtype:
Kidney Chromophobe 
                66 

Laterality:
 Left Right 
   30    36 

Pathologic.Spread.Distant.Metastasis:
[Not Available]              M0              M1              MX 
             21              34               2               9 

Pathologic.Spread.Lymph.Nodes:
N0 N1 N2 NX 
40  3  2 21 


Tumor.Stage:
  Stage I  Stage II Stage III  Stage IV 
       21        25        14         6 

Composite.Tumor.Status:
 [Unknown] TUMOR FREE WITH TUMOR 
         1         52         13 

Composite.Vital.Status:
DECEASED   LIVING 
       9       57 





LDH:
[Not Available] [Not Evaluated]       [Unknown]             Low          Normal 
             14              45               1               1               5 

Eosinophilic.vs.Classic:
     Classic Eosinophilic 
          47           19 

Sarcomatoid.Features..Y.N.:
[Not Available] [Not Evaluated]              NO             YES 
             24              17              22               3 

Calcium:
[Not Available] [Not Evaluated]        Elevated             Low          Normal 
             14               5               1              36              10 

White.Cell.Count:
[Not Available]        Elevated          Normal 
             14              30              22 

Hemoglobin:
[Not Available]             Low          Normal 
             14              29              23 

Platelets:
[Not Available]        Elevated             Low          Normal 
             14               3               3              46 

Path.review.notes:
                                                          chromophobe and sarcomatoid features noted 
                                                                                                   1 
eosinophilic variant; immunostains for CK7, CD1, MIB-1 supported diagnosis; oncocytic features noted 
                                                                                                   1 
                                                                            oncocytic features noted 
                                                                                                   3 
                         suspected BHD but prelim genetic testing negative; oncocytic features noted 
                                                                                                   1 
                                                                                                NA's 
                                                                                                  60 

Copy.Pattern:
   chr1 loss    chr6 gain      classic copy neutral        mixed 
           2            1           57            4            2 


}}
\keyword{datasets}
