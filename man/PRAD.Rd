\name{ PRAD }
\alias{ PRAD }
\docType{data}
\title{ Prostate adenocarcinoma }

\details{
\preformatted{
> experiments( PRAD )
ExperimentList class object of length 10: 
 [1] CNASeq-20160128: RaggedExperiment with 23398 rows and 230 columns 
 [2] CNASNP-20160128: RaggedExperiment with 573776 rows and 1029 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 117345 rows and 1023 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 492 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 492 columns 
 [6] Methylation-20160128: SummarizedExperiment with 485577 rows and 549 columns 
 [7] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 547 columns 
 [8] Mutation-20160128: RaggedExperiment with 12348 rows and 332 columns 
 [9] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 550 columns 
 [10] RPPAArray-20160128: SummarizedExperiment with 195 rows and 352 columns 

> rownames( PRAD )
CharacterList of length 10
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363

> colnames( PRAD )
CharacterList of length 10
[["CNASeq-20160128"]] TCGA-CH-5741-01A-11D-1572-02 ...
[["CNASNP-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["CNVSNP-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-2A-A8VL-01A-21D-A376-01 ...
[["Methylation-20160128"]] TCGA-2A-A8VL-01A-21D-A378-05 ...
[["miRNASeqGene-20160128"]] TCGA-2A-A8VL-01A-21R-A37H-13 ...
[["Mutation-20160128"]] TCGA-2A-A8VL-01A-21D-A377-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-2A-A8VL-01A-21R-A37L-07 ...
[["RPPAArray-20160128"]] TCGA-2A-A8VL-01A-11-A43M-20 ...

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128  0.7 Mb
2                    CNASNP-20160128 15.6 Mb
3                    CNVSNP-20160128  3.4 Mb
4          GISTIC_AllByGene-20160128  4.4 Mb
5  GISTIC_ThresholdedByGene-20160128  4.4 Mb
6               Methylation-20160128 67.5 Mb
7              miRNASeqGene-20160128  0.1 Mb
8                  Mutation-20160128 20.2 Mb
9           RNASeq2GeneNorm-20160128  1.2 Mb
10                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   488 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  10.00   10.00    3.02    1.99      NA 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  41.00   56.00   61.00   61.02   66.00   78.00      11 

vital_status:
  0   1 
488  10 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  146.0   743.8  1102.0  1579.0  2315.5  3502.0     488 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   23.0   523.5   926.0  1076.5  1458.0  5024.0      10 

tumor_tissue_site:
prostate 
     498 


pathology_N_stage:
  n0   n1 NA's 
 346   79   73 

gender:
male 
 498 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2000    2009    2011    2010    2012    2013      31 

radiation_therapy:
  no  yes NA's 
 395   59   44 

histological_type:
   prostate adenocarcinoma acinar type prostate adenocarcinoma, other subtype 
                                   483                                     15 

residual_tumor:
  r0   r1   r2   rx NA's 
 316  147    5   15   15 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4447  0.0000 15.0000      91 

gleason_score:
  6   7   8   9  10 
 45 248  64 137   4 

psa_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.030   0.100   1.742   0.110 323.000      57 

days_to_psa:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -164.0   191.0   512.0   685.6   926.0  3447.0      53 

race:
                    asian black or african american                     white 
                        2                         7                       147 
                     NA's 
                      342 

ethnicity:
not hispanic or latino                   NA's 
                   152                    346 

admin.bcr:
nationwide children's hospital 
                           498 

admin.day_of_dcc_upload:
  1 
498 

admin.disease_code:
prad 
 498 


admin.month_of_dcc_upload:
 10 
498 

admin.patient_withdrawal.withdrawn:
false 
  498 

admin.project_code:
tcga 
 498 

admin.year_of_dcc_upload:
2015 
 498 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  41.00   56.00   61.00   61.01   66.00   78.00 


patient.biochemical_recurrence:
  no  yes NA's 
 372   58   68 

patient.bone_scan_results:
    abnormal (not related to prostate cancer) 
                                           24 
                                    equivocal 
                                            8 
normal (no evidence of prostate cancer) [cm0] 
                                          215 
    prostate cancer metastases present [cm1b] 
                                            1 
                                         NA's 
                                          250 

patient.clinical_cqcf.consent_or_death_status:
consented 
      498 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-142.00   30.00   53.00   98.25   90.00 2364.00      31 

patient.clinical_cqcf.days_to_preop_psa:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
-1365.000   -39.500     0.000     4.279    31.000  1232.000        39 

patient.clinical_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     7                    382                    109 

patient.clinical_cqcf.frozen_specimen_anatomic_site:
prostate 
     498 

patient.clinical_cqcf.gleason_score_combined:
  6   7   8   9  10 
 50 247  73 115  13 

patient.clinical_cqcf.gleason_score_primary:
  2   3   4   5 
  1 206 227  64 

patient.clinical_cqcf.gleason_score_secondary:
  3   4   5 
152 256  90 

patient.clinical_cqcf.histological_type:
prostate adenocarcinoma  other subtype    prostate adenocarcinoma acinar type 
                                    15                                    483 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             496 
yes, pharmaceutical treatment prior to resection 
                                               2 

patient.clinical_cqcf.history_of_prior_malignancy:
                                                     no 
                                                    468 
                       yes, history of prior malignancy 
                                                     27 
yes, history of synchronous and or bilateral malignancy 
                                                      3 

patient.clinical_cqcf.normal_tissue_anatomic_site:
          other seminal vesicle            NA's 
            126              53             319 

patient.clinical_cqcf.other_anatomic_site_normal_tissue:
   blood prostate     NA's 
       4       75      419 

patient.clinical_cqcf.psa_result_preop:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.70    5.10    7.50   10.98   11.35  107.00       3 

patient.clinical_cqcf.race:
american indian or alaska native                            asian 
                               1                               12 
       black or african american                            white 
                              58                              409 
                            NA's 
                              18 

patient.clinical_cqcf.tumor_type:
primary 
    498 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   16.00   16.46   23.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -28721  -24288  -22626  -22482  -20746  -15330      11 

patient.days_to_bone_scan_performed:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1060.00    10.00    19.50    34.19    34.75  1102.00      256 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    146     703     833    1227    1460    3467     490 

patient.days_to_diagnostic_computed_tomography_performed:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1062.00    11.00    21.00    35.39    36.75  1102.00      296 

patient.days_to_diagnostic_mri_performed:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-121.00    7.50   41.00   72.45   82.50  805.00     415 

patient.days_to_first_biochemical_recurrence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   51.0   153.5   341.0   549.3   936.0  1925.0     428 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 467   31 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   170.0   506.5   700.8   998.0  4604.0       8 

patient.diagnostic_ct_abd_pelvis_performed:
  no  yes NA's 
 186  206  106 


patient.diagnostic_mri_performed:
  no  yes NA's 
 297   86  115 

patient.diagnostic_mri_results.diagnostic_mri_result:
                             distant metastasis [e.g. cm1] 
                                                         1 
                                                 equivocal 
                                                         4 
extraprostatic extension localized (e.g. seminal vesicles) 
                                                        23 
                   no evidence of extraprostatic extension 
                                                        54 
                                                      NA's 
                                                       416 

patient.diagnostic_mri_results.diagnostic_mri_result.2:
                                                     equivocal 
                                                             1 
extraprostatic extension (regional lymphadenopathy) [e.g. cn1] 
                                                             3 
                                                          NA's 
                                                           494 



patient.drugs.drug.2.clinical_trail_drug_classification:
 n/a NA's 
   2  496 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.00   20.00   18.71   25.00   30.00     463 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    126     658     837    1160    1494    3467     479 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   74.0   207.0   359.0   719.6  1191.0  2453.0     463 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                           2                           12 
            partial response               stable disease 
                           4                            2 
                        NA's 
                         478 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.500   4.000   6.057   8.500  12.000     463 

patient.drugs.drug.2.prescribed_dose:
  50 NA's 
   1  497 

patient.drugs.drug.2.prescribed_dose_units:
  mg NA's 
   1  497 

patient.drugs.drug.2.regimen_indication:
recurrence       NA's 
         1        497 

patient.drugs.drug.2.regimen_number:
   0 NA's 
   1  497 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
subcutaneous (sc)              NA's 
                1               497 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  20   15  463 

patient.drugs.drug.2.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
              1              34             463 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  29    5  464 

patient.drugs.drug.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   3    1   28    3  463 



patient.drugs.drug.3.clinical_trail_drug_classification:
 n/a NA's 
   1  497 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.75   19.00   16.93   22.75   30.00     484 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  605.0   791.2  1484.5  1644.6  2241.2  3467.0     490 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    245     591    1183    1251    1897    2511     485 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                           2                            2 
            partial response                         NA's 
                           3                          491 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.250   7.000   6.786   8.750  12.000     484 

patient.drugs.drug.3.regimen_indication:
recurrence       NA's 
         1        497 

patient.drugs.drug.3.regimen_number:
   1 NA's 
   1  497 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
oral NA's 
   1  497 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
   9    5  484 

patient.drugs.drug.3.therapy_types.therapy_type:
      ancillary    chemotherapy hormone therapy            NA's 
              1               2              11             484 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  12    1  485 

patient.drugs.drug.3.year_of_form_completion:
2012 2013 2014 2015 NA's 
   2    1   10    1  484 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.50   22.00   18.86   24.00   30.00     491 

patient.drugs.drug.4.days_to_drug_therapy_end:
 727 2170 2738 NA's 
   1    1    1  495 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    727    1660    1852    1849    2360    2535     492 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease             partial response 
                           2                            1 
                        NA's 
                         495 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   4.000   5.286   7.000  11.000     491 

patient.drugs.drug.4.regimen_indication:
recurrence       NA's 
         1        497 

patient.drugs.drug.4.regimen_number:
   1 NA's 
   1  497 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intramuscular (im)               NA's 
                 1                497 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
   4    3  491 

patient.drugs.drug.4.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
              2               5             491 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
   5    1  492 

patient.drugs.drug.4.year_of_form_completion:
2012 2014 2015 NA's 
   1    5    1  491 

patient.drugs.drug.5.bcr_drug_barcode:
tcga-kk-a8ii-d62416                NA's 
                  1                 497 

patient.drugs.drug.5.bcr_drug_uuid:
a00ac439-51f5-40a7-9590-7af6b71ff39c                                 NA's 
                                   1                                  497 

patient.drugs.drug.5.day_of_form_completion:
  25 NA's 
   1  497 

patient.drugs.drug.5.days_to_drug_therapy_end:
2892 NA's 
   1  497 

patient.drugs.drug.5.days_to_drug_therapy_start:
2780 NA's 
   1  497 

patient.drugs.drug.5.drug_name:
jevtana    NA's 
      1     497 

patient.drugs.drug.5.measure_of_response:
clinical progressive disease                         NA's 
                           1                          497 

patient.drugs.drug.5.month_of_form_completion:
   8 NA's 
   1  497 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
   1  497 

patient.drugs.drug.5.therapy_types.therapy_type:
chemotherapy         NA's 
           1          497 

patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
   1  497 

patient.drugs.drug.5.year_of_form_completion:
2014 NA's 
   1  497 



patient.drugs.drug.clinical_trail_drug_classification:
hormonal treatment                n/a               NA's 
                 1                  3                494 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.00   18.00   18.26   25.00   31.00     425 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  143.0   296.8   422.0   680.8   913.5  1984.0     468 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0   118.0   207.0   444.8   481.0  2540.0     425 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           3                           21 
            partial response               stable disease 
                           3                            4 
                        NA's 
                         467 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.973   8.000  12.000     425 

patient.drugs.drug.number_cycles:
  13 NA's 
   1  497 

patient.drugs.drug.prescribed_dose:
22.5  400 NA's 
   1    1  496 

patient.drugs.drug.prescribed_dose_units:
  mg NA's 
   2  496 

patient.drugs.drug.regimen_indication:
recurrence       NA's 
         2        496 

patient.drugs.drug.regimen_number:
   1    3 NA's 
   1    1  496 

patient.drugs.drug.route_of_administrations.route_of_administration:
intramuscular (im)               oral               NA's 
                 1                  1                496 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
  30   42  426 

patient.drugs.drug.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
              2              71             425 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  65    6  427 

patient.drugs.drug.year_of_form_completion:
2012 2013 2014 2015 NA's 
   7    4   50   12  425 

patient.ethnicity:
not hispanic or latino                   NA's 
                   152                    346 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   6    4  488 

patient.follow_ups.follow_up.2.additional_radiation_therapy:
  no  yes NA's 
   8    2  488 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   21.50   18.87   31.00   31.00     428 

patient.follow_ups.follow_up.2.days_to_first_biochemical_recurrence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    308     547    1009    1004    1256    2104     491 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    405    1155    1476    1589    1963    3323     430 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  329.0   974.2  1038.5  1258.3  1753.0  2505.0     488 

patient.follow_ups.follow_up.2.days_to_second_biochemical_recurrence:
1445 1523 1687 NA's 
   1    1    1  495 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                             68 
                          NA's 
                           428 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response  partial remission/response 
                         54                           1 
        progressive disease              stable disease 
                          9                           2 
                       NA's 
                        432 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  65    3  430 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   4.000   5.271   6.000  12.000     428 

patient.follow_ups.follow_up.2.new_neoplasm_event_occurrence_anatomic_site:
bone NA's 
   3  495 

patient.follow_ups.follow_up.2.new_neoplasm_event_type:
biochemical evidence of disease              distant metastasis 
                              6                               2 
              new primary tumor                            NA's 
                              3                             487 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  54   11  433 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        53         12        433 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                         53                           6 
             stable disease                        NA's 
                          5                         434 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  59    3  436 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
  58    4  436 

patient.follow_ups.follow_up.2.tumor_progression_post_ht:
 yes NA's 
   3  495 

patient.follow_ups.follow_up.2.type_ofprogression_of_disease_ht:
biochemical recurrence     distant metastasis                   NA's 
                     1                      2                    495 

patient.follow_ups.follow_up.2.vital_status:
alive  NA's 
   67   431 

patient.follow_ups.follow_up.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   2    5   31   32  428 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
  39   25  434 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
  41   24  433 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   15.00   15.83   24.00   31.00      62 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  146.0   775.2  1592.0  1785.9  2718.5  3502.0     490 

patient.follow_ups.follow_up.days_to_first_biochemical_recurrence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   58.0   292.0   626.0   726.7  1009.0  2310.0     437 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   573.2   921.5  1082.9  1383.0  5024.0      72 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  152.0   431.0   746.0   932.6  1378.0  2473.0     446 

patient.follow_ups.follow_up.days_to_second_biochemical_recurrence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    503     651    1445    1286    1756    2459     485 

patient.follow_ups.follow_up.days_to_third_biochemical_recurrence:
 631  655 1871 2011 NA's 
   1    1    1    1  494 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                            15                            418 
                          NA's 
                            65 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response  partial remission/response 
                        299                          16 
        progressive disease              stable disease 
                         19                          42 
                       NA's 
                        122 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 378   34   86 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   8.000   6.537   9.000  12.000      62 

patient.follow_ups.follow_up.new_neoplasm_event_occurrence_anatomic_site:
          bone           lung other, specify           NA's 
             5              1              6            486 

patient.follow_ups.follow_up.new_neoplasm_event_type:
biochemical evidence of disease              distant metastasis 
                             54                               5 
        locoregional recurrence               new primary tumor 
                              6                               5 
                           NA's 
                            428 


patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 328   72   98 

patient.follow_ups.follow_up.patient_death_reason:
prostate cancer            NA's 
              3             495 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       288         54        156 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        309                          34 
        progressive disease              stable disease 
                         29                          27 
                       NA's 
                         99 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 351   56   91 

patient.follow_ups.follow_up.source_of_patient_death_reason:
medical record           NA's 
             7            491 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
 355   48   95 

patient.follow_ups.follow_up.tumor_progression_post_ht:
  no  yes NA's 
   5   15  478 

patient.follow_ups.follow_up.type_ofprogression_of_disease_ht:
biochemical recurrence     distant metastasis                   NA's 
                    10                      6                    482 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  426     8    64 

patient.follow_ups.follow_up.year_of_form_completion:
2012 2013 2014 2015 NA's 
  27   98  219   92   62 

patient.gender:
male 
 498 

patient.histological_type:
prostate adenocarcinoma  other subtype    prostate adenocarcinoma acinar type 
                                    15                                    483 

patient.history_of_neoadjuvant_treatment:
                                              no 
                                             496 
yes, pharmaceutical treatment prior to resection 
                                               2 

patient.icd_10:
  c61 c61.9 
  497     1 


patient.icd_o_3_site:
c61.9 
  498 

patient.informed_consent_verified:
yes 
498 

patient.initial_pathologic_diagnosis_method:
             core needle biopsy transurethral resection (turbt) 
                            478                              15 
                           NA's 
                              5 

patient.laterality:
bilateral      left     right      NA's 
      433        19        38         8 

patient.lymphnodes_examined:
  no  yes NA's 
  74  419    5 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.767   7.000  12.000 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  21   22  455 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  20   24  454 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   51.0   206.0   582.5   653.8  1031.5  1925.0     438 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
                              bone non-regional / distant lymph nodes 
                                 2                                  2 
                    other, specify                               NA's 
                                 1                                493 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
biochemical evidence of disease              distant metastasis 
                             58                               3 
        locoregional recurrence               new primary tumor 
                              4                               3 
                           NA's 
                            430 

patient.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
melanoma in situ of the right midback                                  NA's 
                                    1                                   497 

patient.new_tumor_events.new_tumor_event.tumor_progression_post_ht:
  no  yes NA's 
  38    9  451 

patient.new_tumor_events.new_tumor_event.type_of_progression_after_ht:
biochemical recurrence     distant metastases                   NA's 
                     6                      3                    489 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 253   69  176 

patient.number_of_lymphnodes_examined:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     5.0     9.0    11.8    16.0    69.0      75 

patient.number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4447  0.0000 15.0000      91 

patient.patient_death_reason:
other non-malignant disease             prostate cancer 
                          1                           3 
                       NA's 
                        494 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       318         94         86 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        165                          29 
        progressive disease              stable disease 
                         15                          22 
                       NA's 
                        267 

patient.prior_dx:
                                                     no 
                                                    469 
                                                    yes 
                                                     12 
                       yes, history of prior malignancy 
                                                     16 
yes, history of synchronous and or bilateral malignancy 
                                                      1 

patient.race:
                    asian black or african american                     white 
                        2                         7                       147 
                     NA's 
                      342 

patient.radiation_therapy:
  no  yes NA's 
 249   40  209 

patient.radiations.radiation.2.anatomic_treatment_site:
 distant recurrence primary tumor field                NA's 
                  1                   1                 496 

patient.radiations.radiation.2.bcr_radiation_barcode:
tcga-fc-a5ob-r61440 tcga-g9-a9s0-r60771                NA's 
                  1                   1                 496 

patient.radiations.radiation.2.bcr_radiation_uuid:
3c8c9090-3caf-48e7-b83f-82c59c72d62e 8385a296-4a40-4467-b775-f6141844b2db 
                                   1                                    1 
                                NA's 
                                 496 

patient.radiations.radiation.2.day_of_form_completion:
  16   30 NA's 
   1    1  496 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
 344  687 NA's 
   1    1  496 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
 331  675 NA's 
   1    1  496 

patient.radiations.radiation.2.measure_of_response:
radiographic progressive disease                             NA's 
                               1                              497 

patient.radiations.radiation.2.month_of_form_completion:
   6 NA's 
   2  496 

patient.radiations.radiation.2.numfractions:
  10 NA's 
   2  496 

patient.radiations.radiation.2.radiation_dosage:
  18   30 NA's 
   1    1  496 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   2  496 

patient.radiations.radiation.2.radiation_type:
external     NA's 
       2      496 

patient.radiations.radiation.2.units:
  gy NA's 
   2  496 

patient.radiations.radiation.2.year_of_form_completion:
2014 NA's 
   2  496 

patient.radiations.radiation.3.anatomic_treatment_site:
distant recurrence               NA's 
                 1                497 

patient.radiations.radiation.3.bcr_radiation_barcode:
tcga-g9-a9s0-r60772                NA's 
                  1                 497 

patient.radiations.radiation.3.bcr_radiation_uuid:
c42e51f2-d8ad-4681-9d01-46b69362f1e8                                 NA's 
                                   1                                  497 

patient.radiations.radiation.3.day_of_form_completion:
  16 NA's 
   1  497 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
 730 NA's 
   1  497 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
 729 NA's 
   1  497 

patient.radiations.radiation.3.measure_of_response:
radiographic progressive disease                             NA's 
                               1                              497 

patient.radiations.radiation.3.month_of_form_completion:
   6 NA's 
   1  497 

patient.radiations.radiation.3.numfractions:
   2 NA's 
   1  497 

patient.radiations.radiation.3.radiation_dosage:
 600 NA's 
   1  497 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   1  497 

patient.radiations.radiation.3.radiation_type:
external     NA's 
       1      497 

patient.radiations.radiation.3.units:
 cgy NA's 
   1  497 

patient.radiations.radiation.3.year_of_form_completion:
2014 NA's 
   1  497 

patient.radiations.radiation.4.anatomic_treatment_site:
regional site          NA's 
            1           497 

patient.radiations.radiation.4.bcr_radiation_barcode:
tcga-g9-a9s0-r60773                NA's 
                  1                 497 

patient.radiations.radiation.4.bcr_radiation_uuid:
a32bebf2-bc27-44d1-9be8-63de5b209c8a                                 NA's 
                                   1                                  497 

patient.radiations.radiation.4.day_of_form_completion:
  16 NA's 
   1  497 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
 238 NA's 
   1  497 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
 179 NA's 
   1  497 

patient.radiations.radiation.4.measure_of_response:
complete response              NA's 
                1               497 

patient.radiations.radiation.4.month_of_form_completion:
   6 NA's 
   1  497 

patient.radiations.radiation.4.numfractions:
  37 NA's 
   1  497 

patient.radiations.radiation.4.radiation_dosage:
66.6 NA's 
   1  497 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   1  497 

patient.radiations.radiation.4.radiation_type:
external     NA's 
       1      497 

patient.radiations.radiation.4.units:
  gy NA's 
   1  497 

patient.radiations.radiation.4.year_of_form_completion:
2014 NA's 
   1  497 




patient.radiations.radiation.course_number:
   1 NA's 
   2  496 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.50   21.00   19.73   25.00   31.00     419 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  147.0   256.8   383.0   580.3   771.2  2679.0     422 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   51.0   208.0   333.0   533.5   774.0  2627.0     421 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              40                               11 
radiographic progressive disease                   stable disease 
                               5                               11 
                            NA's 
                             431 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    5.00    5.43    8.00   12.00     419 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   33.00   35.00   32.61   37.00   41.00     441 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     40      66      70    2143    6480    7200     436 

patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
  77    2  419 

patient.radiations.radiation.radiation_type:
     external external beam          NA's 
           73             2           423 

patient.radiations.radiation.regimen_indication:
recurrence       NA's 
         2        496 

patient.radiations.radiation.units:
 cgy   gy NA's 
  20   39  439 

patient.radiations.radiation.year_of_form_completion:
2012 2013 2014 2015 NA's 
   2   10   54   13  419 

patient.residual_tumor:
  r0   r1   r2   rx NA's 
 316  147    5   15   15 

patient.source_of_patient_death_reason:
death certificate    medical record              NA's 
                1                 5               492 

patient.stage_event.gleason_grading.gleason_score:
  6   7   8   9  10 
 45 248  64 137   4 

patient.stage_event.gleason_grading.primary_pattern:
  2   3   4   5 
  1 198 250  49 

patient.stage_event.gleason_grading.secondary_pattern:
  3   4   5 
152 236 110 

patient.stage_event.gleason_grading.tertiary_pattern:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.00    3.50    3.76    5.00    5.00     344 

patient.stage_event.psa.days_to_psa:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -164.0   191.0   512.0   685.6   926.0  3447.0      53 

patient.stage_event.psa.psa_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.030   0.100   1.742   0.110 323.000      57 

patient.stage_event.tnm_categories.clinical_categories.clinical_m:
  m0  m1a  m1b  m1c NA's 
 456    1    1    1   39 


patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
  n0   n1 NA's 
 346   79   73 


patient.targeted_molecular_therapy:
  no  yes NA's 
 247   43  208 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 259  237    2 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
 237  259    2 


patient.tumor_levels.tumor_level:
  apex   base middle   NA's 
   292     16     47    143 

patient.tumor_levels.tumor_level.2:
  base middle   NA's 
    72    203    223 

patient.tumor_levels.tumor_level.3:
base NA's 
 166  332 

patient.tumor_tissue_site:
prostate 
     498 

patient.vital_status:
alive  dead 
  490     8 

patient.year_of_form_completion:
2011 2012 2013 2014 2015 
   2  179   62  237   18 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2000    2009    2011    2010    2012    2013      31 

patient.zone_of_origin:
                central zone overlapping / multiple zones 
                           4                          127 
             peripheral zone              transition zone 
                         138                            8 
                        NA's 
                         221 


patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              35                               21 
                            NA's 
                             442 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                 29                 469 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             424               74 

patient.biospecimen_cqcf.consent_or_death_status:
consented 
      498 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   174.0   504.0   773.9  1108.5  4669.0      31 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -13.00   43.50   62.00   88.45   94.00 1249.00      31 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
462  36 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     7                    382                    109 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
prostate 
     498 

patient.biospecimen_cqcf.histological_type:
prostate adenocarcinoma  other subtype    prostate adenocarcinoma acinar type 
                                    15                                    483 


patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             496 
yes, pharmaceutical treatment prior to resection 
                                               2 

patient.biospecimen_cqcf.method_of_sample_procurement:
laparoscopic radical prostatectomy w/o robotics 
                                             21 
  laparoscopic radical prostatectomy w/robotics 
                                            171 
                     open radical prostatectomy 
                                            178 
                                          other 
                                            123 
                         other (please specify) 
                                              5 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -6.00   48.00   65.00   90.21   91.00 1264.00     377 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0440  0.1530  0.1930  0.1853  0.2200  0.3920     466 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
 nanodrop picogreen      NA's 
        1        31       466 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  15   20 NA's 
   1   31  466 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   51.0    91.0   104.0   129.9   144.0   455.0     466 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
                                   blood draw 
                                           89 
laparoscopic radical prostatectomy w/robotics 
                                           11 
                   open radical prostatectomy 
                                           15 
                       other (please specify) 
                                            6 
                                         NA's 
                                          377 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
          other seminal vesicle            NA's 
             35               3             460 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
  66   59  373 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
8f0f3239-64b0-4bc6-b869-5635fb7b8109 c2685608-9e8d-46d2-91de-cf3be34362ba 
                                   1                                    1 
d33eccab-ec38-4cf5-b36b-457343f33690                                 NA's 
                                   1                                  495 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
  26   28   49 NA's 
   1    1    1  495 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
open radical prostatectomy     other (please specify) 
                         2                          1 
                      NA's 
                       495 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
other  NA's 
    2   496 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    3 NA's 
   2    1  495 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1098.00    42.00    62.00    94.69    96.00  1249.00       33 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0029   0.0700   0.1175   8.5578   0.1960 200.0000      392 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  5.293  15.000  20.440  32.362  36.825 203.600     392 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0   108.5   173.0   209.8   220.5   987.9     392 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
                                   blood draw 
                                          355 
laparoscopic radical prostatectomy w/robotics 
                                           25 
                   open radical prostatectomy 
                                           72 
                       other (please specify) 
                                           44 
                                         NA's 
                                            2 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
          other seminal vesicle            NA's 
             88              49             361 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
  1   2 
477  21 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 no yes 
 10 488 

patient.biospecimen_cqcf.path_confirm_report_attached:
 no yes 
  1 497 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 no yes 
  2 496 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 no yes 
  5 493 

patient.biospecimen_cqcf.race:
american indian or alaska native                            asian 
                               1                               12 
       black or african american                            white 
                              58                              409 
                            NA's 
                              18 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
                                    macrodissection performed at tss to select for a region containing an acceptable tcga diagnosis 
                                                                                                                                  1 
  pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                  1 
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  8 
                                                                                                                               NA's 
                                                                                                                                488 

patient.biospecimen_cqcf.sample_prescreened:
yes 
498 

patient.biospecimen_cqcf.site_of_disease:
prostate 
     498 

patient.biospecimen_cqcf.submitted_for_lce:
 no yes 
443  55 

patient.biospecimen_cqcf.top_slide_submitted:
 no yes 
 68 430 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0   10   15 NA's 
   8    1    1  488 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   65.00   70.00   73.50   83.75   90.00     488 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   38.25  124.00  142.90  200.00  360.00     488 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1 NA's 
  10  488 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.0     0.0     0.0     1.6     0.0    20.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   66.25   75.00   73.78   80.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   100.0   170.0   195.8   250.0  1469.0       8 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1 
498 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                     51                      36                     145 
               cryovial  other (please specify) 
                    205                      61 

patient.days_to_index:
   0 NA's 
 467   31 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and metastatic tumor(s)                                  NA's 
                                    1                                   497 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   64.0   234.0   601.0   939.4  1324.0  4724.0     223 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   80.0   212.5   260.0   296.7   345.0   620.0     452 

patient.samples.sample.2.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.2     0.4     0.4     0.4     0.4     0.7     485 

patient.samples.sample.2.is_ffpe:
 no yes 
494   4 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   0.800   1.100   1.115   1.400   1.800     485 

patient.samples.sample.2.oct_embedded:
false  true  NA's 
  260    46   192 



patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.7  1.8 1.86 NA's 
   2    2    1  493 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             494 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.244e+08 1.244e+08 4.652e+08 1.050e+09 1.054e+09       487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
   6    5  487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1473  0.1500  0.1700     487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   23.00   25.00   23.09   31.00   31.00     487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
   7    4  487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   1    2    5 NA's 
   2    3    6  487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    2    3    9 NA's 
   3    1    2    5  487 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   2.330   3.000   2.971   3.705   4.010     487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
  10    1  487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3   20 26.7 NA's 
   3    5    3  487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 2014 NA's 
   4    6    1  487 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             494 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.539e+08 1.678e+08 3.671e+08 1.752e+08 1.062e+09        73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   5  414    6   73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4909  0.5000  0.5000      73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    8.00   21.00   17.97   24.00   31.00      73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
 421    4   73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   5.374   7.000  10.000      73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.833   9.000  12.000      73 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.21   40.00   40.00   39.05   40.00   40.00      73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 172  253   73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20 26.7   80 NA's 
   1    6    4  414   73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
 101   71   89  164   73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   164.0   177.0   173.2   191.0   286.2     325 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 414                   11                   73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  11  414   73 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4776  0.5000  0.5000     325 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 414   84 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
 162  252   84 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             425 
                                            NA's 
                                              73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no  yes NA's 
 421    4   73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
highpure mirna (allprep dna) ffpe rna             mirvana (allprep dna) rna 
                                    4                                     7 
                              repli-g                             repli-g x 
                                  408                                     6 
                                 NA's 
                                   73 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.350   1.230   1.020   1.515   1.910     487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.200   3.550   7.300   6.236   8.150   9.800     487 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
      100        73       325 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  25    3  470 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 3.539e+08 1.053e+09 7.423e+08 1.053e+09 1.054e+09       492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  20   21   30 NA's 
   1    3    2  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   1    3    6 NA's 
   2    3    1  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   1    3   11   12 NA's 
   2    1    2    1  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
1610 1611 1821 2161 NA's 
   1    2    1    2  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   a    b    e    f NA's 
   1    1    2    2  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 NA's 
   4    2  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  144.0   158.0   161.5   165.2   171.0   193.0     492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   6                  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   6  492 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               6 
                                            NA's 
                                             492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   6  492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      6     492 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
        3         3       492 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9    2 NA's 
  19   69    7  403 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             494 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.136e+08 1.323e+08 1.641e+08 3.120e+08 1.742e+08 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   8 
120 378 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05000 0.07000 0.08000 0.09257 0.09000 0.17000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   12.00   25.00   20.75   28.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no yes 
494   4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   3.966   5.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.952  10.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   2.800   3.000   2.572   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 22  23 
178 320 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67   20   40   60 
 120    4  320   54 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
 101   87  112  198 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             494 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.210e+08 1.523e+08 5.421e+08 1.050e+09 1.050e+09       354 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   1    5    8 NA's 
  18    6  120  354 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0667  0.0500  0.1700     354 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   23.00   17.69   23.00   31.00     354 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
 140    4  354 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   3.729   5.000   9.000     354 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.618  11.000  12.000     354 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.000   2.000   2.265   3.000   3.000     354 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 125   19  354 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   40.00   40.00   42.43   60.00   60.00     354 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 NA's 
  99   26   13    6  354 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             495 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.244e+08 1.050e+09 6.727e+08 1.050e+09 1.050e+09       481 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    9   26 NA's 
  12    2    3  481 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1300  0.1300  0.1318  0.1500  0.1600     481 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   23.00   23.00   23.18   23.00   31.00     481 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no  yes NA's 
  14    3  481 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1    2    5 NA's 
   3   10    4  481 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   9.000   6.588   9.000  10.000     481 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.400   3.470   2.772   3.740   4.270     481 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
  15    2  481 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10 13.3 13.4 26.7 NA's 
   3    2    2   10  481 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 2015 NA's 
  11    4    2  481 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             494 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.136e+08 1.507e+08 1.641e+08 3.128e+08 1.742e+08 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2 
378 120 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.13 0.14 0.15 0.16 0.17 
  18  114  160  150   56 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   12.00   25.00   20.75   28.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no yes 
494   4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   3.966   5.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   6.914  10.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.750   1.010   1.070   1.222   1.140   2.260 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
178 320 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   5 6.67  6.7   10 13.3 
   1   58  320   37   82 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
 101   87  112  198 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   15.20   27.75  104.56  100.38 1203.40 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
498 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
498 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
  20  116  161  144   57 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
498 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
188 310 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             498 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no yes 
494   4 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type                  allprep ffpe dna 
                              224                                 4 
    chemical lysis dna extraction pre-extracted dna received by tss 
                              127                               143 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      403        95 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  25    3  470 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   17.00   16.46   22.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no yes 
494   4 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   7.000   6.747  11.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   2.088   1.000  21.000 

patient.samples.sample.2.portions.portion.portion_sequence:
   1    2 NA's 
 166   10  322 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  59  439 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     1.5    11.2     4.5    70.0     452 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1   10   20 NA's 
  36    2    2    6  452 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0    4 NA's 
  45    1  452 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1   20   30 NA's 
  35    2    8    1  452 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   18.50   40.00   52.24  100.00  100.00     452 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   60.00   46.63   80.00   96.00     452 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0   48 NA's 
  45    1  452 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0   70 NA's 
  45    1  452 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
  59  439 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    2.00   13.72   30.00  300.00     274 

patient.samples.sample.2.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
  34  127   44  198   95 

patient.samples.sample.2.sample_type:
blood derived normal           metastatic        primary tumor 
                 435                    1                    4 
 solid tissue normal 
                  58 

patient.samples.sample.2.sample_type_id:
  1   6  10  11 
  4   1 435  58 

patient.samples.sample.2.shortest_dimension:
 0.1  0.2  0.3  0.4 NA's 
   1    9    1    2  485 

patient.samples.sample.2.vial_number:
  a   b 
488  10 



patient.samples.sample.3.days_to_collection:
 177  272 NA's 
   1    1  496 

patient.samples.sample.3.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0   105.0   170.0   208.3   242.5   500.0     492 

patient.samples.sample.3.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.4000  0.6000  0.8500  0.8981  1.0000  1.8000     444 

patient.samples.sample.3.is_ffpe:
  no NA's 
  61  437 

patient.samples.sample.3.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.100   1.500   1.506   1.875   2.500     444 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
    1     2   495 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.7  1.8 NA's 
   4    9  485 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.210e+08 1.210e+08 3.886e+08 1.054e+09 1.054e+09       456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
  41    1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1425  0.1500  0.1517  0.1600  0.1700     456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   4   17   25   29 NA's 
  23    6   12    1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   3.024   5.000   8.000     456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   7.000   6.786   8.750  10.000     456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1789    1965    2118    2071    2118    2263     456 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   1.895   2.000   2.043   2.130   3.000     456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3   20 NA's 
  41    1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 NA's 
  12   30  456 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.210e+08 1.210e+08 3.466e+08 1.323e+08 1.054e+09       440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   1   16   41  440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1550  0.2478  0.5000  0.5000     440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    4.00   17.00   14.45   24.50   30.00     440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  58  440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   3.672   5.000   9.000     440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   7.000   6.776   9.000  12.000     440 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00    3.10   13.21   40.00   40.00     440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
  55    3  440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
13.3   20   80 NA's 
   1   41   16  440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 NA's 
  20   35    3  440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.2    49.7    85.2   103.8   167.0   247.2     443 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                  16                   42                  440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  42   16  440 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.150   0.150   0.234   0.170   0.500     443 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  16  482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
  13    3  482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              58 
                                            NA's 
                                             440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  58  440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                 repli-g x 
                       42                        13                         3 
                     NA's 
                      440 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.720   1.117   1.310   1.320   1.512   1.850     456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.500   8.200   8.145   8.775   9.600     456 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
       13        42       443 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.210e+08 1.210e+08 4.108e+08 1.054e+09 1.054e+09       456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  19   20   23   30 NA's 
   6    7    6   23  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   1    5    6    9 NA's 
  23    6    7    6  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   8.500   7.809  10.000  12.000     456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
1821 1999 2161 2288 NA's 
   7    6   23    6  456 


patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 NA's 
  13   29  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  140.0   168.0   176.5   178.3   183.8   213.0     456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  42                  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
  42  456 



patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              42 
                                            NA's 
                                             456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  42  456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     42     456 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen      NA's 
       42       456 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
1.76  1.9 1.92    2 NA's 
   1   17    1    1  478 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.210e+08 1.210e+08 3.694e+08 1.054e+09 1.076e+09       437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    7    8 NA's 
  27    2   32  437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.050   0.050   0.097   0.150   0.170     437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    4.00   17.00   14.92   25.00   29.00     437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  61  437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   3.607   5.000   9.000     437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   7.000   6.246   8.000  10.000     437 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.000   3.000   2.075   3.000   3.000     437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
  58    3  437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67   13   40   60 NA's 
  27    2    1   31  437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2014 NA's 
  20   38    3  437 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.070e+08 1.136e+08 1.401e+08 1.210e+08 1.054e+09       461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8 NA's 
  10   27  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0749  0.1200  0.1700     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   4   17   24 NA's 
  17    7   13  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  37  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   1    5    8 NA's 
  17   13    7  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   7.000   6.432   8.000   9.000     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1786    1786    1961    1970    2114    2116     461 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.320   2.260   3.000   2.692   3.000   3.000     461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
  37  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  11 13.3   60 NA's 
   1    9   27  461 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 NA's 
  20   17  461 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.070e+08 1.210e+08 3.216e+08 1.210e+08 1.054e+09       471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5 NA's 
  27  471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.140   0.150   0.147   0.155   0.170     471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   4   17   25 NA's 
   8    6   13  471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  27  471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1    5    8 NA's 
   8   13    6  471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   3    4    7    8 NA's 
   5    8    9    5  471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
1787 1963 2116 NA's 
  13    6    8  471 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   1.860   2.000   1.957   2.065   2.260     471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
  27  471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
13.3 NA's 
  27  471 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 NA's 
  19    8  471 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             497 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.210e+08 1.210e+08 3.694e+08 1.054e+09 1.076e+09       437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2   13 NA's 
  32   27    2  437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1467  0.1500  0.1700     437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    4.00   17.00   14.92   25.00   29.00     437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  61  437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   3.607   5.000   9.000     437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   4.000   5.672   7.000  10.000     437 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   0.930   1.130   1.373   1.600   3.400     437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
  58    3  437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.670   6.670   6.670   9.285  10.000  20.000     437 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2014 NA's 
  20   38    3  437 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.25   82.30  167.40  166.32  231.30  506.30     437 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna  rna NA's 
  59    2  437 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d    r NA's 
  59    2  437 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1400  0.1500  0.1462  0.1500  0.1700     437 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  59  439 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
  58    1  439 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              61 
                                            NA's 
                                             437 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  61  437 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
        adna preparation type chemical lysis dna extraction 
                           55                             4 
    mirvana (allprep dna) rna                          NA's 
                            2                           437 

patient.samples.sample.3.portions.portion.analytes.analyte.rna.ratio_28s_18s:
 1.5  1.7 NA's 
   1    1  496 

patient.samples.sample.3.portions.portion.analytes.analyte.rna.rinvalue:
 8.1  8.4 NA's 
   1    1  496 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       41        20       437 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   2  496 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   15.00   17.00   16.79   18.00   31.00     437 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  61  437 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   7.000   7.131  11.000  12.000     437 

patient.samples.sample.3.portions.portion.portion_number:
   1    2   11 NA's 
  56    3    2  437 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
   2  496 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  56  442 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   2  496 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   2  496 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   2  496 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   2  496 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
  25   40 NA's 
   1    1  496 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
  60   75 NA's 
   1    1  496 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   2  496 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   2  496 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
  56  442 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   40.00   60.00   54.02   67.00   78.00     441 

patient.samples.sample.3.portions.portion.year_of_creation:
2011 2012 2013 2014 NA's 
  48   10    2    1  437 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   5                   56                  437 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
   5   56  437 

patient.samples.sample.3.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.4000  0.3944  0.5000  0.7000     444 

patient.samples.sample.3.vial_number:
   a NA's 
  61  437 

patient.samples.sample.4.bcr_sample_barcode:
tcga-hc-7740-11a tcga-hc-8258-11a tcga-hc-8261-11a tcga-hc-8265-11a 
               1                1                1                1 
            NA's 
             494 

patient.samples.sample.4.bcr_sample_uuid:
11ddccdc-a5aa-4f81-862d-66bb1576d7cb 30787c2d-1622-4a9e-a2ed-a45a583091d8 
                                   1                                    1 
7747d93c-4c6a-4ad5-ba8c-d47685b247f2 dc4d93e4-6eb3-452f-a216-b360e5353a04 
                                   1                                    1 
                                NA's 
                                 494 

patient.samples.sample.4.intermediate_dimension:
 0.5  0.6 NA's 
   2    2  494 

patient.samples.sample.4.is_ffpe:
  no NA's 
   4  494 

patient.samples.sample.4.longest_dimension:
 0.5  0.6  0.7 NA's 
   1    2    1  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-hc-7740-11a-01r-2118-07 tcga-hc-8258-11a-01r-2263-07 
                           1                            1 
                        NA's 
                         496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
0120b5b1-d204-429a-aa21-704fd98624ea 575740a1-6467-4f58-b462-6fd1a4f63c51 
                                   1                                    1 
                                NA's 
                                 496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
120997910 128285457      NA's 
        1         1       496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   4   25 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   1    4 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   3   10 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
2118 2263 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   a    g NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
1.86    2 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-hc-7740-11a-01r-2117-13 tcga-hc-8258-11a-01r-2262-13 
                           1                            1 
tcga-hc-8261-11a-01w-2288-08 tcga-hc-8265-11a-01w-2288-08 
                           1                            1 
                        NA's 
                         494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
3af5f8c0-7f10-4076-ad17-049555b8bfa3 9ceda515-5073-4d0e-9b61-31449725b239 
                                   1                                    1 
ccb800b1-fa11-44fe-a8ec-4806a5aeb518 ebd551e3-e582-4839-9845-3958ab39e5e1 
                                   1                                    1 
                                NA's 
                                 494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
120997718 128285589 128286928 128286992      NA's 
        1         1         1         1       494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   8   13 NA's 
   2    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.14 0.15  0.5 NA's 
   1    1    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   4   23   25 NA's 
   1    2    1  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   1    4    5 NA's 
   1    1    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   3    4    5   10 NA's 
   1    1    1    1  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
2117 2262 2288 NA's 
   1    1    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   a    b    g NA's 
   1    1    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
 2.8    3   40 NA's 
   1    1    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20   80 NA's 
   2    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.amount:
   20 121.2   212 236.2  NA's 
    1     1     1     1   494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   2                    2                  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   2    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-hc-7740-11a-01r tcga-hc-8258-11a-01r tcga-hc-8261-11a-01w 
                   1                    1                    1 
tcga-hc-8265-11a-01w                 NA's 
                   1                  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
1aaf9750-784c-46de-8e97-287b2e52d0a2 21b29748-804e-4f2c-8ac0-dbba2490799b 
                                   1                                    1 
5cc67a47-749d-4044-9b8a-f7dee7f8d429 ec3cbdd3-0924-4ad7-a237-ca89f582c356 
                                   1                                    1 
                                NA's 
                                 494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.concentration:
0.14 0.15  0.5 NA's 
   1    1    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                        2                         2                       494 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
1.18 1.37 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.rinvalue:
 7.5  8.1 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
        2         2       494 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-hc-7740-11a-01w-2161-08 tcga-hc-8258-11a-01w-2288-08 
                           1                            1 
                        NA's 
                         496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
9d269d18-46c0-4056-89c4-67da5330925c c4020e50-9ec2-4fd7-bdee-5cd4d219ea95 
                                   1                                    1 
                                NA's 
                                 496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
120988858 128286951      NA's 
        1         1       496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  23   30 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   1    5 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   4   11 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
2161 2288 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   d NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.amount:
 159  218 NA's 
   1    1  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-hc-7740-11a-01w tcga-hc-8258-11a-01w                 NA's 
                   1                    1                  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
00ee266d-728d-4b40-90af-74db82e382f5 e706fa67-559e-4caa-8c0b-3fcad413e7d3 
                                   1                                    1 
                                NA's 
                                 496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     496 

patient.samples.sample.4.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen      NA's 
        2       496 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-hc-7740-11a-01d-2114-08 tcga-hc-8258-11a-01d-2260-08 
                           1                            1 
tcga-hc-8261-11a-01d-2260-08 tcga-hc-8265-11a-01d-2260-08 
                           1                            1 
                        NA's 
                         494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
011c6fe3-5efa-4f9a-971f-08330b3968b9 235b6bc1-39a3-4230-b3bc-ed9755a93945 
                                   1                                    1 
2b9ec727-0616-4b5f-991d-e28121bd724b ed3c9d2a-13f3-4fbb-82a7-571ce2987c86 
                                   1                                    1 
                                NA's 
                                 494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
120997334 128285696 128285720 128285750      NA's 
        1         1         1         1       494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   8 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.05 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   4   25 NA's 
   1    3  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   1    4 NA's 
   1    3  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   3    4   10 NA's 
   1    2    1  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
2114 2260 NA's 
   1    3  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   a    c    e    h NA's 
   1    1    1    1  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   3 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  60 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-hc-7740-11a-01d-2112-01 tcga-hc-8258-11a-01d-2259-01 
                           1                            1 
tcga-hc-8261-11a-01d-2259-01 tcga-hc-8265-11a-01d-2259-01 
                           1                            1 
                        NA's 
                         494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
0a447729-1f14-42e1-a837-a72a624ce81f 37700f67-b21d-430a-a649-e2efddb3328c 
                                   1                                    1 
702a24b2-d4ec-4082-b703-f4da90fbae66 f6ea2eef-c693-4dbb-8d89-6992d4593577 
                                   1                                    1 
                                NA's 
                                 494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
120996758 128286518 128287640 128287670      NA's 
        1         1         1         1       494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 NA's 
   2    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   4   25 NA's 
   1    3  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   1    4 NA's 
   1    3  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   3    4   10 NA's 
   1    2    1  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
2112 2259 NA's 
   1    3  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   a    c    e    h NA's 
   1    1    1    1  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.93    1 NA's 
   2    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
153.6 164.6 206.6 348.6  NA's 
    1     1     1     1   494 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-hc-7740-11a-01d tcga-hc-8258-11a-01d tcga-hc-8261-11a-01d 
                   1                    1                    1 
tcga-hc-8265-11a-01d                 NA's 
                   1                  494 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid:
7b6c08d2-3f06-4721-93b2-9bbecd8ab312 c6395e39-530f-40e9-b740-7a084e99d608 
                                   1                                    1 
ccff2794-bc6d-4144-bc9d-1445105cb39c fe203f40-ebfc-4b4d-bc64-7475828238f4 
                                   1                                    1 
                                NA's 
                                 494 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
0.14 0.15 NA's 
   2    2  494 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             494 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   4  494 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    4                   494 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        4       494 

patient.samples.sample.4.portions.portion.bcr_portion_barcode:
tcga-hc-7740-11a-01 tcga-hc-8258-11a-01 tcga-hc-8261-11a-01 tcga-hc-8265-11a-01 
                  1                   1                   1                   1 
               NA's 
                494 

patient.samples.sample.4.portions.portion.bcr_portion_uuid:
3623800f-c1bd-46db-aad4-cc3668038d9e 49c1f9d0-4b3d-496e-9c88-c5c2cb8e8962 
                                   1                                    1 
7ac97ffb-c859-47f4-be62-47fdf38f8c7d e545b686-bfd2-4760-9cfc-2954ebd8135a 
                                   1                                    1 
                                NA's 
                                 494 

patient.samples.sample.4.portions.portion.day_of_creation:
   6   29 NA's 
   3    1  494 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   4  494 

patient.samples.sample.4.portions.portion.month_of_creation:
   4   11 NA's 
   3    1  494 

patient.samples.sample.4.portions.portion.portion_number:
   1 NA's 
   4  494 

patient.samples.sample.4.portions.portion.slides.slide.bcr_slide_barcode:
tcga-hc-7740-11a-01-ts1 tcga-hc-8258-11a-01-ts1 tcga-hc-8261-11a-01-ts1 
                      1                       1                       1 
tcga-hc-8265-11a-01-ts1                    NA's 
                      1                     494 

patient.samples.sample.4.portions.portion.slides.slide.bcr_slide_uuid:
1cb778b1-58b6-4ebe-b7cd-07dc31fb89ba 97531eb7-8578-428d-9348-e00b2e2b9a43 
                                   1                                    1 
d5302bc5-3024-418a-ab1a-75ffbeb6b369 f58acca6-2745-4614-a143-c8f65e27ec31 
                                   1                                    1 
                                NA's 
                                 494 

patient.samples.sample.4.portions.portion.slides.slide.image_file_name:
tcga-hc-7740-11a-01-ts1.d5302bc5-3024-418a-ab1a-75ffbeb6b369.svs 
                                                               1 
tcga-hc-8258-11a-01-ts1.97531eb7-8578-428d-9348-e00b2e2b9a43.svs 
                                                               1 
tcga-hc-8261-11a-01-ts1.1cb778b1-58b6-4ebe-b7cd-07dc31fb89ba.svs 
                                                               1 
tcga-hc-8265-11a-01-ts1.f58acca6-2745-4614-a143-c8f65e27ec31.svs 
                                                               1 
                                                            NA's 
                                                             494 

patient.samples.sample.4.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   4  494 

patient.samples.sample.4.portions.portion.slides.slide.section_location:
 top NA's 
   4  494 

patient.samples.sample.4.portions.portion.weight:
  62   70   72   77 NA's 
   1    1    1    1  494 

patient.samples.sample.4.portions.portion.year_of_creation:
2011 2012 NA's 
   1    3  494 

patient.samples.sample.4.sample_type:
solid tissue normal                NA's 
                  4                 494 

patient.samples.sample.4.sample_type_id:
  11 NA's 
   4  494 

patient.samples.sample.4.shortest_dimension:
 0.3  0.4 NA's 
   1    3  494 

patient.samples.sample.4.vial_number:
   a NA's 
   4  494 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   64.0   238.5   605.5   941.7  1318.8  4724.0     220 





patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.5:
6e2cb3cf-f856-4c91-adf8-739a179a5c8c 7a4d2c8e-f68e-4c22-8808-fbb9a634db93 
                                   1                                    1 
c3fd0e07-99d5-4786-9cfc-01b8fc5ac4e4 c484e378-ef4c-4f91-acc0-53ffc48f166f 
                                   1                                    1 
                                NA's 
                                 494 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
1aa896b6-f5ca-4394-bdc0-7b0b9686854f 31684eb7-1659-4aef-a5d9-8e53b921e75a 
                                   1                                    1 
3645edb2-41d8-4b5f-9a4e-a11c1d354435                                 NA's 
                                   1                                  495 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
9194d7b4-3062-45d7-bc0c-3ca0fea0a5a7 c8756796-ed65-47b2-a66e-c48a6cf345c1 
                                   1                                    1 
                                NA's 
                                 496 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.8:
4ff9463c-c78f-49bb-a073-b93b5722d705 cb4eec3a-9457-40ab-866a-d9c41fa48ac4 
                                   1                                    1 
                                NA's 
                                 496 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
2527cb7f-2878-4d99-b81d-4f66b178dff7                                 NA's 
                                   1                                  497 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    70.0   140.0   229.8   250.0  2630.0     190 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.4000  0.7000  0.7053  0.9000  1.8000     309 

patient.samples.sample.is_ffpe:
 no 
498 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   0.900   1.100   1.164   1.400   2.500     309 

patient.samples.sample.oct_embedded:
false  true  NA's 
   40   269   189 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.820   1.800   1.840   1.841   1.890   2.210      81 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             497 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.210e+08 1.507e+08 1.641e+08 3.467e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  7  13 
302 196 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1612  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   18.00   25.00   21.53   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
498 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   4.747   6.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   4.584   7.000  10.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.730   2.130   3.000   2.835   3.400   4.540 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
 22  23 
178 320 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13 13.3   20 26.7 
 124  113  196   65 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
 101   87  148  162 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
150756964 160750855 164082168 160750880 164082191 164082235       490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7   13 NA's 
   2    6  490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 0.16 0.17 0.18 NA's 
   1    3    2    2  490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2   29 NA's 
   2    6  490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   8  490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   1   10 NA's 
   6    2  490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   2    3    4    5 NA's 
   4    1    1    2  490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a32y a360 NA's 
   2    6  490 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.080   3.178   3.730   3.449   4.018   4.190     490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   8  490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
  13 23.3 NA's 
   2    6  490 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2013 2014 NA's 
   2    6  490 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             497 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.070e+08 1.323e+08 1.641e+08 3.118e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  7  13 
196 302 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1612  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   18.00   25.00   21.53   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
 no 
498 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   4.747   6.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.508   7.000  10.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.820   2.210   3.000   2.764   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
 22  23 
178 320 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
 13  20 
196 302 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
 101   87  148  162 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.20   22.42   38.39   61.14   76.65  379.80 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
498 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
  r 
498 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1612  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             498 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
 no 
498 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                      498 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.040   1.230   1.450   1.449   1.670   2.310 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.800   8.300   8.356   8.900  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
    498 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 395   35    1   67 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.539e+08 1.678e+08 3.696e+08 1.752e+08 1.062e+09        73 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
 425   73 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 425   73 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00    8.00   21.00   17.96   24.00   30.00      73 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 425   73 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   5.379   7.000  10.000      73 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.824   8.000  12.000      73 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 425   73 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
 172  253   73 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 425   73 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
 101   71   89  164   73 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  137.0   159.0   171.0   172.1   187.0   225.0     326 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                 425                   73 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
 425   73 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
 172  326 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 425   73 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
 172  253   73 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             425 
                                            NA's 
                                              73 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 425   73 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
      419         6        73 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
      107        65       326 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 326   31    1  140 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9    2 NA's 
   2   85    7  404 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.136e+08 1.323e+08 1.641e+08 3.119e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   8 
120 378 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05000 0.07000 0.08000 0.09225 0.09000 0.17000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   12.00   25.00   20.66   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
498 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   3.952   5.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.576   8.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   2.800   3.000   2.602   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 22  23 
178 320 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67   40   60 
 120  320   58 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
 101   87  112  198 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.070e+08 1.323e+08 1.603e+08 2.708e+08 1.742e+08 1.054e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  5   8 
378 120 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0500  0.1300  0.1500  0.1296  0.1600  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   18.00   24.00   21.49   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 no 
498 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   4.747   6.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.516   8.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.730   1.950   2.080   2.125   2.080   3.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
 22  23 
178 320 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  13 13.3   40   60 
 320   58   65   55 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 
 101   87  148  162 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.210e+08 1.050e+09 6.840e+08 1.050e+09 1.054e+09       364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   1    5 NA's 
  14  120  364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1501  0.1600  0.1700     364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.50   23.00   17.51   23.00   25.00     364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 134  364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   3.634   5.000   9.000     364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.843   5.750   9.000     364 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.860   2.260   2.907   4.010   4.540     364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
 120   14  364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
 6.7    8 13.3 26.7 NA's 
   1   13   55   65  364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 2013 2014 NA's 
  99   21   13    1  364 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-hc-7079-01a-11d-a46a-09 tcga-hi-7169-01a-11d-a46a-09 
                           1                            1 
                        NA's 
                         496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
55b25361-3bb0-4b45-bbad-6aeba67b5820 a4b5f31f-4a30-4987-ae81-820c791767dd 
                                   1                                    1 
                                NA's 
                                 496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
167811779 167811803      NA's 
        1         1       496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   9 NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  21 NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1 NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   2 NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a46a NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   c    e NA's 
   1    1  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
0.94 NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4 NA's 
   2  496 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2015 NA's 
   2  496 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.136e+08 1.456e+08 1.641e+08 3.123e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2 
378 120 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.13 0.14 0.15 0.16 0.17 
  16   86  163  164   69 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   12.00   25.00   20.66   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
498 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   3.952   5.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   6.000   5.504   8.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.010   1.070   1.232   1.140   2.260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
178 320 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7   10 13.3 
  58  320   37   83 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
 101   87  112  198 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.88   21.34   32.38   80.26  116.60  456.30 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
498 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
498 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
  18   86  160  165   69 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
498 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
183 315 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             498 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
498 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  498 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      404        94 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 395   35    1   67 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   14.00   14.65   20.75   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
498 

patient.samples.sample.portions.portion.lce:
 lce NA's 
   2  496 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   8.000   7.153  11.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   11.00   12.38   11.00   61.00 

patient.samples.sample.portions.portion.portion_sequence:
   1    2    3    6 NA's 
 165   17   13    2  301 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 115  383 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   1.478   2.000   5.000     406 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3587  0.0000  7.0000     406 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5391  0.0000 15.0000     383 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0    1    2 NA's 
  89    1    2  406 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   11.96   20.00   30.00     389 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    15.0    15.0    19.7    25.0    58.0     389 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   60.00   70.00   68.01   75.00   90.00     389 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  55.00   65.00   75.00   73.55   80.00   95.00     383 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
 top NA's 
 115  383 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
498 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   6.368   3.000 100.000      74 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.279   0.000  20.000      75 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4327  0.0000 20.0000       8 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    2   20 NA's 
 346   20   11   47   74 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    5.00   10.18   15.00   70.00      16 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    13.0    20.0    22.6    30.0    80.0      16 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0      55      70      66      80     100      16 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   65.00   75.00   73.21   80.00  100.00       7 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
   115    383 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.00   30.00   30.12   30.00   60.00       3 

patient.samples.sample.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
  34  128   48  200   88 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 352  146 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 352  146 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   20.58   21.00   51.00     146 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    2.00    1.94    2.00    5.00     230 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   3   24 NA's 
 164  188  146 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   6 NA's 
 352  146 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2013 2014 NA's 
 164  188  146 

patient.samples.sample.sample_type:
primary tumor 
          498 

patient.samples.sample.sample_type_id:
  1 
498 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.2000  0.3000  0.3132  0.4000  0.8000     309 

patient.samples.sample.vial_number:
  a   b 
484  14 


SCNA_cluster:
More_SCNA     Quiet Some_SCNA      NA's 
      108        58       167       165 

iCluster:
   1    2    3 NA's 
  85  109  139  165 

mRNA_cluster:
   1    2    3 NA's 
 112  105  116  165 

methylation_cluster:
   1    2    3    4 NA's 
  41  110  101   81  165 

RPPA_cluster:
   1    2    3 NA's 
  52   39   55  352 

miRNA_cluster:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.918   5.000   6.000     168 

ERG_status:
fusion   none   NA's 
   152    181    165 

ETV1_status:
fusion   high   none   NA's 
    17     12    304    165 

ETV4_status:
fusion   high   none   NA's 
    10      6    317    165 

FLI1_status:
fusion   high   none   NA's 
     2      2    329    165 

SPOP_mut:
   0    1 NA's 
 296   37  165 

FOXA1_mut:
   0    1 NA's 
 320   13  165 

MED12_mut:
   0    1 NA's 
 327    6  165 

IDH1_mut:
   0    1 NA's 
 330    3  165 

}}
\keyword{datasets}
