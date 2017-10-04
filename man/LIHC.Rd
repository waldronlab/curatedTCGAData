\name{ LIHC }
\alias{ LIHC }
\docType{data}
\title{ Liver hepatocellular carcinoma }

\details{
\preformatted{
> experiments( LIHC )
ExperimentList class object of length 10: 
 [1] CNASNP-20160128: RaggedExperiment with 363628 rows and 760 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 93328 rows and 760 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 370 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 370 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 429 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 424 columns 
 [7] Mutation-20160128: RaggedExperiment with 27892 rows and 198 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 423 columns 
 [9] RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 26 columns 
 [10] RPPAArray-20160128: SummarizedExperiment with 219 rows and 184 columns 

> rownames( LIHC )
CharacterList of length 10
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RNASeqGene-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363

> colnames( LIHC )
CharacterList of length 10
[["CNASNP-20160128"]] TCGA-2V-A95S-01A-11D-A36W-01 ...
[["CNVSNP-20160128"]] TCGA-2V-A95S-01A-11D-A36W-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-2V-A95S-01A-11D-A36W-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-2V-A95S-01A-11D-A36W-01 ...
[["Methylation-20160128"]] TCGA-2V-A95S-01A-11D-A36Y-05 ...
[["miRNASeqGene-20160128"]] TCGA-2V-A95S-01A-11R-A37G-13 ...
[["Mutation-20160128"]] TCGA-BC-4073-01B-02D-A12Z-10 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-2V-A95S-01A-11R-A37K-07 ...
[["RNASeqGene-20160128"]] TCGA-BC-4073-01B-02R-A131-07 ...
[["RPPAArray-20160128"]] TCGA-BC-4072-01B-21-A40L-20 ...

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASNP-20160128  9.9 Mb
2                    CNVSNP-20160128  2.7 Mb
3          GISTIC_AllByGene-20160128  4.3 Mb
4  GISTIC_ThresholdedByGene-20160128  4.3 Mb
5               Methylation-20160128 67.5 Mb
6              miRNASeqGene-20160128  0.1 Mb
7                  Mutation-20160128 15.9 Mb
8           RNASeq2GeneNorm-20160128  1.2 Mb
9                RNASeqGene-20160128  1.1 Mb
10                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   245 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
132.000 132.000   1.144   0.956   1.633 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  16.00   51.00   61.00   59.27   69.00   87.00       4 

vital_status:
  0   1 
245 132 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   194.8   417.5   672.1   837.0  3258.0     245 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   395.8   649.5   885.8  1222.0  3675.0     133 

tumor_tissue_site:
liver 
  377 



pathology_N_stage:
  n0   n1   nx NA's 
 257    4  115    1 

pathology_M_stage:
 m0  m1  mx 
272   4 101 

gender:
female   male 
   122    255 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1995    2008    2011    2010    2012    2013       3 

radiation_therapy:
  no  yes NA's 
 345    9   23 

histological_type:
         fibrolamellar carcinoma         hepatocellular carcinoma 
                               3                              367 
hepatocholangiocarcinoma (mixed) 
                               7 

residual_tumor:
  r0   r1   r2   rx NA's 
 330   17    1   22    7 

race:
american indian or alaska native                            asian 
                               2                              161 
       black or african american                            white 
                              17                              187 
                            NA's 
                              10 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    18                    340                     19 

admin.bcr:
nationwide children's hospital 
                           377 

admin.day_of_dcc_upload:
  1 
377 

admin.disease_code:
lihc 
 377 


admin.month_of_dcc_upload:
 10 
377 

admin.patient_withdrawal.withdrawn:
false 
  377 

admin.project_code:
tcga 
 377 

admin.year_of_dcc_upload:
2015 
 377 

patient.ablation_embolization_tx_adjuvant:
  no  yes NA's 
 236   13  128 

patient.adjacent_hepatic_tissue_inflammation_extent_type:
  mild   none severe   NA's 
   101    119     19    138 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  16.00   51.00   61.00   59.45   69.00   90.00       1 


patient.cancer_diagnosis_first_degree_relative_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.000   1.737   2.000   9.000     259 

patient.child_pugh_classification_grade:
   a    b    c NA's 
 223   21    1  132 

patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      342        35 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-731.00   -5.00   -1.00   69.06    0.00 1552.00      38 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   223.5   643.0   852.1  1033.0  3258.0     342 


patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                                                 no 
                                                                374 
yes, both radiation and pharmaceutical treatment prior to resection 
                                                                  1 
                   yes, pharmaceutical treatment prior to resection 
                                                                  2 

patient.clinical_cqcf.normal_tissue_anatomic_site:
liver other  NA's 
  184     6   187 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
          191           186 

patient.clinical_cqcf.tumor_type:
primary 
    377 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    7.00   15.00   16.44   26.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32120  -25254  -22457  -21834  -18935   -5862       4 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   182.5   556.0   758.6  1141.0  3258.0     286 

patient.days_to_definitive_surgical_procedure_performed:
   0 NA's 
 374    3 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 376    1 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -8.0    27.0   295.0   558.7   808.0  3675.0      92 



patient.drugs.drug.2.clinical_trail_drug_classification:
torc1/2 inhibitor              NA's 
                1               376 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.75   13.50   11.25   17.25   22.00     365 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  207.0   378.8   508.0   653.1   817.0  1425.0     369 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   25.0   139.5   327.5   383.6   511.8  1349.0     365 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease             partial response 
                           3                            2 
              stable disease                         NA's 
                           2                          370 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.500   6.083   8.000  11.000     365 

patient.drugs.drug.2.prescribed_dose:
 200 NA's 
   1  376 

patient.drugs.drug.2.prescribed_dose_units:
  mg NA's 
   1  376 

patient.drugs.drug.2.regimen_indication:
other: specify in notes             progression                    NA's 
                      1                       1                     375 

patient.drugs.drug.2.regimen_indication_notes:
dose reduction           NA's 
             1            376 

patient.drugs.drug.2.regimen_number:
   1    2 NA's 
   1    1  375 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
oral NA's 
   1  376 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
   9    3  365 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                         8                          1 
targeted molecular therapy                       NA's 
                         1                        367 

patient.drugs.drug.2.total_dose:
 400 NA's 
   1  376 

patient.drugs.drug.2.total_dose_units:
mg/day   NA's 
     1    376 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
   6    4  367 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2013    2013    2014    2015     365 



patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.50   15.00   13.71   21.00   31.00     370 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  217.0   534.5   791.5   947.8  1349.2  1903.0     371 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  217.0   482.5   498.0   805.0  1135.0  1685.0     370 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease               stable disease 
                           3                            2 
                        NA's 
                         372 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   5.500   7.000   7.429   9.000  12.000     370 

patient.drugs.drug.3.number_cycles:
   1 NA's 
   1  376 

patient.drugs.drug.3.prescribed_dose:
  20 NA's 
   1  376 

patient.drugs.drug.3.prescribed_dose_units:
  mg NA's 
   1  376 

patient.drugs.drug.3.regimen_indication:
adjuvant     NA's 
       1      376 

patient.drugs.drug.3.regimen_number:
   1 NA's 
   1  376 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intrahepatic         NA's 
           1          376 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
   6    1  370 

patient.drugs.drug.3.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                         4                          1 
            other, specify targeted molecular therapy 
                         1                          1 
                      NA's 
                       370 

patient.drugs.drug.3.therapy_types.therapy_type_notes:
chemoembolization              NA's 
                1               376 

patient.drugs.drug.3.total_dose:
  20 NA's 
   1  376 

patient.drugs.drug.3.total_dose_units:
  mg NA's 
   1  376 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
   2    4  371 

patient.drugs.drug.3.year_of_form_completion:
2011 2013 2014 NA's 
   1    3    3  370 

patient.drugs.drug.4.bcr_drug_barcode:
tcga-bc-a10q-d10102 tcga-dd-a4ns-d59877 tcga-ub-a7ma-d67138 tcga-ub-a7mb-d63689 
                  1                   1                   1                   1 
               NA's 
                373 

patient.drugs.drug.4.bcr_drug_uuid:
008cfe07-942d-4ead-b4f7-d21ba4f9bf5d 36919e0e-fe64-42da-a63c-712e27f3af55 
                                   1                                    1 
4941edda-d11c-43c2-b550-76dae03c8034 e5e8b541-f299-40df-a792-7514b166b09f 
                                   1                                    1 
                                NA's 
                                 373 

patient.drugs.drug.4.day_of_form_completion:
   7   15   22   31 NA's 
   1    1    1    1  373 

patient.drugs.drug.4.days_to_drug_therapy_end:
 510  751 1425 NA's 
   1    1    1  374 

patient.drugs.drug.4.days_to_drug_therapy_start:
 426  480  485 1349 NA's 
   1    1    1    1  373 

patient.drugs.drug.4.drug_name:
 gemcitabine  oxaliplatin    sorafenib temozolomide         NA's 
           1            1            1            1          373 

patient.drugs.drug.4.measure_of_response:
clinical progressive disease                         NA's 
                           2                          375 

patient.drugs.drug.4.month_of_form_completion:
   4    5    8   10 NA's 
   1    1    1    1  373 

patient.drugs.drug.4.regimen_indication:
progression        NA's 
          1         376 

patient.drugs.drug.4.regimen_number:
   2 NA's 
   1  376 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
   3    1  373 

patient.drugs.drug.4.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         3                          1 
                      NA's 
                       373 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
   1    2  374 

patient.drugs.drug.4.year_of_form_completion:
2011 2014 NA's 
   1    3  373 

patient.drugs.drug.5.bcr_drug_barcode:
tcga-ub-a7mb-d63690                NA's 
                  1                 376 

patient.drugs.drug.5.bcr_drug_uuid:
0a33c84d-d75a-4ea7-9abd-8ef3bb748571                                 NA's 
                                   1                                  376 

patient.drugs.drug.5.clinical_trail_drug_classification:
imid NA's 
   1  376 

patient.drugs.drug.5.day_of_form_completion:
  15 NA's 
   1  376 

patient.drugs.drug.5.days_to_drug_therapy_start:
 525 NA's 
   1  376 

patient.drugs.drug.5.month_of_form_completion:
   8 NA's 
   1  376 

patient.drugs.drug.5.therapy_ongoing:
 yes NA's 
   1  376 

patient.drugs.drug.5.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        376 

patient.drugs.drug.5.tx_on_clinical_trial:
 yes NA's 
   1  376 

patient.drugs.drug.5.year_of_form_completion:
2014 NA's 
   1  376 



patient.drugs.drug.clinical_trail_drug_classification:
          chemotherapy               phase ii              phase iii 
                     1                      2                      1 
sunitinib vs sorafenib                   NA's 
                     1                    372 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   13.50   12.88   17.75   27.00     335 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   85.0   217.8   358.0   524.9   664.0  2304.0     345 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   132.5   232.0   362.6   390.2  2233.0     335 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          18                            2 
            partial response               stable disease 
                           3                            2 
                        NA's 
                         352 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.691   7.750  12.000     335 

patient.drugs.drug.number_cycles:
   1 NA's 
   3  374 

patient.drugs.drug.prescribed_dose:
   5  400 NA's 
   1    1  375 

patient.drugs.drug.prescribed_dose_units:
  mg NA's 
   2  375 

patient.drugs.drug.regimen_indication:
   adjuvant progression        NA's 
          2           2         373 

patient.drugs.drug.regimen_number:
   1 NA's 
   4  373 

patient.drugs.drug.route_of_administrations.route_of_administration:
intrahepatic         oral         NA's 
           2            1          374 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
  33    9  335 

patient.drugs.drug.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         1                         31 
            other, specify targeted molecular therapy 
                         2                          6 
                      NA's 
                       337 

patient.drugs.drug.therapy_types.therapy_type_notes:
chemoembolization              NA's 
                2               375 

patient.drugs.drug.total_dose:
   5  800 NA's 
   1    1  375 

patient.drugs.drug.total_dose_units:
    mg mg/day   NA's 
     1      1    375 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  33    5  339 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2013    2013    2013    2014    2015     335 

patient.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.6348  1.0000  4.0000      84 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    18                    340                     19 

patient.follow_ups.follow_up.2.ablation_embolization_tx_adjuvant:
  no  yes NA's 
  24    2  351 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    13.0    17.0    17.3    21.5    30.0     350 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  596.0   627.0   711.0   819.2   765.0  1397.0     372 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  449.0   803.2   909.5  1125.4   979.5  3308.0     355 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
scheduled follow-up submission                           NA's 
                             1                            376 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  22    3  352 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.519   5.000  11.000     350 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no NA's 
   2  375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   2  375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 656 2742 NA's 
   1    1  375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    2   375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
intrahepatic recurrence locoregional recurrence                    NA's 
                      1                       1                     375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
   1    1  375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_liver_transplant:
  no NA's 
   2  375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   7    2  368 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   7    2  368 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 433  913 NA's 
   1    1  375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    161     239     433     533     656    1286     368 


patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence locoregional recurrence 
                      5                       3                       1 
                   NA's 
                    368 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
                    adrenal retroperitoneal lymph nodes 
                          1                           1 
                       NA's 
                        375 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
   5    4  368 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   7    2  368 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_liver_transplant:
  no NA's 
   9  368 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   1  376 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  16   10  351 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        14         11        352 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  25    1  351 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  24    2  351 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   22     5   350 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2014    2014    2014    2015    2015     350 

patient.follow_ups.follow_up.3.ablation_embolization_tx_adjuvant:
  no NA's 
   1  376 

patient.follow_ups.follow_up.3.bcr_followup_barcode:
tcga-dd-a4nl-f59644 tcga-zp-a9d0-f68255                NA's 
                  1                   1                 375 

patient.follow_ups.follow_up.3.bcr_followup_uuid:
62e6fcc5-e450-49b3-8cbe-80757071734f 79b3dc1f-c7ee-48b6-af43-c3e51f504a57 
                                   1                                    1 
                                NA's 
                                 375 

patient.follow_ups.follow_up.3.day_of_form_completion:
  14   16 NA's 
   1    1  375 

patient.follow_ups.follow_up.3.days_to_last_followup:
 909 NA's 
   1  376 

patient.follow_ups.follow_up.3.lost_follow_up:
  no  yes NA's 
   1    1  375 

patient.follow_ups.follow_up.3.month_of_form_completion:
   7   11 NA's 
   1    1  375 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   1  376 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free       NA's 
         1        376 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no NA's 
   1  376 

patient.follow_ups.follow_up.3.radiation_therapy:
  no NA's 
   1  376 

patient.follow_ups.follow_up.3.vital_status:
alive  NA's 
    1   376 

patient.follow_ups.follow_up.3.year_of_form_completion:
2014 NA's 
   2  375 

patient.follow_ups.follow_up.4.ablation_embolization_tx_adjuvant:
  no NA's 
   1  376 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-zp-a9d0-f69077                NA's 
                  1                 376 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
e6f3de6d-7d7b-4e8d-b369-1f7a5edd760e                                 NA's 
                                   1                                  376 

patient.follow_ups.follow_up.4.day_of_form_completion:
  29 NA's 
   1  376 

patient.follow_ups.follow_up.4.days_to_last_followup:
1091 NA's 
   1  376 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   1  376 

patient.follow_ups.follow_up.4.month_of_form_completion:
  12 NA's 
   1  376 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   1  376 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
tumor free       NA's 
         1        376 

patient.follow_ups.follow_up.4.postoperative_rx_tx:
  no NA's 
   1  376 

patient.follow_ups.follow_up.4.radiation_therapy:
  no NA's 
   1  376 

patient.follow_ups.follow_up.4.vital_status:
alive  NA's 
    1   376 

patient.follow_ups.follow_up.4.year_of_form_completion:
2014 NA's 
   1  376 

patient.follow_ups.follow_up.ablation_embolization_tx_adjuvant:
  no  yes NA's 
 275   25   77 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   17.00   16.95   23.00   31.00      52 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   199.8   383.5   683.7   837.0  3258.0     283 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   406.0   641.0   916.6  1281.8  3675.0     151 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
scheduled follow-up submission                           NA's 
                            18                            359 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 268   25   84 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000  10.000   7.735  11.000  12.000      52 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
  17   11  349 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
  21    7  349 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 359 1525 2088 NA's 
   1    1    1  374 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   49.0   190.0   316.0   599.4   729.0  2742.0     348 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_transplant:
1875 NA's 
   1  376 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
          bone          liver           lung other, specify           NA's 
             5             12              8              3            349 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence locoregional recurrence 
                     15                      10                       3 
      new primary tumor                    NA's 
                      1                     348 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
         adrenal nodule anterior abdominal wall              peritoneal 
                      1                       1                       1 
                   NA's 
                    374 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
  18   10  349 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  26    3  348 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_liver_transplant:
  no  yes NA's 
  27    1  349 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.residual_disease_post_new_tumor_event_margin_status:
  r0   rx NA's 
   1    1  375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   5    2  370 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   6  371 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
 431 NA's 
   1  376 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   98.0   169.5   406.0   599.7   557.0  2241.0     370 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
         liver           lung other, specify           NA's 
             4              1              2            370 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence locoregional recurrence 
                      3                       3                       1 
                   NA's 
                    370 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
intraperitoneum          spleen            NA's 
              1               1             375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
   2    4  371 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   6    1  370 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_liver_transplant:
  no NA's 
   7  370 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    1  375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no NA's 
   2  375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 239  558 NA's 
   1    1  375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
         liver other, specify           NA's 
             1              1            375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence                    NA's 
                      1                       1                     375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_occurrence_anatomic_site_text:
spleen   NA's 
     1    376 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
   1    1  375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_event_liver_transplant:
  no NA's 
   2  375 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
 109   30  238 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
 130    8  239 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  124.0   298.0   660.0   663.7   892.0  1678.0     360 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     20     161     293     433     553    2133     232 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_transplant:
 713 1211 1648 1875 NA's 
   1    1    1    1  373 




patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
  80   58  239 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
 124   18  235 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_liver_transplant:
  no  yes NA's 
 135    4  238 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r1   rx NA's 
  13    1    6  357 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 154  146   77 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       164        125         88 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
 287   16   74 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 296    7   74 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  226    94    57 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2013    2014    2014    2014    2015      52 

patient.gender:
female   male 
   122    255 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   64.0   161.0   168.0   167.4   174.0   196.0      32 

patient.hematology_serum_creatinine_laboratory_result_value_in_mg_dl:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   0.700   0.900   2.735   1.100 124.000      72 

patient.histological_type:
         fibrolamellar carcinoma         hepatocellular carcinoma 
                               3                              367 
hepatocholangiocarcinoma (mixed) 
                               7 



patient.history_hepato_carcinoma_risk_factors.history_hepato_carcinoma_risk_factor.3:
                     hepatitis c nonalcoholic fatty liver disease 
                               4                                1 
                           other                             NA's 
                               2                              370 

patient.history_hepato_carcinoma_risk_factors.history_hepato_carcinoma_risk_factor.4:
hemochromatosis            NA's 
              1             376 


patient.history_of_neoadjuvant_treatment:
 no yes 
375   2 

patient.icd_10:
c22.0 
  377 


patient.icd_o_3_site:
c22.0 
  377 

patient.informed_consent_verified:
yes 
377 

patient.lab_procedure_platelet_result_specified_value:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
     4.0    157.2    209.5  23965.6    295.8 499000.0       67 

patient.laboratory_prcoedure_platelet_result_lower_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     130     150   13744     150  163000      68 

patient.laboratory_prcoedure_platelet_result_upper_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      6     400     400   38600     450  450000      68 

patient.laboratory_procedure_albumin_result_lower_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.30    3.50    3.50   16.58    3.80 3800.00      79 

patient.laboratory_procedure_albumin_result_specified_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.20    3.50    4.00   21.47    4.30 5200.00      74 

patient.laboratory_procedure_albumin_result_upper_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.50    5.00    5.00   22.56    5.30 5100.00      79 

patient.laboratory_procedure_alpha_fetoprotein_outcome_lower_limit_of_normal_value:
   0    6 NA's 
 267    5  105 

patient.laboratory_procedure_alpha_fetoprotein_outcome_upper_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.000   6.250   8.000   9.489  10.000  44.000     107 

patient.laboratory_procedure_alpha_fetoprotein_outcome_value:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
      1.0       4.0      14.5   13692.5     264.8 2035400.0        93 

patient.laboratory_procedure_creatinine_result_lower_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.60    0.70    1.75    0.80   62.00      72 

patient.laboratory_procedure_creatinine_result_upper_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.900   1.200   1.400   3.265   1.500 120.000      70 

patient.laboratory_procedure_international_normalization_ratio_result_lower_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.900   1.000   3.511   8.300  11.000     106 

patient.laboratory_procedure_international_normalization_ratio_result_upper_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.200   4.823  11.500  15.000     106 

patient.laboratory_procedure_prothrombin_time_result_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.800   1.000   1.100   4.042   9.150  36.400      77 

patient.laboratory_procedure_total_bilirubin_result_specified_lower_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.1000  0.2000  0.1369  0.2000  1.0000      76 

patient.laboratory_procedure_total_bilirubin_result_specified_upper_limit_of_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.100   0.500   0.700   1.001   1.000  19.000      71 

patient.laboratory_procedure_total_bilirubin_result_upper_limit_normal_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.200   1.125   1.200   1.408   1.300  21.000      75 


patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    4.00    6.00    6.18    8.00   12.00 

patient.neoplasm_histologic_grade:
  g1   g2   g3   g4 NA's 
  55  180  124   13    5 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   7    5  365 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
  10    2  365 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 485 1882 NA's 
   1    1  375 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   49.0   148.5   398.0   690.9   854.5  2742.0     362 


patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence locoregional recurrence 
                      6                       3                       5 
      new primary tumor                    NA's 
                      1                     362 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
renal met from prior lung primary                              NA's 
                                1                               376 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
   8    5  364 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  11    2  364 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_liver_transplant:
  no NA's 
  13  364 

patient.new_tumor_events.new_tumor_event.2.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   2  375 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   3    2  372 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   5  372 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
1305 NA's 
   1  376 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   98.0   130.0   266.0   788.4  1285.0  2163.0     372 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
         liver other, specify           NA's 
             2              3            372 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence locoregional recurrence 
                      3                       1                       1 
                   NA's 
                    372 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
 adrenal nodule intraperitoneum     small bowel            NA's 
              1               1               1             374 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
   3    2  372 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   4    1  372 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_liver_transplant:
  no NA's 
   5  372 

patient.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no NA's 
   1  376 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no NA's 
   1  376 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 239 NA's 
   1  376 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    1   376 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
intrahepatic recurrence                    NA's 
                      1                     376 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_ablation_embo_tx:
 yes NA's 
   1  376 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  376 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_liver_transplant:
  no NA's 
   1  376 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  70   17  290 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  81    6  290 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0   349.0   699.0   736.2   899.8  2190.0     361 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     15     111     231     421     491    2189     280 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_transplant:
 713 1648 NA's 
   1    1  375 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence locoregional recurrence 
                     14                      48                      31 
      new primary tumor                    NA's 
                      2                     282 


patient.new_tumor_events.new_tumor_event.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
  39   48  290 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  71   16  290 

patient.new_tumor_events.new_tumor_event.new_tumor_event_liver_transplant:
  no  yes NA's 
  89    2  286 

patient.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r1   r2   rx NA's 
  11    1    1    5  359 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 174   98  105 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       236        114         27 

patient.postoperative_rx_tx:
  no  yes NA's 
 226   15  136 

patient.prior_dx:
 no yes 
340  37 

patient.race:
american indian or alaska native                            asian 
                               2                              161 
       black or african american                            white 
                              17                              187 
                            NA's 
                              10 

patient.radiation_therapy:
  no  yes NA's 
 243    4  130 

patient.radiations.radiation.anatomic_treatment_site:
 distant recurrence        distant site primary tumor field       regional site 
                  3                   2                   2                   4 
               NA's 
                366 



patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.50   14.00   14.09   18.50   27.00     366 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   37.0   344.2   418.5   500.4   739.2  1099.0     367 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   18.0   142.5   430.0   451.4   675.0  1092.0     366 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                               2                                1 
radiographic progressive disease                   stable disease 
                               4                                1 
                            NA's 
                             369 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.500   6.000   6.273   8.500  11.000     366 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.500   3.000   6.714   7.500  25.000     370 

patient.radiations.radiation.radiation_dosage:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    0.29    50.00    68.00  2839.54  3625.00 14000.00      369 

patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
  10    1  366 

patient.radiations.radiation.radiation_type:
external internal systemic     NA's 
       6        2        2      367 

patient.radiations.radiation.units:
 cgy   gy NA's 
   3    4  370 

patient.radiations.radiation.year_of_form_completion:
2013 2014 2015 NA's 
   6    4    1  366 

patient.relative_family_cancer_history_ind_3:
  no  yes NA's 
 212  114   51 

patient.residual_tumor:
  r0   r1   r2   rx NA's 
 330   17    1   22    7 



patient.stage_event.system_version:
4th 5th 6th 7th 
  4  23 119 231 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
 m0  m1  mx 
272   4 101 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
  n0   n1   nx NA's 
 257    4  115    1 



patient.tissue_prospective_collection_indicator:
 no yes 
249 128 

patient.tissue_retrospective_collection_indicator:
 no yes 
128 249 


patient.tumor_tissue_site:
liver 
  377 

patient.vascular_tumor_cell_invasion_type:
macro micro  none  NA's 
   17    94   210    56 




patient.viral_hepatitis_serologies.viral_hepatitis_serology.4:
          hbv core antibody                     hbv dna 
                          6                           7 
       hbv surface antibody hepatitis b surface antigen 
                          4                           1 
                       NA's 
                        359 

patient.viral_hepatitis_serologies.viral_hepatitis_serology.5:
hbv core antibody              NA's 
                3               374 

patient.vital_status:
alive  dead 
  286    91 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   59.00   69.00   72.89   82.00  172.00      27 

patient.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2010    2012    2014    2013    2014    2015 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1995    2008    2011    2010    2012    2013       3 


patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              41                                4 
                            NA's 
                             332 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                 13                 364 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus    non-canonical 
             288               80                9 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      342        35 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -24.00   64.25  366.00  926.83 1512.50 5992.00       3 

patient.biospecimen_cqcf.days_to_sample_procurement:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-116.000    0.000    0.000    0.516    0.000  267.000        3 

patient.biospecimen_cqcf.digital_image_submitted:
  no  yes NA's 
  81   26  270 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    12                    228                    137 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
liver 
  377 


patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                                                 no 
                                                                374 
yes, both radiation and pharmaceutical treatment prior to resection 
                                                                  1 
                   yes, pharmaceutical treatment prior to resection 
                                                                  2 

patient.biospecimen_cqcf.method_of_sample_procurement:
other method (please specify)            surgical resection 
                           18                           358 
                   transplant 
                            1 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-116.00    0.00    0.00   13.82    0.00 1458.00     230 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1250  0.1600  0.1831  0.2300  0.4700     322 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
             cdrop           nanodrop            trinean trinean instrument 
                 1                  1                 48                  5 
              NA's 
               322 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   16.00   18.00   19.82   22.00   47.00     322 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     50     100     100     122     150     225     322 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
         blood draw laparoscopic biopsy  surgical resection                NA's 
                 80                   1                  65                 231 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
liver  NA's 
   55   322 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           56           321 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
 127   23  227 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
3171cfeb-80dc-4273-ae72-f173e6f28ff0 c62268e3-9631-4629-ae35-430c9d6628d2 
                                   1                                    1 
f2ed143f-17b6-431d-88e0-5fe0bca5a59e f2f7af17-df75-4fca-a4ab-2245603bbf71 
                                   1                                    1 
                                NA's 
                                 373 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
   0 NA's 
   1  376 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_conc:
0.12 NA's 
   1  376 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qm:
trinean    NA's 
      1     376 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qty:
  18 NA's 
   1  376 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_vol:
 150 NA's 
   1  376 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
surgical resection               NA's 
                 1                376 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
liver  NA's 
    1   376 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            1           376 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    2    3 NA's 
   1    1    2  373 

patient.biospecimen_cqcf.normal_controls.normal_control.4.bcr_sample_uuid:
c0da7822-f933-44ef-8448-085707811077 c9c276c0-f8ae-441e-9b72-86222cb49955 
                                   1                                    1 
                                NA's 
                                 375 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_conc:
0.15 NA's 
   1  376 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_qm:
trinean    NA's 
      1     376 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_qty:
  15 NA's 
   1  376 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_vol:
 100 NA's 
   1  376 

patient.biospecimen_cqcf.normal_controls.normal_control.4.vial_number:
   3    4 NA's 
   1    1  375 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -344       0       0      37       0    2562      19 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0307   0.1300   0.1997  25.6343   0.3100 570.9000      267 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   15.00   20.00   19.31   20.00  138.80     267 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.27   64.25  100.00  130.86  150.00  539.37     267 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
               244                118                 15 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
liver other  NA's 
  140     6   231 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
          146           231 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2 NA's 
 368    8    1 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 no yes 
  1 376 

patient.biospecimen_cqcf.path_confirm_report_attached:
 no yes 
  1 376 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 no yes 
  1 376 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 no yes 
  1 376 

patient.biospecimen_cqcf.race:
american indian or alaska native                            asian 
                               1                              134 
       black or african american                            white 
                               9                              109 
                            NA's 
                             124 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                1 
                                                                                                                             NA's 
                                                                                                                              376 

patient.biospecimen_cqcf.sample_prescreened:
 no yes 
  1 376 

patient.biospecimen_cqcf.site_of_disease:
liver 
  377 

patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
 249   21  107 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
  28   79  270 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0    5   10 NA's 
   5    1    2  369 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   67.50   75.00   75.00   83.75   95.00     369 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
  50  200  230  336 NA's 
   3    3    1    1  369 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   4    4  369 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.bcr_sample_uuid:
9865a741-531c-452f-9264-ea0b8a6871a8 e31d2354-a883-4685-b750-f1738e07051d 
                                   1                                    1 
                                NA's 
                                 375 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_necrosis_percent:
   5   10 NA's 
   1    1  375 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_nuclei_percent:
  80   95 NA's 
   1    1  375 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_weight:
  50  200 NA's 
   1    1  375 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.vial_number:
   1    3 NA's 
   1    1  375 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   3.688   5.000  55.000       2 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  35.00   80.00   90.00   85.81   95.00  100.00       2 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0    50.0   200.0   304.8   280.0  2924.0       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1 NA's 
 376    1 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                     46                      20                      56 
               cryovial  other (please specify) 
                    152                     103 

patient.days_to_index:
   0 NA's 
 374    3 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and recurrent tumor(s)                                 NA's 
                                   2                                  375 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      7     157     519    1055    1694    6014       3 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   107.5   170.0   239.4   245.0  1620.0     325 

patient.samples.sample.2.is_ffpe:
 no 
377 

patient.samples.sample.2.oct_embedded:
false  true 
  360    17 

patient.samples.sample.2.pathology_report_file_name:
tcga-dd-aaca.c4260237-1e27-45c7-ade8-41939d1324a8.pdf 
                                                    1 
tcga-zs-a9cf.5d79d370-f174-4ccb-af64-5617ebf352be.pdf 
                                                    1 
                                                 NA's 
                                                  375 

patient.samples.sample.2.pathology_report_uuid:
5d79d370-f174-4ccb-af64-5617ebf352be c4260237-1e27-45c7-ade8-41939d1324a8 
                                   1                                    1 
                                NA's 
                                 375 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.340   1.805   1.830   1.811   1.870   1.960     342 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99021579 108433656 118955538 117945846 122967752 174164280       342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
  25   10  342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.1623  0.1700  0.1800     342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00   24.00   16.43   30.00   31.00     342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  35  342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   3.429   5.000   8.000     342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   7.314   8.500  12.000     342 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.340   3.400   3.313   4.140   4.540     342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
  35  342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 26.7 NA's 
  11   10   14  342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 2014 NA's 
  16   17    2  342 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99052761 122969234 153888574 143955473 167788814 174191352        85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   10   13 NA's 
  10  257   25   85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.5000  0.5000  0.4595  0.5000  0.5000      85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00    8.00   14.66   24.00   31.00      85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 292   85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.342   7.000  11.000      85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   6.325   8.000  12.000      85 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.95   40.00   40.00   35.56   40.00   40.00      85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
 292   85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
  10   25  257   85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
  59   40   53  140   85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  16.09   43.38   59.60   57.94   70.14  102.21     342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 257                   35                   85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  35  257   85 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.1623  0.1700  0.1800     342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 257  120 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
 257  120 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             292 
                                            NA's 
                                              85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 292   85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                       35                       257                        85 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.200   1.300   1.372   1.450   2.100     342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  5.300   7.400   7.800   7.863   8.400   9.600     342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     35     342 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  48    4  325 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100476909 100476967 122970660 117470997 132271851 167788790       343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
  34  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  34  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.00   11.00   12.88   23.25   29.00     343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  34  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   5.000   5.176   7.000  11.000     343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.750   7.000   6.412   7.750  10.000     343 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
  34  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
  34  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
  34  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 2014 NA's 
  16   17    1  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  34                  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
  34  343 



patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  34  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
  34  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              34 
                                            NA's 
                                             343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  34  343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     34     343 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
  32    2  343 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.640   1.870   1.940   1.935   1.995   2.170     323 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 99021223 122969967 160355524 149200388 167822096 174191135 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   2  10 
 57   2 318 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07000 0.07000 0.08000 0.09027 0.08000 0.22000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   24.00   26.00   23.16   28.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.586   5.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.255   8.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   2.800   3.200   2.777   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
377 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 6.6 6.67  6.7   10   40 
   2   38   17    2  318 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
  59   40  106  172 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97509067 108433694 118956972 120422461 122971467 174189635       253 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   10.00   17.29   26.00   26.00     253 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1400  0.1205  0.1600  0.1700     253 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    7.00   12.85   25.25   31.00     253 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 124  253 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   3.694   5.000   8.000     253 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   7.306   9.000  12.000     253 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.53    1.50    1.70    2.33    3.20    4.40     253 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 124  253 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
 6.6   10   13   40 NA's 
   4   59    4   57  253 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2011    2012    2012    2012    2015     253 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99052832 108477288 122967374 125944569 122971824 175156212       330 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    9   10   26 NA's 
  28    1   16    2  330 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0850  0.1500  0.1315  0.1600  0.1800     330 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00   11.00   13.17   23.50   31.00     330 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  47  330 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    3.00    4.17    7.00    8.00     330 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   5.596   7.000   9.000     330 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.530   1.760   3.200   3.385   4.270  13.600     330 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
  47  330 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.60   13.00   26.70   29.16   26.70  170.00     330 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 2014 2015 NA's 
  23   15    8    1  330 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113422178 113422284 167810867 147963674 175156142 175156187       368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   9   10 NA's 
   1    8  368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 0.08 0.09 NA's 
   2    6    1  368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   5   11   23   29 NA's 
   3    1    4    1  368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   9  368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2    3    7 NA's 
   1    1    7  368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    5 NA's 
   8    1  368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a16z a21k a45c a46d NA's 
   3    1    4    1  368 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.460   0.940   1.070   3.273   4.670   9.120     368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   9  368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.60   13.40   13.40   40.64   58.40  114.00     368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 2012 2014 2015 NA's 
   3    1    4    1  368 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-bc-a10s-11a-11d-a45c-10 tcga-bc-a10t-11a-11d-a45c-10 
                           1                            1 
tcga-bc-a10u-11a-11d-a45c-10                         NA's 
                           1                          374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
0d7ad8a3-a9b6-469c-bb9b-11826c59ba35 62e73a53-1342-4abe-98d7-79b482703ed7 
                                   1                                    1 
90aa13a2-e365-4101-ad05-897b258596bf                                 NA's 
                                   1                                  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
175155419 175156139 175156140      NA's 
        1         1         1       374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
  10 NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.08 NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  23 NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   7 NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   4 NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a45c NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   b    c    d NA's 
   1    1    1  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
1.07 NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
13.4 NA's 
   3  374 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2014 NA's 
   3  374 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97506924 122848612 160355272 149198549 167822070 174191519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   5 
318  57   2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1559  0.1600  0.2200 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   25.00   26.00   23.04   28.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.294   5.000  10.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    4.00    6.00    6.26    8.00   12.00 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.010   1.070   1.188   1.140   2.860 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
377 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7  10  13 
318  12  47 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
  59   40   96  182 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.90   13.52   18.60   27.88   29.60  208.42 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
377 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
377 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1559  0.1600  0.2200 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
377 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
377 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             377 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                              182                               195 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      323        54 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  48    4  325 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    7.00   19.00   16.38   24.00   30.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no 
377 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   5.448   9.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
  1   2  11  12  21 
324   1  46   4   2 

patient.samples.sample.2.portions.portion.portion_sequence:
   1    2 NA's 
 204    3  170 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  52  325 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.269   3.000  25.000     325 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1    2    3 NA's 
  44    6    1    1  325 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
  52  325 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    2 NA's 
  47    4    1  325 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   80.00   96.00   87.96  100.00  100.00     325 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.500   8.577  16.250  30.000     325 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0   80  100 NA's 
  50    1    1  325 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0   80 NA's 
  50    2  325 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
  52  325 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    2.00   10.48   30.00  200.00     195 

patient.samples.sample.2.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
  28   61   37   90  161 

patient.samples.sample.2.sample_type:
blood derived normal      recurrent tumor  solid tissue normal 
                 325                    2                   50 

patient.samples.sample.2.sample_type_id:
  2  10  11 
  2 325  50 

patient.samples.sample.2.vial_number:
  a   b   d 
365  11   1 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
                 germline comparison primary tumor and recurrent tumor(s) 
                                  13                                    1 
                                NA's 
                                 363 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     14     335    1521    1448    2184    3538     336 

patient.samples.sample.3.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   140.0   180.0   245.8   225.0  1140.0     337 

patient.samples.sample.3.is_ffpe:
  no NA's 
  41  336 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
   16    25   336 

patient.samples.sample.3.pathology_report_file_name:
tcga-dd-aaca.e3836223-b3ed-46c6-b257-6909e26d0f09.pdf 
                                                    1 
                                                 NA's 
                                                  376 

patient.samples.sample.3.pathology_report_uuid:
e3836223-b3ed-46c6-b257-6909e26d0f09                                 NA's 
                                   1                                  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.a260_a280_ratio:
1.85 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-dd-a116-11a-12r-a26b-07                         NA's 
                           1                          376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
f099f7fb-f07a-4529-83dc-e7023c52a999                                 NA's 
                                   1                                  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
132160644      NA's 
        1       376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   7 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.18 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  30 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   1 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   1 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
a26b NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   a NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
2.34 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
  13 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2013 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-dd-a116-11a-12r-a25g-13                         NA's 
                           1                          376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
f0452a08-12e7-4ea8-af29-3443ce1decbb                                 NA's 
                                   1                                  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
132178535      NA's 
        1       376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
  13 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.18 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
   2 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   1 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   1 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
a25g NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   c NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
 3.6 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  20 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.amount:
47.62  NA's 
    1   376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.analyte_type:
 rna NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.analyte_type_id:
   r NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.bcr_analyte_barcode:
tcga-dd-a116-11a-12r                 NA's 
                   1                  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.bcr_analyte_uuid:
618d37a5-40ab-45b6-bc24-a47451367141                                 NA's 
                                   1                                  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.concentration:
0.18 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        1                       376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
1.39 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.rna.rinvalue:
 8.6 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      1     376 

patient.samples.sample.3.portions.portion.2.analytes.analyte.subportion_sequence:
   2 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.bcr_portion_barcode:
tcga-dd-a116-11a-12                NA's 
                  1                 376 

patient.samples.sample.3.portions.portion.2.bcr_portion_uuid:
a1504b4f-219c-427d-8da3-6655244b0d0e                                 NA's 
                                   1                                  376 

patient.samples.sample.3.portions.portion.2.day_of_creation:
  16 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.is_ffpe:
  no NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.month_of_creation:
  11 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.portion_number:
  12 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.portion_sequence:
   1 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.bcr_slide_barcode:
tcga-dd-a116-11a-01-ts1                    NA's 
                      1                     376 

patient.samples.sample.3.portions.portion.2.slides.slide.bcr_slide_uuid:
459e75e9-20fe-4e3b-8a49-62ffe4d37991                                 NA's 
                                   1                                  376 

patient.samples.sample.3.portions.portion.2.slides.slide.image_file_name:
tcga-dd-a116-11a-01-ts1.459e75e9-20fe-4e3b-8a49-62ffe4d37991.svs 
                                                               1 
                                                            NA's 
                                                             376 

patient.samples.sample.3.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   1 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.percent_necrosis:
   0 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.percent_normal_cells:
  90 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.percent_stromal_cells:
  10 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.percent_tumor_cells:
   0 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.percent_tumor_nuclei:
   0 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.slides.slide.section_location:
 top NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.weight:
  30 NA's 
   1  376 

patient.samples.sample.3.portions.portion.2.year_of_creation:
2010 NA's 
   1  376 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.740   1.792   1.820   1.827   1.860   1.940     359 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99021576 108433568 108433596 113293230 112175168 174164279       359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
  16    2  359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1600  0.1650  0.1650  0.1775  0.1800     359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   8.222  10.000  30.000     359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  18  359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   1    3    5    7 NA's 
   1   11    5    1  359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.250   7.000   6.944   8.000   9.000     359 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.080   3.635   4.270   3.884   4.540   4.810     359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
  18  359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 26.7 NA's 
   3    2   13  359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 2014 NA's 
  14    3    1  359 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99052749 108433591 132260580 130056535 153887812 174191351       337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   10   13 NA's 
   2   22   16  337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1700  0.5000  0.3492  0.5000  0.5000     337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    8.00   12.05   24.00   30.00     337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  40  337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.475   7.000  10.000     337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    6.00    5.95    7.25    9.00     337 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.08    3.35   40.00   23.43   40.00   40.00     337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
  40  337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
   2   16   22  337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
  16    3   19    2  337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.17   44.22   55.16   59.57   78.09  107.82     359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                  22                   18                  337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  18   22  337 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1600  0.1650  0.1650  0.1775  0.1800     359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  22  355 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
  22  355 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              40 
                                            NA's 
                                             337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  40  337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                       18                        22                       337 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.900   1.200   1.300   1.357   1.400   2.400     359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.400   7.800   7.894   8.250   9.300     359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     18     359 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  33    6  338 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100476727 100476922 100476956 107078936 106948630 132274159       362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   3   11   24   29 NA's 
  11    2    1    1  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   2    5    7    8 NA's 
   1   11    2    1  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   5.000   6.000   6.533   6.000  12.000     362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a14s a17q a21j a237 NA's 
  11    1    1    2  362 


patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 NA's 
  12    3  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  15                  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
  15  362 



patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              15 
                                            NA's 
                                             362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     15     362 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
  12    3  362 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.760   1.930   1.950   1.972   2.050   2.170     360 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108428553 108434075 132214430 128857873 145634906 174189228       339 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1   10 NA's 
  14   24  339 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0800  0.1068  0.1475  0.1800     339 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00   25.50   18.05   30.00   30.00     339 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  38  339 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   3.711   4.000   7.000     339 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   6.000   5.868   6.000  12.000     339 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.130   2.800   2.352   3.200   3.600     339 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
  38  339 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7   40 NA's 
  13    1   24  339 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 NA's 
  14    3   19    2  339 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108428628 108433651 108433719 121209953 132160156 174189612       353 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5   10   26 NA's 
   1   14    9  353 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0900  0.1117  0.1500  0.1700     353 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    1.00   11.38   25.00   30.00     353 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  24  353 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   1    3    5    7 NA's 
   2   18    3    1  353 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    5.75    6.00    6.75    6.50   12.00     353 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   1.600   2.800   2.525   3.200   3.600     353 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
  24  353 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10   13   40 NA's 
   9    1   14  353 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 NA's 
  14    3    6    1  353 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108477220 108477232 108477258 109772985 108477296 122967404       362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5 NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1450  0.1600  0.1567  0.1700  0.1800     362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   1    5   24 NA's 
  13    1    1  362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1    3    7 NA's 
   1   13    1  362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   5.500   6.000   6.667   6.000  12.000     362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a132 a16x a20z NA's 
  13    1    1  362 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.080   3.740   4.010   3.883   4.540   4.810     362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
  15  362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  13 26.7 NA's 
   2   13  362 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 NA's 
  14    1  362 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
175156138 175156146 175156164 175156172 175156200 175156213       371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
  10 NA's 
   6  371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 0.08 0.09 NA's 
   2    3    1  371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  23 NA's 
   6  371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   6  371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   7 NA's 
   6  371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    4 NA's 
   4    2  371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a45c NA's 
   6  371 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
0.94 1.07 1.21 NA's 
   2    3    1  371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   6  371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4 NA's 
   6  371 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2014 NA's 
   6  371 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99052819 108433482 132159521 126869860 145634911 174191148       336 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    5 NA's 
  24   14    3  336 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1578  0.1700  0.1800     336 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00   25.00   17.49   30.00   30.00     336 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  41  336 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   3.805   5.000   7.000     336 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   6.000   5.951   6.000  12.000     336 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.070   1.140   1.631   1.950   4.810     336 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
  41  336 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7   10   13 26.7 NA's 
  24    1   13    3  336 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
  17    3   19    2  336 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.40   22.68   26.65   29.52   31.11   63.05     336 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  41  336 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  41  336 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1578  0.1700  0.1800     336 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  41  336 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
  41  336 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              41 
                                            NA's 
                                             336 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  41  336 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                   41                   336 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       24        17       336 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  34    6  337 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   14.00   14.29   20.00   31.00     336 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  41  336 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000  11.000   7.171  11.000  12.000     336 

patient.samples.sample.3.portions.portion.portion_number:
   1   11   12 NA's 
   1   34    6  336 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
  39  338 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  40  337 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     1.0     3.0     3.8     5.0    14.0     337 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     0.0     0.7     1.0     6.0     337 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
  40  337 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    0.55    0.00    6.00     337 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    80.0    90.0    87.3   100.0   100.0     337 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    10.0    10.7    20.0    40.0     337 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0   80 NA's 
  39    1  337 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0   80 NA's 
  39    1  337 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
  40  337 

patient.samples.sample.3.portions.portion.weight:
   2   10   30 NA's 
   1    1   39  336 

patient.samples.sample.3.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2014     336 

patient.samples.sample.3.sample_type:
blood derived normal      recurrent tumor  solid tissue normal 
                   1                    1                   39 
                NA's 
                 336 

patient.samples.sample.3.sample_type_id:
   2   10   11 NA's 
   1    1   39  336 

patient.samples.sample.3.vial_number:
   a    b NA's 
  40    1  336 

patient.samples.sample.4.bcr_sample_barcode:
tcga-dd-aaca-10a             NA's 
               1              376 

patient.samples.sample.4.bcr_sample_uuid:
370388d3-8c31-4a40-9599-5eac9a502e01                                 NA's 
                                   1                                  376 

patient.samples.sample.4.days_to_collection:
2057 NA's 
   1  376 

patient.samples.sample.4.is_ffpe:
  no NA's 
   1  376 

patient.samples.sample.4.oct_embedded:
false  NA's 
    1   376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-dd-aaca-10a-01d-a40u-10                         NA's 
                           1                          376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
99678528-a049-4ed5-9eb9-d70563f8d53f                                 NA's 
                                   1                                  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
174191087      NA's 
        1       376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  10 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  28 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   5 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   1 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a40u NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   e NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 2.8 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  40 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-dd-aaca-10a-01d-a40t-01                         NA's 
                           1                          376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
51717965-6d92-4062-bb9f-d0b8e2416ea5                                 NA's 
                                   1                                  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
174191471      NA's 
        1       376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  28 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   5 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   1 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a40t NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   e NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
13.72  NA's 
    1   376 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-dd-aaca-10a-01d                 NA's 
                   1                  376 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid:
0cd660db-f280-423d-aea3-27d5a137f47c                                 NA's 
                                   1                                  376 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
0.14 NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             376 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   1  376 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
pre-extracted dna received by tss                              NA's 
                                1                               376 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        1       376 

patient.samples.sample.4.portions.portion.bcr_portion_barcode:
tcga-dd-aaca-10a-01                NA's 
                  1                 376 

patient.samples.sample.4.portions.portion.bcr_portion_uuid:
0332dc9d-59b8-4e2a-a84d-eb311ef49a6d                                 NA's 
                                   1                                  376 

patient.samples.sample.4.portions.portion.day_of_creation:
  19 NA's 
   1  376 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   1  376 

patient.samples.sample.4.portions.portion.month_of_creation:
   3 NA's 
   1  376 

patient.samples.sample.4.portions.portion.portion_number:
   1 NA's 
   1  376 

patient.samples.sample.4.portions.portion.year_of_creation:
2014 NA's 
   1  376 

patient.samples.sample.4.sample_type:
blood derived normal                 NA's 
                   1                  376 

patient.samples.sample.4.sample_type_id:
  10 NA's 
   1  376 

patient.samples.sample.4.vial_number:
   a NA's 
   1  376 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.0   155.5   519.0  1054.4  1693.5  6014.0       3 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10:
bf1b9375-342f-4e13-b6dd-5f9014d6b494                                 NA's 
                                   1                                  376 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
4fb46d36-e67a-4f82-b468-d4ad35c0018c 51970b1c-986d-4e91-a5cc-ac040a881f94 
                                   1                                    1 
                                NA's 
                                 375 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.4:
8bb3b05f-d945-4854-8760-dd160e7cf5ac                                 NA's 
                                   1                                  376 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.5:
871a8acd-a955-41c7-8ebc-e9b3f548141b                                 NA's 
                                   1                                  376 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
b0bf2c3a-8a5b-4615-b4a5-cacecdd92ae9                                 NA's 
                                   1                                  376 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
c7821747-7d73-4273-8a55-9d9a5c55c9e6                                 NA's 
                                   1                                  376 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.8:
fa5838e7-a023-4335-b68b-01b6a3049628                                 NA's 
                                   1                                  376 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
b308f2e4-0f6b-4c4e-b8f7-d3de1e476408                                 NA's 
                                   1                                  376 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   10.0    70.0   160.0   270.8   300.0  2190.0 

patient.samples.sample.is_ffpe:
 no 
377 

patient.samples.sample.oct_embedded:
false  true 
  101   276 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.66    1.83    1.86    1.86    1.89    2.15       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99015792 122967831 160339188 148396220 167788086 174164351         1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
 174  202    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.140   0.160   0.170   0.168   0.170   0.180       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   24.00   26.00   23.35   28.00   31.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 376    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    4.00    4.58    5.00   12.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.763   9.000  12.000       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.308   3.200   3.079   3.400   4.810       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
 376    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 26.7 NA's 
 130  202   44    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 NA's 
  58   40  106  172    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-dd-aacm-01a-11r-a466-07 tcga-dd-aade-01a-11r-a466-07 
                           1                            1 
tcga-dd-aae8-01a-11r-a466-07 tcga-g3-a25w-01a-11r-a466-07 
                           1                            1 
                        NA's 
                         373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
218cd2c9-2cc9-4768-b12a-781d702ecc8c b5fd76f5-8ec0-42e9-a4b4-b90c7ba29f8c 
                                   1                                    1 
c7939e09-181c-4b9b-b6b1-92490e43946a fb84f0b6-1ea0-45fb-b23f-138bde4ec423 
                                   1                                    1 
                                NA's 
                                 373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
167812944 167812945 167812946 167812965      NA's 
        1         1         1         1       373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   4  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.17 0.18 NA's 
   2    2  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   9 NA's 
   4  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   4  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  12 NA's 
   4  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1    2    3 NA's 
   1    1    2  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a466 NA's 
   4  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   c    d NA's 
   1    3  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
1.12 1.19 NA's 
   2    2  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   4  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6 NA's 
   4  373 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   4  373 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 99052748 122969249 160339667 148887238 167787985 174191423 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  7  10  13 
202   1 174 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1400  0.1600  0.1700  0.1688  0.1700  0.5000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   24.00   26.00   23.34   28.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.584   5.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.751   9.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   2.210   2.340   2.824   3.400  40.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
 23 
377 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
 13  20  80 
202 174   1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
  59   40  106  172 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.88   45.68   64.16   63.69   79.78  154.41       1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna 
                   1                  376 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
  r   w 
376   1 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.140   0.160   0.170   0.168   0.170   0.180       1 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   1  376 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   1  376 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             377 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                        2                       374                         1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   1.380   1.590   1.617   1.790  13.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.300   8.000   8.600   8.493   9.100  10.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    376       1 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 337   27    1   12 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100476721 122970884 153889296 144431502 167788717 167803191        87 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
 290   87 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 290   87 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    8.00    8.00   14.21   24.00   29.00      87 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 290   87 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   5.552   7.000  11.000      87 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.731   3.000   9.000      87 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 290   87 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
 290   87 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 290   87 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
  58   40   53  139   87 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                 290                   87 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
 290   87 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 290   87 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
 290   87 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             290 
                                            NA's 
                                              87 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 290   87 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
    290      87 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 252   25    1   99 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.450   1.863   1.930   1.917   1.980   2.140     323 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 99021638 122967437 160340715 149062069 167822072 174190669 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   2  10 
 55   4 318 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07000 0.08000 0.08000 0.09164 0.08000 0.18000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      1      24      26      23      28      30 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.592   5.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.499   4.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   2.800   3.200   2.831   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
377 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7   10   40 
  38   17    4  318 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
  59   40  106  172 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97506928 122969448 160356256 148832045 167788178 174190438 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  1   5  10  26 
  4 282  55  36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1500  0.1500  0.1454  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0    16.0    26.0    21.4    28.0    31.0 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   4.743   5.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.499   4.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.990   1.950   2.080   2.152   2.210   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
 23 
377 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
6.6  10  13  40 
  4  36 282  55 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 
  59   40  106  172 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99052778 108477279 122967384 119359382 122971446 167814094       253 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   5.000   9.468   9.000  26.000     253 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.070   0.150   0.160   0.153   0.160   0.180     253 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00   24.00   15.81   25.00   31.00     253 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 124  253 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   3.371   5.000   9.000     253 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    2.00    2.29    3.00    5.00     253 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.460   1.820   2.080   2.752   4.270   4.810     253 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 124  253 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.60   13.00   13.00   18.12   26.70   40.00     253 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2011    2012    2012    2012    2015     253 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113422285 167810844 175156131 161451537 175156135 175156214       360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   9   10 NA's 
   2   15  360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 0.08 0.09 NA's 
   3   13    1  360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   5   11   23   29 NA's 
   1    4   11    1  360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  17  360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2    3    4    7 NA's 
   1    2    2   12  360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    2    3 NA's 
   6    4    7  360 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.460   1.070   1.070   2.727   1.070  11.520     360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
  17  360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.6    13.4    13.4    35.8    13.4   144.0     360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 2012 2014 2015 NA's 
   1    3   11    2  360 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-cc-5262-01a-01d-a45c-10 tcga-dd-a11b-01a-11d-a45c-10 
                           1                            1 
                        NA's 
                         375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
def07149-7156-43a0-b57e-1d4a6ce668bf f9ad928a-a5ad-42bc-b50d-8f2bad874fd2 
                                   1                                    1 
                                NA's 
                                 375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
175156136 175156186      NA's 
        1         1       375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
  10 NA's 
   2  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 0.08 NA's 
   1    1  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  23 NA's 
   2  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   2  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   7 NA's 
   2  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   1    2 NA's 
   1    1  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a45c NA's 
   2  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   a    f NA's 
   1    1  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
0.94 1.07 NA's 
   1    1  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   2  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
13.4 NA's 
   2  375 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2014 NA's 
   2  375 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97508891 122848291 160355288 149199803 167822066 174191231 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   5 
318  55   4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1572  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0    25.0    26.0    23.2    28.0    31.0 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.289   5.000  10.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.562   4.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.94    1.01    1.07    1.20    1.14    2.34 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
377 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7  10  13 
318  10  49 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
  59   40   96  182 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.86   23.08   31.94   36.01   42.85  149.78 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
377 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
377 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1572  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
377 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
377 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             377 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  377 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      323        54 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 337   27    1   12 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    6.00   12.00   13.52   22.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
377 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    2.00    4.00    5.82    9.00   12.00 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0    11.0    11.0    11.5    11.0    51.0 

patient.samples.sample.portions.portion.portion_sequence:
   1    2    3    5 NA's 
 201   10    2    1  163 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  23  354 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   1.826   2.000  10.000     354 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0    1    2 NA's 
  19    2    2  354 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.956   2.500  20.000     354 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0    2 NA's 
  22    1  354 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   0    4    5 NA's 
  21    1    1  354 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.50   10.00   11.83   15.00   40.00     354 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   80.00   90.00   85.83   93.50  100.00     354 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0    82.5    90.0    86.3    94.0    97.0     354 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
     1     22    354 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
377 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   2.819   3.000 100.000       6 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   0.407   1.000  10.000       6 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.355   1.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4636  1.0000  4.0000       6 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.9363  0.0000 50.0000 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    8.00   10.47   15.00   62.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  30.00   80.00   90.00   87.23  100.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  45.00   80.00   85.00   84.47   95.00  100.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
    28    349 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   10.0    30.0    30.0    28.2    30.0    56.0 

patient.samples.sample.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
  28   63   35   95  156 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 184  193 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 184  193 

patient.samples.sample.portions.shipment_portion.plate_id:
a40l a40m a40n NA's 
  81   81   22  193 

patient.samples.sample.portions.shipment_portion.portion_number:
  11   21   41 NA's 
   7  176    1  193 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    4 NA's 
   7  168    1  201 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   4 NA's 
 184  193 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   6 NA's 
 184  193 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
 184  193 

patient.samples.sample.sample_type:
primary tumor 
          377 

patient.samples.sample.sample_type_id:
  1 
377 

patient.samples.sample.vial_number:
  a   b 
375   2 

}}
\keyword{datasets}
