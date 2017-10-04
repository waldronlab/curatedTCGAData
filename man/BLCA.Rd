\name{ BLCA }
\alias{ BLCA }
\docType{data}
\title{ Bladder Urothelial Carcinoma }

\details{
\preformatted{
> experiments( BLCA )
ExperimentList class object of length 11: 
 [1] CNASeq-20160128: RaggedExperiment with 35692 rows and 232 columns 
 [2] CNASNP-20160128: RaggedExperiment with 479978 rows and 806 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 104349 rows and 797 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 408 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 408 columns 
 [6] Methylation-20160128: SummarizedExperiment with 485577 rows and 434 columns 
 [7] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 429 columns 
 [8] Mutation-20160128: RaggedExperiment with 39312 rows and 130 columns 
 [9] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 427 columns 
 [10] RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 67 columns 
 [11] RPPAArray-20160128: SummarizedExperiment with 208 rows and 344 columns 

> rownames( BLCA )
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

> colnames( BLCA )
CharacterList of length 11
[["CNASeq-20160128"]] TCGA-BL-A0C8-01A-11D-A10R-02 ...
[["CNASNP-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["CNVSNP-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-2F-A9KO-01A-11D-A38F-01 ...
[["Methylation-20160128"]] TCGA-2F-A9KO-01A-11D-A38H-05 ...
[["miRNASeqGene-20160128"]] TCGA-2F-A9KO-01A-11R-A38M-13 ...
[["Mutation-20160128"]] TCGA-BL-A0C8-01A-11D-A10S-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-2F-A9KO-01A-11R-A38B-07 ...
[["RNASeqGene-20160128"]] TCGA-BL-A0C8-01A-11R-A10U-07 ...
...
<1 more element>

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128    1 Mb
2                    CNASNP-20160128   13 Mb
3                    CNVSNP-20160128    3 Mb
4          GISTIC_AllByGene-20160128  4.4 Mb
5  GISTIC_ThresholdedByGene-20160128  4.4 Mb
6               Methylation-20160128 67.5 Mb
7              miRNASeqGene-20160128  0.1 Mb
8                  Mutation-20160128 53.5 Mb
9           RNASeq2GeneNorm-20160128  1.2 Mb
10               RNASeqGene-20160128  1.1 Mb
11                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   232 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
180.000 180.000   1.125   0.992   1.332 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  34.00   60.00   69.00   68.08   76.00   90.00       1 

vital_status:
  0   1 
230 182 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   19.0   235.8   410.5   552.8   654.5  3183.0     232 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -64.0   398.2   639.0  1016.7  1458.8  5050.0     182 

tumor_tissue_site:
bladder 
    412 

pathologic_stage:
  stage i  stage ii stage iii  stage iv      NA's 
        2       131       141       136         2 



pathology_M_stage:
  m0   m1   mx NA's 
 196   11  202    3 

gender:
female   male 
   108    304 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1999    2009    2011    2010    2012    2013      18 

radiation_therapy:
  no  yes NA's 
 366   20   26 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   80.00   90.00   83.09   90.00  100.00     276 

histological_type:
muscle invasive urothelial carcinoma (pt2 or above) 
                                                409 
                                               NA's 
                                                  3 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.15   20.00   30.00   39.04   50.00  730.00     188 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.088   2.000  97.000     115 

race:
                    asian black or african american                     white 
                       44                        23                       327 
                     NA's 
                       18 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     9                    371                     32 

admin.bcr:
nationwide children's hospital 
                           412 

admin.day_of_dcc_upload:
  1 
412 

admin.disease_code:
blca 
 412 


admin.month_of_dcc_upload:
 10 
412 

admin.patient_withdrawal.withdrawn:
false 
  412 

admin.project_code:
tcga 
 412 

admin.year_of_dcc_upload:
2015 
 412 

patient.additional_studies.additional_study.disease_code:
fppp NA's 
   3  409 

patient.additional_studies.additional_study.project_code:
tcga NA's 
   3  409 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   34.0    60.0    69.0    68.1    76.0    90.0 

patient.age_began_smoking_in_years:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    16.0    20.0    32.9    25.0  1966.0     237 




patient.anatomic_neoplasm_subdivision.4:
 wall anterior   wall lateral wall posterior           NA's 
             2              6              9            395 

patient.anatomic_neoplasm_subdivision.5:
  wall lateral wall posterior           NA's 
             2              4            406 

patient.anatomic_neoplasm_subdivision.6:
wall posterior           NA's 
             2            410 


patient.bladder_carcinoma_extracapsular_extension_status:
extensive     focal      NA's 
       17        37       358 


patient.blood_relative_cancer_history_list.blood_relative_cancer_history.2.cancer_diagnosis_cancer_type_icd9_text_name.2:
lungbronchus         NA's 
           1          411 



patient.blood_relative_cancer_history_list.blood_relative_cancer_history.3.cancer_diagnosis_cancer_type_icd9_text_name.2:
multiple myeloma             NA's 
               1              411 


patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.cancer_diagnosis_cancer_type_icd9_text_name:
  breast-fem lungbronchus         NA's 
           1            3          408 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.cancer_diagnosis_cancer_type_icd9_text_name.2:
melanoma     NA's 
       1      411 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.family_medical_history_relative_family_member_relationship_type:
brother   child    NA's 
      2       1     409 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.5.cancer_diagnosis_cancer_type_icd9_text_name:
prostate     NA's 
       1      411 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.5.family_medical_history_relative_family_member_relationship_type:
brother    NA's 
      1     411 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.6.cancer_diagnosis_cancer_type_icd9_text_name:
breast-fem       NA's 
         1        411 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.6.family_medical_history_relative_family_member_relationship_type:
child  NA's 
    1   411 


patient.blood_relative_cancer_history_list.blood_relative_cancer_history.cancer_diagnosis_cancer_type_icd9_text_name.2:
  breast-fem lungbronchus      uterine         NA's 
           1            1            1          409 



patient.clinical_cqcf.anatomic_neoplasm_subdivision:
   dome    neck trigone    NA's 
     13       2      30     367 

patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      391        21 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-533.00    4.00   30.00  112.09   64.25 3891.00      40 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  149.0   251.0   393.0   428.8   579.0   835.0     391 

patient.clinical_cqcf.diagnosis_subtype:
non-papillary     papillary          NA's 
          224           110            78 

patient.clinical_cqcf.histological_type:
muscle invasive urothelial carcinoma (pt2 or above) 
                                                410 
                                               NA's 
                                                  2 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             402 
yes, pharmaceutical treatment prior to resection 
                                              10 

patient.clinical_cqcf.normal_tissue_anatomic_site:
bladder; macroscopic urothelial tissue                                  other 
                                    16                                     10 
                         smooth muscle                                   NA's 
                                    33                                    353 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           58           354 

patient.clinical_cqcf.tumor_type:
tumor specimen from de novo untreated malignancy of the bladder 
                                                            411 
                                                           NA's 
                                                              1 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    7.00   14.50   14.61   21.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32918  -27942  -25259  -25037  -22208  -12557       1 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   19.0   179.8   342.0   508.2   594.5  2828.0     304 

patient.days_to_initial_pathologic_diagnosis:
  0 
412 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -64.0    86.0   238.0   627.5   790.0  4684.0     109 

patient.diagnosis_age:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  48.00   61.50   74.00   70.05   78.50   90.00     392 

patient.diagnosis_subtype:
non-papillary     papillary          NA's 
          274           133             5 

patient.disease_extracapsular_extension_ind.3:
  no  yes NA's 
  89   73  250 

patient.drugs.drug.10.bcr_drug_barcode:
tcga-xf-aan0-d62524                NA's 
                  1                 411 

patient.drugs.drug.10.bcr_drug_uuid:
f0ff819c-fa2c-4e7b-a1ca-8cd498bd3371                                 NA's 
                                   1                                  411 

patient.drugs.drug.10.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.10.days_to_drug_therapy_end:
1318 NA's 
   1  411 

patient.drugs.drug.10.days_to_drug_therapy_start:
1265 NA's 
   1  411 

patient.drugs.drug.10.drug_name:
adriamycin (doxorubicin)                     NA's 
                       1                      411 

patient.drugs.drug.10.measure_of_response:
partial response             NA's 
               1              411 

patient.drugs.drug.10.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.10.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.10.tx_on_clinical_trial:
  no NA's 
   1  411 

patient.drugs.drug.10.year_of_form_completion:
2014 NA's 
   1  411 

patient.drugs.drug.11.bcr_drug_barcode:
tcga-xf-aan0-d62525                NA's 
                  1                 411 

patient.drugs.drug.11.bcr_drug_uuid:
2a7e98ea-34ff-45bb-bb07-d816ef52d85b                                 NA's 
                                   1                                  411 

patient.drugs.drug.11.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.11.days_to_drug_therapy_end:
1318 NA's 
   1  411 

patient.drugs.drug.11.days_to_drug_therapy_start:
1265 NA's 
   1  411 

patient.drugs.drug.11.drug_name:
gemcitabine        NA's 
          1         411 

patient.drugs.drug.11.measure_of_response:
partial response             NA's 
               1              411 

patient.drugs.drug.11.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.11.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.11.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.11.tx_on_clinical_trial:
  no NA's 
   1  411 

patient.drugs.drug.11.year_of_form_completion:
2014 NA's 
   1  411 

patient.drugs.drug.12.bcr_drug_barcode:
tcga-xf-aan0-d62526                NA's 
                  1                 411 

patient.drugs.drug.12.bcr_drug_uuid:
d1bb434a-f94e-4706-a788-71d092772679                                 NA's 
                                   1                                  411 

patient.drugs.drug.12.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.12.days_to_drug_therapy_end:
1486 NA's 
   1  411 

patient.drugs.drug.12.days_to_drug_therapy_start:
1423 NA's 
   1  411 

patient.drugs.drug.12.drug_name:
gemcitabine        NA's 
          1         411 

patient.drugs.drug.12.measure_of_response:
partial response             NA's 
               1              411 

patient.drugs.drug.12.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.12.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.12.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.12.tx_on_clinical_trial:
  no NA's 
   1  411 

patient.drugs.drug.12.year_of_form_completion:
2014 NA's 
   1  411 

patient.drugs.drug.13.bcr_drug_barcode:
tcga-xf-aan0-d62527                NA's 
                  1                 411 

patient.drugs.drug.13.bcr_drug_uuid:
4e2ad69a-535f-4040-b362-1fe4451470f1                                 NA's 
                                   1                                  411 

patient.drugs.drug.13.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.13.days_to_drug_therapy_end:
1486 NA's 
   1  411 

patient.drugs.drug.13.days_to_drug_therapy_start:
1465 NA's 
   1  411 

patient.drugs.drug.13.drug_name:
adriamycin (doxorubicin)                     NA's 
                       1                      411 

patient.drugs.drug.13.measure_of_response:
partial response             NA's 
               1              411 

patient.drugs.drug.13.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.13.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.13.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.13.tx_on_clinical_trial:
  no NA's 
   1  411 

patient.drugs.drug.13.year_of_form_completion:
2014 NA's 
   1  411 

patient.drugs.drug.14.bcr_drug_barcode:
tcga-xf-aan0-d62528                NA's 
                  1                 411 

patient.drugs.drug.14.bcr_drug_uuid:
bf52cad0-17af-465d-b6b0-3e4368d6a4fd                                 NA's 
                                   1                                  411 

patient.drugs.drug.14.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.14.days_to_drug_therapy_end:
1598 NA's 
   1  411 

patient.drugs.drug.14.days_to_drug_therapy_start:
1598 NA's 
   1  411 

patient.drugs.drug.14.drug_name:
gemcitabine        NA's 
          1         411 

patient.drugs.drug.14.measure_of_response:
clinical progressive disease                         NA's 
                           1                          411 

patient.drugs.drug.14.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.14.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.14.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.14.tx_on_clinical_trial:
  no NA's 
   1  411 

patient.drugs.drug.14.year_of_form_completion:
2014 NA's 
   1  411 

patient.drugs.drug.15.bcr_drug_barcode:
tcga-xf-aan0-d62529                NA's 
                  1                 411 

patient.drugs.drug.15.bcr_drug_uuid:
226b95ea-5675-4ba5-a592-3fd8a86e53c5                                 NA's 
                                   1                                  411 

patient.drugs.drug.15.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.15.days_to_drug_therapy_end:
1598 NA's 
   1  411 

patient.drugs.drug.15.days_to_drug_therapy_start:
1598 NA's 
   1  411 

patient.drugs.drug.15.drug_name:
adriamycin (doxorubicin)                     NA's 
                       1                      411 

patient.drugs.drug.15.measure_of_response:
clinical progressive disease                         NA's 
                           1                          411 

patient.drugs.drug.15.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.15.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.15.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.15.tx_on_clinical_trial:
  no NA's 
   1  411 

patient.drugs.drug.15.year_of_form_completion:
2014 NA's 
   1  411 

patient.drugs.drug.16.bcr_drug_barcode:
tcga-xf-aan0-d62530                NA's 
                  1                 411 

patient.drugs.drug.16.bcr_drug_uuid:
1e22652b-7f6c-469d-bfc7-4ae593aaeea0                                 NA's 
                                   1                                  411 

patient.drugs.drug.16.clinical_trail_drug_classification:
mtor inhibitor           NA's 
             1            411 

patient.drugs.drug.16.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.16.days_to_drug_therapy_end:
1703 NA's 
   1  411 

patient.drugs.drug.16.days_to_drug_therapy_start:
1703 NA's 
   1  411 

patient.drugs.drug.16.drug_name:
temsirolimus         NA's 
           1          411 

patient.drugs.drug.16.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.16.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.16.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.16.tx_on_clinical_trial:
 yes NA's 
   1  411 

patient.drugs.drug.16.year_of_form_completion:
2014 NA's 
   1  411 

patient.drugs.drug.17.bcr_drug_barcode:
tcga-xf-aan0-d62531                NA's 
                  1                 411 

patient.drugs.drug.17.bcr_drug_uuid:
6b5260d5-8811-491d-a355-746ec69dd098                                 NA's 
                                   1                                  411 

patient.drugs.drug.17.clinical_trail_drug_classification:
anti-mitotic, vinca alkaloid                         NA's 
                           1                          411 

patient.drugs.drug.17.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.17.days_to_drug_therapy_end:
1703 NA's 
   1  411 

patient.drugs.drug.17.days_to_drug_therapy_start:
1703 NA's 
   1  411 

patient.drugs.drug.17.drug_name:
vinorelbine        NA's 
          1         411 

patient.drugs.drug.17.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.17.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.17.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.17.tx_on_clinical_trial:
 yes NA's 
   1  411 

patient.drugs.drug.17.year_of_form_completion:
2014 NA's 
   1  411 



patient.drugs.drug.2.clinical_trail_drug_classification:
chemoprotectant            NA's 
              1             411 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   16.00   15.46   21.00   31.00     306 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   138.0   177.0   279.8   276.5  2543.0     317 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    67.0   121.0   206.1   170.0  2500.0     309 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                          27                           38 
            partial response               stable disease 
                          13                            9 
                        NA's 
                         325 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   7.028   9.000  12.000     306 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  97    8  307 

patient.drugs.drug.2.therapy_types.therapy_type:
    ancillary  chemotherapy immunotherapy          NA's 
            1           101             2           308 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  94   11  307 

patient.drugs.drug.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
  35   16   38   17  306 



patient.drugs.drug.3.clinical_trail_drug_classification:
platinum-based antineoplastic                          NA's 
                            1                           411 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   17.00   15.33   20.75   28.00     382 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   38.0   130.2   184.0   361.1   335.2  2543.0     384 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    58.0   142.0   329.7   327.0  2500.0     383 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                          10                            6 
            partial response               stable disease 
                           7                            3 
                        NA's 
                         386 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   7.000   7.367   8.750  12.000     382 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  28    2  382 

patient.drugs.drug.3.therapy_types.therapy_type:
chemotherapy         NA's 
          30          382 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  24    6  382 

patient.drugs.drug.3.year_of_form_completion:
2012 2013 2014 2015 NA's 
  11    5   13    1  382 



patient.drugs.drug.4.clinical_trail_drug_classification:
anti-tubulin agent               NA's 
                 1                411 

patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   16.00   13.81   19.00   28.00     391 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   87.75  157.50  327.10  333.75 1782.00     392 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   50.75  124.00  282.55  287.25 1759.00     392 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                           8                            2 
            partial response               stable disease 
                           4                            4 
                        NA's 
                         394 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   7.000   7.095   8.000  12.000     391 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
  20    1  391 

patient.drugs.drug.4.therapy_types.therapy_type:
chemotherapy         NA's 
          20          392 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
  14    7  391 

patient.drugs.drug.4.year_of_form_completion:
2012 2013 2014 2015 NA's 
   9    1   10    1  391 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   13.00   17.00   15.80   19.75   28.00     402 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   119.5   284.5   462.0   431.8  1782.0     402 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    89.0   204.5   410.5   414.8  1759.0     402 

patient.drugs.drug.5.drug_name:
  cisplatin doxorubicin  ifosfamide       mesna        NA's 
          6           2           1           1         402 

patient.drugs.drug.5.measure_of_response:
clinical progressive disease            complete response 
                           6                            1 
            partial response               stable disease 
                           2                            1 
                        NA's 
                         402 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     6.0     7.0     6.8     7.0    11.0     402 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
  10  402 

patient.drugs.drug.5.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           1            9          402 

patient.drugs.drug.5.tx_on_clinical_trial:
  no  yes NA's 
   9    1  402 

patient.drugs.drug.5.year_of_form_completion:
2012 2013 2014 NA's 
   3    1    6  402 

patient.drugs.drug.6.bcr_drug_barcode:
tcga-fd-a3sj-d33648 tcga-g2-a3ie-d36980 tcga-xf-aan0-d62520 tcga-xf-aan7-d62549 
                  1                   1                   1                   1 
               NA's 
                408 

patient.drugs.drug.6.bcr_drug_uuid:
0dbb730f-938a-4675-a8de-8575aca21c6a 10d93d30-4ac6-484f-8772-54a424c9799b 
                                   1                                    1 
535127d6-b422-4608-a7d1-1b0ed7d63611 e6e275ff-bdea-4f7a-b8da-3579d9a765f8 
                                   1                                    1 
                                NA's 
                                 408 

patient.drugs.drug.6.clinical_trail_drug_classification:
blinded: zactima or placebo. zactima is a kinase inhibitor 
                                                         1 
                                                      NA's 
                                                       411 

patient.drugs.drug.6.day_of_form_completion:
  16   19   20   21 NA's 
   1    1    1    1  408 

patient.drugs.drug.6.days_to_drug_therapy_end:
 100  275  442 1052 NA's 
   1    1    1    1  408 

patient.drugs.drug.6.days_to_drug_therapy_start:
  81  274  399  898 NA's 
   1    1    1    1  408 

patient.drugs.drug.6.drug_name:
docetaxel +/- zactima          methotrexate                 taxol 
                    1                     1                     1 
          vinblastine                  NA's 
                    1                   408 

patient.drugs.drug.6.measure_of_response:
clinical progressive disease             partial response 
                           2                            1 
              stable disease                         NA's 
                           1                          408 

patient.drugs.drug.6.month_of_form_completion:
   7   11 NA's 
   3    1  408 

patient.drugs.drug.6.therapy_ongoing:
  no NA's 
   4  408 

patient.drugs.drug.6.therapy_types.therapy_type:
chemotherapy         NA's 
           4          408 

patient.drugs.drug.6.tx_on_clinical_trial:
  no  yes NA's 
   3    1  408 

patient.drugs.drug.6.year_of_form_completion:
2012 2014 NA's 
   2    2  408 

patient.drugs.drug.7.bcr_drug_barcode:
tcga-g2-a3ie-d36983 tcga-xf-aan0-d62521                NA's 
                  1                   1                 410 

patient.drugs.drug.7.bcr_drug_uuid:
60fd150c-836a-4098-905f-cb879f56df75 70591683-fcf3-48e0-8e19-6d1559b9419d 
                                   1                                    1 
                                NA's 
                                 410 

patient.drugs.drug.7.day_of_form_completion:
  19   20 NA's 
   1    1  410 

patient.drugs.drug.7.days_to_drug_therapy_end:
 100 1052 NA's 
   1    1  410 

patient.drugs.drug.7.days_to_drug_therapy_start:
  81  898 NA's 
   1    1  410 

patient.drugs.drug.7.drug_name:
cisplatin      NA's 
        2       410 

patient.drugs.drug.7.measure_of_response:
clinical progressive disease             partial response 
                           1                            1 
                        NA's 
                         410 

patient.drugs.drug.7.month_of_form_completion:
   7   11 NA's 
   1    1  410 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   2  410 

patient.drugs.drug.7.therapy_types.therapy_type:
chemotherapy         NA's 
           2          410 

patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   2  410 

patient.drugs.drug.7.year_of_form_completion:
2012 2014 NA's 
   1    1  410 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-xf-aan0-d62522                NA's 
                  1                 411 

patient.drugs.drug.8.bcr_drug_uuid:
ba0fac90-b83b-4b17-9c03-35c0198a3273                                 NA's 
                                   1                                  411 

patient.drugs.drug.8.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.8.days_to_drug_therapy_end:
1136 NA's 
   1  411 

patient.drugs.drug.8.days_to_drug_therapy_start:
1094 NA's 
   1  411 

patient.drugs.drug.8.drug_name:
adriamycin (doxorubicin)                     NA's 
                       1                      411 

patient.drugs.drug.8.measure_of_response:
partial response             NA's 
               1              411 

patient.drugs.drug.8.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.8.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.8.tx_on_clinical_trial:
  no NA's 
   1  411 

patient.drugs.drug.8.year_of_form_completion:
2014 NA's 
   1  411 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-xf-aan0-d62523                NA's 
                  1                 411 

patient.drugs.drug.9.bcr_drug_uuid:
bb3fecdf-1f45-42b9-bc18-242ab337cfe0                                 NA's 
                                   1                                  411 

patient.drugs.drug.9.day_of_form_completion:
  19 NA's 
   1  411 

patient.drugs.drug.9.days_to_drug_therapy_end:
1136 NA's 
   1  411 

patient.drugs.drug.9.days_to_drug_therapy_start:
1094 NA's 
   1  411 

patient.drugs.drug.9.drug_name:
gemcitabine        NA's 
          1         411 

patient.drugs.drug.9.measure_of_response:
partial response             NA's 
               1              411 

patient.drugs.drug.9.month_of_form_completion:
   7 NA's 
   1  411 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   1  411 

patient.drugs.drug.9.therapy_types.therapy_type:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   1  411 

patient.drugs.drug.9.year_of_form_completion:
2014 NA's 
   1  411 



patient.drugs.drug.clinical_trail_drug_classification:
chemotherapy         NA's 
           1          411 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   16.00   15.89   21.00   31.00     292 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -368.0   126.5   177.0   275.5   318.0  2569.0     305 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    65.0   121.0   205.1   198.0  2569.0     299 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          31                           38 
            partial response               stable disease 
                          13                           11 
                        NA's 
                         319 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.592   9.000  12.000     292 

patient.drugs.drug.number_cycles:
   4    6 NA's 
   1    2  409 

patient.drugs.drug.prescribed_dose:
 800 1000 NA's 
   1    2  409 

patient.drugs.drug.prescribed_dose_units:
mg/m2  NA's 
    3   409 

patient.drugs.drug.regimen_indication:
   adjuvant progression        NA's 
          2           4         406 

patient.drugs.drug.regimen_number:
           1st line adjuvant (1st line)                NA's 
                  3                   2                 407 

patient.drugs.drug.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               4              408 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 111    7  294 

patient.drugs.drug.therapy_types.therapy_type:
 chemotherapy immunotherapy          NA's 
          107             7           298 

patient.drugs.drug.total_dose:
 129 1200 1300 NA's 
   1    1    1  409 

patient.drugs.drug.total_dose_units:
  mg NA's 
   3  409 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  99   13  300 

patient.drugs.drug.year_of_form_completion:
2012 2013 2014 2015 NA's 
  42   20   42   16  292 

patient.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  1.0000  0.8713  1.0000  4.0000     311 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     9                    371                     32 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
  no NA's 
   1  411 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure:
  no NA's 
   1  411 

patient.follow_ups.follow_up.2.additional_treatment_completion_success_outcome:
  complete response progressive disease      stable disease                NA's 
                 42                  18                   3                 349 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   15.00   16.08   22.50   31.00     341 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    146     433     665     767     756    3183     389 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  110.0   621.8   826.0  1133.8  1475.5  4967.0     364 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
 528 NA's 
   1  411 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
additional new tumor event                       NA's 
                         1                        411 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  64    5  343 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   6.775  10.000  12.000     341 

patient.follow_ups.follow_up.2.new_neoplasm_event_occurrence_anatomic_site:
bone NA's 
   1  411 

patient.follow_ups.follow_up.2.new_neoplasm_event_type:
metastatic       NA's 
         1        411 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  411 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    2  408 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   3    1  408 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 791 NA's 
   1  411 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 462  651  791 3086 NA's 
   1    1    1    1  408 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
        bladder            bone            lung lymph node only            NA's 
              1               1               1               1             408 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                    3                                     1 
                                 NA's 
                                  408 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   3    1  408 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    1  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   2  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 501  651 NA's 
   1    1  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             2            410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                    1                                     1 
                                 NA's 
                                  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
    adrenal soft tissue        NA's 
          1           1         410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no NA's 
   2  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no  yes NA's 
   1    1  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 580  651 NA's 
   1    1  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
           bone lymph node only            NA's 
              1               1             410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                    1                                     1 
                                 NA's 
                                  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  410 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   6    4  402 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   9    2  401 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  394.0   486.8   842.0  1171.5  1300.0  3130.0     406 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  139.0   288.0   479.5   691.3   645.8  3086.0     398 


patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                   10                                     1 
                    new primary tumor                                  NA's 
                                    1                                   400 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
abdominal wall           NA's 
             1            411 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   5    6  401 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  43   14  355 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        39         19        354 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  43   21  348 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                         51                           9 
             stable disease                        NA's 
                          2                         350 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  64    1  347 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no NA's 
   1  411 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   48    23   341 

patient.follow_ups.follow_up.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   4    6   43   18  341 

patient.follow_ups.follow_up.3.additional_treatment_completion_success_outcome:
complete response    stable disease              NA's 
                5                 2               405 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0    15.5    16.0    20.0    26.0    30.0     405 

patient.follow_ups.follow_up.3.days_to_death:
 904 1008 NA's 
   1    1  410 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1219    1845    2024    2258    2886    3314     407 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
   7  405 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   7.000   6.571  11.000  12.000     405 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   7  405 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         5          1        406 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no NA's 
   7  405 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          6                         406 

patient.follow_ups.follow_up.3.radiation_therapy:
  no NA's 
   7  405 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
    5     2   405 

patient.follow_ups.follow_up.3.year_of_form_completion:
2013 2014 2015 NA's 
   1    4    2  405 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
   3    4  405 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
   2    3  407 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure:
  no  yes NA's 
   8    1  403 

patient.follow_ups.follow_up.additional_treatment_completion_success_outcome:
  complete response    partial response progressive disease      stable disease 
                180                   6                  96                  15 
               NA's 
                115 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   19.00   17.73   25.00   31.00      83 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure:
 185 NA's 
   1  411 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   56.0   250.8   377.5   486.0   568.5  2954.0     324 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   46.0   389.0   542.0   959.4  1255.0  5050.0     185 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  111.0   141.2   215.0   217.4   267.0   364.0     402 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                             15 
                          NA's 
                           396 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 280   27  105 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.432  10.000  12.000      83 

patient.follow_ups.follow_up.new_neoplasm_event_occurrence_anatomic_site:
           bone            lung lymph node only  other, specify            NA's 
              2               3               2               2             403 

patient.follow_ups.follow_up.new_neoplasm_event_type:
        metastatic no new tumor event               NA's 
                10                  2                400 

patient.follow_ups.follow_up.new_neoplasm_occurrence_anatomic_site_text:
lower abdominal wall and right pelvis                            peritoneum 
                                    1                                     1 
                                 NA's 
                                  410 

patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   8    8  396 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
  17   15  380 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
  22    9  381 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 991 1274 3130 NA's 
   1    1    1  409 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  110.0   211.8   312.0   493.2   584.8  3086.0     376 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                   24                                     9 
                    new primary tumor                                  NA's 
                                    3                                   376 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  30    3  379 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   6    8  398 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no  yes NA's 
  11    2  399 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
1779 NA's 
   1  411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  110.0   230.5   499.0   545.3   704.5  1779.0     397 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                   11                                     4 
                                 NA's 
                                  397 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
         adrenal      lymph nodes pelvic side wall      soft tissue 
               1                1                1                1 
            NA's 
             408 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  13    1  398 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no  yes NA's 
   4    1  407 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no  yes NA's 
   4    1  407 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_additional_surgery_procedure:
2135 NA's 
   1  411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  303.0   580.0   651.0   885.4   758.0  2135.0     407 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
        bladder            bone lymph node only  other, specify            NA's 
              1               1               1               2             407 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                    3                                     2 
                                 NA's 
                                  407 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_occurrence_anatomic_site_text:
adrenal metastases           pancreas               NA's 
                 1                  1                410 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   4    1  407 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    1  410 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.additional_radiation_therapy:
  no NA's 
   2  410 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_additional_surgery_procedure:
2298 NA's 
   1  411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
 303 2298 NA's 
   1    1  410 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site:
        bladder lymph node only            NA's 
              1               1             410 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                    1                                     1 
                                 NA's 
                                  410 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    1  410 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.additional_pharmaceutical_therapy:
  no NA's 
   1  411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.additional_radiation_therapy:
  no NA's 
   1  411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_after_initial_treatment:
 303 NA's 
   1  411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_neoplasm_event_type:
locoregional (urothelial tumor event)                                  NA's 
                                    1                                   411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_neoplasm_occurrence_anatomic_site_text:
pelvic sidewall soft tissue recurrence                                   NA's 
                                     1                                    411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  411 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  40   33  339 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  58   19  335 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  105.0   256.5   446.5   699.1   978.0  2222.0     402 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   32.0   178.0   314.5   523.9   552.8  4907.0     326 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                   57                                    28 
                    new primary tumor                                  NA's 
                                    3                                   324 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  70   10  332 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 179   87  146 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       181        109        122 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
 188   85  139 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        202                          11 
        progressive disease              stable disease 
                         46                          19 
                       NA's 
                        134 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 280   18  114 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
  15    4  393 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  228    89    95 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2013    2014    2014    2014    2015      83 

patient.gender:
female   male 
   108    304 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  139.0   165.0   172.7   171.6   179.0   196.0      49 

patient.histological_type:
muscle invasive urothelial carcinoma (pt2 or above) 
                                                409 
                                               NA's 
                                                  3 

patient.history_non_muscle_invasive_blca.complete_response_observed:
  no  yes NA's 
  26    5  381 

patient.history_non_muscle_invasive_blca.hist_of_non_mibc:
  no  yes NA's 
 241   66  105 

patient.history_non_muscle_invasive_blca.induction_course_complete:
  no  yes NA's 
   1   32  379 

patient.history_non_muscle_invasive_blca.maint_therapy_course_complete:
  no  yes NA's 
  19   14  379 

patient.history_non_muscle_invasive_blca.mibc_90day_post_resection_bcg:
  no  yes NA's 
  35    2  375 

patient.history_non_muscle_invasive_blca.non_mibc_tx:
bacillus calmette-guerin (bcg)  transurethral resection alone 
                            37                             24 
                          NA's 
                           351 

patient.history_non_muscle_invasive_blca.resp_maint_from_bcg_admin_month_dur:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0     8.0    12.0    15.8    26.0    27.0     407 

patient.history_of_neoadjuvant_treatment:
 no yes 
402  10 


patient.icd_o_3_histology:
8010/3 8070/3 8120/3 8130/3 8260/3 
     1      1    341     68      1 


patient.informed_consent_verified:
yes 
412 


patient.initial_pathologic_diagnosis_method:
              endoscopic biopsy          other method, specify: 
                             45                              53 
transurethral resection (turbt)                            NA's 
                            306                               8 

patient.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   80.00   90.00   83.09   90.00  100.00     276 

patient.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   11.00   18.00   27.06   34.00  170.00     109 

patient.lymphovascular_invasion_present:
  no  yes NA's 
 132  153  127 


patient.malignant_neoplasm_metastatic_involvement_sites.malignant_neoplasm_metastatic_involvement_site.2:
 bone  lung other  NA's 
    1     1     1   409 

patient.malignant_neoplasm_metastatic_involvement_sites.malignant_neoplasm_metastatic_involvement_site.3:
liver  NA's 
    1   411 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.954   9.000  12.000 

patient.neoplasm_histologic_grade:
high grade  low grade       NA's 
       388         21          3 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
  11   11  390 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
  14    8  390 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 991 2135 3130 NA's 
   1    1    1  409 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -775.0   194.8   468.5   626.0   826.0  3086.0     388 


patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                   13                                    11 
                                 NA's 
                                  388 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
     pelvis rectal wall      vagina        NA's 
          2           1           2         407 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  20    3  389 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   4    6  402 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no  yes NA's 
   8    2  402 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
2298 NA's 
   1  411 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  157.0   286.8   540.5   831.4   801.8  2420.0     402 


patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                    8                                     2 
                                 NA's 
                                  402 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
adrenal    NA's 
      1     411 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   8    2  402 

patient.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no NA's 
   4  408 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no  yes NA's 
   3    1  408 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_additional_surgery_procedure:
2509 NA's 
   1  411 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 343  651 2509 2663 NA's 
   1    1    1    1  408 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
          liver lymph node only  other, specify            NA's 
              1               1               1             409 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                    2                                     2 
                                 NA's 
                                  408 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_occurrence_anatomic_site_text:
adrenal    NA's 
      1     411 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   3    1  408 

patient.new_tumor_events.new_tumor_event.5.additional_pharmaceutical_therapy:
  no NA's 
   1  411 

patient.new_tumor_events.new_tumor_event.5.additional_radiation_therapy:
  no NA's 
   1  411 

patient.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
2663 NA's 
   1  411 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site:
lung NA's 
   1  411 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                411 

patient.new_tumor_events.new_tumor_event.5.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  411 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  33   29  350 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  46   16  350 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  103.0   219.5   521.0   812.3  1586.5  1779.0     400 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0   191.5   377.0   536.5   648.5  3086.0     340 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
                   distant metastasis locoregional (urothelial tumor event) 
                                   41                                    27 
                    new primary tumor                                  NA's 
                                    3                                   341 


patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  53   12  347 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 231   71  110 

patient.number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.088   2.000  97.000     115 

patient.number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.15   20.00   30.00   39.04   50.00  730.00     188 




patient.person_concomitant_prostate_carcinoma_occurrence_indicator:
  no  yes NA's 
 255   86   71 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       235        137         40 


patient.person_occupation_years_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   20.00   30.00   30.55   38.50   57.00     372 


patient.postoperative_rx_tx:
  no  yes NA's 
 187   78  147 

patient.primary_lymph_node_presentation_assessment:
  no  yes NA's 
  79  296   37 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        154                          17 
        progressive disease              stable disease 
                         42                          23 
                       NA's 
                        176 

patient.prior_dx:
 no yes 
303 109 

patient.race:
                    asian black or african american                     white 
                       44                        23                       327 
                     NA's 
                       18 

patient.radiation_therapy:
  no  yes NA's 
 269   10  133 

patient.radiations.radiation.2.anatomic_treatment_site:
    distant site local recurrence    regional site             NA's 
               1                1                4              406 



patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   11.00   14.00   17.00   25.25   30.00     406 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   187.0   299.0   754.2   662.0  2613.0     407 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0   200.8   447.5   763.8   858.5  2569.0     406 

patient.radiations.radiation.2.measure_of_response:
               complete response                 partial response 
                               1                                3 
radiographic progressive disease                             NA's 
                               2                              406 

patient.radiations.radiation.2.month_of_form_completion:
   1    7   11   12 NA's 
   1    2    1    2  406 

patient.radiations.radiation.2.numfractions:
   5   10 NA's 
   1    2  409 

patient.radiations.radiation.2.radiation_dosage:
  50   60 2000 3000 NA's 
   1    1    1    2  407 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   6  406 

patient.radiations.radiation.2.radiation_type:
external internal     NA's 
       5        1      406 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
   3    2  407 

patient.radiations.radiation.2.year_of_form_completion:
2012 2013 2014 NA's 
   1    1    4  406 

patient.radiations.radiation.3.anatomic_treatment_site:
regional site          NA's 
            1           411 

patient.radiations.radiation.3.bcr_radiation_barcode:
tcga-fj-a3ze-r56453                NA's 
                  1                 411 

patient.radiations.radiation.3.bcr_radiation_uuid:
023ba78f-c50f-4e6a-abb2-9f1c9bfa1b47                                 NA's 
                                   1                                  411 

patient.radiations.radiation.3.day_of_form_completion:
   7 NA's 
   1  411 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
  99 NA's 
   1  411 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
  56 NA's 
   1  411 

patient.radiations.radiation.3.measure_of_response:
radiographic progressive disease                             NA's 
                               1                              411 

patient.radiations.radiation.3.month_of_form_completion:
   2 NA's 
   1  411 

patient.radiations.radiation.3.numfractions:
  28 NA's 
   1  411 

patient.radiations.radiation.3.radiation_dosage:
5600 NA's 
   1  411 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   1  411 

patient.radiations.radiation.3.radiation_type:
external     NA's 
       1      411 

patient.radiations.radiation.3.units:
 cgy NA's 
   1  411 

patient.radiations.radiation.3.year_of_form_completion:
2014 NA's 
   1  411 




patient.radiations.radiation.course_number:
   1 NA's 
   3  409 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   15.00   17.54   27.00   31.00     373 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   131.0   235.0   328.1   397.0  1871.0     375 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0   107.0   174.0   301.6   369.0  1821.0     375 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              11                                5 
radiographic progressive disease                   stable disease 
                               9                                3 
                            NA's 
                             384 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.667   7.500  12.000     373 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   10.00   14.00   17.22   25.00   35.00     385 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   42.75   64.00 1598.19 3000.00 5940.00     380 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  39  373 

patient.radiations.radiation.radiation_type:
               external           external beam other: specify in notes 
                     35                       1                       1 
                   NA's 
                    375 

patient.radiations.radiation.radiation_type_notes:
cyberknife       NA's 
         1        411 

patient.radiations.radiation.regimen_indication:
 palliative progression  recurrence        NA's 
          1           1           1         409 

patient.radiations.radiation.units:
 cgy   gy NA's 
  13   18  381 

patient.radiations.radiation.year_of_form_completion:
2012 2013 2014 2015 NA's 
   7    7   16    9  373 

patient.stage_event.gleason_grading.gleason_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  5.000   6.000   6.000   6.353   7.000   9.000     327 

patient.stage_event.pathologic_stage:
  stage i  stage ii stage iii  stage iv      NA's 
        2       131       141       136         2 



patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
  m0   m1   mx NA's 
 196   11  203    2 



patient.stopped_smoking_year:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1946    1976    1988    1989    2004    2013     238 

patient.tissue_prospective_collection_indicator:
 no yes 
200 212 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
 211  200    1 



patient.tumor_tissue_site:
bladder 
    412 

patient.vital_status:
alive  dead 
  303   109 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  41.00   65.00   78.00   80.98   92.00  292.00      43 

patient.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2010    2012    2013    2013    2014    2015 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1999    2009    2011    2010    2012    2013      18 


patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              14                                1 
                            NA's 
                             397 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                  8                 404 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             382               30 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      391        21 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -6.00   74.75  223.00  851.50 1401.50 4912.00      24 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -64.00   11.00   36.00   63.88   66.00 1361.00      19 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
383  29 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     6                    213                    193 

patient.biospecimen_cqcf.ffpe_tumor_slide_submitted:
digital image         slide          NA's 
            5            85           322 


patient.biospecimen_cqcf.histological_type:
muscle invasive urothelial carcinoma (pt2 or above) 
                                                410 
                                               NA's 
                                                  2 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             402 
yes, pharmaceutical treatment prior to resection 
                                              10 

patient.biospecimen_cqcf.method_of_sample_procurement:
                     cystectomy          other (please specify) 
                            303                              51 
transurethral resection (turbt) 
                             58 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -3.00   25.50   40.50   48.75   59.75  221.00     372 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
0.043 0.439  NA's 
    1     1   410 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
   cdrop nanodrop     NA's 
       1        1      410 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  15 NA's 
   2  410 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
34.1  351 NA's 
   1    1  410 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
                blood draw surgical excision specimen 
                        21                         17 
        surgical resection                       NA's 
                         3                        371 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
bladder; macroscopic urothelial tissue                                  other 
                                    12                                      1 
                         smooth muscle                                   NA's 
                                     7                                    392 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           20           392 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
  37   18  357 


patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -24.00   22.50   37.50  116.70   44.25  935.00     402 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
                blood draw surgical excision specimen 
                         8                          2 
                      NA's 
                       402 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
bladder; macroscopic urothelial tissue                          smooth muscle 
                                     1                                      1 
                                  NA's 
                                   410 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            2           410 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    3 NA's 
  10    1  401 


patient.biospecimen_cqcf.normal_controls.normal_control.4.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -24.0    -1.5    33.0   170.5    52.5   935.0     406 

patient.biospecimen_cqcf.normal_controls.normal_control.4.method_of_normal_sample_procurement:
blood draw       NA's 
         6        406 

patient.biospecimen_cqcf.normal_controls.normal_control.4.vial_number:
   2    4 NA's 
   6    1  405 

patient.biospecimen_cqcf.normal_controls.normal_control.5.bcr_sample_uuid:
85aeb98d-aa43-46f9-9af8-665f65617eff ead2cb20-f83f-435d-bbe6-7e22211c8520 
                                   1                                    1 
f03e629a-9f75-4711-97ba-1f7fb12f643d                                 NA's 
                                   1                                  409 

patient.biospecimen_cqcf.normal_controls.normal_control.5.days_to_normal_sample_procurement:
  -9   45   55 NA's 
   1    1    1  409 

patient.biospecimen_cqcf.normal_controls.normal_control.5.method_of_normal_sample_procurement:
blood draw       NA's 
         3        409 

patient.biospecimen_cqcf.normal_controls.normal_control.5.vial_number:
   3 NA's 
   3  409 

patient.biospecimen_cqcf.normal_controls.normal_control.6.bcr_sample_uuid:
17fae696-d6b4-4524-b1fb-3b86a930ae29 975d9b1d-9dfe-428f-b6f6-04fcf4f45c85 
                                   1                                    1 
                                NA's 
                                 410 

patient.biospecimen_cqcf.normal_controls.normal_control.6.days_to_normal_sample_procurement:
  -9   55 NA's 
   1    1  410 

patient.biospecimen_cqcf.normal_controls.normal_control.6.method_of_normal_sample_procurement:
blood draw       NA's 
         2        410 

patient.biospecimen_cqcf.normal_controls.normal_control.6.vial_number:
   1 NA's 
   2  410 

patient.biospecimen_cqcf.normal_controls.normal_control.7.bcr_sample_uuid:
15e027b6-c6bd-4c88-bfb3-6e61339531cd 73aa1b8e-0fe3-4594-94bc-5d774a4de2ab 
                                   1                                    1 
                                NA's 
                                 410 

patient.biospecimen_cqcf.normal_controls.normal_control.7.days_to_normal_sample_procurement:
  -9   55 NA's 
   1    1  410 

patient.biospecimen_cqcf.normal_controls.normal_control.7.method_of_normal_sample_procurement:
blood draw       NA's 
         2        410 

patient.biospecimen_cqcf.normal_controls.normal_control.7.vial_number:
   2 NA's 
   2  410 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -363.0    16.0    40.0   173.7    80.0  4670.0      29 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0550   0.1765   0.2440  20.7232   0.3765 709.0000      322 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.2    30.0    50.0   107.4   184.8   986.4     322 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   25.0   109.9   222.2   467.6  1200.0  1200.0     322 


patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
bladder; macroscopic urothelial tissue                                  other 
                                     2                                     11 
                         smooth muscle                                   NA's 
                                    29                                    370 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           39           373 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2 NA's 
 401    5    6 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 no yes 
 12 400 

patient.biospecimen_cqcf.path_confirm_report_attached:
 no yes 
 18 394 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 no yes 
  1 411 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 no yes 
  1 411 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                       29                         7                       194 
                     NA's 
                      182 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
                                    macrodissection performed at tss to select for a region containing an acceptable tcga diagnosis 
                                                                                                                                  1 
                macrodissection performed at tss to select for a region containing an acceptable tcga diagnosis (see note at right) 
                                                                                                                                  2 
  pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                  1 
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  8 
                                                                                                                               NA's 
                                                                                                                                400 

patient.biospecimen_cqcf.sample_prescreened:
yes 
412 

patient.biospecimen_cqcf.site_of_disease:
bladder 
    412 


patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
 268   27  117 

patient.biospecimen_cqcf.top_slide_submitted:
 no yes 
 31 381 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0    5 NA's 
   3    1  408 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  70   80   90  100 NA's 
   1    1    1    1  408 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
 200  210  217  270 NA's 
   1    1    1    1  408 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   4    1  407 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.bcr_sample_uuid:
657e5c7f-7601-4948-a088-d23ea5d95b2c                                 NA's 
                                   1                                  411 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.vial_number:
   3 NA's 
   1  411 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   1.000   7.328  10.000  70.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  35.00   70.00   80.00   76.17   85.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    6.0   190.0   240.0   342.9   387.8  3274.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1 
412 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                     30                      80                      81 
               cryovial  other (please specify) 
                    141                      80 

patient.days_to_index:
   0 NA's 
 393   19 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and metastatic tumor(s)                                  NA's 
                                    1                                   411 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     14     139     316     911    1434    4919      19 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   197.5   245.0   494.0   390.0  3800.0     392 

patient.samples.sample.2.is_ffpe:
 no yes 
409   3 

patient.samples.sample.2.oct_embedded:
false  true 
  395    17 

patient.samples.sample.2.pathology_report_file_name:
tcga-bl-a0c8.5077d8c6-57d2-4c6a-9316-71be40224195.pdf 
                                                    1 
tcga-bl-a13i.976fbdbf-55c5-4957-986b-d2de85ff7540.pdf 
                                                    1 
tcga-bl-a13j.3abf8c6b-606c-41a9-b050-e115834a7616.pdf 
                                                    1 
tcga-dk-a1a6.fa3a7697-0e2c-4c79-b486-0aae52065be3.pdf 
                                                    1 
                                                 NA's 
                                                  408 

patient.samples.sample.2.pathology_report_uuid:
3abf8c6b-606c-41a9-b050-e115834a7616 5077d8c6-57d2-4c6a-9316-71be40224195 
                                   1                                    1 
976fbdbf-55c5-4957-986b-d2de85ff7540 fa3a7697-0e2c-4c79-b486-0aae52065be3 
                                   1                                    1 
                                NA's 
                                 408 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.710   1.775   1.820   1.815   1.860   1.910     401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   3  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   3  409 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97491253  97491278 113390007 116511658 132179172 167823137       401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
   6    5  401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.13 0.16 0.17 NA's 
   1    7    3  401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.00   13.00   11.73   15.50   27.00     401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
   8    3  401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.636   5.500   9.000     401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   3.909   5.500   7.000     401 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.080   2.900   3.400   3.379   4.270   4.270     401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
  11  401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 26.7 NA's 
   2    5    4  401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2013 2014 NA's 
   7    3    1  401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-bl-a0c8-01b-04r-a46x-36 tcga-bl-a13j-01b-04r-a46x-36 
                           1                            1 
tcga-dk-a1a6-06a-11r-a466-07                         NA's 
                           1                          409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
1b11b644-f9d9-4ba5-9beb-ce054c0e0f86 2c6edcfe-bc3b-4e96-8c5e-7b49fd566050 
                                   1                                    1 
54bc6778-9c1c-4db6-8764-79ccd841f566                                 NA's 
                                   1                                  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
167812948 167814186 167814191      NA's 
        1         1         1       409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7   36 NA's 
   1    2  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.16 0.17 NA's 
   1    2  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   9   26 NA's 
   1    2  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
   1    2  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   8   12 NA's 
   2    1  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1    5    6 NA's 
   1    1    1  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a466 a46x NA's 
   1    2  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   d    e NA's 
   1    2  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
 0.8 0.85 1.12 NA's 
   1    1    1  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   3  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   5  6.6 NA's 
   2    1  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 2015 NA's 
   1    2  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   3  409 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   3  409 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97490606 122968432 150706080 144261464 167803013 167822561        46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   5  355    6   46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.13 0.16 0.17  0.5 NA's 
   1    7    3  355   46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     8.0    15.5    15.3    22.0    30.0      46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
 363    3   46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    7.00    6.06    7.00   12.00      46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   6.000   6.068   9.000  12.000      46 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.08   40.00   40.00   38.88   40.00   40.00      46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
 366   46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20 26.7   80 NA's 
   4    6    1  355   46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
  49   84   78  155   46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.17   10.76   14.22   26.45   44.77   57.79     401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 355                   11                   46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  11  355   46 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
0.13 0.16 0.17 NA's 
   1    7    3  401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 355   57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
 355   57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             366 
                                            NA's 
                                              46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no  yes NA's 
 363    3   46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
highpure mirna (allprep dna) ffpe rna             mirvana (allprep dna) rna 
                                    3                                     8 
                              repli-g                                  NA's 
                                  355                                    46 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.500   1.200   1.006   1.280   2.200     401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.300   4.750   7.800   6.464   8.150   9.000     401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     11     401 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  24    2  386 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108433927 113388440 113420020 112699495 113421860 113421918       405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   6   15   20   21 NA's 
   1    1    1    4  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   4    6    9   11 NA's 
   1    4    1    1  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   7    8   11 NA's 
   4    2    1  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a12w a16j a18a a19s NA's 
   1    4    1    1  405 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   7                  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   7  405 



patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      7     405 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   7  405 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.760   1.895   1.945   1.939   1.980   2.110     378 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   3  409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   3  409 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97484813 122967204 153878363 144803296 164106186 174164734 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   2   5   8 
 38   3   1 370 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07000 0.07000 0.08000 0.08502 0.08000 0.20000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    3.0    18.0    26.0    22.5    29.0    31.0 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no yes 
409   3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.869   6.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   6.000   5.883   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.770   2.800   3.200   2.834   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
412 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   40.00   40.00   36.29   40.00   40.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
  55   94   92  171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   2  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   2  410 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97486989 113388216 117830960 118110558 122970312 174164674       277 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   26.00   17.41   26.00   26.00     277 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0900  0.1500  0.1342  0.1600  0.1700     277 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    7.00   27.00   19.39   29.00   31.00     277 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
 133    2  277 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    5.00    5.17    8.00   12.00     277 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   7.000   8.000   8.281  10.000  12.000     277 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.500   1.600   1.998   3.200   4.000     277 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 135  277 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   10.00   10.00   18.37   40.00   40.00     277 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 NA's 
  54   77    2    2  277 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99039608 100475901 113421396 122338000 132158658 167811833       394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8    9 NA's 
  10    5    3  394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1450  0.1233  0.1600  0.1800     394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.00   16.50   14.33   21.00   30.00     394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no  yes NA's 
  16    2  394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   5.000   4.111   5.750   9.000     394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.250   7.000   6.333   7.000  11.000     394 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.600   2.015   2.598   4.205   4.540     394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
  18  394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  13 13.4   20 26.7 NA's 
   3    3    5    7  394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2013 2015 NA's 
  10    3    5  394 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-bt-a20v-11a-11d-a23d-05                         NA's 
                           1                          411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
e5d1dd4e-47ed-484e-abed-c44424764ddc                                 NA's 
                                   1                                  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
132268714      NA's 
        1       411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   5 NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.16 NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  31 NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   7 NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   8 NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a23d NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   f NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
 6.4 NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
  40 NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2012 NA's 
   1  411 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   3  409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   3  409 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486797 122969676 153879396 145100203 164104510 174164729 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   8 
363  46   3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1400  0.1500  0.1532  0.1600  0.2000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   18.00   25.00   21.92   29.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no yes 
409   3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.888   6.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   6.000   5.883   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.94    1.01    1.01    1.13    1.07    2.34 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
412 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7   7  10  13  20 
362   1  15  31   3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
  55   94   92  171 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.30   14.73   26.19   32.75   45.80  178.36 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
412 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
412 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1100  0.1500  0.1500  0.1538  0.1600  0.2000 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
412 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
412 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             412 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no yes 
409   3 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type                  allprep ffpe dna 
                              308                                 3 
pre-extracted dna received by tss 
                              101 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      378        34 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  25    3  384 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    7.75   15.00   16.03   25.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no yes 
409   3 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   5.189   8.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   1.973   1.000  31.000 

patient.samples.sample.2.portions.portion.portion_sequence:
   1    2    3    4 NA's 
 240    8    2    3  159 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-bl-a0c8-01b-04-bs4 tcga-bl-a13i-01b-04-bs4 tcga-bl-a13j-01b-04-bs4 
                      1                       1                       1 
                   NA's 
                    409 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_uuid:
0d55c909-15b8-464c-a663-6cb80d28c82c 1061ba91-692b-4183-b473-99236fa21899 
                                   1                                    1 
8ab97d27-3fb1-41c1-aad9-ab244adb4bce                                 NA's 
                                   1                                  409 

patient.samples.sample.2.portions.portion.slides.slide.2.image_file_name:
tcga-bl-a0c8-01b-04-bs4.8ab97d27-3fb1-41c1-aad9-ab244adb4bce.svs 
                                                               1 
tcga-bl-a13i-01b-04-bs4.0d55c909-15b8-464c-a663-6cb80d28c82c.svs 
                                                               1 
tcga-bl-a13j-01b-04-bs4.1061ba91-692b-4183-b473-99236fa21899.svs 
                                                               1 
                                                            NA's 
                                                             409 

patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
 yes NA's 
   3  409 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   0 NA's 
   3  409 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   3  409 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
   0    5 NA's 
   2    1  409 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   3  409 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
   0   15 NA's 
   2    1  409 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
   5   10   20 NA's 
   1    1    1  409 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
  80   85 NA's 
   2    1  409 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
  80   90   95 NA's 
   1    1    1  409 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
bottom   NA's 
     3    409 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no  yes NA's 
  21    3  388 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.333   1.250  30.000     388 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
  24  388 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0    5 NA's 
  23    1  388 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    5 NA's 
  23    1  388 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0   15   20  100 NA's 
  11    1    1   11  388 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    2.50   38.12  100.00  100.00     388 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0   80   85  100 NA's 
  20    2    1    1  388 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   13.96    0.00   95.00     388 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
  24  388 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    2.00   10.45    2.00  400.00     104 

patient.samples.sample.2.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
  20   53   84  107  148 

patient.samples.sample.2.sample_type:
blood derived normal           metastatic        primary tumor 
                 388                    1                    3 
 solid tissue normal 
                  20 

patient.samples.sample.2.sample_type_id:
  1   6  10  11 
  3   1 388  20 


patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  7                 405 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     76     123     154     212     235     874     393 

patient.samples.sample.3.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   100.0   130.0   187.3   215.0   490.0     397 

patient.samples.sample.3.is_ffpe:
  no NA's 
  19  393 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
   12     7   393 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.780   1.820   1.840   1.839   1.865   1.880     401 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100491801 115624498 117832676 124436879 134303165 150756695       401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
   3    8  401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1550  0.1600  0.1618  0.1750  0.1800     401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   25.50   27.00   25.36   28.50   31.00     401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.182   6.500   9.000     401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.455   5.500  10.000     401 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.080   2.470   3.000   2.918   3.300   3.600     401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 NA's 
   3    8  401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 NA's 
   3    5    3  401 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97490975 113396671 130615875 130133789 151834247 167802542       394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   8    7    3  394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1600  0.1800  0.2933  0.5000  0.5000     394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.25   24.00   20.61   27.00   31.00     394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  18  394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.611   7.000  10.000     394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.500   6.278   9.750  12.000     394 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.243   3.335  17.134  40.000  40.000     394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
  18  394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20 26.7   80 NA's 
   7    3    1    7  394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
   6    5    6    1  394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.850   8.155  10.270  10.953  12.940  16.840     401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   7                   11                  394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  11    7  394 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1550  0.1600  0.1618  0.1750  0.1800     401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   7  405 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   7  405 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              18 
                                            NA's 
                                             394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  18  394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                       11                         7                       394 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.100   1.300   1.400   1.389   1.465   1.700     401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.300   7.400   7.718   8.200   9.000     401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     11     401 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
  12    1    1  398 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108433932 113390993 118956637 125654040 136838008 153887522       401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   15.50   21.00   17.91   22.00   24.00     401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   5.500   8.000   7.636   9.000  12.000     401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   3    6   11   12 NA's 
   1    6    2    2  401 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 2013 NA's 
   3    5    3  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  11                  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
  11  401 



patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              11 
                                            NA's 
                                             401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  11  401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     11     401 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1    3 NA's 
  10    1  401 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
1.93 1.95    2 2.03 NA's 
   1    1    1    2  407 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100489209 113396484 120403001 124468169 132271747 160339694       394 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
   3   15  394 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0800  0.0928  0.0800  0.1800     394 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0    12.0    25.5    21.5    27.0    31.0     394 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  18  394 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.250   5.000   4.556   6.000   9.000     394 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   8.500   8.278  11.000  12.000     394 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.050   1.885   3.200   2.617   3.200   3.600     394 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
  18  394 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   26.00   40.00   32.13   40.00   40.00     394 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 NA's 
   6    5    6    1  394 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched methylation control                             NA's 
                               1                              411 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100487780 109702730 117830904 119877424 122971551 150754466       398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   1    5    8   26 NA's 
   3    6    3    2  398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1400  0.1500  0.1414  0.1600  0.1700     398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   18.75   27.00   23.79   29.25   31.00     398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  14  398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.250   4.000   4.429   6.000   9.000     398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   7.000   8.143  11.000  12.000     398 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.425   2.080   2.074   2.210   3.600     398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
  14  398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
6.67   10   13   40 NA's 
   3    2    6    3  398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 NA's 
   6    5    3  398 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   2  410 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100492185 117830909 122971497 122927704 132160690 132173115       403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8   26 NA's 
   4    2    3  403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1367  0.1600  0.1700     403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   12.00   14.00   20.22   30.00   31.00     403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   9  403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.222   8.000   9.000     403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   3    6    7   11 NA's 
   2    4    2    1  403 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.500   1.700   1.966   1.950   4.270     403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   9  403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10   13   20 26.7 NA's 
   3    3    2    1  403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 2013 NA's 
   2    5    2  403 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-bl-a0c8-10a-01d-a273-01 tcga-bl-a13j-10a-01d-a273-01 
                           1                            1 
                        NA's 
                         410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
4a0f7ad1-73d5-460f-a3cb-7b8856eeac79 51e58d38-23bd-4426-9960-d2edb865fa1b 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
132158820 132158825      NA's 
        1         1       410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.14 0.15 NA's 
   1    1  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  14 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   3 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   7 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a273 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   a    c NA's 
   1    1  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
0.94 1.01 NA's 
   1    1  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
 6.7 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2013 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-bl-a0c8-10a-01d-a46w-08 tcga-bl-a13j-10a-01d-a46w-08 
                           1                            1 
                        NA's 
                         410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
49aa01dd-8cdc-47f6-9d09-87d4862fa071 c56a2abe-72ae-4e66-956a-c20c23ce02dc 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
132159825 132179160      NA's 
        1         1       410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   3 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   8 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   8   11 NA's 
   1    1  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a46w NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   a    d NA's 
   1    1  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
 1.4 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  20 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2015 NA's 
   2  410 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   1  411 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   1  411 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             411 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100490636 113403678 122971349 127522269 148189549 160338062       393 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    8 NA's 
  12    6    1  393 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0300  0.1500  0.1500  0.1511  0.1700  0.1800     393 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   24.00   19.53   27.00   30.00     393 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  19  393 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   4.000   4.526   6.000   9.000     393 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    6.00    8.00    8.21   11.00   12.00     393 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.010   1.140   1.297   1.650   2.210     393 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
  19  393 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7   10   13   20 NA's 
  12    2    4    1  393 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
   6    5    7    1  393 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.84    8.23   15.01   16.27   18.00   67.49     393 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  19  393 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  19  393 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1500  0.1500  0.1521  0.1700  0.1800     393 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  19  393 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
  19  393 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              19 
                                            NA's 
                                             393 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  19  393 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                               18                                 1 
                             NA's 
                              393 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       14         5       393 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
  12    1    1  398 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   10.50   15.00   16.95   24.50   31.00     393 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  19  393 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.500   5.000   6.632  10.500  12.000     393 

patient.samples.sample.3.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   11.00   10.63   11.50   21.00     393 

patient.samples.sample.3.portions.portion.portion_sequence:
   1    2 NA's 
  17    2  393 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  15  397 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   8.467   9.000  65.000     397 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1    2 NA's 
  11    2    2  397 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
  15  397 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    3   10 NA's 
   9    3    2    1  397 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   10.00   20.00   42.27  100.00  100.00     397 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   80.00   57.73   90.00  100.00     397 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
  15  397 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
  15  397 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
  15  397 

patient.samples.sample.3.portions.portion.weight:
  30  200 NA's 
  15    3  394 

patient.samples.sample.3.portions.portion.year_of_creation:
2010 2011 2012 2013 NA's 
   5    2    6    6  393 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   4                   15                  393 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
   4   15  393 

patient.samples.sample.3.vial_number:
   a NA's 
  19  393 

patient.samples.sample.4.bcr_sample_barcode:
tcga-bl-a13i-11a tcga-bl-a13j-11a             NA's 
               1                1              410 

patient.samples.sample.4.bcr_sample_uuid:
2a834a44-9490-4c60-b587-ebe890c3fb0a dd38f3f6-5a39-41b7-b2ea-8917298e76d2 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.4.days_to_collection:
 127  246 NA's 
   1    1  410 

patient.samples.sample.4.initial_weight:
 500 NA's 
   1  411 

patient.samples.sample.4.is_ffpe:
  no NA's 
   2  410 

patient.samples.sample.4.oct_embedded:
false  NA's 
    2   410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.84 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-bl-a13j-11a-13r-a10v-13                         NA's 
                           1                          411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
d9b6d719-68e5-4994-8b34-1be49779751a                                 NA's 
                                   1                                  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
100491812      NA's 
        1       411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  13 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.13 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  12 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   1 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   6 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a10v NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   f NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
 2.6 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  20 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-bl-a13i-11a-11w-a14t-08 tcga-bl-a13j-11a-13r-a10u-07 
                           1                            1 
                        NA's 
                         410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
c7290ba6-5d86-4ebd-9baa-f48c408ad3f0 c808f209-8527-4105-819b-610508d9d3b3 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
 97490957 100491734      NA's 
        1         1       410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.13  0.5 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   3   12 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   1    5 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   6   10 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a10u a14t NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   e    f NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
3.47   40 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
26.7   80 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.amount:
13.01  NA's 
    1   411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   1                    1                  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-bl-a13i-11a-11w tcga-bl-a13j-11a-13r                 NA's 
                   1                    1                  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
2e654640-37f5-4d0e-a221-d027a27317e1 a4338e9e-9d1e-4b97-8911-049681bf6d54 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.concentration:
0.13 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                        1                         1                       410 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
 1.1 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.rinvalue:
 6.8 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      1     411 

patient.samples.sample.4.portions.portion.analytes.analyte.2.subportion_sequence:
   1    3 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-bl-a13j-11a-13w-a12w-08                         NA's 
                           1                          411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
b8033a13-4878-43a5-84b9-a20341ac4cac                                 NA's 
                                   1                                  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
108433955      NA's 
        1       411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   6 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   4 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
  12 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a12w NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   f NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-bl-a13j-11a-13w                 NA's 
                   1                  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
17c49281-bcfa-4708-b3b6-adb20efd9687                                 NA's 
                                   1                                  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     411 

patient.samples.sample.4.portions.portion.analytes.analyte.3.subportion_sequence:
   3 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.a260_a280_ratio:
1.94 1.98 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-bl-a13i-11a-11d-a13v-01 tcga-bl-a13j-11a-13d-a10s-08 
                           1                            1 
                        NA's 
                         410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
3eb2af14-5a2f-47aa-a421-c70913ab0b46 7f26857a-3586-4bce-aa3b-97cc3a407c14 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
100489208 108474278      NA's 
        1         1       410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.16 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  12   30 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   1    3 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
  10   12 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a10s a13v NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   e    f NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
1.07 1.82 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67   26 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-bl-a13i-11a-11d-a13w-08 tcga-bl-a13j-11a-13d-a10t-01 
                           1                            1 
                        NA's 
                         410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
5e1e29aa-bf11-4297-9681-e883ca7558d3 c71b3462-c2af-433e-9962-3e1a9a718b03 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
100488982 108474566      NA's 
        1         1       410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   1    8 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.08 0.14 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  12   30 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   1    3 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
  10   12 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a10t a13w NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   e    f NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
0.93  3.2 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
6.67   40 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-bl-a13j-11a-13d-a10w-05                         NA's 
                           1                          411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
e97d7f8c-b9c4-4a21-99bb-508ca17d7dc7                                 NA's 
                                   1                                  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
100492196      NA's 
        1       411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.14 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  12 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   6 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a10w NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   f NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
3.74 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
26.7 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 NA's 
   1  411 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-bl-a13i-11a-11d-a13u-02 tcga-bl-a13j-11a-13d-a10r-02 
                           1                            1 
                        NA's 
                         410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
325dc686-e598-42c6-b2e7-3ae19b1ea4f9 9b4ffd23-8b53-4dfc-a98e-e4c70f7680f0 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
100490635 108474470      NA's 
        1         1       410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   2 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.16 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  12   30 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   1    3 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
  10   12 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a10r a13u NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   e    f NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
1.82 2.08 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
  13 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
  9.8 13.33  NA's 
    1     1   410 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-bl-a13i-11a-11d tcga-bl-a13j-11a-13d                 NA's 
                   1                    1                  410 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid:
11815750-dad7-43d5-8474-4b8d427020b8 8b8d9cbf-a090-4fd1-aa16-e6b9fe2f403c 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
0.14 0.16 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             410 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    2                   410 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      2     410 

patient.samples.sample.4.portions.portion.analytes.analyte.subportion_sequence:
   1    3 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.bcr_portion_barcode:
tcga-bl-a13i-11a-11 tcga-bl-a13j-11a-13                NA's 
                  1                   1                 410 

patient.samples.sample.4.portions.portion.bcr_portion_uuid:
76763975-ca07-4ab6-b884-90a451a5f9b8 f2202050-aa9a-41b1-8b18-06a0e0fb7600 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.4.portions.portion.day_of_creation:
   3 NA's 
   2  410 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   2  410 

patient.samples.sample.4.portions.portion.month_of_creation:
  12 NA's 
   2  410 

patient.samples.sample.4.portions.portion.portion_number:
  11   13 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.portion_sequence:
   1 NA's 
   2  410 

patient.samples.sample.4.portions.portion.slides.slide.bcr_slide_barcode:
tcga-bl-a13i-11a-01-ts1 tcga-bl-a13j-11a-01-ts1                    NA's 
                      1                       1                     410 

patient.samples.sample.4.portions.portion.slides.slide.bcr_slide_uuid:
02e153ec-4497-4d52-a6b1-6b80e50a426d 6f663daa-1b69-4f51-a1e5-fe61e026d700 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.4.portions.portion.slides.slide.image_file_name:
tcga-bl-a13i-11a-01-ts1.6f663daa-1b69-4f51-a1e5-fe61e026d700.svs 
                                                               1 
tcga-bl-a13j-11a-01-ts1.02e153ec-4497-4d52-a6b1-6b80e50a426d.svs 
                                                               1 
                                                            NA's 
                                                             410 

patient.samples.sample.4.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.4.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   1    2 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   2  410 

patient.samples.sample.4.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   2  410 

patient.samples.sample.4.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   2  410 

patient.samples.sample.4.portions.portion.slides.slide.percent_normal_cells:
   3   70 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.slides.slide.percent_stromal_cells:
  30   97 NA's 
   1    1  410 

patient.samples.sample.4.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   2  410 

patient.samples.sample.4.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   2  410 

patient.samples.sample.4.portions.portion.slides.slide.section_location:
 top NA's 
   2  410 

patient.samples.sample.4.portions.portion.weight:
  30 NA's 
   2  410 

patient.samples.sample.4.portions.portion.year_of_creation:
2010 NA's 
   2  410 

patient.samples.sample.4.sample_type:
solid tissue normal                NA's 
                  2                 410 

patient.samples.sample.4.sample_type_id:
  11 NA's 
   2  410 

patient.samples.sample.4.vial_number:
   a NA's 
   2  410 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   135.0   297.0   896.6  1418.0  4919.0      19 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10:
efdebc10-7fe6-4a81-984a-9b0fb45bb7be                                 NA's 
                                   1                                  411 








patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
175d6533-186b-4c62-95fc-6cddbdab4278 77e9e77b-0a41-4045-a1ee-19fec7259910 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0   160.0   310.0   398.7   550.0  1830.0       1 

patient.samples.sample.is_ffpe:
 no 
412 

patient.samples.sample.oct_embedded:
false  true 
   61   351 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.510   1.820   1.860   1.853   1.890   2.110 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97490568 122940102 150756720 144730551 164104797 167823161 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  7  13 
192 220 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0800  0.1600  0.1700  0.1664  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   17.00   26.00   22.65   29.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
412 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   5.206   6.250  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.277   6.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.600   2.210   3.200   2.909   3.400   4.540 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
 23 
412 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 26.7 
 169  220   23 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 2014 
   2   56   95  100  159 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   3  409 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132178536 145632009 148194505 152326188 167812920 167812964       402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7   13 NA's 
   8    2  402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1600  0.1600  0.1610  0.1675  0.1800     402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2    9   14 NA's 
   3    4    3  402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  10  402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   1    3   10   12 NA's 
   2    3    1    4  402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1    2    4    7 NA's 
   2    2    3    3  402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a25g a277 a32y a466 NA's 
   2    3    1    4  402 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.060   1.137   1.885   1.882   2.080   3.200     402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
  10  402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6   13   20 NA's 
   4    4    2  402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2013 2014 NA's 
   6    4  402 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-bl-a0c8-01a-11r-a27d-13 tcga-bl-a13i-01a-11r-a27d-13 
                           1                            1 
tcga-bl-a13j-01a-11r-a27d-13                         NA's 
                           1                          409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
40160bd1-cd4f-4563-a4d6-5e6c88951f7e 50761798-77ec-4f5a-92bf-71b26d49b909 
                                   1                                    1 
99566c9b-b078-463b-8760-354132483a51                                 NA's 
                                   1                                  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
132179167 132179176 132179191      NA's 
        1         1         1       409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
  13 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.14 0.15 0.16 NA's 
   1    1    1  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  13 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   3 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   7 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
a27d NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   f    g    h NA's 
   1    1    1  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
 2.8    3  3.2 NA's 
   1    1    1  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  23 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
  20 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2013 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-bl-a0c8-01a-11r-a46x-36 tcga-bl-a13j-01a-11r-a46x-36 
                           1                            1 
                        NA's 
                         410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_uuid:
7e41c27a-2632-4475-9fa3-68b837227784 e1eb7a37-c452-4b85-872d-6c6999003741 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
167814187 167814190      NA's 
        1         1       410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.center_id:
  36 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.14 0.15 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
  26 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   8 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   2    5 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_id:
a46x NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_row:
   e NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.quantity:
 0.7 0.75 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.source_center:
  23 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.volume:
   5 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2015 NA's 
   2  410 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97490542 128958364 153879984 144316416 164105853 167822585 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  7  13 
220 192 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0800  0.1600  0.1700  0.1664  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   17.00   26.00   22.65   29.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
 no 
412 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   5.206   6.250  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.277   6.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   2.210   2.340   2.759   3.400   4.540 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
 23 
412 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20 26.7 
 209  192   11 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
   2   56   95  100  159 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.31   33.43   47.58   47.58   60.55  129.49 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
412 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
  r 
412 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0800  0.1600  0.1700  0.1664  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             412 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
 no 
412 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                      412 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.500   1.400   1.600   1.612   1.800   3.300 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.400   8.000   8.088   8.600  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
    412 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
 336   66   10 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97490609 122968438 150705214 144141240 167788667 167804056        46 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
 366   46 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 366   46 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   16.00   15.33   22.00   30.00      46 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 366   46 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.101   7.000  12.000      46 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.536   5.000  12.000      46 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 366   46 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
 366   46 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 366   46 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
  53   84   75  154   46 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                 366                   46 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
 366   46 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 366   46 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
 366   46 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             366 
                                            NA's 
                                              46 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 366   46 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
    366      46 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
 297   59   56 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.64    1.90    1.94    1.94    2.00    2.28     374 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486215 122850159 150757680 144635549 164106237 174163001 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   2   8 
 38   3 371 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07000 0.08000 0.08000 0.08641 0.08000 0.18000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   18.00   26.00   22.56   29.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
412 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.852   6.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.558   5.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   2.800   3.200   2.894   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
412 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   40.00   40.00   36.31   40.00   40.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
  59   94   89  170 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486983 122849471 150754512 145355607 164105277 174164653 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  1   5   8  26 
 12 330  41  29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1500  0.1500  0.1477  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   17.00   26.00   22.16   29.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 no 
412 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   5.238   6.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.558   5.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.930   1.950   2.080   2.075   2.080   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
 23 
412 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
6.67   10   13   40 
  12   29  330   41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 
  59   94  100  159 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99039605 108476663 117830950 116222490 122970252 132181428       277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    5   26 NA's 
   5   82   48  277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.1573  0.1600  0.1800     277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   12.00   27.00   20.99   29.00   31.00     277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 135  277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.822   8.000  12.000     277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   2.933   4.000   6.000     277 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.600   1.950   2.426   3.875   4.810     277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 135  277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10   13 26.7 NA's 
  53   47   35  277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 NA's 
  58   77  277 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   2  410 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132158639 132160672 132268717 140437313 132268763 167811822       399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   5    8    9 NA's 
   6    4    3  399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0900  0.1162  0.1500  0.1700     399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  14   21   30   31 NA's 
   2    3    2    6  399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  13  399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1    3    7 NA's 
   5    2    6  399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   7.000   5.231   8.000   8.000     399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a23d a25t a271 a46a NA's 
   6    2    2    3  399 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.070   1.400   1.800   3.642   6.000   6.800     399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
  13  399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4   20   40 NA's 
   3    4    6  399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2012 2013 2015 NA's 
   6    4    3  399 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-bl-a0c8-01a-11d-a273-01 tcga-bl-a13i-01a-11d-a276-05 
                           1                            1 
tcga-bl-a13j-01a-11d-a271-08                         NA's 
                           1                          409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
2cff629b-29ce-43c5-a814-3965a80eaebc 93f66218-cc1a-42be-9173-3fb50df3d97e 
                                   1                                    1 
9c0eb584-cf70-47cd-9902-83d3d5779b85                                 NA's 
                                   1                                  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
132158805 132160700 145675760      NA's 
        1         1         1       409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   1    5    8 NA's 
   1    1    1  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.08 0.15 0.17 NA's 
   1    1    1  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  13   14 NA's 
   1    2  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   3 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   1 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a271 a273 a276 NA's 
   1    1    1  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   a    b    c NA's 
   1    1    1  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
1.01 1.14  1.6 NA's 
   1    1    1  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
 6.7   20 NA's 
   2    1  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2013 NA's 
   3  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.disease_code:
fppp NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.project_code:
tcga NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-bl-a0c8-01a-11d-a276-05 tcga-bl-a13j-01a-11d-a273-01 
                           1                            1 
                        NA's 
                         410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
008f75de-3e64-4887-9ec2-85e20ebbaa67 0b4defce-bfc9-419d-9017-075475306fb6 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
132158797 145675767      NA's 
        1         1       410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   1    5 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.15 0.17 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  13   14 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   3 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   1 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
a273 a276 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   a    c NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
1.01 1.14 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
 6.7 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2013 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.disease_code:
fppp NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.project_code:
tcga NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-bl-a0c8-01a-11d-a46w-08 tcga-bl-a13j-01a-11d-a276-05 
                           1                            1 
                        NA's 
                         410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
075acfe9-bf63-4904-8ba7-df3f5b19ec4e 76c0c40d-0ae1-494e-96ac-0c043152fd68 
                                   1                                    1 
                                NA's 
                                 410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
132157308 145675719      NA's 
        1         1       410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   5    8 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.09 0.15 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   3   13 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   3    8 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   1   10 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
a276 a46w NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   c    g NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
1.01  1.8 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  23 NA's 
   2  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
 6.7   20 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2013 2015 NA's 
   1    1  410 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-bl-a13j-01a-11d-a46w-08                         NA's 
                           1                          411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
95694bd7-7201-4a6c-9ebd-1e456972cf76                                 NA's 
                                   1                                  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
132159824      NA's 
        1       411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   8 NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.08 NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   3 NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   8 NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   8 NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
a46w NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   b NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
 1.6 NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  23 NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
  20 NA's 
   1  411 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2015 NA's 
   1  411 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97483773 122849369 150756336 144637894 164105181 174163672 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2 
362  50 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 0.18 
  53  126  179   48    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   18.00   25.00   21.98   29.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
412 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.871   6.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.558   5.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.010   1.070   1.149   1.070   2.340 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
412 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7   7  10  13 
361   1  15  35 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
  59   94   89  170 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.39   25.43   36.07   37.94   48.04  124.41 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
412 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
412 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 0.18 
  53  126  179   48    6 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
412 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
412 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             412 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
412 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  412 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      374        38 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
 336   66   10 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    7.00   14.00   14.63   22.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
412 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   5.238   8.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  11.00   11.00   11.00   13.64   12.00   52.00 

patient.samples.sample.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.284   1.000   5.000     141 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  13  399 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   3.000   3.308   4.000  10.000     399 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0    1 NA's 
  11    2  399 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   5.000   7.385  10.000  25.000     399 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   3.846   5.000  22.000     399 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   6.615   5.000  49.000     399 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00   12.00   17.69   30.00   45.00     399 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  48.00   60.00   70.00   68.31   75.00   90.00     399 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  55.00   70.00   75.00   76.92   84.00   97.00     399 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
     1     12    399 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
412 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    1.00    2.00    3.67    4.00   90.00 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.6359  1.0000 70.0000 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   2.427   3.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.015   1.000  10.000 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    0.00    2.24    0.00   60.00 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00   19.50   17.62   27.25   75.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.00   70.00   80.00   77.72   90.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   70.00   80.00   78.12   85.00   99.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
    12    400 

patient.samples.sample.portions.portion.weight:
  6  10  20  25  30 
  1   8  24   1 378 

patient.samples.sample.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
  13   67   93   99  140 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 344   68 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 344   68 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   21.58   21.00   61.00      68 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.089   2.000   6.000     177 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  10   14   30 NA's 
  54   73  217   68 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   1    7   11 NA's 
  54  217   73   68 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 2014 NA's 
 127  217   68 

patient.samples.sample.sample_type:
primary tumor 
          412 

patient.samples.sample.sample_type_id:
  1 
412 

patient.samples.sample.vial_number:
  a   b 
409   3 

batchID:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   86.0   139.0   199.0   186.5   235.0   249.0     281 

studyName:
Bladder    NA's 
    131     281 

AdditionalSurgeryForNewTumorEventLocoReg:
  No  Yes NA's 
   6    2  404 

AddtlTreatmentNewTumorEventPharmaceuticalTherapy:
  No  Yes NA's 
   3    5  404 

AddtlTreatmentNewTumorEventRadiationTherapy:
  No  Yes NA's 
   4    4  404 

AdjuvantPostOperativePharmaceuticalTherapy:
     No Unknown     Yes    NA's 
     19      11      19     363 

AdjuvantPostOperativeRadiationTherapy:
     No Unknown     Yes    NA's 
     37      11       1     363 

Age.at.Diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  34.38   60.35   68.85   68.49   76.46   87.98     281 

AgeAtOnsetOfTobaccoSmoking:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   15.00   20.00   21.39   25.00   53.00     355 





CancerType3:
                                Colon                             Esophagus 
                                    1                                     1 
Kidney-except Pelvis,Multiple Myeloma                                Rectum 
                                    1                                     1 
                                 NA's 
                                  408 

CancerType4:
Lung Bronchus,Melanoma                   NA's 
                     1                    411 



Days.from.Diagnosis.to.Form.Completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  120.0   261.0   344.0   747.9   824.5  4740.0     281 

Days.from.Diagnosis.to.New.Tumor.Event:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   84.0   143.0   244.0   353.7   547.0   991.0     403 

Days.from.Diagnosis.to.New.Tumor.Event.Surgery:
 167  991 NA's 
   1    1  410 

Days.from.Diagnosis.to.Death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   152.8   258.5   351.1   548.2  1423.0     380 

Year.of.Diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1999    2009    2011    2010    2011    2012     281 

Days.to.Follow.up:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    82.5   187.0   414.4   320.5  3989.0     313 

DiagnosisSubtype:
Non-papillary     Papillary          NA's 
           88            41           283 


Ethnicity:
    Hispanic or Latino          Not Evaluated Not Hispanic or Latino 
                     2                      1                    117 
                  NA's 
                   292 

ExtracapsularExtension:
     No Unknown     Yes    NA's 
     22       9      21     360 

Gender:
Female   Male   NA's 
    32     99    281 

GleasonScore:
   6    7    8 NA's 
  19    8    1  384 

HistologicalSubtype:
Muscle invasive urothelial carcinoma (pT2 or above) 
                                                130 
                                               NA's 
                                                282 

IfExtracapsularExtensionPresent:
Extensive     Focal   Unknown      NA's 
        3         5         9       395 

IncidentalProstateCancer:
     No Unknown     Yes    NA's 
     77       3      28     304 


LymphNodesExamined:
  No  Yes NA's 
  22   92  298 

LymphovascularInvasion:
                          No Not mentioned in path report 
                          22                            8 
                         Yes                         NA's 
                          45                          337 


MethodOfInitialPathologicDiagnosis:
              Endoscopic biopsy          Other (please specify) 
                             15                              21 
Transurethral resection (TURBT)                            NA's 
                             94                             282 

NeoAdjuvantTherapy:
  No NA's 
 131  281 

NewTumorEventAfterInitialTreatment:
     No Unknown     Yes    NA's 
     40       1       8     363 

Nodes_Ex:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    8.00   14.00   18.45   26.00   85.00     313 

NumberOfPackYearsSmoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.50   20.00   30.50   37.18   48.25  132.00     334 

OccupationYearsWorked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   25.00   40.00   35.44   40.00   49.00     403 

OtherMetastaticSite:
ileocecum      Lung      NA's 
        1         1       410 


OtherSiteOfNewTumorEvent:
                                                          Brain 
                                                              1 
pelvic mass with adherence to pelvic sidewall and sigmoid colon 
                                                              1 
                                                         pelvis 
                                                              2 
                                                           NA's 
                                                            408 

PathologicDistantSpread:
  M0   M1   MX NA's 
  77    4   49  282 




PatientCompleteResponse:
  No  Yes NA's 
   4    2  406 

PatientHaveHistoryOfBladderCancer:
     No Unknown     Yes    NA's 
     34       1      12     365 

PatientInductionCourse:
     No Unknown     Yes    NA's 
      1       1       5     405 

PatientMaintenanceCourse:
     No Unknown     Yes    NA's 
      2       1       4     405 

PatientMonthsFromFirstBCG:
   8   26 NA's 
   1    1  410 

PatientReceivedBCGTreatment90Days:
  No NA's 
   8  404 

PatientTreated:
Bacillus Calmette-Guerin (BCG)  Transurethral resection alone 
                             6                              3 
                          NA's 
                           403 

Pos_Light_Micro:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.812   2.000  22.000     316 




PrimarySiteOfDisease:
Bladder    NA's 
    131     281 


PriorTumor:
  No  Yes NA's 
  98   33  281 

ProspectiveCollection:
  No  Yes NA's 
  42   89  281 

PtHeightCm:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  147.0   166.5   175.0   172.9   180.4   196.0     305 

PtWeightKg:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  48.00   70.00   81.00   84.98   92.05  292.00     302 

Race:
                    Asian Black or African American             Not Evaluated 
                        8                         6                         1 
                    White                      NA's 
                      108                       289 



Relative3:
Brother  Sister    NA's 
      3       1     408 

Relative4:
Child  NA's 
    1   411 

RetrospectiveCollection:
  No  Yes NA's 
  89   42  281 

SiteOfNewTumorEvent:
                 Liver                   Lung        Lymph Node Only 
                     1                      2                      2 
Other (please specify)                   NA's 
                     4                    403 


TumorGrade:
High Grade  Low Grade    Unknown       NA's 
       124          6          1        281 

TumorStage:
  Stage I  Stage II Stage III  Stage IV      NA's 
        1        37        44        45       285 

TumorStatus:
Tumor Free    Unknown With Tumor       NA's 
        71          5         42        294 

TypeOfNewTumorEvent:
                   Distant Metastasis Locoregional (Urothelial tumor event) 
                                    5                                     4 
                                 NA's 
                                  403 

VitalStatus:
Deceased   Living     NA's 
      32       99      281 

YearTobaccoSmokingCeased:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1950    1980    1995    1991    2008    2012     347 

}}
\keyword{datasets}
