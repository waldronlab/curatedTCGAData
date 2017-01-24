\name{ PCPG }
\alias{ PCPG }
\docType{data}
\title{ PCPG }

\details{
\preformatted{
> PCPG
A MultiAssayExperiment object of 6 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 6: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 187 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 187 columns 
 [3] Methylation: SummarizedExperiment with 485577 rows and 187 columns 
 [4] RPPAArray: ExpressionSet with 192 rows and 82 columns 
 [5] gistica: SummarizedExperiment with 24776 rows and 162 columns 
 [6] gistict: SummarizedExperiment with 24776 rows and 162 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( PCPG )
CharacterList of length 6
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( PCPG )
CharacterList of length 6
[["RNASeq2GeneNorm"]] TCGA-P7-A5NX-01A-11R-A35K-07 ...
[["miRNASeqGene"]] TCGA-P7-A5NX-01A-11R-A35M-13 ...
[["Methylation"]] TCGA-P7-A5NX-01A-11D-A35E-05 ... TCGA-XG-A823-01A-11D-A35J-05
[["RPPAArray"]] TCGA-P7-A5NX-01A-21-A43B-20 ... TCGA-WB-A81W-01A-21-A43B-20
[["gistica"]] TCGA-P7-A5NY-01A-12D-A35C-01 ... TCGA-XG-A823-01A-11D-A35H-01
[["gistict"]] TCGA-P7-A5NY-01A-12D-A35C-01 ... TCGA-XG-A823-01A-11D-A35H-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.2 Mb
2    miRNASeqGene  0.1 Mb
3     Methylation 67.4 Mb
4       RPPAArray    0 Mb
5         gistica  4.3 Mb
6         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   173 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
   6.00    6.00    1.24    0.26      NA 


--------------------------- 
Available sample meta-data: 
--------------------------- 


years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  19.00   35.00   46.00   47.33   58.50   83.00 

vital_status: 
  0   1 
173   6 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   88.0   148.5   452.5   901.0   715.2  3563.0     173 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     352     766    1069    1301    9634       6 

tumor_tissue_site: 
     adrenal gland extra-adrenal site 
               147                 32 

gender: 
female   male 
   101     78 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1988    2009    2011    2010    2012    2013 

radiation_therapy: 
  no  yes NA's 
 172    5    2 

karnofsky_performance_score: 
  70   80   90  100 NA's 
   1    2   13   46  117 

histological_type: 
                                paraganglioma 
                                           18 
paraganglioma; extra-adrenal pheochromocytoma 
                                           13 
                             pheochromocytoma 
                                          148 

number_of_lymph_nodes: 
   0    1    2   13 NA's 
  16    3    1    1  158 

race: 
american indian or alaska native                            asian 
                               1                                6 
       black or african american                            white 
                              20                              148 
                            NA's 
                               4 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     5                    138                     36 

admin.bcr: 
nationwide children's hospital 
                           179 

admin.day_of_dcc_upload: 
  1 
179 

admin.disease_code: 
pcpg 
 179 


admin.month_of_dcc_upload: 
 10 
179 

admin.patient_withdrawal.withdrawn: 
false 
  179 

admin.project_code: 
tcga 
 179 

admin.year_of_dcc_upload: 
2015 
 179 


patient.clinical_cqcf.consent_or_death_status: 
consented  deceased 
      178         1 


patient.clinical_cqcf.days_to_consent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-941.00  -33.25   -7.00  251.15    0.00 3661.00       1 

patient.clinical_cqcf.days_to_death: 
 755 NA's 
   1  178 

patient.clinical_cqcf.histological_type: 
                                paraganglioma 
                                           25 
paraganglioma; extra-adrenal pheochromocytoma 
                                            6 
                             pheochromocytoma 
                                          148 

patient.clinical_cqcf.history_of_neoadjuvant_treatment: 
 no yes 
178   1 

patient.clinical_cqcf.normal_tissue_anatomic_site: 
kidney  other   NA's 
     1      5    173 

patient.clinical_cqcf.normal_tissue_proximity: 
distal (>2cm)          NA's 
            6           173 

patient.clinical_cqcf.tumor_type: 
primary 
    179 

patient.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    7.00   13.00   15.05   21.00   30.00 

patient.days_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -30555  -21494  -16828  -17460  -13075   -7240 

patient.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   88.0   148.5   452.5   901.0   715.2  3563.0     173 

patient.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   137.0   395.0   792.9   928.0  9219.0       6 

patient.drugs.drug.2.bcr_drug_barcode: 
tcga-qr-a700-d68527 tcga-qt-a5xj-d43422 tcga-wb-a81q-d61216                NA's 
                  1                   1                   1                 176 

patient.drugs.drug.2.bcr_drug_uuid: 
ab46a29d-8a98-45be-ae90-9b67b052bcea b6282500-2bb2-40d8-b7c1-a4ca2a497893 
                                   1                                    1 
c2b9fc6e-d83a-4a2a-89ba-f71369952b80                                 NA's 
                                   1                                  176 

patient.drugs.drug.2.clinical_trail_drug_classification: 
vincristine        NA's 
          1         178 

patient.drugs.drug.2.day_of_form_completion: 
   4   15   21 NA's 
   1    1    1  176 

patient.drugs.drug.2.days_to_drug_therapy_end: 
 326  652  704 NA's 
   1    1    1  176 

patient.drugs.drug.2.days_to_drug_therapy_start: 
 113  535  620 NA's 
   1    1    1  176 

patient.drugs.drug.2.drug_name: 
dacarbazine     temodar        NA's 
          1           1         177 

patient.drugs.drug.2.measure_of_response: 
clinical progressive disease                         NA's 
                           2                          177 

patient.drugs.drug.2.month_of_form_completion: 
   5    7   12 NA's 
   1    1    1  176 

patient.drugs.drug.2.therapy_ongoing: 
  no NA's 
   3  176 

patient.drugs.drug.2.therapy_types.therapy_type: 
chemotherapy         NA's 
           3          176 

patient.drugs.drug.2.tx_on_clinical_trial: 
  no  yes NA's 
   2    1  176 

patient.drugs.drug.2.year_of_form_completion: 
2013 2014 NA's 
   1    2  176 

patient.drugs.drug.3.bcr_drug_barcode: 
tcga-qr-a700-d68529 tcga-wb-a81q-d61218                NA's 
                  1                   1                 177 

patient.drugs.drug.3.bcr_drug_uuid: 
67c44e48-2f4d-4976-bfe8-f377a0a8217f d9ab3eee-1f63-4f38-874d-a0782ba8dad6 
                                   1                                    1 
                                NA's 
                                 177 

patient.drugs.drug.3.clinical_trail_drug_classification: 
dacarbazine        NA's 
          1         178 

patient.drugs.drug.3.day_of_form_completion: 
   4   15 NA's 
   1    1  177 

patient.drugs.drug.3.days_to_drug_therapy_end: 
 652  749 NA's 
   1    1  177 

patient.drugs.drug.3.days_to_drug_therapy_start: 
 535  707 NA's 
   1    1  177 

patient.drugs.drug.3.drug_name: 
gemcitabine        NA's 
          1         178 

patient.drugs.drug.3.measure_of_response: 
clinical progressive disease                         NA's 
                           1                          178 

patient.drugs.drug.3.month_of_form_completion: 
   7   12 NA's 
   1    1  177 

patient.drugs.drug.3.therapy_ongoing: 
  no NA's 
   2  177 

patient.drugs.drug.3.therapy_types.therapy_type: 
chemotherapy         NA's 
           2          177 

patient.drugs.drug.3.tx_on_clinical_trial: 
  no  yes NA's 
   1    1  177 

patient.drugs.drug.3.year_of_form_completion: 
2014 NA's 
   2  177 

patient.drugs.drug.4.bcr_drug_barcode: 
tcga-wb-a81q-d61219                NA's 
                  1                 178 

patient.drugs.drug.4.bcr_drug_uuid: 
5cd70e48-b19f-4aaf-a773-600513ba7ec3                                 NA's 
                                   1                                  178 

patient.drugs.drug.4.day_of_form_completion: 
  15 NA's 
   1  178 

patient.drugs.drug.4.days_to_drug_therapy_end: 
 704 NA's 
   1  178 

patient.drugs.drug.4.days_to_drug_therapy_start: 
 620 NA's 
   1  178 

patient.drugs.drug.4.drug_name: 
cyclophosphamide             NA's 
               1              178 

patient.drugs.drug.4.measure_of_response: 
clinical progressive disease                         NA's 
                           1                          178 

patient.drugs.drug.4.month_of_form_completion: 
   7 NA's 
   1  178 

patient.drugs.drug.4.therapy_ongoing: 
  no NA's 
   1  178 

patient.drugs.drug.4.therapy_types.therapy_type: 
chemotherapy         NA's 
           1          178 

patient.drugs.drug.4.tx_on_clinical_trial: 
  no NA's 
   1  178 

patient.drugs.drug.4.year_of_form_completion: 
2014 NA's 
   1  178 



patient.drugs.drug.clinical_trail_drug_classification: 
             cvd cyclophosphamide             NA's 
               1                1              177 

patient.drugs.drug.day_of_form_completion: 
  15   21   22   24 NA's 
   1    1    1    2  174 

patient.drugs.drug.days_to_drug_therapy_end: 
 326  652  704 3452 NA's 
   1    1    1    1  175 

patient.drugs.drug.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  113.0   535.0   620.0   933.8   966.0  2435.0     174 

patient.drugs.drug.drug_name: 
cyclophosphamide, vincristine, and dacarbazine 
                                             1 
          cytoxan, vincristine and dacarbazine 
                                             1 
                                   vincristine 
                                             1 
                                        xeloda 
                                             1 
                                          NA's 
                                           175 

patient.drugs.drug.measure_of_response: 
clinical progressive disease             partial response 
                           3                            1 
                        NA's 
                         175 

patient.drugs.drug.month_of_form_completion: 
   5    7   10   11 NA's 
   1    1    1    2  174 

patient.drugs.drug.therapy_ongoing: 
  no NA's 
   5  174 

patient.drugs.drug.therapy_types.therapy_type: 
chemotherapy         NA's 
           5          174 

patient.drugs.drug.tx_on_clinical_trial: 
  no  yes NA's 
   2    3  174 

patient.drugs.drug.year_of_form_completion: 
2013 2014 NA's 
   1    4  174 

patient.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     5                    138                     36 



patient.follow_ups.follow_up.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   10.00   14.00   15.64   20.25   31.00     151 

patient.follow_ups.follow_up.2.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  405.0   682.2  1038.5  1137.9  1294.2  3138.0     151 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group: 
   0    1    2 NA's 
  16    6    1  156 

patient.follow_ups.follow_up.2.karnofsky_performance_score: 
  70   80   90  100 NA's 
   1    2    4   16  156 

patient.follow_ups.follow_up.2.lost_follow_up: 
  no  yes NA's 
  25    3  151 

patient.follow_ups.follow_up.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   6.500   6.893  11.250  12.000     151 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
 377 NA's 
   1  178 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name: 
imaging    NA's 
      1     178 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site: 
bone NA's 
   1  178 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
biochemical evidence of disease              distant metastasis 
                              1                               1 
                           NA's 
                            177 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
  26    2  151 

patient.follow_ups.follow_up.2.performance_status_scale_timing: 
        other pre-operative          NA's 
           11             2           166 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
        25          2        152 

patient.follow_ups.follow_up.2.postoperative_rx_tx: 
  no  yes NA's 
  27    1  151 

patient.follow_ups.follow_up.2.radiation_therapy: 
  no  yes NA's 
  27    1  151 

patient.follow_ups.follow_up.2.vital_status: 
alive  NA's 
   28   151 

patient.follow_ups.follow_up.2.year_of_form_completion: 
2014 2015 NA's 
  14   14  151 



patient.follow_ups.follow_up.3.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    9.00   17.00   15.29   20.00   29.00     172 

patient.follow_ups.follow_up.3.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    776    1263    1299    1410    1743    1946     173 

patient.follow_ups.follow_up.3.eastern_cancer_oncology_group: 
   0    1 NA's 
   5    1  173 

patient.follow_ups.follow_up.3.karnofsky_performance_score: 
  80  100 NA's 
   1    5  173 

patient.follow_ups.follow_up.3.lost_follow_up: 
  no  yes NA's 
   5    2  172 

patient.follow_ups.follow_up.3.month_of_form_completion: 
   3    4    6 NA's 
   2    4    1  172 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name: 
pathology      NA's 
        1       178 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site: 
other, specify           NA's 
             1            178 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
new primary tumor              NA's 
                1               178 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text: 
skin NA's 
   1  178 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
   5    1  173 

patient.follow_ups.follow_up.3.performance_status_scale_timing: 
other  NA's 
    4   175 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status: 
tumor free       NA's 
         6        173 

patient.follow_ups.follow_up.3.postoperative_rx_tx: 
  no NA's 
   6  173 

patient.follow_ups.follow_up.3.radiation_therapy: 
  no NA's 
   6  173 

patient.follow_ups.follow_up.3.vital_status: 
alive  NA's 
    6   173 

patient.follow_ups.follow_up.3.year_of_form_completion: 
2015 NA's 
   7  172 



patient.follow_ups.follow_up.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   13.00   22.50   18.87   26.00   31.00      61 

patient.follow_ups.follow_up.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     405     770    1129    1281    9634      62 

patient.follow_ups.follow_up.eastern_cancer_oncology_group: 
   0    1 NA's 
  41    8  130 

patient.follow_ups.follow_up.karnofsky_performance_score: 
  90  100 NA's 
  11   39  129 

patient.follow_ups.follow_up.lost_follow_up: 
  no  yes NA's 
  99   19   61 

patient.follow_ups.follow_up.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   9.000   7.992  10.000  12.000      61 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
2278 3282 6925 NA's 
   1    1    1  176 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_confirmed_diagnosis_method_name: 
  imaging pathology      NA's 
        2         1       176 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site: 
          lung other, specify           NA's 
             2              1            176 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
distant metastasis  new primary tumor               NA's 
                 2                  1                176 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text: 
right forearm          NA's 
            1           178 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment: 
2278 NA's 
   1  178 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_confirmed_diagnosis_method_name: 
imaging    NA's 
      1     178 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site: 
liver  NA's 
    1   178 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type: 
distant metastasis               NA's 
                 1                178 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    377     680    1044    2181    2780    6925     172 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name: 
  imaging pathology      NA's 
        2         4       173 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site: 
          bone  lymph node(s) other, specify           NA's 
             2              1              3            173 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
biochemical evidence of disease              distant metastasis 
                              1                               4 
              new primary tumor                            NA's 
                              2                             172 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text: 
  l breast left thigh    thyroid       NA's 
         1          1          1        176 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 100    7   72 

patient.follow_ups.follow_up.performance_status_scale_timing: 
                other post-adjuvant therapy         pre-operative 
                   32                     3                    10 
                 NA's 
                  134 

patient.follow_ups.follow_up.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       106          7         66 

patient.follow_ups.follow_up.postoperative_rx_tx: 
  no  yes NA's 
 116    1   62 

patient.follow_ups.follow_up.radiation_therapy: 
  no  yes NA's 
 115    2   62 

patient.follow_ups.follow_up.vital_status: 
alive  NA's 
  118    61 

patient.follow_ups.follow_up.year_of_form_completion: 
2014 2015 NA's 
 102   16   61 

patient.gender: 
female   male 
   101     78 

patient.history_of_neoadjuvant_treatment: 
 no yes 
178   1 


patient.history_pheo_or_para_include_benign: 
 no yes 
157  22 




patient.informed_consent_verified: 
yes 
179 

patient.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   3.000   3.631   3.000  12.000 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    106     501     606    1911    1476    6866     174 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_confirmed_diagnosis_method_name: 
  imaging pathology      NA's 
        3         2       174 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site: 
         liver           lung other, specify           NA's 
             1              2              2            174 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
distant metastasis  new primary tumor               NA's 
                 3                  2                174 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text: 
           r breast right paraganglioma                NA's 
                  1                   1                 177 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment: 
  93 1476 6866 NA's 
   1    1    1  176 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_confirmed_diagnosis_method_name: 
imaging    NA's 
      3     176 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site: 
         bone lymph node(s)          NA's 
            1             2           176 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type: 
  distant metastasis locoregional disease                 NA's 
                   2                    1                  176 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment: 
  93 NA's 
   1  178 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_confirmed_diagnosis_method_name: 
imaging    NA's 
      1     178 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site: 
lymph node(s)          NA's 
            1           178 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type: 
distant metastasis               NA's 
                 1                178 

patient.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment: 
 227 NA's 
   1  178 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_confirmed_diagnosis_method_name: 
imaging    NA's 
      1     178 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site: 
bone NA's 
   1  178 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type: 
distant metastasis               NA's 
                 1                178 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   133.5   606.0   954.1   829.5  6866.0     164 

patient.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name: 
  imaging pathology      NA's 
        4         9       166 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
  distant metastasis locoregional disease    new primary tumor 
                   6                    3                    6 
                NA's 
                 164 


patient.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 163   15    1 


patient.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       163         12          4 

patient.postoperative_rx_tx: 
  no  yes NA's 
 173    4    2 

patient.primary_pathology.age_at_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  19.00   35.00   46.00   47.33   58.50   83.00 

patient.primary_pathology.ct_scan: 
  no  yes NA's 
   3  164   12 

patient.primary_pathology.days_to_initial_pathologic_diagnosis: 
  0 
179 

patient.primary_pathology.disease_detected_on_screening: 
  no  yes NA's 
  41   91   47 

patient.primary_pathology.histological_type: 
                                paraganglioma 
                                           18 
paraganglioma; extra-adrenal pheochromocytoma 
                                           13 
                             pheochromocytoma 
                                          148 

patient.primary_pathology.laterality: 
bilateral      left     right      NA's 
        2        73        76        28 

patient.primary_pathology.lymph_node_examined_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   3.083   4.000  15.000     155 

patient.primary_pathology.number_of_lymphnodes_positive_by_he: 
   0    1    2   13 NA's 
  16    3    1    1  158 

patient.primary_pathology.outside_adrenal: 
multiple tumors outside the adrenal glands 
                                        10 
   single tumor outside the adrenal glands 
                                         7 
                                      NA's 
                                       162 

patient.primary_pathology.primary_lymph_node_presentation_assessment: 
  no  yes NA's 
 107   24   48 

patient.primary_pathology.tumor_tissue_site: 
     adrenal gland extra-adrenal site 
               147                 32 


patient.primary_pathology.year_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1988    2009    2011    2010    2012    2013 

patient.primary_therapy_outcome_success: 
complete remission/response  partial remission/response 
                        160                           1 
        progressive disease              stable disease 
                         10                           1 
                       NA's 
                          7 

patient.prior_dx: 
 no yes 
152  27 

patient.race: 
american indian or alaska native                            asian 
                               1                                6 
       black or african american                            white 
                              20                              148 
                            NA's 
                               4 

patient.radiation_therapy: 
  no  yes NA's 
 174    3    2 

patient.radiations.radiation.2.anatomic_treatment_site: 
primary tumor field                NA's 
                  1                 178 

patient.radiations.radiation.2.bcr_radiation_barcode: 
tcga-qr-a70p-r66730                NA's 
                  1                 178 

patient.radiations.radiation.2.bcr_radiation_uuid: 
de1d5805-3493-42e5-a8ee-2fed1abfe824                                 NA's 
                                   1                                  178 

patient.radiations.radiation.2.day_of_form_completion: 
  24 NA's 
   1  178 

patient.radiations.radiation.2.days_to_radiation_therapy_end: 
 704 NA's 
   1  178 

patient.radiations.radiation.2.days_to_radiation_therapy_start: 
 704 NA's 
   1  178 

patient.radiations.radiation.2.measure_of_response: 
radiographic progressive disease                             NA's 
                               1                              178 

patient.radiations.radiation.2.month_of_form_completion: 
  10 NA's 
   1  178 

patient.radiations.radiation.2.numfractions: 
gamma knife radiosurgery                     NA's 
                       1                      178 

patient.radiations.radiation.2.radiation_treatment_ongoing: 
  no NA's 
   1  178 

patient.radiations.radiation.2.radiation_type: 
external     NA's 
       1      178 

patient.radiations.radiation.2.year_of_form_completion: 
2014 NA's 
   1  178 

patient.radiations.radiation.anatomic_treatment_site: 
distant site         NA's 
           3          176 



patient.radiations.radiation.day_of_form_completion: 
   7   15   24 NA's 
   1    2    2  174 

patient.radiations.radiation.days_to_radiation_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    418     551    1468    1744    2266    4015     174 

patient.radiations.radiation.days_to_radiation_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    123     415    1252    1404    2266    2963     174 

patient.radiations.radiation.measure_of_response: 
                partial response radiographic progressive disease 
                               1                                2 
                  stable disease                             NA's 
                               1                              175 

patient.radiations.radiation.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     4.0     7.0     6.6    10.0    11.0     174 

patient.radiations.radiation.numfractions: 
   2    3 NA's 
   1    1  177 

patient.radiations.radiation.radiation_dosage: 
      129 2,000 cgy       310       600      NA's 
        1         1         1         1       175 

patient.radiations.radiation.radiation_treatment_ongoing: 
  no NA's 
   5  174 

patient.radiations.radiation.radiation_type: 
external systemic     NA's 
       2        3      174 

patient.radiations.radiation.units: 
 mci NA's 
   3  176 

patient.radiations.radiation.year_of_form_completion: 
2014 2015 NA's 
   4    1  174 

patient.tissue_prospective_collection_indicator: 
  no  yes NA's 
 158   19    2 

patient.tissue_retrospective_collection_indicator: 
  no  yes NA's 
  19  159    1 


patient.vital_status: 
alive  dead 
  173     6 

patient.year_of_form_completion: 
2013 2014 
  34  145 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason: 
              case matched expression control 
                                            2 
primary tumor and additional primary tumor(s) 
                                            3 
        primary tumor and metastatic tumor(s) 
                                            1 
                                         NA's 
                                          173 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2: 
     case matched methylation control primary tumor and metastatic tumor(s) 
                                    2                                     1 
                                 NA's 
                                  176 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3: 
germline comparison                NA's 
                  2                 177 

patient.bcr_canonical_check.bcr_patient_canonical_status: 
       canonical canonical - plus 
             173                6 

patient.biospecimen_cqcf.consent_or_death_status: 
consented  deceased 
      178         1 


patient.biospecimen_cqcf.days_to_sample_procurement: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-427.000   -1.000    0.000   -2.039    0.000  153.000 

patient.biospecimen_cqcf.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     4                    111                     64 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site: 
adrenal gland         other 
          147            32 

patient.biospecimen_cqcf.histological_type: 
                                paraganglioma 
                                           25 
paraganglioma; extra-adrenal pheochromocytoma 
                                            6 
                             pheochromocytoma 
                                          148 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment: 
 no yes 
178   1 

patient.biospecimen_cqcf.laterality: 
 left right  NA's 
   75    77    27 

patient.biospecimen_cqcf.method_of_sample_procurement: 
surgical resection 
               179 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -366.0     0.0    33.0   360.1   228.2  2755.0     167 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0930  0.1114  0.1260  7.2554  0.1730 50.0000     172 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm: 
   cdrop nanodrop    qubit     NA's 
       1        5        1      172 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty: 
  15   16   20 NA's 
   2    2    3  172 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   85.8   123.5   158.7   165.7   184.3   300.0     172 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement: 
            blood draw other (please specify)     surgical resection 
                    10                      1                      1 
                  NA's 
                   167 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site: 
other  NA's 
    1   178 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
            1           178 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number: 
   1    2 NA's 
  17    6  156 


patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement: 
   0 3269 NA's 
   2    1  176 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement: 
blood draw       NA's 
         3        176 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number: 
   1 NA's 
   7  172 

patient.biospecimen_cqcf.normal_controls.normal_control.4.bcr_sample_uuid: 
b1ded7e6-20f5-4cb6-b104-17af110a7055 b973c726-b4ae-4d1c-a6f1-1729ea89e50e 
                                   1                                    1 
                                NA's 
                                 177 

patient.biospecimen_cqcf.normal_controls.normal_control.4.vial_number: 
   2 NA's 
   2  177 

patient.biospecimen_cqcf.normal_controls.normal_control.5.bcr_sample_uuid: 
c0a16863-77e2-4c90-bfd8-b124bbd6de5f                                 NA's 
                                   1                                  178 

patient.biospecimen_cqcf.normal_controls.normal_control.5.days_to_normal_sample_procurement: 
   0 NA's 
   1  178 

patient.biospecimen_cqcf.normal_controls.normal_control.5.method_of_normal_sample_procurement: 
blood draw       NA's 
         1        178 

patient.biospecimen_cqcf.normal_controls.normal_control.5.vial_number: 
   1 NA's 
   1  178 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -941.0   -31.0    -4.0   284.1    34.0  9180.0       6 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0158  0.1130  0.2000  0.9524  0.3565 43.5000      76 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.24   15.00   15.00   17.07   17.00   79.09      76 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.70   49.85   80.20  136.32  150.00 1400.00      76 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement: 
        blood draw surgical resection               NA's 
               168                  5                  6 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site: 
kidney  other   NA's 
     1      4    174 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
            5           174 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number: 
  1   2   4 
177   1   1 


patient.biospecimen_cqcf.path_confirm_diagnosis_matching: 
yes 
179 

patient.biospecimen_cqcf.path_confirm_report_attached: 
yes 
179 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics: 
 no yes 
  1 178 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics: 
 no yes 
  2 177 

patient.biospecimen_cqcf.race: 
american indian or alaska native                            asian 
                               1                                5 
       black or african american                            white 
                              20                              127 
                            NA's 
                              26 

patient.biospecimen_cqcf.sample_prescreened: 
yes 
179 

patient.biospecimen_cqcf.site_of_disease: 
adrenal gland extra-adrenal 
          147            32 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid: 
5bc1936e-49d1-4843-99bf-c032df997f8e 925c665f-26c1-4a72-9533-0295bcb8381b 
                                   1                                    1 
f3effb76-35a6-4efe-ab7f-5f03f4e3a667 faf6ade6-439b-4ccb-b7f9-e513f4d046c3 
                                   1                                    1 
                                NA's 
                                 175 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent: 
   0 NA's 
   4  175 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent: 
  90   98  100 NA's 
   1    1    2  175 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight: 
  30   71  153  228 NA's 
   1    1    1    1  175 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number: 
   1 NA's 
   4  175 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.536   0.000  80.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  40.00   80.00   90.00   87.51   99.50  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   30.0    50.0   100.0   188.1   220.0  1800.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number: 
  1   2 
178   1 

patient.biospecimen_cqcf.vessel_used: 
biospecimen storage bag                cassette                cryomold 
                      5                       1                      52 
               cryovial  other (please specify) 
                     88                      33 

patient.days_to_index: 
  0 
179 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
primary tumor and additional primary tumor(s) 
                                            3 
        primary tumor and metastatic tumor(s) 
                                            1 
                                         NA's 
                                          175 



patient.samples.sample.2.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   26.0   308.5   730.0  1132.6  1564.0  9332.0 

patient.samples.sample.2.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0    82.5    95.0   105.0   107.5   200.0     173 

patient.samples.sample.2.is_ffpe: 
 no 
179 

patient.samples.sample.2.oct_embedded: 
false  true 
  177     2 

patient.samples.sample.2.pathology_report_file_name: 
tcga-p7-a5ny.c46323fe-b5d9-45d4-b87b-1f8c89e8397d.pdf 
                                                    1 
tcga-qr-a6gz.9bcad0f3-eca4-4522-843a-aecdef712dc2.pdf 
                                                    1 
tcga-rw-a686.ab1b615a-eba8-4004-a508-0622b1fb29d5.pdf 
                                                    1 
tcga-sr-a6mx.db0b2d04-686f-4935-a5e5-deb421367016.pdf 
                                                    1 
                                                 NA's 
                                                  175 

patient.samples.sample.2.pathology_report_uuid: 
9bcad0f3-eca4-4522-843a-aecdef712dc2 ab1b615a-eba8-4004-a508-0622b1fb29d5 
                                   1                                    1 
c46323fe-b5d9-45d4-b87b-1f8c89e8397d db0b2d04-686f-4935-a5e5-deb421367016 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio: 
1.79  1.8 1.85 1.96 NA's 
   1    1    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode: 
tcga-p7-a5ny-05a-11r-a35m-13 tcga-qr-a6gz-05a-11r-a35m-13 
                           1                            1 
tcga-rw-a686-06a-11r-a35m-13 tcga-sr-a6mx-05a-11r-a35n-13 
                           1                            1 
                        NA's 
                         175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid: 
4fd8626c-1e35-4c16-a5a3-f95abe1aa58a 50da60b8-ae20-4fb4-936a-eb84415bb216 
                                   1                                    1 
5e700cdd-2951-4f9f-9fec-fdb7b5541424 6583a928-6d7d-4a96-a401-1fce8fa98fb3 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
160339322 160339357 160339367 160339435      NA's 
        1         1         1         1       175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
  13 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.16 0.17 0.18 NA's 
   2    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
  18 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
  12 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   1    3    5   11 NA's 
   1    1    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
a35m a35n NA's 
   3    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row: 
   c    e    f NA's 
   2    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
 3.2  3.4  3.6 NA's 
   2    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  23 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
  20 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2013 NA's 
   4  175 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160337978 167802956 167803005 167636943 167804590 167804639 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
  7   8 
  4 175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
0.16 0.17 0.18  0.5 
   2    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
  8  18 
175   4 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
  7  12 
175   4 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.106   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
a35k a35l a441 a442 
   3    1   87   88 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
2.08 2.21 2.34   40 
   2    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
 23 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
 13  80 
  4 175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2013 2014 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount: 
19.05 30.63 35.49 41.95  NA's 
    1     1     1     1   175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna 
                 175                    4 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id: 
  r   w 
  4 175 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration: 
0.16 0.17 0.18 NA's 
   2    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
 175    4 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
 175    4 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             179 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                   repli-g 
                        4                       175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
0.77 1.12 1.22 NA's 
   1    1    2  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue: 
   7  7.1    8  8.8 NA's 
   1    1    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec    NA's 
      4     175 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2 NA's 
   6    1  172 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode: 
tcga-p7-a5ny-05a-11w-a43z-08 tcga-qr-a6gz-05a-11w-a43z-08 
                           1                            1 
tcga-rw-a686-06a-11w-a43z-08 tcga-sr-a6mx-05a-11w-a440-08 
                           1                            1 
                        NA's 
                         175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid: 
150314eb-f06b-4830-99f0-d9e9b0d187ce 3b84702f-2ca1-45cc-a3af-b3c4aac8e4cd 
                                   1                                    1 
bd2bd82b-5279-4d29-af48-ba06a782f69e d1e21129-6b15-430f-8c34-e2bf87d69ce9 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
167802252 167802275 167802300 167803356      NA's 
        1         1         1         1       175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   8 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   7 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
  12 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id: 
a43z a440 NA's 
   3    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row: 
   c    f    g NA's 
   2    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2014 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                   4                  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_barcode: 
tcga-p7-a5ny-05a-11w tcga-qr-a6gz-05a-11w tcga-rw-a686-06a-11w 
                   1                    1                    1 
tcga-sr-a6mx-05a-11w                 NA's 
                   1                  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_uuid: 
04993369-afd2-4b71-a8f1-008177b40203 2798b3c2-5cd2-46f4-afc9-0fe907e32720 
                                   1                                    1 
43a60648-5b0f-4f74-b199-75d654534198 955951d0-a09c-49e4-a312-78304e50c0e2 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
      4     175 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence: 
   1 NA's 
   4  175 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160336829 160337896 160337950 160338237 160338672 160339548 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  8 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
0.06 0.07 0.08 0.09 
   1   72   89   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
  6 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
  1 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.263   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
a35b a35d a35g a35i 
  87    3   88    1 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
2.4 2.8 3.2 3.6 
  1  72  89  17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 23 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
 40 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2014 
 179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode: 
tcga-p7-a5ny-05a-11d-a35e-05 tcga-qr-a6gz-05a-11d-a35e-05 
                           1                            1 
tcga-rw-a686-06a-11d-a35e-05 tcga-sr-a6mx-05a-11d-a35j-05 
                           1                            1 
                        NA's 
                         175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid: 
5af5fdc5-f06f-491c-bcbc-fef68c2392ee 72f44cf2-a516-4406-a560-e642bc4acd9b 
                                   1                                    1 
888abf7e-143d-44d0-b8e6-fb1260c4867d cc986a8e-2ea9-4dbd-bce3-519a9bb4874f 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
160336892 160338444 160338467 160338588      NA's 
        1         1         1         1       175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   5 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
0.14 0.17 NA's 
   2    2  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
  18 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
  12 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
  12 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id: 
a35e a35j NA's 
   3    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row: 
   c    f    g NA's 
   2    1    1  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
1.82 2.21 NA's 
   2    2  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  23 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
  13 NA's 
   4  175 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2013 NA's 
   4  175 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160336863 160337616 160337762 160337736 160337812 160339068 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  1 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.13 0.14 0.15 0.16 0.17 
   1   38   63   47   30 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
  6 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
  1 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.263   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
a35a a35c a35f a35h 
  87    3   88    1 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
0.87 0.94 1.01 1.07 1.14 
   1   38   63   47   30 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 23 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.7 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2014 
 179 

patient.samples.sample.2.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.26   10.43   12.95   18.70   19.86  131.16 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type: 
dna 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id: 
  d 
179 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration: 
0.13 0.14 0.15 0.16 0.17 
   1   38   63   47   30 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
yes 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             179 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
            adna preparation type pre-extracted dna received by tss 
                               76                               103 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen 
      179 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence: 
   1    2 NA's 
   6    1  172 



patient.samples.sample.2.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00   14.00   23.00   19.98   27.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe: 
 no 
179 

patient.samples.sample.2.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   5.000   7.000   7.575  10.000  12.000 

patient.samples.sample.2.portions.portion.portion_number: 
  1  11  12  21 
172   4   1   2 

patient.samples.sample.2.portions.portion.portion_sequence: 
   1    2 NA's 
 176    2    1 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
   6  173 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   0    2 NA's 
   5    1  173 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration: 
   0 NA's 
   6  173 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis: 
   0 NA's 
   6  173 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0 NA's 
   6  173 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells: 
   0  100 NA's 
   4    2  173 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells: 
   0    5   50 NA's 
   3    2    1  173 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells: 
   0   50   95  100 NA's 
   2    1    2    1  173 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei: 
   0   75   80   95 NA's 
   2    1    1    2  173 

patient.samples.sample.2.portions.portion.slides.slide.section_location: 
 top NA's 
   6  173 

patient.samples.sample.2.portions.portion.weight: 
   2   30 NA's 
  70    6  103 

patient.samples.sample.2.portions.portion.year_of_creation: 
2013 
 179 

patient.samples.sample.2.portions.shipment_portion.bcr_shipment_portion_uuid: 
6865218f-9599-4f77-9786-3f581172f6a5 b39d162d-35f2-48e7-a22b-bc913599b1cf 
                                   1                                    1 
                                NA's 
                                 177 

patient.samples.sample.2.portions.shipment_portion.center_id: 
  20 NA's 
   2  177 

patient.samples.sample.2.portions.shipment_portion.is_ffpe: 
  no NA's 
   2  177 

patient.samples.sample.2.portions.shipment_portion.plate_id: 
a43b NA's 
   2  177 

patient.samples.sample.2.portions.shipment_portion.portion_number: 
  21 NA's 
   2  177 

patient.samples.sample.2.portions.shipment_portion.portion_sequence: 
   2 NA's 
   2  177 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode: 
tcga-rw-a686-06a-21-a43b-20 tcga-sr-a6mx-05a-21-a43b-20 
                          1                           1 
                       NA's 
                        177 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment: 
  10 NA's 
   2  177 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment: 
   6 NA's 
   2  177 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment: 
2014 NA's 
   2  177 

patient.samples.sample.2.sample_type: 
additional - new primary     blood derived normal               metastatic 
                       3                      173                        1 
     solid tissue normal 
                       2 

patient.samples.sample.2.sample_type_id: 
  5   6  10  11 
  3   1 173   2 

patient.samples.sample.2.vial_number: 
  a   b   c   d 
160  12   5   2 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
                  germline comparison primary tumor and metastatic tumor(s) 
                                    2                                     1 
                                 NA's 
                                  176 



patient.samples.sample.3.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  125.0   164.0   310.0   469.3   579.0  1364.0     172 

patient.samples.sample.3.initial_weight: 
  60  100  180 NA's 
   2    1    1  175 

patient.samples.sample.3.is_ffpe: 
  no NA's 
   7  172 

patient.samples.sample.3.oct_embedded: 
false  true  NA's 
    4     3   172 

patient.samples.sample.3.pathology_report_file_name: 
tcga-sr-a6mx.5e8f4633-9672-4aea-8898-acdda3539057.pdf 
                                                    1 
                                                 NA's 
                                                  178 

patient.samples.sample.3.pathology_report_uuid: 
5e8f4633-9672-4aea-8898-acdda3539057                                 NA's 
                                   1                                  178 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio: 
1.84 1.87  1.9 2.02 NA's 
   1    1    1    1  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode: 
tcga-p8-a5kc-11a-11r-a35m-13 tcga-p8-a5kd-11a-11r-a35m-13 
                           1                            1 
tcga-sq-a6i4-11a-11r-a35n-13 tcga-sr-a6mx-06a-11r-a35n-13 
                           1                            1 
                        NA's 
                         175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid: 
060c26b8-f352-4660-8362-ffb2715d90e3 82f9e456-7c53-482c-b643-ff0e931b801f 
                                   1                                    1 
9662fc9f-5470-4cf4-a0d1-f9cbcc492bc1 eefa3954-22bb-4e39-a882-5b6e81717c64 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched expression control                            NA's 
                              2                             177 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
160339319 160339343 160339420 160339460      NA's 
        1         1         1         1       175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
  13 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.16 0.17 0.18 NA's 
   1    1    2  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
  18 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
  12 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   1    4    5 NA's 
   2    1    1  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
a35m a35n NA's 
   2    2  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row: 
   c    e    f    g NA's 
   1    1    1    1  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
 3.2  3.4  3.6 NA's 
   1    1    2  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  23 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
  20 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2013 NA's 
   4  175 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched expression control                            NA's 
                              2                             177 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
160337975 160338374 160338788 163538189 167804596 167804618       172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   7    8 NA's 
   4    3  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
0.16 0.17 0.18  0.5 NA's 
   1    1    2    3  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   8   18 NA's 
   3    4  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   7   12 NA's 
   3    4  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   3.571   4.500  10.000     172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
a35k a35l a441 NA's 
   2    2    3  172 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
2.08 2.21 2.34   40 NA's 
   1    1    2    3  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  13   80 NA's 
   4    3  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2013 2014 NA's 
   4    3  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount: 
24.58 29.44 43.29 49.06  NA's 
    1     1     1     1   175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna                 NA's 
                   3                    4                  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    w NA's 
   4    3  172 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration: 
0.16 0.17 0.18 NA's 
   1    1    2  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
   3  176 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
   3  176 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                   repli-g                      NA's 
                        4                         3                       172 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
1.04 1.13 1.42 1.88 NA's 
   1    1    1    1  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue: 
 8.6  8.7    9  9.7 NA's 
   1    1    1    1  175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec    NA's 
      4     175 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence: 
   1 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode: 
tcga-p8-a5kc-11a-11w-a43z-08 tcga-p8-a5kd-11a-11w-a43z-08 
                           1                            1 
tcga-sq-a6i4-11a-11w-a440-08 tcga-sr-a6mx-06a-11w-a440-08 
                           1                            1 
                        NA's 
                         175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid: 
54f4c929-b007-49a7-ab0e-add8d9cecb0d 5ead36fd-a2ba-4b7c-a418-4952054d1f26 
                                   1                                    1 
bbe8e5ac-4d99-4702-84f9-afe16d30f728 c84cc312-9fe1-4572-8333-7542feb0b422 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
167802276 167802299 167803355 167803379      NA's 
        1         1         1         1       175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   8 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   7 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
  12 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id: 
a43z a440 NA's 
   2    2  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row: 
   b    d    e NA's 
   1    2    1  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2014 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                   4                  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_barcode: 
tcga-p8-a5kc-11a-11w tcga-p8-a5kd-11a-11w tcga-sq-a6i4-11a-11w 
                   1                    1                    1 
tcga-sr-a6mx-06a-11w                 NA's 
                   1                  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_uuid: 
1364e579-7600-49f9-aa52-b2b4b0cd10ae 1ae50686-df1b-400e-b181-9f12edd4d2ce 
                                   1                                    1 
204714a4-512d-4517-a3c3-3d836cbf785b c6965c4d-0b04-4531-b53d-3fc57f9e94e3 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
      4     175 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence: 
   1 NA's 
   4  175 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
160338657 160338697 160338948 160339013 160339259 160339571       172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
   8 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
0.07 0.08 0.09 NA's 
   3    3    1  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   6 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   1 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   1    3   10   12 NA's 
   1    1    1    4  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
a35b a35d a35i NA's 
   3    2    2  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row: 
   b    d    e    f NA's 
   3    2    1    1  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
 2.8  3.2  3.6 NA's 
   3    3    1  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  23 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
  40 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2014 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode: 
tcga-p8-a5kc-11a-11d-a35e-05 tcga-p8-a5kd-11a-11d-a35e-05 
                           1                            1 
tcga-sq-a6i4-11a-11d-a35j-05 tcga-sr-a6mx-06a-11d-a35j-05 
                           1                            1 
                        NA's 
                         175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid: 
7a9e0da4-8239-407b-b51d-8f9134f209a7 d30f306b-45c1-472f-9aa1-db02a848bf43 
                                   1                                    1 
dbf9fe44-7e80-44a3-9957-85682a6141b7 df21bba1-af8b-4802-a83b-493e7758a1db 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched methylation control                             NA's 
                               2                              177 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
160338468 160338491 160338587 160338611      NA's 
        1         1         1         1       175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   5 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
0.15 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
  18 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
  12 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
  12 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id: 
a35e a35j NA's 
   2    2  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row: 
   b    d    e NA's 
   1    2    1  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
1.95 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  23 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
  13 NA's 
   4  175 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2013 NA's 
   4  175 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
160336887 160337735 160337793 160338013 160338438 160339067       172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   1 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.15 0.17 NA's 
   6    1  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   6 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   1 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   1    3   10   12 NA's 
   1    1    1    4  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
a35a a35c a35h NA's 
   3    2    2  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_row: 
   b    d    e    f NA's 
   3    2    1    1  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
1.01 1.14 NA's 
   6    1  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
 6.7 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2014 NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.64   11.64   22.35   20.65   28.91   33.46     172 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type: 
 dna NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id: 
   d NA's 
   7  172 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration: 
0.15 0.17 NA's 
   6    1  172 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             172 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
   7  172 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
            adna preparation type pre-extracted dna received by tss 
                                4                                 3 
                             NA's 
                              172 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen      NA's 
        7       172 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence: 
   1 NA's 
   4  175 



patient.samples.sample.3.portions.portion.day_of_creation: 
   4    6   12   29 NA's 
   2    1    2    2  172 

patient.samples.sample.3.portions.portion.is_ffpe: 
  no NA's 
   7  172 

patient.samples.sample.3.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     2.0     4.0     4.0     5.5     7.0     172 

patient.samples.sample.3.portions.portion.portion_number: 
   1   11 NA's 
   3    4  172 

patient.samples.sample.3.portions.portion.portion_sequence: 
   1 NA's 
   7  172 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_barcode: 
tcga-p8-a5kc-11a-01-ts1 tcga-p8-a5kd-11a-01-tsa tcga-sq-a6i4-11a-01-ts1 
                      1                       1                       1 
tcga-sr-a6mx-06a-01-ts1                    NA's 
                      1                     175 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_uuid: 
48bb1f62-b99a-490b-8b6a-9060817618f1 6543d8d3-a78f-42ec-b813-db1520e96953 
                                   1                                    1 
853da773-8f33-43f1-a838-0a7bcc30a9b9 f185a97e-b68e-4858-b21e-4d57e10482ff 
                                   1                                    1 
                                NA's 
                                 175 

patient.samples.sample.3.portions.portion.slides.slide.image_file_name: 
tcga-p8-a5kc-11a-01-ts1.48bb1f62-b99a-490b-8b6a-9060817618f1.svs 
                                                               1 
tcga-p8-a5kd-11a-01-tsa.853da773-8f33-43f1-a838-0a7bcc30a9b9.svs 
                                                               1 
tcga-sq-a6i4-11a-01-ts1.f185a97e-b68e-4858-b21e-4d57e10482ff.svs 
                                                               1 
tcga-sr-a6mx-06a-01-ts1.6543d8d3-a78f-42ec-b813-db1520e96953.svs 
                                                               1 
                                                            NA's 
                                                             175 

patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
   4  175 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   1 NA's 
   4  175 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration: 
   0 NA's 
   4  175 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis: 
   0 NA's 
   4  175 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0 NA's 
   4  175 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells: 
   5  100 NA's 
   1    3  175 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells: 
   0 NA's 
   4  175 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells: 
   0   95 NA's 
   3    1  175 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei: 
   0   80 NA's 
   3    1  175 

patient.samples.sample.3.portions.portion.slides.slide.section_location: 
 top NA's 
   4  175 

patient.samples.sample.3.portions.portion.weight: 
  20   30 NA's 
   1    3  175 

patient.samples.sample.3.portions.portion.year_of_creation: 
2013 NA's 
   7  172 

patient.samples.sample.3.portions.shipment_portion.bcr_shipment_portion_uuid: 
a77873f1-988b-4862-b32d-ce7dcc9aa5c3                                 NA's 
                                   1                                  178 

patient.samples.sample.3.portions.shipment_portion.center_id: 
  20 NA's 
   1  178 

patient.samples.sample.3.portions.shipment_portion.is_ffpe: 
  no NA's 
   1  178 

patient.samples.sample.3.portions.shipment_portion.plate_id: 
a43b NA's 
   1  178 

patient.samples.sample.3.portions.shipment_portion.portion_number: 
  21 NA's 
   1  178 

patient.samples.sample.3.portions.shipment_portion.portion_sequence: 
   2 NA's 
   1  178 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode: 
tcga-sr-a6mx-06a-21-a43b-20                        NA's 
                          1                         178 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_day_of_shipment: 
  10 NA's 
   1  178 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_month_of_shipment: 
   6 NA's 
   1  178 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_year_of_shipment: 
2014 NA's 
   1  178 

patient.samples.sample.3.sample_type: 
blood derived normal           metastatic  solid tissue normal 
                   3                    1                    3 
                NA's 
                 172 

patient.samples.sample.3.sample_type_id: 
   6   10   11 NA's 
   1    3    3  172 

patient.samples.sample.3.vial_number: 
   a NA's 
   7  172 

patient.samples.sample.4.bcr_sample_barcode: 
tcga-sr-a6mx-10a             NA's 
               1              178 

patient.samples.sample.4.bcr_sample_uuid: 
130eadd2-2dc4-475d-9de8-ac08e4fd9d64                                 NA's 
                                   1                                  178 

patient.samples.sample.4.days_to_collection: 
 139 NA's 
   1  178 

patient.samples.sample.4.is_ffpe: 
  no NA's 
   1  178 

patient.samples.sample.4.oct_embedded: 
false  NA's 
    1   178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode: 
tcga-sr-a6mx-10a-01w-a442-08                         NA's 
                           1                          178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid: 
150b0473-5de0-45c7-ab54-07365a5b731b                                 NA's 
                                   1                                  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
167802988      NA's 
        1       178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   8 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
 0.5 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   8 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   7 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   5 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
a442 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row: 
   b NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
  40 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  80 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2014 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                 NA's 
                   1                  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id: 
   w NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_barcode: 
tcga-sr-a6mx-10a-01w                 NA's 
                   1                  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_uuid: 
e0fd7df3-f717-41f5-b837-523641cdb362                                 NA's 
                                   1                                  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
      1     178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode: 
tcga-sr-a6mx-10a-01d-a35g-08                         NA's 
                           1                          178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid: 
99e20de7-1106-4a42-a936-dfcc7017ed47                                 NA's 
                                   1                                  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
160337932      NA's 
        1       178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
   8 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
0.08 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   6 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   1 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   5 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
a35g NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row: 
   b NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
 3.2 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  23 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
  40 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2014 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode: 
tcga-sr-a6mx-10a-01d-a35f-01                         NA's 
                           1                          178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid: 
b3291590-2d2a-4106-b06d-3b28f3d65a98                                 NA's 
                                   1                                  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
160337644      NA's 
        1       178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   1 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.15 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   6 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   1 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   5 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
a35f NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row: 
   b NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
1.01 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
 6.7 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2014 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.amount: 
62.6 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type: 
 dna NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id: 
   d NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode: 
tcga-sr-a6mx-10a-01d                 NA's 
                   1                  178 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid: 
e45b4db6-267a-41fe-9f04-5f15ccf13d84                                 NA's 
                                   1                                  178 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration: 
0.15 NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             178 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
   1  178 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
adna preparation type                  NA's 
                    1                   178 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen      NA's 
        1       178 

patient.samples.sample.4.portions.portion.bcr_portion_barcode: 
tcga-sr-a6mx-10a-01                NA's 
                  1                 178 

patient.samples.sample.4.portions.portion.bcr_portion_uuid: 
91a5bcba-c3c1-4709-8cc4-507c58cd371c                                 NA's 
                                   1                                  178 

patient.samples.sample.4.portions.portion.day_of_creation: 
  15 NA's 
   1  178 

patient.samples.sample.4.portions.portion.is_ffpe: 
  no NA's 
   1  178 

patient.samples.sample.4.portions.portion.month_of_creation: 
   7 NA's 
   1  178 

patient.samples.sample.4.portions.portion.portion_number: 
   1 NA's 
   1  178 

patient.samples.sample.4.portions.portion.portion_sequence: 
   1 NA's 
   1  178 

patient.samples.sample.4.portions.portion.weight: 
   2 NA's 
   1  178 

patient.samples.sample.4.portions.portion.year_of_creation: 
2013 NA's 
   1  178 

patient.samples.sample.4.sample_type: 
blood derived normal                 NA's 
                   1                  178 

patient.samples.sample.4.sample_type_id: 
  10 NA's 
   1  178 

patient.samples.sample.4.vial_number: 
   a NA's 
   1  178 



patient.samples.sample.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   26.0   308.5   726.0  1125.2  1517.0  9332.0 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3: 
6f1f7dce-b986-47eb-93f1-2c005c7d8f2b                                 NA's 
                                   1                                  178 

patient.samples.sample.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   10.0    50.0    80.0   139.7   160.0   980.0 

patient.samples.sample.is_ffpe: 
 no 
179 

patient.samples.sample.oct_embedded: 
false  true 
   53   126 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.730   1.830   1.870   1.871   1.910   2.260 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160338214 160339344 160339391 160339386 160339440 160339487 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
 13 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.15 0.16 0.17 0.18 
  10   49   67   53 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
 18 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
 12 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.464   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
a35m a35n 
  89   90 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
  3 3.2 3.4 3.6 
 10  49  67  53 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
 23 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
 20 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2013 
 179 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160336803 160337994 160338045 160338322 160338770 160338815 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
  7 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
0.15 0.16 0.17 0.18 
  10   49   67   53 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
 18 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
 12 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.464   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
a35k a35l 
  89   90 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
1.95 2.08 2.21 2.34 
  10   49   67   53 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
 23 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
 13 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2013 
 179 

patient.samples.sample.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.49   22.59   31.00   30.89   39.08   66.30 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type: 
rna 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id: 
  r 
179 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration: 
0.15 0.16 0.17 0.18 
  10   49   67   53 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             179 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
   allprep rna extraction mirvana (allprep dna) rna 
                        1                       178 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.360   1.085   1.250   1.241   1.450   2.270 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.600   8.200   8.155   8.700   9.500 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec 
    179 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2    3 NA's 
 153   22    1    3 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167789705 167802286 167802334 167802597 167803344 167803391 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
  8 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
0.5 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
  8 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
  7 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.101   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id: 
a43z a440 
  90   89 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
 40 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
 23 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
 80 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2014 
 179 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna 
                 179 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id: 
  w 
179 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
yes 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
yes 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             179 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g 
    179 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2    3 NA's 
 153   22    1    3 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160338153 160338952 160339000 160339180 160339532 160339583 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  8 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
0.07 0.08 0.09 
  66  104    9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
  6 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
  1 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.101   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
a35d a35i 
  90   89 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
2.8 3.2 3.6 
 66 104   9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 23 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
 40 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2014 
 179 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160336844 160338468 160338517 160338405 160338568 160338623 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
  5 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
0.14 0.15 0.16 0.17 
  29   73   54   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
 18 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
 12 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.101   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id: 
a35e a35j 
  90   89 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
1.82 1.95 2.08 2.21 
  29   73   54   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
 23 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
 13 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2013 
 179 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160336860 160337706 160338177 160338336 160339056 160339103 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  1 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.14 0.15 0.16 0.17 
  29   73   54   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
  6 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
  1 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.101   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
a35c a35h 
  90   89 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
0.94 1.01 1.07 1.14 
  29   73   54   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 23 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.7 
179 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2014 
 179 

patient.samples.sample.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.30   14.59   22.67   24.35   31.57   74.58 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type: 
dna 
179 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id: 
  d 
179 



patient.samples.sample.portions.portion.analytes.analyte.concentration: 
0.14 0.15 0.16 0.17 
  29   73   54   23 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
179 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
yes 
179 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             179 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
adna preparation type 
                  179 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen 
      179 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence: 
   1    2    3 NA's 
 153   22    1    3 



patient.samples.sample.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    8.00   12.00   13.48   19.50   30.00 

patient.samples.sample.portions.portion.is_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   6.000   8.000   7.732  10.000  12.000 

patient.samples.sample.portions.portion.portion_number: 
 11  12  13  21 
153  22   1   3 

patient.samples.sample.portions.portion.portion_sequence: 
   1    2 NA's 
 175    3    1 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe: 
 no 
179 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
 0  1  2  3 40 
88 56 12  5 18 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration: 
  0   1   2  20 
145  15   1  18 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.011   0.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration: 
  0   1   2  20 
150  10   1  18 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells: 
  0   5  10  15  40 
171   4   2   1   1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   5.000   7.598  11.000  40.000 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  50.00   84.50   95.00   90.86  100.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   80.00   80.00   86.18   95.00  100.00 

patient.samples.sample.portions.portion.slides.slide.section_location: 
top 
179 

patient.samples.sample.portions.portion.weight: 
 10  20  30 
 10  20 149 

patient.samples.sample.portions.portion.year_of_creation: 
2013 
 179 


patient.samples.sample.portions.shipment_portion.center_id: 
  20 NA's 
  79  100 

patient.samples.sample.portions.shipment_portion.is_ffpe: 
  no NA's 
  79  100 

patient.samples.sample.portions.shipment_portion.plate_id: 
a43b a43c NA's 
  78    1  100 

patient.samples.sample.portions.shipment_portion.portion_number: 
  11   21   22 NA's 
   1   77    1  100 

patient.samples.sample.portions.shipment_portion.portion_sequence: 
   1    2 NA's 
   1   77  101 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment: 
  10 NA's 
  79  100 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment: 
   6 NA's 
  79  100 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment: 
2014 NA's 
  79  100 

patient.samples.sample.sample_type: 
primary tumor 
          179 

patient.samples.sample.sample_type_id: 
  1 
179 

patient.samples.sample.vial_number: 
  a   b 
176   3 

}}
\keyword{datasets}
