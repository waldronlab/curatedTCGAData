\name{ THYM }
\alias{ THYM }
\docType{data}
\title{ Thymoma }

\details{
\preformatted{
> experiments( THYM )
ExperimentList class object of length 9: 
 [1] CNASNP-20160128: RaggedExperiment with 105646 rows and 248 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 15571 rows and 248 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 123 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 123 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 126 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 126 columns 
 [7] Mutation-20160128: RaggedExperiment with 3064 rows and 123 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 122 columns 
 [9] RPPAArray-20160128: SummarizedExperiment with 192 rows and 90 columns 

> rownames( THYM )
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

> colnames( THYM )
CharacterList of length 9
[["CNASNP-20160128"]] TCGA-3G-AB0O-01A-22D-A422-01 ...
[["CNVSNP-20160128"]] TCGA-3G-AB0O-01A-22D-A422-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-3G-AB0O-01A-22D-A422-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-3G-AB0O-01A-22D-A422-01 ...
[["Methylation-20160128"]] TCGA-3G-AB0O-01A-22D-A424-05 ...
[["miRNASeqGene-20160128"]] TCGA-3G-AB0O-01A-22R-A42W-13 ...
[["Mutation-20160128"]] TCGA-3G-AB0O-01A-22D-A423-09 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-3G-AB0O-01A-22R-A42C-07 ...
[["RPPAArray-20160128"]] TCGA-3G-AB0O-01A-11-A45R-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  2.9 Mb
2                   CNVSNP-20160128  0.5 Mb
3         GISTIC_AllByGene-20160128  4.3 Mb
4 GISTIC_ThresholdedByGene-20160128  4.3 Mb
5              Methylation-20160128 67.4 Mb
6             miRNASeqGene-20160128  0.1 Mb
7                 Mutation-20160128    3 Mb
8          RNASeq2GeneNorm-20160128  1.1 Mb
9                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   115 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
   9.00    9.00    2.34    1.04      NA 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  17.00   49.50   60.00   58.15   68.50   84.00       1 

vital_status:
  0   1 
115   9 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    124     379     853    1423    2488    3488     115 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   725.2  1267.5  1486.3  1947.0  4575.0      10 

tumor_tissue_site:
anterior mediastinum               thymus 
                  27                   97 

gender:
female   male 
    60     64 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2000    2008    2010    2010    2012    2013       1 

radiation_therapy:
 no yes 
 81  43 


race:
                    asian black or african american                     white 
                       13                         6                       103 
                     NA's 
                        2 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    10                    100                     14 

admin.bcr:
nationwide children's hospital 
                           124 

admin.day_of_dcc_upload:
  1 
124 

admin.disease_code:
thym 
 124 


admin.month_of_dcc_upload:
 10 
124 

admin.patient_withdrawal.withdrawn:
false 
  124 

admin.project_code:
tcga 
 124 

admin.year_of_dcc_upload:
2015 
 124 


patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      123         1 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -294.0   -12.0     0.0   179.4    53.5  2241.0       2 

patient.clinical_cqcf.days_to_death:
2488 NA's 
   1  123 


patient.clinical_cqcf.history_of_neoadjuvant_treatment:
 no yes 
122   2 

patient.clinical_cqcf.normal_tissue_anatomic_site:
 other thymus   NA's 
    14      4    106 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           18           106 

patient.clinical_cqcf.tumor_type:
primary 
    124 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -30893  -25278  -22092  -21431  -18265   -6572       1 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  124.0   497.5  1231.0  1490.2  2268.2  3488.0     118 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     325     766    1071    1537    4213       7 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     5.0     9.0    13.8    23.0    29.0     119 

patient.drugs.drug.2.days_to_drug_therapy_end:
 201  221  369 2000 NA's 
   1    1    1    1  120 

patient.drugs.drug.2.days_to_drug_therapy_start:
  15  137  261 NA's 
   1    1    1  121 

patient.drugs.drug.2.drug_name:
  cisplatin cisplatinum   etoposide       taxol        NA's 
          1           1           2           1         119 

patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                           1                            1 
            partial response                         NA's 
                           1                          121 

patient.drugs.drug.2.month_of_form_completion:
   2    8    9   10 NA's 
   1    1    1    2  119 

patient.drugs.drug.2.therapy_ongoing:
  no NA's 
   4  120 

patient.drugs.drug.2.therapy_types.therapy_type:
chemotherapy         NA's 
           4          120 

patient.drugs.drug.2.tx_on_clinical_trial:
  no NA's 
   4  120 

patient.drugs.drug.2.year_of_form_completion:
2014 2015 NA's 
   4    1  119 

patient.drugs.drug.3.bcr_drug_barcode:
tcga-x7-a8m5-d70365 tcga-x7-a8m6-d66992                NA's 
                  1                   1                 122 

patient.drugs.drug.3.bcr_drug_uuid:
28923673-503b-4238-b601-0fc4ac61b761 ea4cb9aa-e2db-47c6-8b8f-871561b2dc39 
                                   1                                    1 
                                NA's 
                                 122 

patient.drugs.drug.3.day_of_form_completion:
   9   29 NA's 
   1    1  122 

patient.drugs.drug.3.days_to_drug_therapy_end:
2000 NA's 
   1  123 

patient.drugs.drug.3.drug_name:
  cisplatin doxorubicin        NA's 
          1           1         122 

patient.drugs.drug.3.measure_of_response:
clinical progressive disease                         NA's 
                           1                          123 

patient.drugs.drug.3.month_of_form_completion:
   2   10 NA's 
   1    1  122 

patient.drugs.drug.3.therapy_ongoing:
  no NA's 
   1  123 

patient.drugs.drug.3.therapy_types.therapy_type:
chemotherapy         NA's 
           2          122 

patient.drugs.drug.3.tx_on_clinical_trial:
  no NA's 
   1  123 

patient.drugs.drug.3.year_of_form_completion:
2014 2015 NA's 
   1    1  122 

patient.drugs.drug.4.bcr_drug_barcode:
tcga-x7-a8m6-d66993                NA's 
                  1                 123 

patient.drugs.drug.4.bcr_drug_uuid:
58085d58-c71c-4c7f-9a49-645d82c04d33                                 NA's 
                                   1                                  123 

patient.drugs.drug.4.day_of_form_completion:
  29 NA's 
   1  123 

patient.drugs.drug.4.days_to_drug_therapy_end:
2000 NA's 
   1  123 

patient.drugs.drug.4.drug_name:
cyclophosphamide             NA's 
               1              123 

patient.drugs.drug.4.measure_of_response:
clinical progressive disease                         NA's 
                           1                          123 

patient.drugs.drug.4.month_of_form_completion:
  10 NA's 
   1  123 

patient.drugs.drug.4.therapy_ongoing:
  no NA's 
   1  123 

patient.drugs.drug.4.therapy_types.therapy_type:
chemotherapy         NA's 
           1          123 

patient.drugs.drug.4.year_of_form_completion:
2014 NA's 
   1  123 



patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   5.000   8.375   6.750  31.000     116 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     55     201     218    1011     369    4213     119 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      2      14      92     660     199    4100     117 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           1                            1 
            partial response               stable disease 
                           1                            1 
                        NA's 
                         120 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.500   8.500   7.625  10.000  10.000     116 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
   5    2  117 

patient.drugs.drug.therapy_types.therapy_type:
 chemotherapy immunotherapy          NA's 
            6             2           116 

patient.drugs.drug.tx_on_clinical_trial:
  no NA's 
   7  117 

patient.drugs.drug.year_of_form_completion:
2014 2015 NA's 
   7    1  116 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    10                    100                     14 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   6.000   9.333  13.000  29.000      97 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    386     627     813    1161    1538    2936      97 

patient.follow_ups.follow_up.2.lost_follow_up:
  no NA's 
  27   97 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   5.926  11.000  12.000      97 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no NA's 
   3  121 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   3  121 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 752  937 1549 NA's 
   1    1    1  121 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             3            121 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      1                       2                     121 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
pleura   NA's 
     3    121 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   3  121 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  24    3   97 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        23          3         98 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  24    3   97 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  17   10   97 

patient.follow_ups.follow_up.2.vital_status:
alive  NA's 
   27    97 

patient.follow_ups.follow_up.2.year_of_form_completion:
2014 2015 NA's 
  10   17   97 

patient.follow_ups.follow_up.3.bcr_followup_barcode:
tcga-4x-a9f9-f72120 tcga-yt-a95f-f72392 tcga-yt-a95h-f68756                NA's 
                  1                   1                   1                 121 

patient.follow_ups.follow_up.3.bcr_followup_uuid:
549c35bb-deec-481a-bc34-b52f2cefde17 8d079259-6e70-4c00-bdac-2c51e77b1c17 
                                   1                                    1 
fd0c1a65-fa98-41c2-8e39-94dea7e90e23                                 NA's 
                                   1                                  121 

patient.follow_ups.follow_up.3.day_of_form_completion:
  15   16   23 NA's 
   1    1    1  121 

patient.follow_ups.follow_up.3.days_to_last_followup:
 627  669  775 NA's 
   1    1    1  121 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
   3  121 

patient.follow_ups.follow_up.3.month_of_form_completion:
   4    7   12 NA's 
   1    1    1  121 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   3  121 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free       NA's 
         3        121 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no NA's 
   3  121 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
   2    1  121 

patient.follow_ups.follow_up.3.vital_status:
alive  NA's 
    3   121 

patient.follow_ups.follow_up.3.year_of_form_completion:
2014 2015 NA's 
   1    2  121 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-yt-a95h-f70290                NA's 
                  1                 123 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
15c6b5c1-ffa0-4847-b071-a0cfd8a558f4                                 NA's 
                                   1                                  123 

patient.follow_ups.follow_up.4.day_of_form_completion:
   3 NA's 
   1  123 

patient.follow_ups.follow_up.4.days_to_last_followup:
 711 NA's 
   1  123 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   1  123 

patient.follow_ups.follow_up.4.month_of_form_completion:
   2 NA's 
   1  123 

patient.follow_ups.follow_up.4.postoperative_rx_tx:
  no NA's 
   1  123 

patient.follow_ups.follow_up.4.radiation_therapy:
  no NA's 
   1  123 

patient.follow_ups.follow_up.4.vital_status:
alive  NA's 
    1   123 

patient.follow_ups.follow_up.4.year_of_form_completion:
2015 NA's 
   1  123 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    6.00   12.00   12.83   16.00   31.00       5 

patient.follow_ups.follow_up.days_to_death:
 180  775  853 2910 NA's 
   1    1    1    1  120 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     14     687    1220    1450    1819    4575      11 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 107   12    5 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.731  11.000  12.000       5 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    1  121 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   1    2  121 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 702 1365 3143 NA's 
   1    1    1  121 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
         liver other, specify           NA's 
             1              2            121 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      1                       1                       1 
                   NA's 
                    121 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
  face thymus   NA's 
     1      1    122 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    1  122 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    1  122 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no  yes NA's 
   1    1  122 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 702 3502 NA's 
   1    1  122 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
          bone other, specify           NA's 
             1              1            122 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis  new primary tumor               NA's 
                 1                  1                122 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
nose NA's 
   1  123 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  123 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
 yes NA's 
   1  123 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
 yes NA's 
   1  123 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 702 NA's 
   1  123 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            123 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                123 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_occurrence_anatomic_site_text:
superior vena cava lymph node                          NA's 
                            1                           123 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  123 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   4    1  119 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   3    2  119 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  196.0   374.2   896.5  1267.0  2087.8  2928.0     118 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
         liver           lung other, specify           NA's 
             1              2              3            118 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
distant metastasis  new primary tumor               NA's 
                 2                  4                118 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
        breast    lung pleura right axillary           NA's 
             1              1              1            121 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    3  119 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 110    6    8 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       107          4         13 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
 108    9    7 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
  78   39    7 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  114     4     6 

patient.follow_ups.follow_up.year_of_form_completion:
2014 2015 NA's 
  72   47    5 

patient.gender:
female   male 
    60     64 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  139.0   158.0   167.0   167.5   176.0   194.0      23 

patient.history_of_neoadjuvant_treatment:
 no yes 
122   2 

patient.icd_10:
  c37 c38.1 c38.3 c76.1 
   85    37     1     1 


patient.icd_o_3_site:
c37.9 c38.1 c38.3 c76.1 
   85    37     1     1 

patient.informed_consent_verified:
yes 
124 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    1  121 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   3  121 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 296  890 3538 NA's 
   1    1    1  121 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
                  brain pleura/pleural effusion                    NA's 
                      1                       2                     121 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      1                       2                     121 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    1  121 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 296 NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    1   123 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                123 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 296 NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
bone NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                123 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.5.additional_pharmaceutical_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.5.additional_radiation_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
 296 NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            123 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                123 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_occurrence_anatomic_site_text:
adrenal gland          NA's 
            1           123 

patient.new_tumor_events.new_tumor_event.5.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.6.additional_pharmaceutical_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.6.additional_radiation_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_after_initial_treatment:
 296 NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.6.new_neoplasm_event_occurrence_anatomic_site:
pleura/pleural effusion                    NA's 
                      1                     123 

patient.new_tumor_events.new_tumor_event.6.new_neoplasm_event_type:
locoregional recurrence                    NA's 
                      1                     123 

patient.new_tumor_events.new_tumor_event.6.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.7.additional_pharmaceutical_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.7.additional_radiation_therapy:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.7.days_to_new_tumor_event_after_initial_treatment:
 296 NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.7.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            123 

patient.new_tumor_events.new_tumor_event.7.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                123 

patient.new_tumor_events.new_tumor_event.7.new_neoplasm_occurrence_anatomic_site_text:
lymph nodes        NA's 
          1         123 

patient.new_tumor_events.new_tumor_event.7.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  123 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  10    4  110 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  10    4  110 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   91.0   182.0   539.0   829.8  1338.8  2428.0     110 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
                   lung          other, specify pleura/pleural effusion 
                      3                       9                       3 
                   NA's 
                    109 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      1                       7                       7 
                   NA's 
                    109 


patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   7    7  110 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 108   15    1 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       115          6          3 

patient.post_op_ablation_embolization_tx:
  no NA's 
 122    2 

patient.postoperative_rx_tx:
  no  yes NA's 
 117    6    1 

patient.primary_pathology.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  17.00   49.50   60.00   58.16   68.50   84.00       1 

patient.primary_pathology.days_to_initial_pathologic_diagnosis:
   0 NA's 
 123    1 


patient.primary_pathology.histological_type_list.histological_type.2:
thymoma; type ab thymoma; type b2 thymoma; type b3             NA's 
               2                1                3              118 

patient.primary_pathology.history_myasthenia_gravis:
  no  yes NA's 
  90   31    3 

patient.primary_pathology.init_pathology_dx_method_other:
clinical judgment, imaging            mediastinoscopy 
                         1                          1 
            mediastinotomy            surgical biopsy 
                         1                          1 
                      NA's 
                       120 

patient.primary_pathology.initial_pathologic_diagnosis_method:
           core needle biopsy fine needle aspiration biopsy 
                           11                            11 
       other method, specify:            surgical resection 
                            4                            98 


patient.primary_pathology.tumor_tissue_site:
anterior mediastinum               thymus 
                  27                   97 

patient.primary_pathology.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2000    2008    2010    2010    2012    2013 

patient.prior_dx:
 no yes 
114  10 

patient.race:
                    asian black or african american                     white 
                       13                         6                       103 
                     NA's 
                        2 

patient.radiation_therapy:
  no  yes NA's 
  81   42    1 

patient.radiations.radiation.2.anatomic_treatment_site:
regional site          NA's 
            2           122 

patient.radiations.radiation.2.bcr_radiation_barcode:
tcga-xu-a936-r66165 tcga-xu-aaxx-r66120                NA's 
                  1                   1                 122 

patient.radiations.radiation.2.bcr_radiation_uuid:
48a66c10-e6f8-46bf-9e08-5a8742ead7da f27c8aad-a583-4d35-b44f-ce230cadb192 
                                   1                                    1 
                                NA's 
                                 122 

patient.radiations.radiation.2.day_of_form_completion:
   2    3 NA's 
   1    1  122 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
 101  123 NA's 
   1    1  122 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
  73   77 NA's 
   1    1  122 

patient.radiations.radiation.2.month_of_form_completion:
  10 NA's 
   2  122 

patient.radiations.radiation.2.numfractions:
  12   20 NA's 
   1    1  122 

patient.radiations.radiation.2.radiation_dosage:
  40 2400 NA's 
   1    1  122 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   2  122 

patient.radiations.radiation.2.radiation_type:
external     NA's 
       2      122 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
   1    1  122 

patient.radiations.radiation.2.year_of_form_completion:
2014 NA's 
   2  122 

patient.radiations.radiation.anatomic_treatment_site:
   local recurrence primary tumor field       regional site                NA's 
                  2                  24                  14                  84 



patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00    5.00   10.86   19.50   31.00      81 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   24.0    86.5   104.0   205.9   152.0  2092.0      81 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0    51.0    65.0   168.4   104.0  2058.0      81 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              23                                1 
radiographic progressive disease                   stable disease 
                               2                                1 
                            NA's 
                              97 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000  10.000   8.674  10.000  12.000      81 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   21.25   26.00   25.47   30.00   50.00      86 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.2    49.0    50.0   716.3    60.0  5940.0      85 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  43   81 

patient.radiations.radiation.radiation_type:
external     NA's 
      41       83 

patient.radiations.radiation.units:
 cgy   gy NA's 
   5   35   84 

patient.radiations.radiation.year_of_form_completion:
2013 2014 2015 NA's 
   1   40    2   81 


patient.tissue_prospective_collection_indicator:
 no yes 
111  13 

patient.tissue_retrospective_collection_indicator:
 no yes 
 13 111 


patient.vital_status:
alive  dead 
  118     6 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  44.00   63.00   75.00   77.08   89.50  155.00      21 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  2                 122 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             122                2 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      123         1 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   339.5  1037.5  1255.9  1852.2  4605.0       8 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -13.00    0.00    0.00   27.09    4.50 1786.00       1 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     8                     80                     36 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
mediastinum; anterior                thymus 
                   27                    97 


patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
 no yes 
122   2 

patient.biospecimen_cqcf.method_of_sample_procurement:
other (please specify)     surgical resection 
                     1                    123 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    5.50  129.88   53.75  917.00     116 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
                 1                  7                116 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
 other thymus   NA's 
     3      4    117 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            7           117 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
   8    2  114 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -54.0    -1.0     8.5   292.1   103.8  3646.0       2 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.1000    0.2025    0.3350   47.4049    0.4625 1120.0000        74 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   15.00   18.54   17.71   20.00   26.10      74 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.90   43.50   70.00   87.15  120.00  200.00      74 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
        blood draw          mouthwash surgical resection               NA's 
               109                  3                 11                  1 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
other  NA's 
   11   113 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           11           113 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
  1   2 
123   1 

patient.biospecimen_cqcf.other_method_of_sample_procurement:
anterior mediastinotomy                    NA's 
                      1                     123 

patient.biospecimen_cqcf.other_vessel_used:
cryosette      NA's 
        2       122 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 yes NA's 
 119    5 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
124 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 yes NA's 
 117    7 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 yes NA's 
 117    7 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                       12                         6                        98 
                     NA's 
                        8 

patient.biospecimen_cqcf.sample_prescreened:
 yes NA's 
 119    5 

patient.biospecimen_cqcf.site_of_disease:
mediastinum      thymus 
         27          97 

patient.biospecimen_cqcf.site_of_disease_description:
anterior     NA's 
      27       97 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.308   0.000  50.000       7 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   60.00   80.00   73.41   90.00  100.00       7 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   66.0   110.0   160.0   203.7   300.0   510.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1 
124 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                      4                       1                      26 
               cryovial  other (please specify) 
                     91                       2 

patient.days_to_index:
   0 NA's 
 123    1 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   29.0   421.5  1122.0  1373.9  1891.5  4923.0       1 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    110     140     140     180     220     320     113 

patient.samples.sample.2.is_ffpe:
 no 
124 

patient.samples.sample.2.oct_embedded:
false 
  124 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174162653 174163657 174164000 174163882 174164032 174164063 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  9 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
  30   82   12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
 18 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
  6 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.363   8.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a426 a42b 
  81   43 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
 30  82  12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 40 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 
 124 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174162652 174164381 174164414 174164309 174164446 174164536 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  19   38   38   29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
 18 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
  6 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.363   8.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a425 a42a 
  81   43 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  19   38   38   29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 
 124 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.03   12.84   17.84   20.40   23.66   73.47 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
124 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  19   38   38   29 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             124 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                               74                                50 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
      124 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
   9    2  113 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    3.75   12.00   14.02   24.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no 
124 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   4.121   5.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
  1  11  21  22  31 
113   6   2   2   1 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
  12  112 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  11  113 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    1.00   15.91    6.50   80.00     113 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1    2 NA's 
   8    2    1  113 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
  11  113 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    2 NA's 
   8    1    2  113 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0   96  100 NA's 
   3    1    7  113 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   0    4  100 NA's 
   7    1    3  113 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
  11  113 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
  11  113 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
  11  113 

patient.samples.sample.2.portions.portion.weight:
   2   30 NA's 
  63   11   50 

patient.samples.sample.2.portions.portion.year_of_creation:
2013 2014 
  29   95 

patient.samples.sample.2.sample_type:
blood derived normal   buccal cell normal  solid tissue normal 
                 111                    2                   11 

patient.samples.sample.2.sample_type_id:
 10  11  12 
111  11   2 

patient.samples.sample.2.vial_number:
  a   b 
121   3 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  2                 122 

patient.samples.sample.3.bcr_sample_barcode:
tcga-x7-a8d6-11a tcga-x7-a8d7-11a             NA's 
               1                1              122 

patient.samples.sample.3.bcr_sample_uuid:
9cfa1dad-df62-4e0e-8fda-7d4a4e55384c a21a9863-3ad7-44df-8063-c32400820dc1 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.days_to_collection:
3800 4090 NA's 
   1    1  122 

patient.samples.sample.3.initial_weight:
 150  160 NA's 
   1    1  122 

patient.samples.sample.3.is_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.oct_embedded:
false  NA's 
    2   122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.89 1.94 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-x7-a8d6-11a-22r-a42w-13 tcga-x7-a8d7-11a-11r-a42w-13 
                           1                            1 
                        NA's 
                         122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
ba66a0e2-eb00-494f-8e51-61bd4085d9f5 e06f327e-c855-41bb-9c8b-25acf5aabef1 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
174162868 174162892      NA's 
        1         1       122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  13 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.17 0.18 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  18 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   5 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a42w NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   b    d NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
 3.4  3.6 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  20 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2014 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-x7-a8d6-11a-22r-a42c-07 tcga-x7-a8d7-11a-11r-a42c-07 
                           1                            1 
                        NA's 
                         122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
5ab9c57d-ad8a-4d8e-a34a-d02a455a35bd 7f6f6ce6-2901-4f09-a37a-be0b066591d4 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
174163540 174163564      NA's 
        1         1       122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.17 0.18 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  18 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   6 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   5 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a42c NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   b    d NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
2.21 2.34 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2014 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
 6.78 19.58  NA's 
    1     1   122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-x7-a8d6-11a-22r tcga-x7-a8d7-11a-11r                 NA's 
                   1                    1                  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
4839b97c-be95-4a8e-9fea-599ca8d1c92b 52e0f5c3-ce08-4dbe-ad32-ca6ce407977c 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.17 0.18 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        2                       122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
1.31 1.76 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
 7.1  9.1 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      2     122 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-x7-a8d6-11a-22d-a423-09 tcga-x7-a8d7-11a-11d-a423-09 
                           1                            1 
                        NA's 
                         122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
d71b2270-7707-46aa-ac4a-73f74fc7349b f7c2375b-be9d-40a4-a4ed-8975d7ac4552 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
174164099 174164100      NA's 
        1         1       122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   9 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  18 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
  12 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a423 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   e    f NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 2.8  3.2 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  40 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-x7-a8d6-11a-22d-a424-05 tcga-x7-a8d7-11a-11d-a424-05 
                           1                            1 
                        NA's 
                         122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
1a8ca416-2001-4076-b196-c3a547c98600 70a62b84-43c3-4f20-92dd-60ef992feee3 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
174163043 174163044      NA's 
        1         1       122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.15 0.17 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  18 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   6 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
  12 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a424 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   e    f NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.95 2.21 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  13 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-x7-a8d6-11a-22d-a422-01 tcga-x7-a8d7-11a-11d-a422-01 
                           1                            1 
                        NA's 
                         122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
3ea9a8ca-15bb-4a1e-92bd-d66fa2e296c0 5bd83021-fe82-4b25-91ca-da51dff6b450 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
174163907 174163908      NA's 
        1         1       122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.15 0.17 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  18 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   6 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
  12 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a422 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   e    f NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
1.01 1.14 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
14.86 76.39  NA's 
    1     1   122 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-x7-a8d6-11a-22d tcga-x7-a8d7-11a-11d                 NA's 
                   1                    1                  122 

patient.samples.sample.3.portions.portion.analytes.analyte.bcr_analyte_uuid:
4ab660a3-a8e3-448d-9dff-ff4df2fd51d9 6005014c-48b5-4562-bb1f-f007e1a533f8 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
0.15 0.17 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             122 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    2                   122 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        2       122 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.bcr_portion_barcode:
tcga-x7-a8d6-11a-22 tcga-x7-a8d7-11a-11                NA's 
                  1                   1                 122 

patient.samples.sample.3.portions.portion.bcr_portion_uuid:
25fe76bb-15dc-4891-a81d-567b5b674682 9d00a542-6b3b-4538-bf8d-494bc6c9564b 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.portions.portion.day_of_creation:
   4   13 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.portions.portion.month_of_creation:
  11   12 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.portion_number:
  11   22 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.portion_sequence:
   1    2 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_barcode:
tcga-x7-a8d6-11a-02-ts2 tcga-x7-a8d7-11a-01-ts1                    NA's 
                      1                       1                     122 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_uuid:
287568d3-b1b2-4c31-b8cb-541b2a1b16e3 6d8514c9-586d-4e14-b8d9-256482364219 
                                   1                                    1 
                                NA's 
                                 122 

patient.samples.sample.3.portions.portion.slides.slide.image_file_name:
tcga-x7-a8d6-11a-02-ts2.6d8514c9-586d-4e14-b8d9-256482364219.svs 
                                                               1 
tcga-x7-a8d7-11a-01-ts1.287568d3-b1b2-4c31-b8cb-541b2a1b16e3.svs 
                                                               1 
                                                            NA's 
                                                             122 

patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   2  122 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   2  122 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   2  122 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   2  122 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   2  122 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
 100 NA's 
   2  122 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0 NA's 
   2  122 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   2  122 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   2  122 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
   2  122 

patient.samples.sample.3.portions.portion.weight:
  10   30 NA's 
   1    1  122 

patient.samples.sample.3.portions.portion.year_of_creation:
2013 NA's 
   2  122 

patient.samples.sample.3.sample_type:
solid tissue normal                NA's 
                  2                 122 

patient.samples.sample.3.sample_type_id:
  11 NA's 
   2  122 

patient.samples.sample.3.vial_number:
   a NA's 
   2  122 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   29.0   421.5  1122.0  1373.9  1891.5  4923.0       1 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10:
6b94c249-720f-413d-8d3c-90ec9687ba1d 7afdd4eb-131c-4f28-b6ef-4224292a0266 
                                   1                                    1 
f59749fa-c724-4179-8806-8fc32eefeff1                                 NA's 
                                   1                                  121 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.11:
908d41c2-4195-468a-855b-47318be75d4b b51f5046-3b76-4f84-be59-59b180536171 
                                   1                                    1 
fbe664dc-6b0b-470a-a638-26b051849661                                 NA's 
                                   1                                  121 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.12:
e841de60-7836-4a21-8ff7-2512d73424ed                                 NA's 
                                   1                                  123 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.13:
ed7db698-c766-4e84-816a-1daf551bc67f                                 NA's 
                                   1                                  123 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.14:
ef8ea1f6-8354-4eb4-bd98-a5293ad73430                                 NA's 
                                   1                                  123 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.15:
deded354-3346-4cc7-8bbb-28b6bf5b6d77                                 NA's 
                                   1                                  123 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.16:
c36787e3-fee8-4230-a161-b00f3eda284a                                 NA's 
                                   1                                  123 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.17:
46c852c7-845e-47a4-b22c-a0c4cf8c23d6                                 NA's 
                                   1                                  123 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.18:
a15a8a41-c0f0-4350-bacd-7398a1c528c2                                 NA's 
                                   1                                  123 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.19:
313be53a-6546-4619-9c3e-aa8e2aa8c19a                                 NA's 
                                   1                                  123 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.20:
4d6c9b79-2979-498c-a3b7-c086c757b2d6                                 NA's 
                                   1                                  123 








patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   20.0    90.0   130.0   200.6   220.0  1170.0 

patient.samples.sample.is_ffpe:
 no 
124 

patient.samples.sample.oct_embedded:
false  true 
   65    59 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.710   1.870   1.890   1.887   1.920   2.040 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167822982 167823064 174162846 172526770 174162878 174162911 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
 13 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.16 0.17 0.18 
  11   22   56   35 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
 18 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
  6 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   8.000   7.056  10.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a42u a42w 
  32   92 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
  3 3.2 3.4 3.6 
 11  22  56  35 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
 23 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
 20 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2014 
 124 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167812897 167812898 167812899 167812917 167812922 167812969       119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   5  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.16 0.17 0.18 NA's 
   1    2    2  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   9 NA's 
   5  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   5  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  12 NA's 
   5  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   2    3    4 NA's 
   2    2    1  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a466 NA's 
   5  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   b    f    h NA's 
   1    1    3  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
1.06 1.12 1.19 NA's 
   1    2    2  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   5  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6 NA's 
   5  119 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   5  119 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174162654 174163516 174163548 174163562 174163582 174163864 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  7 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.16 0.17 0.18 
  11   22   56   35 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
 18 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
  6 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   8.000   7.056  10.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a42c a42s 
  92   32 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
1.95 2.08 2.21 2.34 
  11   22   56   35 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
 23 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
 13 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2014 
 124 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  16.46   44.38   60.27   58.27   67.46  103.85 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
  r 
124 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
0.15 0.16 0.17 0.18 
  11   22   56   35 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             124 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                      124 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.670   1.377   1.565   1.603   1.792   2.860 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00    7.90    8.50    8.46    9.00    9.70 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
    124 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
 121    2    1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174162650 174164087 174164120 174164089 174164151 174164507 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  9 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
  22   91   11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
 18 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
  6 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.363   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a423 a428 
  81   43 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
 22  91  11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 40 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 
 124 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174162657 174163050 174163080 174163548 174164480 174164543 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  5 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 
  10   44   38   32 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
 18 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
  6 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.363   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a424 a429 
  81   43 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.82 1.95 2.08 2.21 
  10   44   38   32 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
 23 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
 13 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 
 124 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174162645 174163897 174163930 174163882 174163962 174164388 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  10   44   38   32 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
 18 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
  6 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.363   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a422 a427 
  81   43 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  10   44   38   32 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
124 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 
 124 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  19.43   40.45   55.65   57.20   73.17  136.85 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
124 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
124 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  10   44   38   32 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
124 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
124 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             124 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  124 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
      124 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
 121    2    1 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    6.75   13.00   16.40   30.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
124 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   5.919  12.000  12.000 

patient.samples.sample.portions.portion.portion_number:
 11  12  21  22  31 
119   1   1   1   2 

patient.samples.sample.portions.portion.portion_sequence:
   1 NA's 
  21  103 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
124 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   1.000   1.613   2.000  25.000 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
  0   1   2 
107  15   2 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
  0   3 
123   1 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
  0   1   2 
110  10   4 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.815   0.000  50.000 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   1.000   8.532  15.000  65.000 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  35.00   85.00   99.00   89.63  100.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   80.00   80.00   84.78   95.00   99.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
top 
124 

patient.samples.sample.portions.portion.weight:
 20  30 
  2 122 

patient.samples.sample.portions.portion.year_of_creation:
2013 2014 
  48   76 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
  90   34 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
  90   34 

patient.samples.sample.portions.shipment_portion.plate_id:
a45r a45s NA's 
  81    9   34 

patient.samples.sample.portions.shipment_portion.portion_number:
  11   21 NA's 
   2   88   34 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   2 NA's 
  17  107 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  30 NA's 
  90   34 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   7 NA's 
  90   34 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
  90   34 

patient.samples.sample.sample_type:
primary tumor 
          124 

patient.samples.sample.sample_type_id:
  1 
124 

patient.samples.sample.vial_number:
  a 
124 

}}
\keyword{datasets}
