\name{ TGCT }
\alias{ TGCT }
\docType{data}
\title{ Testicular Germ Cell Tumors }

\details{
\preformatted{
> experiments( TGCT )
ExperimentList class object of length 9: 
 [1] CNASNP-20160128: RaggedExperiment with 137968 rows and 271 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 25479 rows and 271 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 134 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 134 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 139 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 139 columns 
 [7] Mutation-20160128: RaggedExperiment with 14672 rows and 138 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 139 columns 
 [9] RPPAArray-20160128: SummarizedExperiment with 192 rows and 107 columns 

> rownames( TGCT )
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

> colnames( TGCT )
CharacterList of length 9
[["CNASNP-20160128"]] TCGA-2G-AAEW-01A-11D-A42X-01 ...
[["CNVSNP-20160128"]] TCGA-2G-AAEW-01A-11D-A42X-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-2G-AAEW-01A-11D-A42X-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-2G-AAEW-01A-11D-A42X-01 ...
[["Methylation-20160128"]] TCGA-2G-AAEW-01A-11D-A42Z-05 ...
[["miRNASeqGene-20160128"]] TCGA-2G-AAEW-01A-11R-A439-13 ...
[["Mutation-20160128"]] TCGA-2G-AAEW-01A-11D-A42Y-10 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-2G-AAEW-01A-11R-A430-07 ...
[["RPPAArray-20160128"]] TCGA-2G-AAEW-01A-21-A45P-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  3.8 Mb
2                   CNVSNP-20160128  0.8 Mb
3         GISTIC_AllByGene-20160128  4.3 Mb
4 GISTIC_ThresholdedByGene-20160128  4.3 Mb
5              Methylation-20160128 67.4 Mb
6             miRNASeqGene-20160128  0.1 Mb
7                 Mutation-20160128  9.5 Mb
8          RNASeq2GeneNorm-20160128  1.1 Mb
9                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   130 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 4.0000  4.0000  1.5493  0.0466      NA 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.00   26.00   31.00   31.99   37.00   67.00 

vital_status:
  0   1 
130   4 

days_to_death:
  17  513  618 6972 NA's 
   1    1    1    1  130 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   688.2  1265.5  2092.7  2826.2  7437.0       4 

tumor_tissue_site:
testes 
   134 


pathology_T_stage:
t1 t2 t3 tx 
76 51  6  1 

pathology_N_stage:
  n0   n1   n2   nx NA's 
  46   11    2   65   10 

pathology_M_stage:
  m0   m1  m1a  m1b NA's 
 115    2    1    1   15 

gender:
male 
 134 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1992    2006    2009    2008    2012    2013 

radiation_therapy:
  no  yes NA's 
 111   21    2 

karnofsky_performance_score:
  80   90  100 NA's 
   5   41   56   32 

race:
                    asian black or african american                     white 
                        4                         6                       119 
                     NA's 
                        5 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    12                    111                     11 

admin.bcr:
nationwide children's hospital 
                           134 

admin.day_of_dcc_upload:
  1 
134 

admin.disease_code:
tgct 
 134 


admin.month_of_dcc_upload:
 10 
134 

admin.patient_withdrawal.withdrawn:
false 
  134 

admin.project_code:
tcga 
 134 

admin.year_of_dcc_upload:
2015 
 134 




patient.blood_relative_cancer_history_list.blood_relative_cancer_history.3.family_member_relationship_type:
grandfather grandmother       other        NA's 
          2           1           5         126 


patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.family_member_relationship_type:
grandmother       other        NA's 
          1           2         131 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.relative_family_cancer_hx_text:
                  breast lung and prostate cancer                     NA's 
                       2                        1                      131 



patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      133         1 

patient.clinical_cqcf.country:
       brazil       croatia   netherlands        russia united states 
           10             8            64             4            48 

patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -232.0    -1.0    20.0   863.4  1819.0  6138.0       1 

patient.clinical_cqcf.days_to_death:
 618 NA's 
   1  133 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
 no 
134 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
            5           129 

patient.clinical_cqcf.tumor_type:
primary 
    134 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00    4.00    8.00   12.17   20.00   30.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -24759  -13857  -11543  -11857   -9575   -5321 

patient.days_to_death:
  17  513  618 NA's 
   1    1    1  131 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    -11     335    1067    1466    2032    6279       3 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   21.00   28.00   23.51   28.00   30.00      79 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0    71.0    83.5   291.9   191.8  3420.0      80 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -4.0    11.0    24.0   238.6   136.5  3349.0      79 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                           1                           39 
            partial response                         NA's 
                          12                           82 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   7.000   6.964   7.000  11.000      79 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  54    1   79 

patient.drugs.drug.2.therapy_types.therapy_type:
chemotherapy         NA's 
          55           79 

patient.drugs.drug.2.tx_on_clinical_trial:
  no NA's 
  54   80 

patient.drugs.drug.2.year_of_form_completion:
2014 2015 NA's 
  54    1   79 



patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   21.00   28.00   23.82   28.00   29.00      83 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   45.0    71.0    83.0   242.8   191.8  2565.0      84 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -4.0    11.0    25.0   190.8   136.5  2506.0      83 

patient.drugs.drug.3.drug_name:
 bleomycin  cisplatin ifosfamide   platinum       NA's 
         4         44          1          2         83 

patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                           1                           35 
            partial response                         NA's 
                          12                           86 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   7.000   7.118   7.000  11.000      83 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  50    1   83 

patient.drugs.drug.3.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
             50               1              83 

patient.drugs.drug.3.tx_on_clinical_trial:
  no NA's 
  51   83 

patient.drugs.drug.3.year_of_form_completion:
2014 2015 NA's 
  50    1   83 

patient.drugs.drug.4.bcr_drug_barcode:
tcga-xe-aanj-d62815 tcga-yu-aa4l-d65295                NA's 
                  1                   1                 132 

patient.drugs.drug.4.bcr_drug_uuid:
6b3e0774-411b-48cf-9ccb-dde143aadcc3 a3c35ed5-97a8-4be3-85fe-a8b8e741c676 
                                   1                                    1 
                                NA's 
                                 132 

patient.drugs.drug.4.day_of_form_completion:
  21   25 NA's 
   1    1  132 

patient.drugs.drug.4.days_to_drug_therapy_end:
 358  413 NA's 
   1    1  132 

patient.drugs.drug.4.days_to_drug_therapy_start:
 250  349 NA's 
   1    1  132 

patient.drugs.drug.4.drug_name:
     gemox ifosfamide       NA's 
         1          1        132 

patient.drugs.drug.4.measure_of_response:
complete response              NA's 
                2               132 

patient.drugs.drug.4.month_of_form_completion:
   7    9 NA's 
   1    1  132 

patient.drugs.drug.4.therapy_ongoing:
  no NA's 
   2  132 

patient.drugs.drug.4.therapy_types.therapy_type:
chemotherapy         NA's 
           2          132 

patient.drugs.drug.4.tx_on_clinical_trial:
  no NA's 
   2  132 

patient.drugs.drug.4.year_of_form_completion:
2014 NA's 
   2  132 

patient.drugs.drug.5.bcr_drug_barcode:
tcga-xe-aanj-d62816                NA's 
                  1                 133 

patient.drugs.drug.5.bcr_drug_uuid:
b7cd6a5d-3660-4120-8658-44e2d0a8f431                                 NA's 
                                   1                                  133 

patient.drugs.drug.5.day_of_form_completion:
  25 NA's 
   1  133 

patient.drugs.drug.5.days_to_drug_therapy_end:
 413 NA's 
   1  133 

patient.drugs.drug.5.days_to_drug_therapy_start:
 349 NA's 
   1  133 

patient.drugs.drug.5.drug_name:
paclitaxol       NA's 
         1        133 

patient.drugs.drug.5.measure_of_response:
complete response              NA's 
                1               133 

patient.drugs.drug.5.month_of_form_completion:
   7 NA's 
   1  133 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
   1  133 

patient.drugs.drug.5.therapy_types.therapy_type:
chemotherapy         NA's 
           1          133 

patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
   1  133 

patient.drugs.drug.5.year_of_form_completion:
2014 NA's 
   1  133 

patient.drugs.drug.6.bcr_drug_barcode:
tcga-xe-aanj-d62817                NA's 
                  1                 133 

patient.drugs.drug.6.bcr_drug_uuid:
e231730f-3149-4d98-a36b-83d4f9ccb31f                                 NA's 
                                   1                                  133 

patient.drugs.drug.6.day_of_form_completion:
  25 NA's 
   1  133 

patient.drugs.drug.6.days_to_drug_therapy_end:
 413 NA's 
   1  133 

patient.drugs.drug.6.days_to_drug_therapy_start:
 349 NA's 
   1  133 

patient.drugs.drug.6.drug_name:
platinum     NA's 
       1      133 

patient.drugs.drug.6.measure_of_response:
complete response              NA's 
                1               133 

patient.drugs.drug.6.month_of_form_completion:
   7 NA's 
   1  133 

patient.drugs.drug.6.therapy_ongoing:
  no NA's 
   1  133 

patient.drugs.drug.6.therapy_types.therapy_type:
chemotherapy         NA's 
           1          133 

patient.drugs.drug.6.tx_on_clinical_trial:
  no NA's 
   1  133 

patient.drugs.drug.6.year_of_form_completion:
2014 NA's 
   1  133 



patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00    5.00   11.28   20.00   31.00      73 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   28.0    71.0    83.5   271.3   179.2  3420.0      74 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -4.0    11.0    26.0   219.9   134.0  3349.0      73 

patient.drugs.drug.drug_name:
  bleomycin carboplatin   cisplatin   etoposide        NA's 
         48           3           4           6          73 

patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           1                           45 
            partial response               stable disease 
                          12                            1 
                        NA's 
                          75 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   6.000   6.148   6.000  11.000      73 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
  60    1   73 

patient.drugs.drug.therapy_types.therapy_type:
chemotherapy         NA's 
          61           73 

patient.drugs.drug.tx_on_clinical_trial:
  no NA's 
  60   74 

patient.drugs.drug.year_of_form_completion:
2014 2015 NA's 
  59    2   73 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    12                    111                     11 

patient.family_history_other_cancer:
  no  yes NA's 
  59   47   28 

patient.family_history_testicular_cancer:
  no  yes NA's 
 104   14   16 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    5.00    9.00   12.42   21.25   30.00      68 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    413    1378    2598    3149    4878    7437      68 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   0    1 NA's 
  57    4   73 

patient.follow_ups.follow_up.2.karnofsky_performance_score:
  80   90  100 NA's 
   3   21   37   73 

patient.follow_ups.follow_up.2.lost_follow_up:
  no NA's 
  66   68 

patient.follow_ups.follow_up.2.molecular_test_result_list.molecular_test_result:
human chorionic gonadotropin (hcg) - normal 
                                          1 
        lactate dehydrogenase (ldh)- normal 
                                         60 
                                       NA's 
                                         73 

patient.follow_ups.follow_up.2.molecular_test_result_list.molecular_test_result.2:
             alpha fetaprotein (afp) - elevated 
                                              1 
    human chorionic gonadotropin (hcg) - normal 
                                             57 
human chorionic gonadotropin (hcg) - progressed 
                                              3 
                                           NA's 
                                             73 

patient.follow_ups.follow_up.2.molecular_test_result_list.molecular_test_result.3:
    alpha fetaprotein (afp) - normal alpha fetaprotein (afp) - progressed 
                                  55                                    2 
                                NA's 
                                  77 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.250  11.000   8.045  11.000  12.000      68 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    130    3234    4616    3767    5027    5827     129 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
                       lung              other, specify 
                          1                           2 
retroperitoneal lymph nodes                      testes 
                          1                           1 
                       NA's 
                        129 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      1                       1                       3 
                   NA's 
                    129 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
        bladder skin (melanoma)            NA's 
              1               1             132 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0   121.8   384.0  1036.5  1160.2  4323.0     114 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
                       lung retroperitoneal lymph nodes 
                          1                          10 
                     testes                        NA's 
                          8                         115 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      1                      10                       8 
                   NA's 
                    115 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  46   20   68 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free       NA's 
        66         68 

patient.follow_ups.follow_up.2.post_orchi_lymph_node_dissection:
                         no    yes - after chemotherapy 
                         54                          10 
yes - prior to chemotherapy                        NA's 
                          2                          68 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  38   28   68 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
                            complete remission/response 
                                                      1 
                  no measureable tumor or tumor markers 
                                                     53 
normalization of tumor markers, but residual tumor mass 
                                                      9 
                                                   NA's 
                                                     71 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  57    9   68 

patient.follow_ups.follow_up.2.vital_status:
alive  NA's 
   66    68 

patient.follow_ups.follow_up.2.year_of_form_completion:
2014 2015 NA's 
  42   24   68 

patient.follow_ups.follow_up.3.bcr_followup_barcode:
tcga-yu-a94i-f68750 tcga-yu-aa4l-f70283                NA's 
                  1                   1                 132 

patient.follow_ups.follow_up.3.bcr_followup_uuid:
0db882db-7258-4efc-954a-f8e4c9c4b2ce 42b51242-846c-429b-90d4-91eec82c0bbd 
                                   1                                    1 
                                NA's 
                                 132 

patient.follow_ups.follow_up.3.day_of_form_completion:
   3   15 NA's 
   1    1  132 

patient.follow_ups.follow_up.3.days_to_last_followup:
 472 1573 NA's 
   1    1  132 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
   2  132 

patient.follow_ups.follow_up.3.month_of_form_completion:
   2   12 NA's 
   1    1  132 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   1  133 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free       NA's 
         1        133 

patient.follow_ups.follow_up.3.post_orchi_lymph_node_dissection:
                         no yes - prior to chemotherapy 
                          1                           1 
                       NA's 
                        132 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no  yes NA's 
   1    1  132 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
no measureable tumor or tumor markers                                  NA's 
                                    2                                   132 

patient.follow_ups.follow_up.3.radiation_therapy:
  no NA's 
   2  132 

patient.follow_ups.follow_up.3.vital_status:
alive  NA's 
    2   132 

patient.follow_ups.follow_up.3.year_of_form_completion:
2014 2015 NA's 
   1    1  132 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-yu-a94i-f70870                NA's 
                  1                 133 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
031f6685-65ab-4a2e-82d4-7cab23b46788                                 NA's 
                                   1                                  133 

patient.follow_ups.follow_up.4.day_of_form_completion:
  10 NA's 
   1  133 

patient.follow_ups.follow_up.4.days_to_last_followup:
 536 NA's 
   1  133 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   1  133 

patient.follow_ups.follow_up.4.month_of_form_completion:
   3 NA's 
   1  133 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   1  133 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
tumor free       NA's 
         1        133 

patient.follow_ups.follow_up.4.post_orchi_lymph_node_dissection:
  no NA's 
   1  133 

patient.follow_ups.follow_up.4.postoperative_rx_tx:
  no NA's 
   1  133 

patient.follow_ups.follow_up.4.primary_therapy_outcome_success:
no measureable tumor or tumor markers                                  NA's 
                                    1                                   133 

patient.follow_ups.follow_up.4.radiation_therapy:
  no NA's 
   1  133 

patient.follow_ups.follow_up.4.vital_status:
alive  NA's 
    1   133 

patient.follow_ups.follow_up.4.year_of_form_completion:
2015 NA's 
   1  133 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    6.00   25.00   19.98   30.00   31.00       5 

patient.follow_ups.follow_up.days_to_death:
6972 NA's 
   1  133 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0   609.5  1217.0  1627.9  2071.0  6468.0       7 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   0    1 NA's 
  77   24   33 

patient.follow_ups.follow_up.karnofsky_performance_score:
  80   90  100 NA's 
   5   43   53   33 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 123    6    5 



patient.follow_ups.follow_up.molecular_test_result_list.molecular_test_result.3:
    alpha fetaprotein (afp) - normal alpha fetaprotein (afp) - progressed 
                                  90                                    1 
    luteinizing hormone (lh)- normal                 testosterone- normal 
                                   2                                    1 
                                NA's 
                                  40 

patient.follow_ups.follow_up.molecular_test_result_list.molecular_test_result.4:
testosterone- normal                 NA's 
                   3                  131 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   8.000   8.147  10.000  12.000       5 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    130     371    3234    2797    4822    5827     127 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
                       lung              other, specify 
                          1                           3 
retroperitoneal lymph nodes                      testes 
                          2                           1 
                       NA's 
                        127 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      1                       3                       3 
                   NA's 
                    127 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
          bladder lt spermatic cord   skin (melanoma)              NA's 
                1                 1                 1               131 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 328 NA's 
   1  133 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            133 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
locoregional recurrence                    NA's 
                      1                     133 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
lt inguinal ring             NA's 
               1              133 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   126.2   322.5   967.2   998.2  4323.0     104 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      3                      17                       9 
                   NA's 
                    105 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
                           bone mature teratoma: abdominal wall 
                              1                               1 
                           NA's 
                            132 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  91   30   13 

patient.follow_ups.follow_up.patient_death_reason:
non-cancer related               NA's 
                 1                133 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       120          4         10 

patient.follow_ups.follow_up.post_orchi_lymph_node_dissection:
                         no    yes - after chemotherapy 
                        108                          10 
yes - prior to chemotherapy                        NA's 
                          5                          11 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  83   43    8 


patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 105   20    9 

patient.follow_ups.follow_up.source_of_patient_death_reason:
medical record           NA's 
             1            133 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  127     1     6 

patient.follow_ups.follow_up.year_of_form_completion:
2014 2015 NA's 
 119   10    5 

patient.gender:
male 
 134 


patient.history_hypospadias:
  no  yes NA's 
 124    2    8 

patient.history_of_neoadjuvant_treatment:
 no 
134 

patient.history_of_undescended_testis:
                      no           yes; bilateral  yes; left testicle only 
                     104                        2                       14 
yes; right testicle only                     NA's 
                       7                        7 

patient.icd_10:
 c62.9 c62.90 
    66     68 


patient.icd_o_3_site:
c62.9 
  134 

patient.informed_consent_verified:
yes 
134 

patient.level_of_non_descent:
           inguinal non-palpable - high                NA's 
                 18                   3                 113 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   5.597   6.000  12.000 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  130.0   418.5  1833.0  2308.8  4270.5  5027.0     128 


patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      2                       2                       2 
                   NA's 
                    128 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
bladder    NA's 
      1     133 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 432 NA's 
   1  133 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
lung NA's 
   1  133 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                133 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 432 NA's 
   1  133 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
lymph nodes-nos (not otherwise specified) 
                                        1 
                                     NA's 
                                      133 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                133 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   126.2   289.5   804.8   785.8  4323.0     104 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
biochemical evidence of disease              distant metastasis 
                              2                               3 
        locoregional recurrence               new primary tumor 
                             16                               9 
                           NA's 
                            104 

patient.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
          mature teratoma: abdominal wall 
                                        1 
rpln, lt spermatic cord; lt inguinal ring 
                                        1 
                                     NA's 
                                      132 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  99   30    5 

patient.patient_death_reason:
testicular germ cell tumor (tgct)                              NA's 
                                3                               131 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       125          5          4 

patient.postoperative_rx_tx:
  no  yes NA's 
  81   42   11 

patient.postoperative_tx_list.postoperative_tx:
          none pharmaceutical      radiation           NA's 
            68             40             19              7 

patient.primary_pathology.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.00   26.00   31.00   31.99   37.00   67.00 

patient.primary_pathology.bilateral_diagnosis_timing_type:
metachronous  synchronous         NA's 
           4            1          129 

patient.primary_pathology.days_to_bilateral_tumor_dx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -5179   -4872     -18   -1384       0    3149     129 

patient.primary_pathology.days_to_initial_pathologic_diagnosis:
  0 
134 

patient.primary_pathology.first_treatment_success:
                  no measureable tumor or tumor markers 
                                                     96 
normalization of tumor markers, but residual tumor mass 
                                                     15 
               progressive tumor mass and tumor markers 
                                                      3 
                                                   NA's 
                                                     20 

patient.primary_pathology.histology_list.histology.2.histological_percentage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   10.00   20.00   23.89   35.00   95.00      89 


patient.primary_pathology.histology_list.histology.3.histological_percentage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00   20.00   20.21   20.00   90.00     105 


patient.primary_pathology.histology_list.histology.4.histological_percentage:
   5   10   20   30 NA's 
  13    5    2    2  112 


patient.primary_pathology.histology_list.histology.5.histological_percentage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   12.50   20.00   20.83   27.50   40.00     128 

patient.primary_pathology.histology_list.histology.5.histological_type:
    non-seminoma; choriocarcinoma non-seminoma; teratoma (immature) 
                                1                                 1 
                    seminoma; nos                              NA's 
                                4                               128 

patient.primary_pathology.histology_list.histology.histological_percentage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   81.25  100.00   84.87  100.00  100.00 


patient.primary_pathology.init_pathology_dx_method_other:
laparotomy       NA's 
         1        133 

patient.primary_pathology.initial_pathologic_diagnosis_method:
           orchiectomy other method, specify: 
                   133                      1 

patient.primary_pathology.intratubular_germ_cell_neoplasm:
 absent present    NA's 
     68      57       9 

patient.primary_pathology.laterality:
bilateral      left     right 
        5        72        57 

patient.primary_pathology.lymphovascular_invasion_present:
  no  yes NA's 
  75   55    4 



patient.primary_pathology.molecular_test_result_list.molecular_test_result.3:
alpha fetaprotein (afp) - normal luteinizing hormone (lh)- normal 
                              96                                2 
                            NA's 
                              36 

patient.primary_pathology.molecular_test_result_list.molecular_test_result.4:
testosterone- normal                 NA's 
                   5                  129 

patient.primary_pathology.post_orchi_lymph_node_dissection:
                         no    yes - after chemotherapy 
                        109                           9 
yes - prior to chemotherapy                        NA's 
                          5                          11 

patient.primary_pathology.post_orchi_serum_marker_info.days_to_post_orchi_serum_test:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -61.00   11.00   23.50   87.08   60.25 1455.00      16 

patient.primary_pathology.post_orchi_serum_marker_info.post_orchi_afp:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   0.600    2.000    3.000   47.304    5.575 2150.500       20 

patient.primary_pathology.post_orchi_serum_marker_info.post_orchi_hcg:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    0.00     0.32     1.65   735.52    14.40 38014.00       68 

patient.primary_pathology.post_orchi_serum_marker_info.post_orchi_ldh:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  101.0   166.2   241.0   304.9   336.0  2035.0      24 

patient.primary_pathology.post_orchi_serum_marker_info.post_orchi_lh:
11.5 NA's 
   1  133 

patient.primary_pathology.post_orchi_serum_marker_info.post_orchi_testosterone:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.50    8.00   14.66  155.83  298.00  452.00     129 

patient.primary_pathology.pre_orchi_serum_marker_info.days_to_pre_orchi_serum_test:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-310.000   -7.000   -4.000   -5.888   -3.000  361.000        9 

patient.primary_pathology.pre_orchi_serum_marker_info.pre_orchi_afp:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.80    3.00    5.54  279.65   87.95 7263.00      11 

patient.primary_pathology.pre_orchi_serum_marker_info.pre_orchi_hcg:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     4.7    16.3  1257.0   146.7 89796.0      37 

patient.primary_pathology.pre_orchi_serum_marker_info.pre_orchi_ldh:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   70.0   203.8   308.0   455.9   435.8  7374.0      20 

patient.primary_pathology.pre_orchi_serum_marker_info.pre_orchi_lh:
 5.5 NA's 
   1  133 

patient.primary_pathology.pre_orchi_serum_marker_info.pre_orchi_testosterone:
 500 NA's 
   1  133 

patient.primary_pathology.synchronous_tumor_histology_list.synchronous_tumor_histology.synchronous_tumor_histology_pct:
 100 NA's 
   1  133 

patient.primary_pathology.synchronous_tumor_histology_list.synchronous_tumor_histology.synchronous_tumor_histology_type:
seminoma     NA's 
       1      133 


patient.primary_pathology.testis_tumor_macroextent_other:
epididymis and spermatic cord and tunica vaginalis 
                                                 1 
                                       rete testis 
                                                 1 
                         spermatic cord, epididmis 
                                                 1 
                                              NA's 
                                               131 

patient.primary_pathology.testis_tumor_microextent:
    epididymis      hilar fat    rete testis spermatic cord           NA's 
             8              1             24              3             98 

patient.primary_pathology.tumor_tissue_site:
testes 
   134 

patient.primary_pathology.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1992    2006    2009    2008    2012    2013 

patient.prior_dx:
 no yes 
131   3 

patient.race:
                    asian black or african american                     white 
                        4                         6                       119 
                     NA's 
                        5 

patient.radiation_therapy:
  no  yes NA's 
 103   19   12 

patient.radiations.radiation.anatomic_treatment_site:
       distant site    local recurrence primary tumor field       regional site 
                  1                   1                   2                  21 
               NA's 
                109 



patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    5.00    8.00   11.96   20.00   31.00     109 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     21      65      90     546     125    4687     109 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0    46.0    62.0   524.3   103.0  4670.0     109 

patient.radiations.radiation.measure_of_response:
complete response              NA's 
               24               110 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     4.0     6.0     6.2     7.0    11.0     109 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   13.00   13.00   15.04   17.00   24.00     110 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   24.0    26.0    26.0   936.4  2500.0  3600.0     110 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  25  109 

patient.radiations.radiation.radiation_type:
external     NA's 
      25      109 

patient.radiations.radiation.units:
 cgy   gy NA's 
   8   16  110 

patient.radiations.radiation.year_of_form_completion:
2014 2015 NA's 
  23    2  109 


patient.relation_testicular_cancer_list.relation_testicular_cancer.2:
brother  cousin    NA's 
      1       1     132 

patient.source_of_patient_death_reason:
          death certificate              medical record 
                          1                           1 
social security death index                        NA's 
                          1                         131 


patient.stage_event.igcccg_stage:
        good intermediate         poor         NA's 
          32            9            2           91 



patient.stage_event.system_version:
3rd 4th 5th 6th 7th 
  1  10  13  44  66 

patient.stage_event.tnm_categories.clinical_categories.clinical_m:
  m0   m1  m1a  m1b NA's 
 120    1    4    3    6 


patient.stage_event.tnm_categories.clinical_categories.clinical_t:
  t1   t2   t3 NA's 
  64   46    3   21 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
  m0   m1  m1a  m1b NA's 
 115    2    1    1   15 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
  n0   n1   n2   nx NA's 
  46   11    2   65   10 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_t:
t1 t2 t3 tx 
76 51  6  1 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 112   20    2 

patient.tissue_retrospective_collection_indicator:
 no yes 
 20 114 


patient.undescended_testis_corrected:
  no  yes NA's 
   6   17  111 

patient.undescended_testis_corrected_age:
  1-2 years 10-14 years 2-11 months   3-9 years        NA's 
          3           4           2           8         117 

patient.undescended_testis_method_left:
         orchiopexy spontaneous descent      testis removed                NA's 
                 11                   1                   1                 121 

patient.undescended_testis_method_right:
orchiopexy       NA's 
         3        131 

patient.vital_status:
alive  dead 
  131     3 

patient.year_of_form_completion:
2013 2014 2015 
   7  126    1 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
primary tumor and additional primary tumor(s) 
                                            5 
                                         NA's 
                                          129 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             129                5 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      133         1 

patient.biospecimen_cqcf.country:
       brazil       croatia   netherlands        russia united states 
           10             8            64             4            48 

patient.biospecimen_cqcf.days_to_sample_procurement:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-173.000   -3.000   -2.000   -3.746    0.000   13.000 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    12                    100                     22 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
testes 
   134 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
 no 
134 

patient.biospecimen_cqcf.laterality:
 left right 
   77    57 

patient.biospecimen_cqcf.method_of_sample_procurement:
orchiectomy 
        134 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
 -10   -3    0 2495 NA's 
   1    2    5    1  125 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   0.110    0.350    0.658  317.824  375.000 1213.000      129 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
nanodrop     NA's 
       5      129 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  15 NA's 
   5  129 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     25      50      80      88      85     200     129 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
 blood draw orchiectomy        NA's 
          4           5         125 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            5           129 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
   9    1  124 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -38.00   -4.00   -2.00   14.36    0.00  816.00       4 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.0730    0.4012    0.8770  121.3001   50.0000 1455.0000        54 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm:
            nanodrop                qubit spec (epoch, biotek) 
                  72                    7                    1 
                NA's 
                  54 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
  15   16 NA's 
  79    1   54 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   25.0    40.0    60.5   102.7   126.2   707.9      54 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
blood draw       NA's 
       130          4 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
  1   2 
130   4 

patient.biospecimen_cqcf.other_vessel_used:
cryobox    NA's 
     17     117 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 no yes 
 41  93 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
134 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
yes 
134 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
yes 
134 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                        3                         2                       105 
                     NA's 
                       24 

patient.biospecimen_cqcf.sample_prescreened:
yes 
134 

patient.biospecimen_cqcf.site_of_disease:
testicle 
     134 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid:
1835fa3a-417f-4e7e-bd45-3cee57f97d15                                 NA's 
                                   1                                  133 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0 NA's 
   1  133 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  75 NA's 
   1  133 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
  35 NA's 
   1  133 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   2 NA's 
   1  133 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   4.313  10.000  30.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  30.00   70.00   80.00   79.62   90.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   30.0   161.0   290.0   301.5   410.0   874.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1 
134 

patient.biospecimen_cqcf.vessel_used:
              cryomold               cryovial other (please specify) 
                    14                    103                     17 

patient.days_to_index:
  0 
134 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and additional primary tumor(s) 
                                            5 
                                         NA's 
                                          129 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     34     507    1480    2064    2654    7751 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     80      90     110     180     170     450     129 

patient.samples.sample.2.is_ffpe:
 no 
134 

patient.samples.sample.2.oct_embedded:
false  true 
  132     2 



patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.820   1.860   1.900   1.916   1.920   2.080     129 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174167056 174167069 174167104 174167099 174167131 174167134       129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  13 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.16 0.17 0.18 NA's 
   3    1    1  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  18 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   2    5    6    8 NA's 
   1    1    1    2  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a439 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   a    c    f    g NA's 
   2    1    1    1  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
 3.2  3.4  3.6 NA's 
   3    1    1  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  20 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2014 NA's 
   5  129 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174162641 174163133 174163168 174163067 174163195 174163198       129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.16 0.17 0.18 NA's 
   3    1    1  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  18 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   6 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   2    5    6    8 NA's 
   1    1    1    2  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a430 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   a    c    f    g NA's 
   2    1    1    1  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
2.08 2.21 2.34 NA's 
   3    1    1  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2014 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  43.24   48.05   50.14   54.53   57.96   73.24     129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r NA's 
   5  129 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
0.16 0.17 0.18 NA's 
   3    1    1  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        5                       129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.440   1.460   1.500   1.542   1.570   1.740     129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.3     8.2     9.1     8.7     9.3     9.6     129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      5     129 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   5  129 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167822592 174162252 174167246 173597133 174167284 174167326 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
 10 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
  34   86   14 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
 18 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
  6 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.746   8.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a42y a433 a438 
   5   59   70 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
 34  86  14 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 40 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 
 134 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167822628 174165648 174166098 173863833 174166133 174166175        29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5   26 NA's 
   5  100   29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 NA's 
  15   28   38   24   29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  18 NA's 
 105   29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 105   29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   6 NA's 
 105   29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       3       5       5       7      12      29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a42z a43h a43j NA's 
   5   57   43   29 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.500   1.600   1.589   1.700   2.080      29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 105   29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10   13 NA's 
 100    5   29 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
 105   29 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174165828 174165851 174165852 174165856 174165875 174165876       129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
  26 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.14 0.15 0.16 NA's 
   2    1    2  129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  18 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   6 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
  12 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a43g NA's 
   5  129 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 1.4  1.5  1.6 NA's 
   2    1    2  129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10 NA's 
   5  129 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2014 NA's 
   5  129 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174159836 174162576 174166560 174164836 174166740 174167604 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  20   35   51   28 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
 18 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
  6 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.746   8.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a42x a432 a437 
   5   59   70 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  20   35   51   28 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 
 134 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.64   14.17   26.66   39.17   56.80  203.92 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
134 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  20   35   51   28 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             134 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                               59                                75 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
      134 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   5  129 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   11.00   15.51   23.50   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no 
134 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   2.000   3.731   4.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
  1  11 
129   5 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
  20  114 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   5  129 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   1    2   25   90 NA's 
   2    1    1    1  129 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0   10 NA's 
   4    1  129 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0    1 NA's 
   4    1  129 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   5  129 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0 NA's 
   5  129 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   0   14   20 NA's 
   3    1    1  129 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
  80   85  100 NA's 
   1    1    3  129 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
  70   72   75   80 NA's 
   1    1    2    1  129 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
   5  129 

patient.samples.sample.2.portions.portion.weight:
   2   30 NA's 
  54    5   75 

patient.samples.sample.2.portions.portion.year_of_creation:
2013 2014 
  26  108 

patient.samples.sample.2.portions.shipment_portion.bcr_shipment_portion_uuid:
204d3eb2-7277-4814-8b64-1217b931b7fe 7c0acf92-67f1-4542-b285-79f4accb64cb 
                                   1                                    1 
e446e40a-c2a2-4dbc-bcc3-92065de540c8                                 NA's 
                                   1                                  131 

patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
   3  131 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
   3  131 

patient.samples.sample.2.portions.shipment_portion.plate_id:
a45p a45q NA's 
   1    2  131 

patient.samples.sample.2.portions.shipment_portion.portion_number:
  21 NA's 
   3  131 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode:
tcga-2g-aafg-05a-21-a45p-20 tcga-2g-aagy-05a-21-a45q-20 
                          1                           1 
tcga-2g-aakg-05a-21-a45q-20                        NA's 
                          1                         131 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
  30 NA's 
   3  131 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   7 NA's 
   3  131 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
   3  131 

patient.samples.sample.2.sample_type:
additional - new primary     blood derived normal 
                       5                      129 

patient.samples.sample.2.sample_type_id:
  5  10 
  5 129 

patient.samples.sample.2.vial_number:
  a 
134 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2572    5084    5251    4968    5671    6261     129 

patient.samples.sample.3.is_ffpe:
  no NA's 
   5  129 

patient.samples.sample.3.oct_embedded:
false  NA's 
    5   129 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174162049 174162241 174162301 174162331 174162528 174162534       129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  10 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.08 0.09 NA's 
   4    1  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  18 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     5.0     7.0     6.2     8.0     9.0     129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a433 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   a    b    c    f NA's 
   2    1    1    1  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 3.2  3.6 NA's 
   4    1  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  40 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 NA's 
   5  129 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174162695 174166111 174166147 174165459 174166169 174166174       129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  26 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.16 0.17 NA's 
   2    3  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  18 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   6 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     5.0     7.0     6.2     8.0     9.0     129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a43h NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   a    b    c    f NA's 
   2    1    1    1  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
 1.6  1.7 NA's 
   2    3  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   5  129 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174162559 174162595 174162608 174162600 174162617 174162622       129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.16 0.17 NA's 
   2    3  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  18 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   6 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     5.0     7.0     6.2     8.0     9.0     129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a432 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   a    b    c    f NA's 
   2    1    1    1  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
1.07 1.14 NA's 
   2    3  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   21.32   31.92   35.75   38.78   66.74     129 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   5  129 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
0.16 0.17 NA's 
   2    3  129 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             129 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   5  129 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
pre-extracted dna received by tss                              NA's 
                                5                               129 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        5       129 



patient.samples.sample.3.portions.portion.day_of_creation:
  10   29 NA's 
   3    2  129 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
   5  129 

patient.samples.sample.3.portions.portion.month_of_creation:
   1    2 NA's 
   2    3  129 

patient.samples.sample.3.portions.portion.portion_number:
   1 NA's 
   5  129 

patient.samples.sample.3.portions.portion.year_of_creation:
2014 NA's 
   5  129 

patient.samples.sample.3.sample_type:
blood derived normal                 NA's 
                   5                  129 

patient.samples.sample.3.sample_type_id:
  10 NA's 
   5  129 

patient.samples.sample.3.vial_number:
   a NA's 
   5  129 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     34     507    1480    2064    2654    7751 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10:
80245c64-1da8-4a2a-9e79-2b45e7c1031f 95512a55-d6dc-4b63-bd7e-d94b880a4e19 
                                   1                                    1 
de1a7777-25b0-4f7c-bc65-a2eb9bd4a1ac                                 NA's 
                                   1                                  131 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.11:
06994c72-e6e0-448c-92fa-f70faf2c39f3                                 NA's 
                                   1                                  133 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.12:
445c8711-150e-4375-a488-fffe8f0c8b72                                 NA's 
                                   1                                  133 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.13:
9239bffd-959b-4b4f-8cef-5d97a0aa48d7                                 NA's 
                                   1                                  133 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.14:
46672dbf-d443-4897-9b49-9b10e2401f26                                 NA's 
                                   1                                  133 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.15:
6c14a453-d951-439c-ae81-7c069e1e698a                                 NA's 
                                   1                                  133 




patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.5:
04d3e265-5b1c-47ae-9788-37a17295b87d 432c8d5f-95a5-4c9f-a797-bf041b45b0e7 
                                   1                                    1 
43e66954-56dd-478e-9f8b-9e5b18d1e823 6b7e04bf-8078-47f0-989f-405b547831cd 
                                   1                                    1 
                                NA's 
                                 130 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
03b7e7ed-1d73-4569-a0ad-a4b1fa269db1 7bd26ec2-4788-4e8b-974d-846a7d75929f 
                                   1                                    1 
dcd9eb0f-af72-4efc-85dd-5154db9a1d5f                                 NA's 
                                   1                                  131 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
6deda517-e34d-400e-86fd-2281fcdf9404 c68411c6-f7ac-47dc-b38a-9810b864f699 
                                   1                                    1 
f335383f-4655-4f1e-8192-61ccd1f8fee4                                 NA's 
                                   1                                  131 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.8:
0b203f9a-8526-4ae9-a622-ad65aaa2fa56 94820688-2747-423f-820f-0cd1a48d3ba2 
                                   1                                    1 
edadc900-39df-4c71-b387-ef8709826b4f                                 NA's 
                                   1                                  131 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
7841b74b-f915-401c-9fc0-2043e2e3ecb1 ae88f73e-146c-460c-98a3-b3f3b3c678f2 
                                   1                                    1 
bd0766ba-f869-4e74-bfdf-23fa5af066c6                                 NA's 
                                   1                                  131 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   137.5   225.0   305.0   352.5  1710.0       2 

patient.samples.sample.is_ffpe:
 no 
134 

patient.samples.sample.oct_embedded:
false  true 
   80    54 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.710   1.820   1.855   1.856   1.890   2.300 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174162669 174164785 174167042 174165941 174167088 174167135 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
 13 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.16 0.17 0.18 
  14   44   43   33 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
 18 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
  6 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.007   7.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a439 a43a 
  72   62 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
  3 3.2 3.4 3.6 
 14  44  43  33 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
 23 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
 20 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2014 
 134 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174159795 174162302 174163104 174162721 174163150 174163197 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  7 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.16 0.17 0.18 
  14   44   43   33 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
 18 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
  6 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.007   7.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a430 a431 
  72   62 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
1.95 2.08 2.21 2.34 
  14   44   43   33 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
 23 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
 13 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2014 
 134 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   8.14   33.01   43.65   45.40   55.12   98.70 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
  r 
134 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
0.15 0.16 0.17 0.18 
  14   44   43   33 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             134 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                      134 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.110   1.353   1.510   1.490   1.728   2.090 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.900   8.700   8.567   9.200  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
    134 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
 115   10    9 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174159787 174162101 174167536 174164931 174167574 174167614 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
 10 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
  34   92    8 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
 18 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
  6 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    3.00    5.50    5.53    8.00   11.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a42y a435 
  64   70 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
 34  92   8 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 40 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 
 134 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167822607 167822646 174166576 171136659 174166614 174166654 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  5 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 
  13   46   49   26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
 18 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
  6 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    3.00    5.50    5.53    8.00   11.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a42z a436 
  64   70 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.82 1.95 2.08 2.21 
  13   46   49   26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
 23 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
 13 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 
 134 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174162686 174164823 174165835 174165677 174165867 174167316        29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
  26 NA's 
 105   29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.14 0.15 0.16 0.17 NA's 
  12   30   40   23   29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  18 NA's 
 105   29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 105   29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   6 NA's 
 105   29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.724   7.000  10.000      29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a43g a43i NA's 
  62   43   29 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 1.4  1.5  1.6  1.7 NA's 
  12   30   40   23   29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 105   29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10 NA's 
 105   29 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2014 NA's 
 105   29 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167822507 174163350 174166672 174070474 174166710 174166750 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  13   46   49   26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
 18 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
  6 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    3.00    5.50    5.53    8.00   11.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a42x a434 
  64   70 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  13   46   49   26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
134 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 
 134 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.95   26.18   34.72   33.86   41.04   66.13 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
134 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
134 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  13   46   49   26 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
134 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
134 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             134 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  134 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
      134 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
 115   10    9 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00   11.50   18.00   15.92   20.00   30.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
134 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   2.000   4.239   4.000  12.000 

patient.samples.sample.portions.portion.portion_number:
 11  12  21  31 
109  10   7   8 

patient.samples.sample.portions.portion.portion_sequence:
   1    3 NA's 
  28    1  105 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
134 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    2.00   17.74   40.00   90.00 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   5.276  10.000  25.000 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   2.403   5.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   4.261   4.250  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
  0  10  25  40  50 
130   1   1   1   1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00   10.00   10.17   20.00   40.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  45.00   75.00   90.00   86.49  100.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   70.00   80.00   77.87   85.00  100.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
top 
134 

patient.samples.sample.portions.portion.weight:
 10  15  20  30 
  2   1   2 129 

patient.samples.sample.portions.portion.year_of_creation:
2013 2014 
  31  103 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 104   30 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 104   30 

patient.samples.sample.portions.shipment_portion.plate_id:
a45p a45q NA's 
  65   39   30 

patient.samples.sample.portions.shipment_portion.portion_number:
  11   21   41 NA's 
   4   98    2   30 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   2 NA's 
  17  117 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  30 NA's 
 104   30 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   7 NA's 
 104   30 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
 104   30 

patient.samples.sample.sample_type:
primary tumor 
          134 

patient.samples.sample.sample_type_id:
  1 
134 

patient.samples.sample.vial_number:
  a   b 
133   1 

}}
\keyword{datasets}
