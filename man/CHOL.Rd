\name{ CHOL }
\alias{ CHOL }
\docType{data}
\title{ Cholangiocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( CHOL )
ExperimentList class object of length 9: 
 [1] CNASNP-20160128: RaggedExperiment with 39698 rows and 85 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 7570 rows and 85 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 36 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 36 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 45 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 45 columns 
 [7] Mutation-20160128: RaggedExperiment with 6755 rows and 35 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 45 columns 
 [9] RPPAArray-20160128: SummarizedExperiment with 192 rows and 30 columns 

> rownames( CHOL )
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

> colnames( CHOL )
CharacterList of length 9
[["CNASNP-20160128"]] TCGA-3X-AAV9-01A-72D-A416-01 ...
[["CNVSNP-20160128"]] TCGA-3X-AAV9-01A-72D-A416-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-3X-AAV9-01A-72D-A416-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-3X-AAV9-01A-72D-A416-01 ...
[["Methylation-20160128"]] TCGA-3X-AAV9-01A-72D-A418-05 ...
[["miRNASeqGene-20160128"]] TCGA-3X-AAV9-01A-72R-A41D-13 ...
[["Mutation-20160128"]] TCGA-3X-AAV9-01A-72D-A417-09 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-3X-AAV9-01A-72R-A41I-07 ...
[["RPPAArray-20160128"]] TCGA-3X-AAV9-01A-81-A45N-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  1.1 Mb
2                   CNVSNP-20160128  0.2 Mb
3         GISTIC_AllByGene-20160128  4.3 Mb
4 GISTIC_ThresholdedByGene-20160128  4.3 Mb
5              Methylation-20160128 67.4 Mb
6             miRNASeqGene-20160128  0.1 Mb
7                 Mutation-20160128  4.3 Mb
8          RNASeq2GeneNorm-20160128  1.1 Mb
9                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   18 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 18.000  18.000   1.370   0.742   2.532 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  29.00   56.50   66.50   63.03   72.00   82.00 

vital_status:
 0  1 
18 18 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0   195.2   500.0   592.9   731.0  1939.0      18 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   539.8   942.5   936.6  1394.0  1976.0      18 

tumor_tissue_site:
bile duct 
       36 


pathology_T_stage:
 t1  t2 t2a t2b  t3 
 19   6   2   4   5 

pathology_N_stage:
n0 n1 nx 
26  5  5 

pathology_M_stage:
m0 m1 mx 
28  5  3 

gender:
female   male 
    20     16 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2005    2010    2011    2010    2012    2013 

radiation_therapy:
  no NA's 
  35    1 

histological_type:
         cholangiocarcinoma; distal cholangiocarcinoma; hilar/perihilar 
                                  2                                   4 
   cholangiocarcinoma; intrahepatic 
                                 30 

residual_tumor:
r0 r1 rx 
28  5  3 

race:
                    asian black or african american                     white 
                        3                         2                        31 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     2                     33                      1 

admin.bcr:
nationwide children's hospital 
                            36 

admin.day_of_dcc_upload:
 1 
36 

admin.disease_code:
chol 
  36 


admin.month_of_dcc_upload:
10 
36 

admin.patient_withdrawal.withdrawn:
false 
   36 

admin.project_code:
tcga 
  36 

admin.year_of_dcc_upload:
2015 
  36 





patient.blood_relative_cancer_history_list.blood_relative_cancer_history.3.family_member_relationship_type:
    brother      father grandmother      sister        NA's 
          1           1           1           2          31 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.family_cancer_type_txt:
non-invasive ductal carcinoma in situ of the breast 
                                                  1 
                                               NA's 
                                                 35 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.family_member_relationship_type:
sister   NA's 
     1     35 



patient.cancer_first_degree_relative:
   1    2    3    4 NA's 
  10    6    3    1   16 

patient.clinical_cqcf.consent_or_death_status:
consented    waiver 
       34         2 

patient.clinical_cqcf.country:
       brazil        canada         italy united states       vietnam 
            1             5             2            27             1 

patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-408.00  -17.75   -6.50   59.76    0.00 1225.00       2 

patient.clinical_cqcf.histological_type:
      cholangiocarcinoma; distal cholangiocarcinoma; intrahepatic 
                               2                               30 
                            NA's 
                               4 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                              35 
yes, pharmaceutical treatment prior to resection 
                                               1 

patient.clinical_cqcf.normal_tissue_anatomic_site:
liver  NA's 
   16    20 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           16            20 

patient.clinical_cqcf.tumor_type:
primary 
     36 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00   11.00   23.00   19.58   24.50   29.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -30039  -26385  -24592  -23185  -20910  -10659 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0   245.8   536.0   621.8   786.8  1939.0      20 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   145.0   360.0   531.4   771.2  1976.0      16 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0    16.0    20.0    18.8    21.0    26.0      31 

patient.drugs.drug.2.days_to_drug_therapy_end:
  76  123  231  326 NA's 
   1    1    1    1   32 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   33.0    41.0    63.0   104.4   132.0   253.0      31 

patient.drugs.drug.2.drug_name:
capecitabine    cisplatin  gemcitabine         NA's 
           1            3            1           31 

patient.drugs.drug.2.measure_of_response:
clinical progressive disease             partial response 
                           2                            1 
              stable disease                         NA's 
                           1                           32 

patient.drugs.drug.2.month_of_form_completion:
   3    5    8   10 NA's 
   1    1    1    2   31 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
   4    1   31 

patient.drugs.drug.2.therapy_types.therapy_type:
chemotherapy         NA's 
           5           31 

patient.drugs.drug.2.tx_on_clinical_trial:
  no NA's 
   5   31 

patient.drugs.drug.2.year_of_form_completion:
2014 NA's 
   5   31 



patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   16.00   20.00   19.44   24.00   26.00      27 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   76.0   142.5   205.0   201.3   258.5   326.0      29 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     33      56      63     126     132     427      27 

patient.drugs.drug.drug_name:
  cisplatin gemcitabine      gemzar        NA's 
          1           6           2          27 

patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           2                            2 
            partial response               stable disease 
                           2                            2 
                        NA's 
                          28 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   7.000   6.778  10.000  10.000      27 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
   7    2   27 

patient.drugs.drug.therapy_types.therapy_type:
chemotherapy         NA's 
           9           27 

patient.drugs.drug.tx_on_clinical_trial:
  no NA's 
   9   27 

patient.drugs.drug.year_of_form_completion:
2014 NA's 
   9   27 

patient.eastern_cancer_oncology_group:
   0    1    3 NA's 
  19    9    1    7 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     2                     33                      1 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   21.00   21.00   21.67   26.00   31.00      21 

patient.follow_ups.follow_up.days_to_death:
 169  555 NA's 
   1    1   34 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   309.0   519.0   666.7  1015.0  1976.0      23 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
   9    6   21 

patient.follow_ups.follow_up.month_of_form_completion:
   1   10   11   12 NA's 
   3   10    1    1   21 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   1   35 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   1   35 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 152 NA's 
   1   35 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    1    35 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
intrahepatic recurrence                    NA's 
                      1                      35 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_ablation_embo_tx:
  no NA's 
   1   35 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_liver_transplant:
  no NA's 
   1   35 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no NA's 
   2   34 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   3   33 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
  40   59  549 NA's 
   1    1    1   33 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
                             liver non-regional / distant lymph nodes 
                                 2                                  1 
                              NA's 
                                33 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence locoregional recurrence 
                      1                       1                       1 
                   NA's 
                     33 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
   2    1   33 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2   34 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_liver_transplant:
  no NA's 
   3   33 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  10    4   22 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        10          4         22 

patient.follow_ups.follow_up.post_op_ablation_embolization_tx:
  no NA's 
  14   22 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  11    3   22 

patient.follow_ups.follow_up.radiation_therapy:
  no NA's 
  14   22 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
   13     2    21 

patient.follow_ups.follow_up.year_of_form_completion:
2014 2015 NA's 
  12    3   21 

patient.gender:
female   male 
    20     16 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  144.0   159.5   167.0   167.3   175.5   192.0       1 


patient.history_hepato_carcinoma_risk_factors.hist_hepato_carc_fact.2:
                diabetes mellitus non-alcoholic fatty liver disease 
                                1                                 1 
                          smoking                              NA's 
                                3                                31 

patient.history_hepato_carcinoma_risk_factors.hist_hepato_carc_fact.3:
smoking    NA's 
      1      35 

patient.history_hepato_carcinoma_risk_factors.hist_hepato_carcinoma_risk:
pancreatitis         NA's 
           1           35 

patient.history_of_neoadjuvant_treatment:
 no yes 
 35   1 

patient.icd_10:
c22.0 c22.1 c24.0 
    1    31     4 

patient.icd_o_3_histology:
8160/3 
    36 

patient.icd_o_3_site:
c22.0 c22.1 c24.0 
    1    31     4 

patient.informed_consent_verified:
yes 
 36 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   5.000   5.694   7.000  12.000 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    4   31 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   3    2   31 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 907 NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   72.0   168.0   240.0   271.2   337.5   561.0      30 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
          bone          liver           lung other, specify           NA's 
             2              1              2              1             30 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence                    NA's 
                      5                       1                      30 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
mesentery, gerota's fascia, extensive involvement in chest 
                                                         1 
                                                      NA's 
                                                        35 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_ablation_embo_tx:
  no NA's 
   5   31 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   4    1   31 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_liver_transplant:
  no NA's 
   5   31 

patient.new_tumor_events.new_tumor_event.2.residual_disease_new_tumor_event:
  rx NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    1   34 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   2   34 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 264  896 NA's 
   1    1   34 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
bone lung NA's 
   1    1   34 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
extrahepatic recurrence                    NA's 
                      2                      34 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_ablation_embo_tx:
  no NA's 
   2   34 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2   34 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_liver_transplant:
  no NA's 
   2   34 

patient.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
 yes NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_additional_surgery_procedure:
1021 NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 993 NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
bone NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
extrahepatic recurrence                    NA's 
                      1                      35 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_ablation_embo_tx:
  no NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_liver_transplant:
  no NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.4.residual_disease_new_tumor_event:
  rx NA's 
   1   35 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   4   11   21 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  11    3   22 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 226  245  296 NA's 
   1    1    1   33 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0    95.0   217.0   211.9   281.0   424.0      19 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
          bone          liver           lung other, specify           NA's 
             1              9              2              2             22 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
extrahepatic recurrence intrahepatic recurrence locoregional recurrence 
                      2                       7                       5 
      new primary tumor                    NA's 
                      3                      19 

patient.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
adrenal gland       bladder          NA's 
            1             1            34 

patient.new_tumor_events.new_tumor_event.new_tumor_event_ablation_embo_tx:
  no  yes NA's 
  13    1   22 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  11    4   21 

patient.new_tumor_events.new_tumor_event.new_tumor_event_liver_transplant:
  no NA's 
  12   24 

patient.new_tumor_events.new_tumor_event.residual_disease_new_tumor_event:
  r0   r1 NA's 
   2    2   32 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  13   17    6 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        15         19          2 

patient.post_op_ablation_embolization_tx:
  no NA's 
  33    3 

patient.postoperative_rx_tx:
  no  yes NA's 
  23   10    3 

patient.primary_pathology.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  29.00   56.50   66.50   63.03   72.00   82.00 

patient.primary_pathology.albumin_result_lower_limit:
 3.3  3.4  3.5 NA's 
   4    5   22    5 

patient.primary_pathology.albumin_result_specified_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.400   3.950   4.200   4.084   4.400   4.800       5 

patient.primary_pathology.albumin_result_upper_limit:
 4.7  4.8    5 NA's 
   1    9   21    5 

patient.primary_pathology.bilirubin_lower_limit:
   0  0.1  0.2  0.3 NA's 
   9   17    3    3    4 

patient.primary_pathology.bilirubin_upper_limit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.4000  0.6000  0.6375  0.7000  1.7000       4 

patient.primary_pathology.ca_19_9_level:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    25.5    48.4   343.6   202.0  6910.0       6 

patient.primary_pathology.ca_19_9_level_lower:
   0 NA's 
  28    8 

patient.primary_pathology.ca_19_9_level_upper:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  35.00   36.95   55.00   48.35   55.00   55.00       5 

patient.primary_pathology.child_pugh_classification_grade:
   a    b NA's 
  19    2   15 

patient.primary_pathology.cholangitis_tissue_evidence:
none NA's 
  26   10 

patient.primary_pathology.creatinine_lower_level:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.4000  0.6000  0.6000  0.6412  0.7000  0.9000       2 

patient.primary_pathology.creatinine_upper_limit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.100   1.100   1.188   1.300   1.400       2 

patient.primary_pathology.creatinine_value_in_mg_dl:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.5000  0.8000  0.9000  0.8588  0.9750  1.4000       2 

patient.primary_pathology.days_to_initial_pathologic_diagnosis:
 0 
36 

patient.primary_pathology.fetoprotein_outcome_lower_limit:
   0 NA's 
  25   11 

patient.primary_pathology.fetoprotein_outcome_upper_limit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.000   6.000   6.000   7.484   9.000  15.000      11 

patient.primary_pathology.fetoprotein_outcome_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.380   2.350   3.000   3.753   4.075  14.000      12 

patient.primary_pathology.fibrosis_ishak_score:
      0 - no fibrosis 1,2 - portal fibrosis   3,4 - fibrous speta 
                   16                     9                     2 
                 NA's 
                    9 

patient.primary_pathology.histological_type:
         cholangiocarcinoma; distal cholangiocarcinoma; hilar/perihilar 
                                  2                                   4 
   cholangiocarcinoma; intrahepatic 
                                 30 

patient.primary_pathology.inter_norm_ratio_lower_limit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.800   0.900   2.137   0.900  10.400       9 

patient.primary_pathology.intern_norm_ratio_upper_limit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.100   1.200   1.200   2.904   1.200  13.100       9 

patient.primary_pathology.neoplasm_histologic_grade:
g1 g2 g3 g4 
 1 15 18  2 

patient.primary_pathology.perineural_invasion_present:
  no  yes NA's 
  26    7    3 

patient.primary_pathology.platelet_result_count:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   134.0    205.8    254.5  60388.8    498.0 354000.0        2 

patient.primary_pathology.platelet_result_lower_limit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    140     150     150   33496     150  150000       2 

patient.primary_pathology.platelet_result_upper_limit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    400     450     450  103569     450  450000       2 

patient.primary_pathology.prothrombin_time_result_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.80    1.00    1.05    2.30    1.10   12.20       4 

patient.primary_pathology.residual_tumor:
r0 r1 rx 
28  5  3 


patient.primary_pathology.surgical_procedure_name:
right hepatectomy              NA's 
                2                34 

patient.primary_pathology.total_bilirubin_upper_limit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.000   1.000   1.072   1.200   1.400       4 

patient.primary_pathology.tumor_tissue_site:
bile duct 
       36 

patient.primary_pathology.vascular_tumor_cell_type:
micro  none  NA's 
    5    29     2 

patient.primary_pathology.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2005    2010    2011    2010    2012    2013 

patient.prior_dx:
 no yes 
 30   6 

patient.race:
                    asian black or african american                     white 
                        3                         2                        31 

patient.radiation_therapy:
  no NA's 
  33    3 

patient.radiations.radiation.anatomic_treatment_site:
distant recurrence               NA's 
                 1                 35 

patient.radiations.radiation.bcr_radiation_barcode:
tcga-3x-aava-r64408                NA's 
                  1                  35 

patient.radiations.radiation.bcr_radiation_uuid:
69454ccd-16f1-4e8a-8bd3-218fa9004560                                 NA's 
                                   1                                   35 

patient.radiations.radiation.day_of_form_completion:
  29 NA's 
   1   35 

patient.radiations.radiation.days_to_radiation_therapy_end:
 277 NA's 
   1   35 

patient.radiations.radiation.days_to_radiation_therapy_start:
 271 NA's 
   1   35 

patient.radiations.radiation.measure_of_response:
radiographic progressive disease                             NA's 
                               1                               35 

patient.radiations.radiation.month_of_form_completion:
   8 NA's 
   1   35 

patient.radiations.radiation.numfractions:
   5 NA's 
   1   35 

patient.radiations.radiation.radiation_dosage:
2000 NA's 
   1   35 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
   1   35 

patient.radiations.radiation.radiation_type:
external     NA's 
       1       35 

patient.radiations.radiation.units:
 cgy NA's 
   1   35 

patient.radiations.radiation.year_of_form_completion:
2014 NA's 
   1   35 

patient.relative_family_cancer_history:
  no  yes NA's 
  12   21    3 


patient.stage_event.system_version:
6th 7th 
  7  29 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
m0 m1 mx 
28  5  3 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
n0 n1 nx 
26  5  5 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_t:
 t1  t2 t2a t2b  t3 
 19   6   2   4   5 

patient.tissue_prospective_collection_indicator:
 no yes 
 24  12 

patient.tissue_retrospective_collection_indicator:
 no yes 
 12  24 


patient.vital_status:
alive  dead 
   20    16 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  49.00   66.50   76.50   79.53   89.50  128.00 

patient.year_of_form_completion:
2013 2014 
   1   35 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
case matched expression control             germline comparison 
                              9                               4 
                           NA's 
                             23 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control                             NA's 
                               9                               27 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                  9                  27 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
              23               13 

patient.biospecimen_cqcf.consent_or_death_status:
consented    waiver 
       34         2 

patient.biospecimen_cqcf.country:
       brazil        canada         italy united states       vietnam 
            1             5             2            27             1 

patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    5.0   311.2   700.0   939.7  1273.0  3061.0 

patient.biospecimen_cqcf.days_to_sample_procurement:
-6  0  1  7 
 2 32  1  1 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     2                     29                      5 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
bile duct 
       36 

patient.biospecimen_cqcf.histological_type:
      cholangiocarcinoma; distal cholangiocarcinoma; intrahepatic 
                               2                               30 
                            NA's 
                               4 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                              35 
yes, pharmaceutical treatment prior to resection 
                                               1 

patient.biospecimen_cqcf.method_of_sample_procurement:
surgical resection 
                36 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   0  590 NA's 
  15    1   20 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
                 2                 14                 20 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
liver  NA's 
   14    22 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           14            22 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1 NA's 
  16   20 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-210.00  -17.25   -6.00   -3.50    0.00  451.00 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1000  0.1000  0.1361  0.1890  0.2550       7 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm:
nanodrop  trinean     NA's 
      11       18        7 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
  15   20 NA's 
  24    5    7 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  58.79  111.00  150.00  128.91  150.00  155.00       7 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
        blood draw surgical resection 
                34                  2 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
liver  NA's 
    2    34 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            2            34 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
 1 
36 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 no yes 
  1  35 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
 36 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
yes 
 36 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
yes 
 36 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                        2                         2                        31 
                     NA's 
                        1 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
macrodissection performed at tss to select for a region containing an acceptable tcga diagnosis (see note at right) 
                                                                                                                  1 
                                                                                                               NA's 
                                                                                                                 35 

patient.biospecimen_cqcf.sample_prescreened:
yes 
 36 

patient.biospecimen_cqcf.site_of_disease:
bile duct 
       36 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid:
3e6a3029-7ce4-4675-afdd-09b73611aa3e                                 NA's 
                                   1                                   35 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0 NA's 
   1   35 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  60 NA's 
   1   35 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
 200 NA's 
   1   35 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1 NA's 
   1   35 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   5.000   5.000   8.222  10.000  60.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  20.00   50.00   67.50   64.31   80.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   35.0   172.5   200.0   205.1   200.0   510.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
 1 
36 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cryomold                cryovial 
                      1                       7                      28 

patient.days_to_index:
 0 
36 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     28     416     928    1047    1344    3131 

patient.samples.sample.2.initial_weight:
  60  150 NA's 
   1    1   34 

patient.samples.sample.2.is_ffpe:
no 
36 

patient.samples.sample.2.oct_embedded:
false  true 
   35     1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174190579 174190588 174190610 174190615 174190633 174190655 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
 9 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 
  16   20 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
28 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 5 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.083   4.000   6.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a41a 
  36 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 
 16  20 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
23 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
40 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 
  36 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174189315 174189348 174190568 174190210 174190602 174190740 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  11    9   14    2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
28 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 5 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.083   4.000   6.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a419 
  36 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  11    9   14    2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
23 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
 36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 
  36 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.66   11.32   12.37   17.75   15.24   98.03 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
 36 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
 d 
36 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  11    9   14    2 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 36 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
 36 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              36 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
36 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                                8                                28 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       36 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   2   34 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    6.0    13.0    13.0    16.5    18.0    30.0 

patient.samples.sample.2.portions.portion.is_ffpe:
no 
36 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   3.583   4.250  12.000 

patient.samples.sample.2.portions.portion.portion_number:
 1 11 
34  2 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
   1   35 

patient.samples.sample.2.portions.portion.slides.slide.bcr_slide_barcode:
tcga-4g-aazo-11a-01-ts1 tcga-w5-aa2z-11a-01-tsa                    NA's 
                      1                       1                      34 

patient.samples.sample.2.portions.portion.slides.slide.bcr_slide_uuid:
2933f8fc-ae3e-4df4-b469-acc4aa7f5d7c 8ac8dbde-0535-4232-b785-c355ba604e61 
                                   1                                    1 
                                NA's 
                                  34 

patient.samples.sample.2.portions.portion.slides.slide.image_file_name:
tcga-4g-aazo-11a-01-ts1.8ac8dbde-0535-4232-b785-c355ba604e61.svs 
                                                               1 
tcga-w5-aa2z-11a-01-tsa.2933f8fc-ae3e-4df4-b469-acc4aa7f5d7c.svs 
                                                               1 
                                                            NA's 
                                                              34 

patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   2   34 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   6 NA's 
   2   34 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   2   34 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   2   34 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   2   34 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
  90 NA's 
   2   34 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
  10 NA's 
   2   34 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   2   34 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   2   34 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
   2   34 

patient.samples.sample.2.portions.portion.weight:
   2   30 NA's 
   6    2   28 

patient.samples.sample.2.portions.portion.year_of_creation:
2013 2014 
   8   28 

patient.samples.sample.2.sample_type:
blood derived normal  solid tissue normal 
                  34                    2 

patient.samples.sample.2.sample_type_id:
10 11 
34  2 

patient.samples.sample.2.vial_number:
 a 
36 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     45     959    1238    1524    2542    3131      23 

patient.samples.sample.3.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   80.0   150.0   190.0   185.4   240.0   320.0      23 

patient.samples.sample.3.is_ffpe:
  no NA's 
  13   23 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
    2    11    23 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   1.820   1.870   1.856   1.880   1.910      27 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              9                              27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174164267 174164291 174164315 174169968 174164339 174189827        27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.16 0.17 0.18 NA's 
   4    4    1   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  28 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   5 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
  11   12 NA's 
   8    1   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a41i NA's 
   9   27 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
2.08 2.21 2.34 NA's 
   4    4    1   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2014 NA's 
   9   27 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              9                              27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174191626 174191650 174191674 174191668 174191698 174191701        27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  13 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.16 0.17 0.18 NA's 
   4    4    1   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  28 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   5 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
  11   12 NA's 
   8    1   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a41d NA's 
   9   27 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
 3.2  3.4  3.6 NA's 
   4    4    1   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2014 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  33.10   47.57   59.85   56.06   67.55   77.18      27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r NA's 
   9   27 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.16 0.17 0.18 NA's 
   4    4    1   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               9 
                                            NA's 
                                              27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        9                        27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.060   1.180   1.320   1.294   1.370   1.540      27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.100   7.600   8.000   7.933   8.200   8.700      27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      9      27 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   9   27 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                 13                  23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174189408 174189446 174190092 174190046 174190623 174190647        23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   9 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 NA's 
   1   10    2   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  28 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   5 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   8    9   10 NA's 
   1   10    2   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a417 a41a NA's 
   9    4   23 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 2.8  3.2  3.6 NA's 
   1   10    2   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  40 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 NA's 
  13   23 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched methylation control                             NA's 
                               9                               27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174190095 174190119 174190143 174190140 174190161 174190167        27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 NA's 
   1    2    3    3   27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  28 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   5 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   9   10 NA's 
   7    2   27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a418 NA's 
   9   27 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.82 1.95 2.08 2.21 NA's 
   1    2    3    3   27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  13 NA's 
   9   27 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   9   27 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                 13                  23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174189423 174189464 174189489 174189818 174190567 174190691        23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 NA's 
   1    2    6    4   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  28 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   5 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   8    9   10 NA's 
   1   10    2   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a416 a419 NA's 
   9    4   23 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 NA's 
   1    2    6    4   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.22   16.49   20.81   20.46   23.84   31.17      23 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  13   23 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 NA's 
   1    2    6    4   23 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              13 
                                            NA's 
                                              23 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  13   23 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                   13                    23 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
       13        23 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
  13   23 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   20.00   20.00   20.54   23.00   27.00      23 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  13   23 

patient.samples.sample.3.portions.portion.month_of_creation:
   1    4   12 NA's 
  11    1    1   23 

patient.samples.sample.3.portions.portion.portion_number:
  11 NA's 
  13   23 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  13   23 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    2    3    4 NA's 
   4    5    1    3   23 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1 NA's 
   9    4   23 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
  13   23 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    2    3 NA's 
  10    2    1   23 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
  70   80   94  100 NA's 
   1    2    1    9   23 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0    6   20   30 NA's 
   9    1    2    1   23 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
  13   23 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
  13   23 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
  13   23 

patient.samples.sample.3.portions.portion.weight:
  30 NA's 
  13   23 

patient.samples.sample.3.portions.portion.year_of_creation:
2013 2014 NA's 
   1   12   23 

patient.samples.sample.3.sample_type:
solid tissue normal                NA's 
                 13                  23 

patient.samples.sample.3.sample_type_id:
  11 NA's 
  13   23 

patient.samples.sample.3.vial_number:
   a NA's 
  13   23 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     28     416     928    1047    1344    3131 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10:
862f3b72-f434-44e5-b04a-9397a3f3f8b6                                 NA's 
                                   1                                   35 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.2:
443ef23c-06bf-4706-b7b3-0c775479393f                                 NA's 
                                   1                                   35 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
e225a636-c3de-4263-9584-7c2fe9fc284a                                 NA's 
                                   1                                   35 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.4:
441bfdcf-ef8a-4421-bd74-2e442da7534a                                 NA's 
                                   1                                   35 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.5:
56844b4a-6156-4d41-9d1b-4968eb072934                                 NA's 
                                   1                                   35 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
c5e84431-ba77-4697-a171-7cde92871a88                                 NA's 
                                   1                                   35 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
a673d61d-4926-4c9e-9cba-cbfd0458e022                                 NA's 
                                   1                                   35 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.8:
fa47062a-99b8-4095-8747-1f97178ca7f3                                 NA's 
                                   1                                   35 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
b427f36e-5326-4839-a3d4-440aba859721                                 NA's 
                                   1                                   35 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0   120.0   200.0   230.6   265.0   740.0       1 

patient.samples.sample.is_ffpe:
no 
36 

patient.samples.sample.oct_embedded:
false  true 
   10    26 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.710   1.820   1.860   1.849   1.880   1.940 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174189802 174191573 174192131 174191806 174192157 174192183 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
 7 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.16 0.17 0.18 
   7   12   10    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
28 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
no 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
 5 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   2.861   4.000   8.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a41i 
  36 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
1.95 2.08 2.21 2.34 
   7   12   10    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
23 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2014 
  36 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174191616 174191641 174191666 174191667 174191689 174191711 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
13 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.16 0.17 0.18 
   7   12   10    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
28 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
no 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
 5 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   2.861   4.000   8.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a41d 
  36 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
  3 3.2 3.4 3.6 
  7  12  10   7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
23 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
20 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2014 
  36 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   9.19   24.45   39.10   40.48   53.20   76.39 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
 36 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
 r 
36 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
0.15 0.16 0.17 0.18 
   7   12   10    7 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              36 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
no 
36 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                       36 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.190   1.327   1.525   1.537   1.752   2.010 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.100   7.500   7.900   8.056   8.800   9.100 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
     36 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  29    6    1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174189410 174189445 174190082 174189850 174190117 174190646 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
 9 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
   3   31    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
28 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 5 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.083   4.000   6.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a417 
  36 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
  3  31   2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
23 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
40 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 
  36 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174190099 174190108 174190130 174190142 174190154 174190413 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
 5 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.15 0.16 0.17 
   6   21    9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
28 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
no 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
 5 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.083   4.000   6.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a418 
  36 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.95 2.08 2.21 
   6   21    9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
23 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
13 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 
  36 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
174189427 174189436 174189458 174189463 174189481 174189503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.15 0.16 0.17 
   6   21    9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
28 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 5 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.083   4.000   6.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a416 
  36 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
1.01 1.07 1.14 
   6   21    9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
23 
36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
 36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 
  36 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   8.77   19.84   27.36   26.48   31.89   48.17 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
 36 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
 d 
36 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.15 0.16 0.17 
   6   21    9 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 36 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
 36 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              36 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
36 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                   36 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       36 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  29    6    1 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   16.75   20.00   18.44   23.00   27.00 

patient.samples.sample.portions.portion.is_ffpe:
no 
36 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   3.694   4.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  11.00   11.00   11.00   16.44   12.00   72.00 

patient.samples.sample.portions.portion.portion_sequence:
   1 NA's 
   2   34 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
no 
36 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.500   3.000   6.028   4.250  80.000 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
 0  1  2 20 
23  8  4  1 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.472   1.000  15.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0       0       0       1       2       5 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
 0  4 10 15 35 
32  1  1  1  1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.0     0.0    20.0    20.5    34.5    75.0 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.00   62.25   75.00   76.25  100.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   63.75   70.00   71.67   75.00   95.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
top 
 36 

patient.samples.sample.portions.portion.weight:
10 30 
 1 35 

patient.samples.sample.portions.portion.year_of_creation:
2013 2014 
   8   28 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
  30    6 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
  30    6 

patient.samples.sample.portions.shipment_portion.plate_id:
a45n NA's 
  30    6 

patient.samples.sample.portions.shipment_portion.portion_number:
  21   31   41   81 NA's 
  26    1    2    1    6 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   2 NA's 
   2   34 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  30 NA's 
  30    6 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   7 NA's 
  30    6 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
  30    6 

patient.samples.sample.sample_type:
primary tumor 
           36 

patient.samples.sample.sample_type_id:
 1 
36 

patient.samples.sample.vial_number:
 a 
36 

}}
\keyword{datasets}
