\name{ UCS }
\alias{ UCS }
\docType{data}
\title{ UCS }

\details{
\preformatted{
> UCS
A MultiAssayExperiment object of 6 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 6: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 57 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 56 columns 
 [3] Methylation: SummarizedExperiment with 485577 rows and 57 columns 
 [4] RPPAArray: ExpressionSet with 192 rows and 48 columns 
 [5] gistica: SummarizedExperiment with 24776 rows and 56 columns 
 [6] gistict: SummarizedExperiment with 24776 rows and 56 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( UCS )
CharacterList of length 6
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( UCS )
CharacterList of length 6
[["RNASeq2GeneNorm"]] TCGA-N5-A4R8-01A-11R-A28V-07 ...
[["miRNASeqGene"]] TCGA-N5-A4R8-01A-11R-A28Z-13 ...
[["Methylation"]] TCGA-N5-A4R8-01A-11D-A28S-05 ... TCGA-QN-A5NN-01A-11D-A28S-05
[["RPPAArray"]] TCGA-N5-A4R8-01A-21-A41P-20 ... TCGA-QN-A5NN-01A-21-A41P-20
[["gistica"]] TCGA-N5-A4RA-01A-11D-A28Q-01 ... TCGA-QN-A5NN-01A-11D-A28Q-01
[["gistict"]] TCGA-N5-A4RA-01A-11D-A28Q-01 ... TCGA-QN-A5NN-01A-11D-A28Q-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.1 Mb
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

   22 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  35.00   35.00    1.43    1.04    1.96 


--------------------------- 
Available sample meta-data: 
--------------------------- 


years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  51.00   62.00   68.00   69.72   76.00   90.00 

vital_status: 
 0  1 
22 35 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   304.0   522.0   705.4   790.5  3115.0      22 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   575.8   828.0  1160.1  1647.5  4269.0      31 

tumor_tissue_site: 
uterus 
    57 

gender: 
female 
    57 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2002    2007    2009    2009    2011    2012 

radiation_therapy: 
  no  yes NA's 
  29   25    3 

histological_type: 
uterine carcinosarcoma/ malignant mixed mullerian tumor (mmmt): nos 
                                                                 24 
                    uterine carcinosarcoma/ mmmt: heterologous type 
                                                                 20 
                      uterine carcinosarcoma/ mmmt: homologous type 
                                                                 13 

race: 
                    asian black or african american                     white 
                        3                         9                        44 
                     NA's 
                        1 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     1                     43                     13 

admin.bcr: 
nationwide children's hospital 
                            57 

admin.day_of_dcc_upload: 
 1 
57 

admin.disease_code: 
ucs 
 57 


admin.month_of_dcc_upload: 
10 
57 

admin.patient_withdrawal.withdrawn: 
false 
   57 

admin.project_code: 
tcga 
  57 

admin.year_of_dcc_upload: 
2015 
  57 


patient.birth_control_pill_history_usage_category: 
former user  never used        NA's 
          7          17          33 

patient.clinical_cqcf.anatomic_neoplasm_subdivision: 
                         endometrium                         fundus uteri 
                                  33                                    1 
lower uterine segment/ isthmus uteri                           myometrium 
                                   1                                    1 
                unknown - uterus nos 
                                  21 

patient.clinical_cqcf.consent_or_death_status: 
consented  deceased 
       56         1 

patient.clinical_cqcf.country: 
united states       vietnam 
           56             1 

patient.clinical_cqcf.days_to_consent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -13.00    5.00   14.00  108.68   25.25 2841.00       1 

patient.clinical_cqcf.days_to_death: 
 949 NA's 
   1   56 

patient.clinical_cqcf.histological_type: 
uterine carcinosarcoma/ malignant mixed mullerian tumor (mmmt): nos 
                                                                 57 

patient.clinical_cqcf.history_of_neoadjuvant_treatment: 
no 
57 

patient.clinical_cqcf.normal_tissue_anatomic_site: 
fallopian tube     myometrium          other          ovary           NA's 
             1              2              2              3             49 

patient.clinical_cqcf.normal_tissue_proximity: 
distal (>2cm)          NA's 
            8            49 

patient.clinical_cqcf.tumor_type: 
primary 
     57 

patient.colorectal_cancer: 
  no  yes NA's 
  50    1    6 

patient.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00    5.00   17.00   15.49   25.00   26.00 

patient.days_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -32872  -27772  -25185  -25686  -22978  -18806 

patient.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   269.0   501.5   657.1   780.8  3115.0      25 

patient.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   325.2   513.5   903.1  1381.5  2841.0      31 

patient.diabetes: 
  no  yes NA's 
  45    6    6 



patient.drugs.drug.2.clinical_trail_drug_classification: 
paclitaxel       NA's 
         1         56 

patient.drugs.drug.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    6.25   19.00   16.79   26.00   26.00      23 

patient.drugs.drug.2.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   63.0   147.0   168.0   205.7   194.8  1128.0      25 

patient.drugs.drug.2.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0    57.0    69.5   105.0    86.5   771.0      25 


patient.drugs.drug.2.measure_of_response: 
clinical progressive disease            complete response 
                           5                           21 
            partial response               stable disease 
                           3                            4 
                        NA's 
                          24 

patient.drugs.drug.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   5.000   4.059   5.000   7.000      23 

patient.drugs.drug.2.therapy_ongoing: 
  no NA's 
  34   23 

patient.drugs.drug.2.therapy_types.therapy_type: 
   ancillary chemotherapy         NA's 
           1           33           23 

patient.drugs.drug.2.tx_on_clinical_trial: 
  no  yes NA's 
  32    2   23 

patient.drugs.drug.2.year_of_form_completion: 
2013 2014 NA's 
  22   12   23 



patient.drugs.drug.3.clinical_trail_drug_classification: 
gemcitabine        NA's 
          1          56 

patient.drugs.drug.3.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    7.00   18.50   16.67   24.00   26.00      45 

patient.drugs.drug.3.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  140.0   203.0   236.0   427.9   565.8  1555.0      45 

patient.drugs.drug.3.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   57.0   116.8   181.0   350.6   503.8  1470.0      45 


patient.drugs.drug.3.measure_of_response: 
clinical progressive disease            complete response 
                           4                            5 
            partial response                         NA's 
                           2                           46 

patient.drugs.drug.3.month_of_form_completion: 
   2    3    5    6 NA's 
   4    4    2    2   45 

patient.drugs.drug.3.therapy_ongoing: 
  no NA's 
  12   45 

patient.drugs.drug.3.therapy_types.therapy_type: 
chemotherapy         NA's 
          12           45 

patient.drugs.drug.3.tx_on_clinical_trial: 
  no  yes NA's 
  11    1   45 

patient.drugs.drug.3.year_of_form_completion: 
2013 2014 NA's 
   4    8   45 



patient.drugs.drug.4.clinical_trail_drug_classification: 
docetaxel      NA's 
        1        56 

patient.drugs.drug.4.day_of_form_completion: 
   6    7   19   24 NA's 
   1    2    1    1   52 

patient.drugs.drug.4.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  314.0   557.0   649.0   833.8   690.0  1959.0      52 

patient.drugs.drug.4.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    291     495     530     776     690    1874      52 

patient.drugs.drug.4.drug_name: 
   avastin    cytoxan  docetaxel paclitaxel       NA's 
         1          1          1          2         52 

patient.drugs.drug.4.measure_of_response: 
clinical progressive disease             partial response 
                           3                            1 
                        NA's 
                          53 

patient.drugs.drug.4.month_of_form_completion: 
   2    3    6 NA's 
   2    2    1   52 

patient.drugs.drug.4.therapy_ongoing: 
  no NA's 
   5   52 

patient.drugs.drug.4.therapy_types.therapy_type: 
 chemotherapy immunotherapy          NA's 
            4             1            52 

patient.drugs.drug.4.tx_on_clinical_trial: 
  no  yes NA's 
   4    1   52 

patient.drugs.drug.4.year_of_form_completion: 
2013 2014 NA's 
   1    4   52 

patient.drugs.drug.5.bcr_drug_barcode: 
tcga-n7-a4y0-d56713 tcga-n9-a4pz-d57300 tcga-n9-a4q1-d57294 tcga-ng-a4vu-d44645 
                  1                   1                   1                   1 
               NA's 
                 53 

patient.drugs.drug.5.bcr_drug_uuid: 
2c1cf146-347c-403c-9747-c4f0add48ffb c958773d-c7ff-4da4-bc77-ce6eb6bbe424 
                                   1                                    1 
d96f651e-be0b-469f-b670-5243ce8497ae f1a4d20f-e485-4139-8305-2004a364c4f4 
                                   1                                    1 
                                NA's 
                                  53 

patient.drugs.drug.5.day_of_form_completion: 
   7   19   24 NA's 
   2    1    1   53 

patient.drugs.drug.5.days_to_drug_therapy_end: 
 314  324  993 1959 NA's 
   1    1    1    1   53 

patient.drugs.drug.5.days_to_drug_therapy_start: 
 291  305  586 1874 NA's 
   1    1    1    1   53 

patient.drugs.drug.5.drug_name: 
 adriamycin carboplatin   cisplatin        NA's 
          1           2           1          53 

patient.drugs.drug.5.measure_of_response: 
clinical progressive disease               stable disease 
                           2                            1 
                        NA's 
                          54 

patient.drugs.drug.5.month_of_form_completion: 
   2    3    6 NA's 
   1    2    1   53 

patient.drugs.drug.5.therapy_ongoing: 
  no NA's 
   4   53 

patient.drugs.drug.5.therapy_types.therapy_type: 
chemotherapy         NA's 
           4           53 

patient.drugs.drug.5.tx_on_clinical_trial: 
  no NA's 
   4   53 

patient.drugs.drug.5.year_of_form_completion: 
2013 2014 NA's 
   1    3   53 

patient.drugs.drug.6.bcr_drug_barcode: 
tcga-ng-a4vu-d44646                NA's 
                  1                  56 

patient.drugs.drug.6.bcr_drug_uuid: 
80cdb638-d5d1-4276-a857-c655bd4a2fef                                 NA's 
                                   1                                   56 

patient.drugs.drug.6.day_of_form_completion: 
  24 NA's 
   1   56 

patient.drugs.drug.6.days_to_drug_therapy_end: 
 314 NA's 
   1   56 

patient.drugs.drug.6.days_to_drug_therapy_start: 
 291 NA's 
   1   56 

patient.drugs.drug.6.drug_name: 
cisplatin      NA's 
        1        56 

patient.drugs.drug.6.month_of_form_completion: 
   6 NA's 
   1   56 

patient.drugs.drug.6.therapy_ongoing: 
  no NA's 
   1   56 

patient.drugs.drug.6.therapy_types.therapy_type: 
chemotherapy         NA's 
           1           56 

patient.drugs.drug.6.tx_on_clinical_trial: 
  no NA's 
   1   56 

patient.drugs.drug.6.year_of_form_completion: 
2013 NA's 
   1   56 



patient.drugs.drug.clinical_trail_drug_classification: 
carboplatin   cisplatin        NA's 
          1           1          55 

patient.drugs.drug.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    6.50   18.00   16.28   26.00   26.00      18 

patient.drugs.drug.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   95.0   147.0   168.0   204.5   200.2  1128.0      21 

patient.drugs.drug.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   57.00   67.00   99.95   85.00  771.00      20 


patient.drugs.drug.measure_of_response: 
clinical progressive disease            complete response 
                           6                           23 
            partial response               stable disease 
                           3                            4 
                        NA's 
                          21 

patient.drugs.drug.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   5.000   4.231   5.000  11.000      18 

patient.drugs.drug.therapy_ongoing: 
  no  yes NA's 
  38    1   18 

patient.drugs.drug.therapy_types.therapy_type: 
                 ancillary               chemotherapy 
                         1                         36 
targeted molecular therapy                       NA's 
                         1                         19 

patient.drugs.drug.tx_on_clinical_trial: 
  no  yes NA's 
  35    4   18 

patient.drugs.drug.year_of_form_completion: 
2013 2014 NA's 
  25   14   18 

patient.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     1                     43                     13 

patient.follow_ups.follow_up.2.bcr_followup_barcode: 
tcga-n8-a4pp-f57616 tcga-n8-a56s-f50694 tcga-nd-a4wc-f58371                NA's 
                  1                   1                   1                  54 

patient.follow_ups.follow_up.2.bcr_followup_uuid: 
6fcc578f-3bcd-414d-a01c-422957b6f110 98db6c5b-81bf-4729-a5f4-075d0c0952cf 
                                   1                                    1 
d744de82-5993-46c7-9472-d440b92f5712                                 NA's 
                                   1                                   54 

patient.follow_ups.follow_up.2.birth_control_pill_history_usage_category: 
never used       NA's 
         1         56 

patient.follow_ups.follow_up.2.colorectal_cancer: 
  no NA's 
   1   56 

patient.follow_ups.follow_up.2.day_of_form_completion: 
   8   18   29 NA's 
   1    1    1   54 

patient.follow_ups.follow_up.2.days_to_death: 
 371  611 NA's 
   1    1   55 

patient.follow_ups.follow_up.2.days_to_last_followup: 
 766 NA's 
   1   56 

patient.follow_ups.follow_up.2.diabetes: 
  no NA's 
   1   56 

patient.follow_ups.follow_up.2.horm_ther: 
no, i have never taken menopausal hormone therapy. 
                                                 1 
                                              NA's 
                                                56 

patient.follow_ups.follow_up.2.hypertension: 
 yes NA's 
   1   56 

patient.follow_ups.follow_up.2.lost_follow_up: 
  no NA's 
   3   54 

patient.follow_ups.follow_up.2.month_of_form_completion: 
   3    4   10 NA's 
   1    1    1   54 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
   1    2   54 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status: 
tumor free       NA's 
         1         56 

patient.follow_ups.follow_up.2.postoperative_rx_tx: 
  no  yes NA's 
   1    2   54 

patient.follow_ups.follow_up.2.pregnancies: 
  4+ NA's 
   1   56 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success: 
complete remission/response  partial remission/response 
                          1                           1 
             stable disease                        NA's 
                          1                          54 

patient.follow_ups.follow_up.2.prior_tamoxifen_administered_usage_category: 
never used       NA's 
         1         56 

patient.follow_ups.follow_up.2.radiation_therapy: 
  no  yes NA's 
   2    1   54 

patient.follow_ups.follow_up.2.vital_status: 
alive  dead  NA's 
    1     2    54 

patient.follow_ups.follow_up.2.year_of_form_completion: 
2013 2014 NA's 
   1    2   54 

patient.follow_ups.follow_up.3.bcr_followup_barcode: 
tcga-nd-a4wc-f51564                NA's 
                  1                  56 

patient.follow_ups.follow_up.3.bcr_followup_uuid: 
07ba4a87-1b93-4ce0-b13f-c1c98087ff4d                                 NA's 
                                   1                                   56 

patient.follow_ups.follow_up.3.birth_control_pill_history_usage_category: 
never used       NA's 
         1         56 

patient.follow_ups.follow_up.3.colorectal_cancer: 
  no NA's 
   1   56 

patient.follow_ups.follow_up.3.day_of_form_completion: 
  29 NA's 
   1   56 

patient.follow_ups.follow_up.3.days_to_last_followup: 
 698 NA's 
   1   56 

patient.follow_ups.follow_up.3.diabetes: 
  no NA's 
   1   56 

patient.follow_ups.follow_up.3.horm_ther: 
no, i have never taken menopausal hormone therapy. 
                                                 1 
                                              NA's 
                                                56 

patient.follow_ups.follow_up.3.hypertension: 
 yes NA's 
   1   56 

patient.follow_ups.follow_up.3.lost_follow_up: 
  no NA's 
   1   56 

patient.follow_ups.follow_up.3.month_of_form_completion: 
   4 NA's 
   1   56 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no NA's 
   1   56 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status: 
tumor free       NA's 
         1         56 

patient.follow_ups.follow_up.3.postoperative_rx_tx: 
 yes NA's 
   1   56 

patient.follow_ups.follow_up.3.pregnancies: 
  4+ NA's 
   1   56 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success: 
complete remission/response                        NA's 
                          1                          56 

patient.follow_ups.follow_up.3.prior_tamoxifen_administered_usage_category: 
never used       NA's 
         1         56 

patient.follow_ups.follow_up.3.radiation_therapy: 
 yes NA's 
   1   56 

patient.follow_ups.follow_up.3.vital_status: 
alive  NA's 
    1    56 

patient.follow_ups.follow_up.3.year_of_form_completion: 
2014 NA's 
   1   56 



patient.follow_ups.follow_up.birth_control_pill_history_usage_category: 
former user  never used        NA's 
          2           3          52 

patient.follow_ups.follow_up.colorectal_cancer: 
  no NA's 
   8   49 

patient.follow_ups.follow_up.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   13.00   14.05   17.00   27.00      36 

patient.follow_ups.follow_up.days_to_death: 
 167  317  714 2680 NA's 
   1    1    1    1   53 

patient.follow_ups.follow_up.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    295     587     829    1312    1806    4269      40 

patient.follow_ups.follow_up.diabetes: 
  no  yes NA's 
   9    1   47 

patient.follow_ups.follow_up.horm_ther: 
no, i have never taken menopausal hormone therapy. 
                                                 7 
                                              NA's 
                                                50 

patient.follow_ups.follow_up.hypertension: 
  no  yes NA's 
   3    7   47 

patient.follow_ups.follow_up.lost_follow_up: 
  no NA's 
  21   36 

patient.follow_ups.follow_up.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.857   5.000   7.000      36 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy: 
 yes NA's 
   1   56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
 199 NA's 
   1   56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site: 
lymph node(s)          NA's 
            1            56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
progression of disease                   NA's 
                     1                     56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   1   56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no  yes NA's 
   2    2   53 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no  yes NA's 
   2    1   54 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure: 
 155 NA's 
   1   56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  153.0   231.0   410.0   708.2   516.2  2553.0      51 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site: 
          lung  lymph node(s) other, specify         pelvis           NA's 
             1              2              1              2             51 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence  progression of disease 
                      1                       4                       1 
                   NA's 
                     51 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text: 
vagina   NA's 
     1     56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
   4    1   52 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.recurrence_second_surgery_neoplasm_surgical_procedure_name: 
excisional biopsy              NA's 
                1                56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.residual_tumor: 
  rx NA's 
   1   56 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
  13    8   36 

patient.follow_ups.follow_up.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
        11          9         37 

patient.follow_ups.follow_up.postoperative_rx_tx: 
  no  yes NA's 
   6   14   37 

patient.follow_ups.follow_up.pregnancies: 
   0    2    3   4+ NA's 
   1    4    3    1   48 

patient.follow_ups.follow_up.primary_therapy_outcome_success: 
complete remission/response  partial remission/response 
                         12                           1 
        progressive disease              stable disease 
                          3                           1 
                       NA's 
                         40 

patient.follow_ups.follow_up.prior_tamoxifen_administered_usage_category: 
never used       NA's 
        10         47 

patient.follow_ups.follow_up.radiation_therapy: 
  no  yes NA's 
   9   11   37 

patient.follow_ups.follow_up.vital_status: 
alive  dead  NA's 
   17     4    36 

patient.follow_ups.follow_up.year_of_form_completion: 
2013 2014 2015 NA's 
   5   13    3   36 

patient.gender: 
female 
    57 

patient.height: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  138.0   153.5   157.5   157.7   162.0   172.0       5 

patient.history_of_neoadjuvant_treatment: 
no 
57 

patient.horm_ther: 
                         no, i have never taken menopausal hormone therapy. 
                                                                         29 
yes, i have taken menopausal hormone therapy in the past, but no longer do. 
                                                                          7 
                                                                       NA's 
                                                                         21 

patient.hypertension: 
  no  yes NA's 
  24   28    5 


patient.icd_o_3_histology: 
8090/3 8950/3 8951/3 8980/3 
     1     45      1     10 


patient.informed_consent_verified: 
yes 
 57 

patient.menopause_status: 
                                   peri (6-12 months since last menstrual period) 
                                                                                1 
post (prior bilateral ovariectomy or >12 mo since lmp with no prior hysterectomy) 
                                                                               54 
                                                                             NA's 
                                                                                2 

patient.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.386   6.000  11.000 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy: 
  no  yes NA's 
   4    5   48 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy: 
  no  yes NA's 
   5    3   49 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  144.0   239.5   354.0   485.6   470.0  1840.0      46 


patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence                    NA's 
                      5                       6                      46 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text: 
vagina   NA's 
     1     56 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   8   49 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy: 
  no  yes NA's 
   2    2   53 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy: 
  no  yes NA's 
   4    1   52 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  144.0   354.0   427.0   636.4   674.0  1583.0      52 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site: 
          lung  lymph node(s) other, specify           NA's 
             1              2              2             52 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence  progression of disease 
                      1                       3                       1 
                   NA's 
                     52 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text: 
subcarinal mass          vagina            NA's 
              1               1              55 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   4   53 

patient.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy: 
 yes NA's 
   1   56 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy: 
  no  yes NA's 
   1    1   55 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment: 
 743 1595 NA's 
   1    1   55 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site: 
brain  lung  NA's 
    1     1    55 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type: 
    distant metastasis progression of disease                   NA's 
                     1                      1                     55 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   1   56 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no  yes NA's 
   7   13   37 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no  yes NA's 
  12    5   40 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure: 
 215  281  748 NA's 
   1    1    1   54 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   49.0   158.0   275.0   442.5   487.0  2566.0      28 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence  progression of disease 
                     11                      13                       5 
                   NA's 
                     28 


patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
  19    3   35 

patient.new_tumor_events.new_tumor_event.recurrence_second_surgery_neoplasm_surgical_procedure_name: 
          excisional biopsy           incisional biopsy 
                          2                           1 
other method, specify below                        NA's 
                          2                          52 

patient.new_tumor_events.new_tumor_event.recurrence_second_surgery_neoplasm_surgical_procedure_name_other: 
fine needle aspiration        palliative care                   NA's 
                     1                      1                     55 

patient.new_tumor_events.new_tumor_event.residual_tumor: 
  r2   rx NA's 
   2    1   54 

patient.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
  24   29    4 


patient.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
        18         36          3 

patient.postoperative_rx_tx: 
  no  yes NA's 
  18   36    3 


patient.primary_pathology.age_at_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  51.00   62.00   68.00   69.74   76.00   90.00 

patient.primary_pathology.aln_pos_ihc: 
   0 NA's 
  32   25 

patient.primary_pathology.aln_pos_light_micro: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.8158  0.0000  7.0000      19 

patient.primary_pathology.anatomic_neoplasm_subdivision: 
                         endometrium                         fundus uteri 
                                  33                                    1 
lower uterine segment/ isthmus uteri                           myometrium 
                                   2                                    2 
                                NA's 
                                  19 

patient.primary_pathology.days_to_initial_pathologic_diagnosis: 
 0 
57 

patient.primary_pathology.histological_type: 
uterine carcinosarcoma/ malignant mixed mullerian tumor (mmmt): nos 
                                                                 24 
                    uterine carcinosarcoma/ mmmt: heterologous type 
                                                                 20 
                      uterine carcinosarcoma/ mmmt: homologous type 
                                                                 13 


patient.primary_pathology.pct_tumor_invasion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   15.00   40.00   45.01   69.00  100.00      12 

patient.primary_pathology.peritoneal_wash: 
negative positive     NA's 
      25       12       20 

patient.primary_pathology.pln_pos_ihc: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.9167  0.0000 14.0000      21 

patient.primary_pathology.pln_pos_light_micro: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.9149  1.0000  9.0000      10 

patient.primary_pathology.residual_tumor: 
  r0   r1   r2   rx NA's 
  28    2   10   12    5 

patient.primary_pathology.surgical_approach: 
minimally invasive               open               NA's 
                31                 20                  6 

patient.primary_pathology.total_aor.lnp: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.8108  0.0000  7.0000      20 

patient.primary_pathology.total_aor_lnr: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   4.412   7.000  21.000       6 

patient.primary_pathology.total_pelv_lnp: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   0.925   1.000   9.000      17 

patient.primary_pathology.total_pelv_lnr: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    1.25    7.50   10.02   17.00   34.00       3 

patient.primary_pathology.tumor_tissue_site: 
uterus 
    57 

patient.primary_pathology.year_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2002    2007    2009    2009    2011    2012 

patient.primary_therapy_outcome_success: 
complete remission/response  partial remission/response 
                         30                           4 
        progressive disease                        NA's 
                         13                          10 

patient.prior_dx: 
 no yes 
 49   8 

patient.prior_tamoxifen_administered_usage_category: 
never used       NA's 
        36         21 

patient.race: 
                    asian black or african american                     white 
                        3                         9                        44 
                     NA's 
                        1 

patient.radiation_therapy: 
  no  yes NA's 
  29   25    3 

patient.radiations.radiation.2.anatomic_treatment_site: 
 distant recurrence    local recurrence primary tumor field       regional site 
                  1                   2                   1                   2 
               NA's 
                 51 



patient.radiations.radiation.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.50   18.00   14.29   18.50   31.00      50 

patient.radiations.radiation.2.days_to_radiation_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   88.0   154.5   240.0   479.0   541.5  1633.0      50 

patient.radiations.radiation.2.days_to_radiation_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   29.0   149.5   205.0   453.9   523.0  1598.0      50 

patient.radiations.radiation.2.measure_of_response: 
               complete response radiographic progressive disease 
                               3                                2 
                            NA's 
                              52 

patient.radiations.radiation.2.month_of_form_completion: 
   2    3   10 NA's 
   3    3    1   50 

patient.radiations.radiation.2.numfractions: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     3.0     6.5     9.5    13.0    25.0      51 

patient.radiations.radiation.2.radiation_dosage: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   18.75   32.50   90.83   50.00  400.00      51 

patient.radiations.radiation.2.radiation_treatment_ongoing: 
  no NA's 
   7   50 

patient.radiations.radiation.2.radiation_type: 
external internal     NA's 
       3        3       51 

patient.radiations.radiation.2.units: 
 cgy   gy NA's 
   1    5   51 

patient.radiations.radiation.2.year_of_form_completion: 
2013 2014 NA's 
   1    6   50 

patient.radiations.radiation.3.anatomic_treatment_site: 
distant recurrence               NA's 
                 1                 56 

patient.radiations.radiation.3.bcr_radiation_barcode: 
tcga-n9-a4q3-r57628                NA's 
                  1                  56 

patient.radiations.radiation.3.bcr_radiation_uuid: 
ea3fd92a-674a-4523-b2bb-9c00c1feb1f6                                 NA's 
                                   1                                   56 

patient.radiations.radiation.3.day_of_form_completion: 
  18 NA's 
   1   56 

patient.radiations.radiation.3.days_to_radiation_therapy_end: 
1633 NA's 
   1   56 

patient.radiations.radiation.3.days_to_radiation_therapy_start: 
1598 NA's 
   1   56 

patient.radiations.radiation.3.measure_of_response: 
radiographic progressive disease                             NA's 
                               1                               56 

patient.radiations.radiation.3.month_of_form_completion: 
   3 NA's 
   1   56 

patient.radiations.radiation.3.numfractions: 
  14 NA's 
   1   56 

patient.radiations.radiation.3.radiation_dosage: 
  42 NA's 
   1   56 

patient.radiations.radiation.3.radiation_treatment_ongoing: 
  no NA's 
   1   56 

patient.radiations.radiation.3.radiation_type: 
external     NA's 
       1       56 

patient.radiations.radiation.3.units: 
  gy NA's 
   1   56 

patient.radiations.radiation.3.year_of_form_completion: 
2014 NA's 
   1   56 

patient.radiations.radiation.anatomic_treatment_site: 
 distant recurrence        distant site primary tumor field       regional site 
                  1                   1                  16                   7 
               NA's 
                 32 



patient.radiations.radiation.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   10.00   21.50   19.19   26.00   31.00      31 

patient.radiations.radiation.days_to_radiation_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   63.0    91.0   111.0   207.2   219.2  1249.0      31 

patient.radiations.radiation.days_to_radiation_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  21.00   63.75   83.00  181.23  184.50 1219.00      31 

patient.radiations.radiation.measure_of_response: 
               complete response                 partial response 
                              16                                1 
radiographic progressive disease                   stable disease 
                               4                                1 
                            NA's 
                              35 

patient.radiations.radiation.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   5.000   4.731   5.000  10.000      31 

patient.radiations.radiation.numfractions: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00   10.00   12.65   25.00   28.00      34 

patient.radiations.radiation.radiation_dosage: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  21.00   44.25  922.50 1570.00 3000.00 5040.00      33 

patient.radiations.radiation.radiation_treatment_ongoing: 
  no NA's 
  26   31 

patient.radiations.radiation.radiation_type: 
external internal     NA's 
      12       12       33 

patient.radiations.radiation.units: 
 cgy   gy NA's 
  13   12   32 

patient.radiations.radiation.year_of_form_completion: 
2013 2014 NA's 
  16   10   31 


patient.stage_event.system_version: 
1988 2009 
   7   50 

patient.tissue_prospective_collection_indicator: 
 no yes 
 34  23 

patient.tissue_retrospective_collection_indicator: 
 no yes 
 23  34 


patient.vital_status: 
alive  dead 
   25    32 

patient.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  42.00   60.00   72.00   73.31   82.25  167.00       5 

patient.year_of_form_completion: 
2013 2014 
  47   10 


patient.microsatellite_instability_test_results.microsatellite_instability_test_result.mononucleotide_and_dinucleotide_marker_panel_analysis_status: 
indeterminate         msi-h         msi-l           mss 
            1             2             3            51 

patient.bcr_canonical_check.bcr_patient_canonical_status: 
canonical 
       57 

patient.biospecimen_cqcf.consent_or_death_status: 
consented  deceased 
       56         1 

patient.biospecimen_cqcf.country: 
united states       vietnam 
           56             1 

patient.biospecimen_cqcf.days_to_pathology_review: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     34     430    1195    1422    2113    3980       8 

patient.biospecimen_cqcf.days_to_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  -3.00    7.00   25.00   21.79   32.00   58.00 

patient.biospecimen_cqcf.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     1                     38                     18 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site: 
                         endometrium; fundus uteri 
                                                 1 
endometrium; lower uterine segment (isthmus uteri) 
                                                 1 
                                        myometrium 
                                                 1 
                                            uterus 
                                                21 
                                              NA's 
                                                33 

patient.biospecimen_cqcf.histological_type: 
uterine carcinosarcoma/ malignant mixed mullerian tumor (mmmt): nos 
                                                                 57 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment: 
no 
57 

patient.biospecimen_cqcf.method_of_sample_procurement: 
 full hysterectomy surgical resection 
                18                 39 

patient.biospecimen_cqcf.normal_controls.normal_control.2.bcr_sample_uuid: 
35f0f6bf-a59d-4d23-b195-97103ab38a6f a7da4798-0ec1-45cb-a58c-7a1a2c54879e 
                                   1                                    1 
b2df9fa2-6d1c-4f59-bc08-336748d802db                                 NA's 
                                   1                                   54 

patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement: 
   0   25   56 NA's 
   1    1    1   54 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc: 
 643 NA's 
   1   56 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm: 
nanodrop     NA's 
       1       56 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty: 
  17 NA's 
   1   56 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol: 
  27 NA's 
   1   56 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement: 
        blood draw surgical resection               NA's 
                 2                  1                 54 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site: 
ovary  NA's 
    1    56 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
            1            56 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number: 
   1 NA's 
   3   54 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -3.00    6.75   25.00  133.41   35.00 2841.00       1 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0000   0.1595   0.2790  54.2185   0.4250 833.0000       23 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm: 
automatic dna extraction                    cdrop                 nanodrop 
                       2                        2                       25 
              pico green                     NA's 
                       4                       24 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   15.00   20.00   20.95   27.27   31.30      23 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0    42.5    70.5    71.6    85.0   207.0      23 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement: 
        blood draw surgical resection               NA's 
                49                  7                  1 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site: 
fallopian tube     myometrium          other          ovary           NA's 
             1              2              2              2             50 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
            7            50 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number: 
 1 
57 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching: 
yes 
 57 

patient.biospecimen_cqcf.path_confirm_report_attached: 
  no  yes NA's 
   1   48    8 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics: 
 no yes 
  1  56 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics: 
yes 
 57 

patient.biospecimen_cqcf.race: 
                    asian black or african american                     white 
                        3                         9                        44 
                     NA's 
                        1 

patient.biospecimen_cqcf.sample_prescreened: 
 no yes 
  8  49 

patient.biospecimen_cqcf.site_of_disease: 
uterus 
    57 

patient.biospecimen_cqcf.site_of_disease_description: 
                         endometrium                         fundus uteri 
                                  33                                    1 
lower uterine segment/ isthmus uteri                           myometrium 
                                   1                                    1 
                                NA's 
                                  21 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid: 
d38baef3-cc4c-498e-a032-2645a91e166a                                 NA's 
                                   1                                   56 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent: 
   1 NA's 
   1   56 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent: 
  80 NA's 
   1   56 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight: 
 200 NA's 
   1   56 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number: 
   1 NA's 
   1   56 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   9.857  20.000  85.000       8 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   70.00   80.00   80.41   90.00  100.00       8 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   30.0   200.0   200.0   315.2   250.0  1735.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number: 
 1  2 
56  1 

patient.biospecimen_cqcf.vessel_used: 
biospecimen storage bag                cassette                cryomold 
                      8                      14                      15 
               cryovial 
                     20 

patient.days_to_index: 
 0 
57 



patient.samples.sample.2.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     93     456    1101    1363    2022    3983 

patient.samples.sample.2.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  160.0   170.0   210.0   213.3   242.5   290.0      51 

patient.samples.sample.2.is_ffpe: 
no 
57 

patient.samples.sample.2.oct_embedded: 
false  true 
   53     4 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
153887725 153887744 153887766 153887765 153887788 153887804 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
 8 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
0.5 
 57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
24 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
 7 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   7.421   9.000  11.000 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
a317 
  57 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
40 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
23 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
80 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2013 
  57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna 
                  57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id: 
 w 
57 



patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
yes 
 57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
yes 
 57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
repli-g 
     57 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2 NA's 
   5    1   51 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
132269180 132270173 132270196 132270173 132270219 132270236 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
 8 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
0.07 0.08 0.09 
  19   34    4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
30 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
 4 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   7.421   9.000  11.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
a28u 
  57 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
2.8 3.2 3.6 
 19  34   4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
23 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
40 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2013 
  57 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
132267370 132267871 132267893 132267884 132267916 132267932 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
 1 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.14 0.15 0.16 0.17 
  13   13   20   11 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
30 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
 4 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   7.421   9.000  11.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
a28t 
  57 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
0.94 1.01 1.07 1.14 
  13   13   20   11 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
23 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.7 
 57 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2013 
  57 

patient.samples.sample.2.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.93   10.92   21.40   22.13   26.04   67.73 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type: 
dna 
 57 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id: 
 d 
57 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration: 
0.14 0.15 0.16 0.17 
  13   13   20   11 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
 57 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
yes 
 57 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              57 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
            adna preparation type pre-extracted dna received by tss 
                               23                                34 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen 
       57 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence: 
   1    2 NA's 
   5    1   51 



patient.samples.sample.2.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00    8.00   12.00   12.28   14.00   28.00 

patient.samples.sample.2.portions.portion.is_ffpe: 
no 
57 

patient.samples.sample.2.portions.portion.month_of_creation: 
 1  2  3  4 12 
16 21  5  7  8 

patient.samples.sample.2.portions.portion.portion_number: 
 1 11 12 
51  5  1 

patient.samples.sample.2.portions.portion.portion_sequence: 
 1 
57 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
   6   51 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   0   10   60 NA's 
   4    1    1   51 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration: 
   0   10   15 NA's 
   4    1    1   51 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis: 
   0   10 NA's 
   5    1   51 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0    5 NA's 
   5    1   51 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells: 
  80   90  100 NA's 
   1    2    3   51 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells: 
   0   10   20 NA's 
   4    1    1   51 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells: 
   0 NA's 
   6   51 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei: 
   0 NA's 
   6   51 

patient.samples.sample.2.portions.portion.slides.slide.section_location: 
 top NA's 
   6   51 

patient.samples.sample.2.portions.portion.weight: 
   2   30 NA's 
  17    6   34 

patient.samples.sample.2.portions.portion.year_of_creation: 
2012 2013 
   8   49 

patient.samples.sample.2.sample_type: 
blood derived normal  solid tissue normal 
                  51                    6 

patient.samples.sample.2.sample_type_id: 
10 11 
51  6 

patient.samples.sample.2.vial_number: 
 a  b 
52  5 



patient.samples.sample.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     93     438    1094    1352    2022    3983 





patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.5: 
7a91d15a-1dcb-4b22-96fd-740f0f3332b4 8024fd57-2a1a-4579-a008-db0cddb83a58 
                                   1                                    1 
                                NA's 
                                  55 

patient.samples.sample.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   60.0   210.0   340.0   412.5   450.0  1670.0 

patient.samples.sample.is_ffpe: 
no 
57 

patient.samples.sample.oct_embedded: 
false  true 
   16    41 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.780   1.840   1.870   1.863   1.890   1.960 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
132269148 132269959 132269984 132269863 132270009 132270033 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
13 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.15 0.16 0.17 0.18 
   5    8   27   17 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
30 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
 4 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   6.561   8.000  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
a28z 
  57 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
  3 3.2 3.4 3.6 
  5   8  27  17 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
23 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
20 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2013 
  57 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
114991760 114993604 132267849 128491044 132270180 145672883 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
 7 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
0.15 0.16 0.17 0.18 
   5    8   27   17 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
30 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
 4 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   6.561   8.000  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
a28v 
  57 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
1.95 2.08 2.21 2.34 
   5    8   27   17 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
23 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
13 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2013 
  57 

patient.samples.sample.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  20.73   32.76   43.50   45.25   57.55   78.55 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type: 
rna 
 57 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id: 
 r 
57 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration: 
0.15 0.16 0.17 0.18 
   5    8   27   17 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              57 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna 
                       57 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.730   1.310   1.460   1.485   1.710   2.090 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.500   8.100   8.026   8.500   9.600 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec 
     57 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence: 
 1  2 
51  6 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
153887821 153887840 153887862 153887861 153887884 153887900 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
 8 
57 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
0.5 
 57 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
24 
57 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
 7 
57 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   7.421   9.000  11.000 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id: 
a316 
  57 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
40 
57 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
23 
57 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
80 
57 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2013 
  57 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna 
                  57 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id: 
 w 
57 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
yes 
 57 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
yes 
 57 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              57 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g 
     57 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence: 
 1  2 
51  6 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
145672813 145672832 145672854 145672853 145672876 145672892 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
 1 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
0.14 0.15 0.16 0.17 
  13   18   20    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
30 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
 4 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   7.421   9.000  11.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
a28q 
  57 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
0.94 1.01 1.07 1.14 
  13   18   20    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
23 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
6.7 
 57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2013 
  57 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
1.15e+08 1.15e+08 1.15e+08 1.15e+08 1.15e+08 1.15e+08 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
 8 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
0.07 0.08 0.09 
  23   32    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
30 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
 4 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   7.421   9.000  11.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id: 
a28r 
  57 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
2.8 3.2 3.6 
 23  32   2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
23 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
40 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2013 
  57 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
1.15e+08 1.15e+08 1.15e+08 1.15e+08 1.15e+08 1.15e+08 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
 5 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
0.14 0.15 0.16 0.17 
  13   18   20    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
 8 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
 5 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   7.421   9.000  11.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id: 
a28s 
  57 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
1.82 1.95 2.08 2.21 
  13   18   20    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
23 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
13 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2013 
  57 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
23 
57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
yyyy 
  57 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
23 
57 

patient.samples.sample.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  13.31   29.77   37.88   41.86   53.53  111.37 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type: 
dna 
 57 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id: 
 d 
57 



patient.samples.sample.portions.portion.analytes.analyte.concentration: 
0.14 0.15 0.16 0.17 
  13   18   20    6 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
 57 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
yes 
 57 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              57 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
adna preparation type 
                   57 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen 
       57 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence: 
 1  2 
51  6 



patient.samples.sample.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   12.00   18.00   16.79   22.00   31.00 

patient.samples.sample.portions.portion.is_ffpe: 
no 
57 

patient.samples.sample.portions.portion.month_of_creation: 
 1  2  3 12 
25 20  4  8 

patient.samples.sample.portions.portion.portion_number: 
11 12 21 22 31 
43  5  4  1  4 

patient.samples.sample.portions.portion.portion_sequence: 
 1  2  3 
48  5  4 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe: 
no 
57 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    2.00   18.28   40.00   70.00 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration: 
 0  1 10 15 20 
32  1  4  2 18 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   5.000   5.509  10.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration: 
 0  5 20 40 
33  5 14  5 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells: 
 0  5 10 30 
51  2  2  2 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells: 
 0  5 10 15 20 
24 14 11  2  6 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   80.00   90.00   87.12   95.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   80.00   85.00   83.68   90.00  100.00 

patient.samples.sample.portions.portion.slides.slide.section_location: 
top 
 57 

patient.samples.sample.portions.portion.weight: 
20 30 
 3 54 

patient.samples.sample.portions.portion.year_of_creation: 
2012 2013 
   8   49 


patient.samples.sample.portions.shipment_portion.center_id: 
  20 NA's 
  48    9 

patient.samples.sample.portions.shipment_portion.is_ffpe: 
  no NA's 
  48    9 

patient.samples.sample.portions.shipment_portion.plate_id: 
a41p NA's 
  48    9 

patient.samples.sample.portions.shipment_portion.portion_number: 
  11   21   41 NA's 
   3   44    1    9 

patient.samples.sample.portions.shipment_portion.portion_sequence: 
   1    2 NA's 
   3   44   10 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment: 
   4 NA's 
  48    9 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment: 
   6 NA's 
  48    9 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment: 
2014 NA's 
  48    9 

patient.samples.sample.sample_type: 
primary tumor 
           57 

patient.samples.sample.sample_type_id: 
 1 
57 

patient.samples.sample.vial_number: 
 a 
57 

}}
\keyword{datasets}
