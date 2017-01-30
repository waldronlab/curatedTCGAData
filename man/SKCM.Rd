\name{ SKCM }
\alias{ SKCM }
\docType{data}
\title{ SKCM }

\details{
\preformatted{
> SKCM
A MultiAssayExperiment object of 8 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 8: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 473 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 452 columns 
 [3] CNASNP: RangedRaggedAssay with 452114 rows and 938 columns 
 [4] CNVSNP: RangedRaggedAssay with 108084 rows and 937 columns 
 [5] CNAseq: RangedRaggedAssay with 31416 rows and 238 columns 
 [6] Methylation: SummarizedExperiment with 485577 rows and 475 columns 
 [7] RPPAArray: ExpressionSet with 181 rows and 206 columns 
 [8] Mutations: RangedRaggedAssay with 290322 rows and 345 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( SKCM )
CharacterList of length 8
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-229677 1:235670-235938 ... 24:28438116-59018259
[["CNVSNP"]] 1:3218610-36474198 1:36475406-36477996 ... 23:3157107-154905589
[["CNAseq"]] 1:10208-72766248 1:72766249-72812119 ... 24:2649474-28783838
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_epsilon 4E-BP1 4E-BP1_pS65 ... p90RSK p90RSK_pT359_S363
[["Mutations"]] EPHB6 BC101079 LYPD6B AK8 FAM123C ... CDKN2A CCDC90B RYR3 UBFD1

> colnames( SKCM )
CharacterList of length 8
[["RNASeq2GeneNorm"]] TCGA-3N-A9WB-06A-11R-A38C-07 ...
[["miRNASeqGene"]] TCGA-3N-A9WC-06A-11R-A38N-13 ...
[["CNASNP"]] TCGA-3N-A9WB-06A-11D-A38F-01 ... TCGA-Z2-AA3V-10A-01D-A399-01
[["CNVSNP"]] TCGA-3N-A9WB-06A-11D-A38F-01 ... TCGA-Z2-AA3V-10A-01D-A399-01
[["CNAseq"]] TCGA-BF-A1PU-01A-11D-A18Z-02 ... TCGA-HR-A2OH-10A-01D-A190-02
[["Methylation"]] TCGA-3N-A9WB-06A-11D-A38H-05 ... TCGA-Z2-AA3V-06A-11D-A398-05
[["RPPAArray"]] TCGA-BF-A1PU-01A-21-A20M-20 ... TCGA-IH-A3EA-01A-21-A241-20
[["Mutations"]] TCGA-BF-A1PU-01A-11D-A19A-08 ... TCGA-RP-A695-06A-11D-A30X-08

Sizes of each ExperimentList element:

            assay  size.Mb
1 RNASeq2GeneNorm   1.2 Mb
2    miRNASeqGene   0.1 Mb
3          CNASNP  32.8 Mb
4          CNVSNP   9.7 Mb
5          CNAseq     2 Mb
6     Methylation  67.5 Mb
7       RPPAArray   0.1 Mb
8       Mutations 289.5 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   249 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 221.00  219.00    2.99    2.40    3.92 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   48.00   58.00   58.24   71.00   90.00       8 

vital_status:
  0   1 
249 221 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     79     518    1093    1789    2073   10870     249 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -3     422    1010    1763    2359   11252     161 

days_to_submitted_specimen_dx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -2       0     344    1017    1372   10847      14 





melanoma_ulceration:
  no  yes NA's 
 146  167  157 

melanoma_primary_known:
 no yes 
 47 423 

Breslow_thickness:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.300   3.000   5.585   6.950  75.000     111 

gender:
female   male 
   180    290 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1978    2003    2008    2006    2011    2013      11 

radiation_therapy:
  no  yes NA's 
 420   49    1 

race:
                    asian black or african american                     white 
                       12                         1                       447 
                     NA's 
                       10 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    11                    446                     13 

admin.bcr:
nationwide children's hospital 
                           470 

admin.day_of_dcc_upload:
  1  21 
469   1 

admin.disease_code:
skcm 
 470 


admin.month_of_dcc_upload:
  5  10 
  1 469 

admin.patient_withdrawal.withdrawn:
false 
  470 

admin.project_code:
tcga 
 470 

admin.year_of_dcc_upload:
2015 
 470 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   48.00   58.00   58.22   71.00   90.00       8 


patient.breslow_depth_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.300   3.000   5.585   6.950  75.000     111 

patient.clinical_cqcf.consent_or_death_status:
  consented    deceased exemption 4        NA's 
        390          57          18           5 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -226.0     8.0   114.0   916.6  1184.5 10837.0      91 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    151     808    1026    2169    2927   10870     413 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             451 
yes, pharmaceutical treatment prior to resection 
                                              19 

patient.clinical_cqcf.normal_tissue_anatomic_site:
other  skin  NA's 
    6     3   461 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
            9           461 


patient.clinical_cqcf.tumor_type:
metastatic    primary 
       366        104 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   13.00   14.26   20.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32873  -26058  -21480  -21454  -17636   -5684       8 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     79     604    1101    1840    2190   10870     316 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 462    8 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -6.0    28.5   595.0  1416.7  1982.0 10860.0     163 

patient.days_to_submitted_specimen_dx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -2       0     344    1017    1372   10847      14 




patient.drugs.drug.2.clinical_trail_drug_classification:
                 mage a3 vaccine                    mek inhibitor 
                               3                                1 
melanoma-derived helper peptides                          vaccine 
                               1                                3 
                            NA's 
                             462 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   17.00   15.95   22.00   30.00     413 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   52.0   247.2   929.5  1458.3  1766.5  6814.0     424 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0   135.0   825.5  1282.6  1665.2  6660.0     414 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                          21                           12 
            partial response               stable disease 
                           5                            2 
                        NA's 
                         430 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.298  10.000  12.000     413 

patient.drugs.drug.2.number_cycles:
   1    2 NA's 
   1    1  468 

patient.drugs.drug.2.prescribed_dose:
   1  480 NA's 
   1    1  468 

patient.drugs.drug.2.prescribed_dose_units:
g/day    mg  NA's 
    1     1   468 

patient.drugs.drug.2.regimen_indication:
   adjuvant progression        NA's 
          2           2         466 

patient.drugs.drug.2.regimen_number:
08-144      6   NA's 
     1      1    468 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              469 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  51    6  413 


patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  37   16  417 

patient.drugs.drug.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
  10   16   28    3  413 




patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.25   12.50   14.43   20.75   30.00     440 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   92.0   310.5  1034.0  1255.4  1655.0  6440.0     447 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     20     384     894    1088    1492    6355     441 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                          12                            7 
            partial response               stable disease 
                           1                            1 
                        NA's 
                         449 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.250   6.500   6.333   7.750  12.000     440 

patient.drugs.drug.3.number_cycles:
   4 NA's 
   1  469 

patient.drugs.drug.3.regimen_indication:
progression        NA's 
          2         468 

patient.drugs.drug.3.regimen_number:
08-144 10-095   NA's 
     1      1    468 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              469 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  26    4  440 


patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  20    8  442 

patient.drugs.drug.3.year_of_form_completion:
2012 2013 2014 NA's 
   5    7   18  440 



patient.drugs.drug.4.clinical_trail_drug_classification:
braf and mek inhibitor           chemotherapy dendritic cell therapy 
                     1                      1                      1 
                  NA's 
                   467 

patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    9.00   14.50   15.06   23.75   26.00     452 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     92     579     645    1048    1603    2413     457 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     23     525     772    1118    1644    3105     453 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                          10                            3 
            partial response                         NA's 
                           1                          456 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   6.000   5.667   7.000  11.000     452 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
  16    2  452 

patient.drugs.drug.4.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         2                          7 
             immunotherapy targeted molecular therapy 
                         7                          2 
                      NA's 
                       452 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
  12    6  452 

patient.drugs.drug.4.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    5    9    3  452 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   14.50   15.00   16.43   18.50   28.00     463 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    157     559    1249    1229    1767    2413     465 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   23.0   846.5  1644.0  1389.9  1998.5  2372.0     463 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease            complete response 
                           4                            1 
            partial response                         NA's 
                           1                          464 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   7.000   7.286  10.000  12.000     463 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
   7  463 

patient.drugs.drug.5.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         3                          2 
targeted molecular therapy                       NA's 
                         2                        463 

patient.drugs.drug.5.tx_on_clinical_trial:
  no  yes NA's 
   6    1  463 

patient.drugs.drug.5.year_of_form_completion:
2013 2014 NA's 
   2    5  463 

patient.drugs.drug.6.bcr_drug_barcode:
tcga-eb-a24c-d40315 tcga-eb-a57m-d49923 tcga-w3-a825-d59259 tcga-w3-aa1q-d61974 
                  1                   1                   1                   1 
               NA's 
                466 

patient.drugs.drug.6.bcr_drug_uuid:
1847d54c-5c9f-426c-9678-31ced121e3b8 265c2eea-fa05-462a-9598-af6c79f5dea4 
                                   1                                    1 
839fdc5d-5d0e-453f-b8b8-3300cc5a1885 a0f6e18a-b10b-4902-89aa-bf9808530488 
                                   1                                    1 
                                NA's 
                                 466 

patient.drugs.drug.6.clinical_trail_drug_classification:
vaccine    NA's 
      1     469 

patient.drugs.drug.6.day_of_form_completion:
   6   14   15   17 NA's 
   1    1    1    1  466 

patient.drugs.drug.6.days_to_drug_therapy_end:
 157  559 1767 NA's 
   1    1    1  467 

patient.drugs.drug.6.days_to_drug_therapy_start:
  23  505  913 1644 NA's 
   1    1    1    1  466 

patient.drugs.drug.6.drug_name:
   dacarbazine erythropoietin  interferon-α2           NA's 
             1              1              1            467 

patient.drugs.drug.6.measure_of_response:
clinical progressive disease            complete response 
                           1                            1 
            partial response                         NA's 
                           1                          467 

patient.drugs.drug.6.month_of_form_completion:
   2    5    7   10 NA's 
   1    1    1    1  466 

patient.drugs.drug.6.therapy_ongoing:
  no NA's 
   4  466 

patient.drugs.drug.6.therapy_types.therapy_type:
    ancillary  chemotherapy immunotherapy       vaccine          NA's 
            1             1             1             1           466 

patient.drugs.drug.6.tx_on_clinical_trial:
  no  yes NA's 
   3    1  466 

patient.drugs.drug.6.year_of_form_completion:
2013 2014 NA's 
   2    2  466 

patient.drugs.drug.7.bcr_drug_barcode:
tcga-eb-a24c-d40316 tcga-eb-a57m-d49924                NA's 
                  1                   1                 468 

patient.drugs.drug.7.bcr_drug_uuid:
294c8564-de5c-4fc3-9fee-ef9988c95177 eb700b3f-b6e9-43d5-87d0-fa0310f6d5fa 
                                   1                                    1 
                                NA's 
                                 468 

patient.drugs.drug.7.day_of_form_completion:
  14   15 NA's 
   1    1  468 

patient.drugs.drug.7.days_to_drug_therapy_end:
 157  517 NA's 
   1    1  468 

patient.drugs.drug.7.days_to_drug_therapy_start:
  23  505 NA's 
   1    1  468 

patient.drugs.drug.7.drug_name:
ciclosporin   cisplatin        NA's 
          1           1         468 

patient.drugs.drug.7.measure_of_response:
complete response              NA's 
                1               469 

patient.drugs.drug.7.month_of_form_completion:
   2   10 NA's 
   1    1  468 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   2  468 

patient.drugs.drug.7.therapy_types.therapy_type:
 chemotherapy immunotherapy          NA's 
            1             1           468 

patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   2  468 

patient.drugs.drug.7.year_of_form_completion:
2013 NA's 
   2  468 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-eb-a57m-d49925                NA's 
                  1                 469 

patient.drugs.drug.8.bcr_drug_uuid:
b420b3bf-54c1-41a1-b89b-ccd03d217bcb                                 NA's 
                                   1                                  469 

patient.drugs.drug.8.day_of_form_completion:
  14 NA's 
   1  469 

patient.drugs.drug.8.days_to_drug_therapy_end:
 157 NA's 
   1  469 

patient.drugs.drug.8.days_to_drug_therapy_start:
  23 NA's 
   1  469 

patient.drugs.drug.8.drug_name:
renamin    NA's 
      1     469 

patient.drugs.drug.8.measure_of_response:
complete response              NA's 
                1               469 

patient.drugs.drug.8.month_of_form_completion:
  10 NA's 
   1  469 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   1  469 

patient.drugs.drug.8.therapy_types.therapy_type:
ancillary      NA's 
        1       469 

patient.drugs.drug.8.tx_on_clinical_trial:
  no NA's 
   1  469 

patient.drugs.drug.8.year_of_form_completion:
2013 NA's 
   1  469 




patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   14.50   15.28   20.00   31.00     324 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     24     321     761    1253    1639    6869     357 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      6     153     632    1172    1491   11015     329 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          46                           27 
            partial response               stable disease 
                          10                            5 
                        NA's 
                         382 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   7.014  10.000  12.000     324 

patient.drugs.drug.number_cycles:
   1    2    4 NA's 
   2    4    2  462 

patient.drugs.drug.prescribed_dose:
   1   20  400 NA's 
   1    1    2  466 

patient.drugs.drug.prescribed_dose_units:
    mg mg/day  mg/kg   NA's 
     1      1      1    467 

patient.drugs.drug.regimen_indication:
               adjuvant other: specify in notes             progression 
                      6                       1                       9 
             recurrence                    NA's 
                      3                     451 

patient.drugs.drug.regimen_indication_notes:
at same time as surgery                    NA's 
                      1                     469 


patient.drugs.drug.route_of_administrations.route_of_administration:
 intravenous (iv)              oral subcutaneous (sc)              NA's 
                6                 1                 1               462 

patient.drugs.drug.route_of_administrations.route_of_administration.2:
subcutaneous (sc)              NA's 
                1               469 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 124   22  324 


patient.drugs.drug.therapy_types.therapy_type_notes:
isolated limb perfusion (ilp)                          NA's 
                            1                           469 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  82   44  344 

patient.drugs.drug.year_of_form_completion:
2012 2013 2014 2015 NA's 
  39   47   57    3  324 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    11                    446                     13 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    9.00   20.00   17.93   24.75   30.00     416 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    472     620     721    1378    1235    5370     461 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  111.0   832.8  1510.5  2198.9  2987.2  6826.0     428 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  50    4  416 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   5.204   9.000  12.000     416 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
5286 NA's 
   1  469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
5276 NA's 
   1  469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
extremities        NA's 
          1         469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_non_melanoma_event_histologic_type_text:
 bcc NA's 
   1  469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_additional_rad_tx:
  no NA's 
   1  469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_additional_rx_tx:
  no NA's 
   1  469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
1697 3515 5286 NA's 
   1    1    1  467 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    599     852    1689    2480    3488    5305     461 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
extremities        NA's 
          1         469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence     regional lymph node 
                      4                       1                       2 
                   NA's 
                    463 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_non_melanoma_event_histologic_type_text:
 scc NA's 
   1  469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_additional_rad_tx:
  no  yes NA's 
   4    1  465 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_additional_rx_tx:
  no NA's 
   5  465 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    3  466 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_metastasis_anatomic_site:
         brain other, specify           NA's 
             4              1            465 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_metastasis_anatomic_site_other_text:
subcutaneous skin chest wall                         NA's 
                           1                          469 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  41    8  421 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        34         15        421 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  45    6  419 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  47    5  418 

patient.follow_ups.follow_up.2.subsequent_primary_melanoma_during_followup:
  no NA's 
  35  435 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   42     9   419 

patient.follow_ups.follow_up.2.year_of_form_completion:
2013 2014 2015 NA's 
   1   18   35  416 



patient.follow_ups.follow_up.3.day_of_form_completion:
   7   12   20   26 NA's 
   1    2   10    1  456 

patient.follow_ups.follow_up.3.days_to_death:
1032 6873 NA's 
   1    1  468 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    765    1076    2036    2325    2659    6311     458 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
  14  456 

patient.follow_ups.follow_up.3.month_of_form_completion:
   3    5 NA's 
  11    3  456 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
6841 NA's 
   1  469 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                469 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_metastasis_anatomic_site:
lung NA's 
   1  469 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  13    1  456 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        10          4        456 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no NA's 
  14  456 

patient.follow_ups.follow_up.3.radiation_therapy:
  no NA's 
  14  456 

patient.follow_ups.follow_up.3.subsequent_primary_melanoma_during_followup:
  no NA's 
   1  469 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
   12     2   456 

patient.follow_ups.follow_up.3.year_of_form_completion:
2015 NA's 
  14  456 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   21.00   18.98   27.00   31.00     149 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  190.0   368.5  1093.0  1554.4  2004.5 10346.0     407 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0   441.8   922.0  1772.1  2366.5 11252.0     244 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
scheduled follow-up submission                           NA's 
                             6                            464 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 267   48  155 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   7.103  11.000  12.000     149 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.10.days_to_new_tumor_event_after_initial_treatment:
2367 NA's 
   1  469 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.10.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                469 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.10.new_tumor_additional_rad_tx:
  no NA's 
   1  469 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.10.new_tumor_additional_rx_tx:
  no NA's 
   1  469 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.10.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  469 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.10.new_tumor_metastasis_anatomic_site:
other, specify           NA's 
             1            469 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.10.new_tumor_metastasis_anatomic_site_other_text:
ln left pelvic           NA's 
             1            469 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  339.0   498.8   858.5  2062.4  1743.5 11007.0     460 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     24     482    1313    2039    2414   10990     434 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             2            468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence     regional lymph node 
                     27                       5                       3 
                   NA's 
                    435 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_non_melanoma_event_histologic_type_text:
         adenocarcinoma adenocarcinoma pancreas                    NA's 
                      1                       1                     468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_additional_rad_tx:
  no  yes NA's 
  31    2  437 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_additional_rx_tx:
  no  yes NA's 
  13   19  438 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  26    9  435 



patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
 1328  1965  3896 11007  NA's 
    1     1     1     1   466 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    126     576    1729    2613    3070   10990     450 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
     distant metastasis locoregional recurrence     regional lymph node 
                     16                       2                       2 
                   NA's 
                    450 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_additional_rad_tx:
  no  yes NA's 
  19    1  450 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_additional_rx_tx:
  no  yes NA's 
  10   10  450 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  16    4  450 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_metastasis_anatomic_site:
          bone          liver           lung other, specify           NA's 
             5              2              2              7            454 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_additional_surgery_procedure:
 6218 10916  NA's 
    1     1   468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    126    1463    2315    3032    3121   10916     457 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
 distant metastasis regional lymph node                NA's 
                 12                   1                 457 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_additional_rad_tx:
  no  yes NA's 
  12    1  457 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_additional_rx_tx:
  no  yes NA's 
   6    7  457 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  11    2  457 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_metastasis_anatomic_site:
         brain          liver           lung other, specify           NA's 
             1              1              3              7            458 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_additional_surgery_procedure:
 771 1828 2085 6218 NA's 
   1    1    1    1  466 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    126    1630    2072    3149    3585   11006     459 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                     10                       1                     459 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_tumor_additional_rad_tx:
  no NA's 
  11  459 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_tumor_additional_rx_tx:
  no  yes NA's 
   4    7  459 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.5.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   7    4  459 



patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_additional_surgery_procedure:
1800 2253 6253 NA's 
   1    1    1  467 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    648    1751    2310    3747    4599   11006     462 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      7                       1                     462 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_tumor_additional_rad_tx:
  no  yes NA's 
   7    1  462 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_tumor_additional_rx_tx:
  no  yes NA's 
   3    5  462 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   5    3  462 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_tumor_metastasis_anatomic_site:
         brain          liver other, specify           NA's 
             2              2              3            463 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.6.new_tumor_metastasis_anatomic_site_other_text:
                left parotid tumour left upper arm skin and soft tissue 
                                  1                                   1 
                             spleen                                NA's 
                                  1                                 467 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.days_to_new_tumor_event_additional_surgery_procedure:
2408 NA's 
   1  469 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1574    2367    2408    4725    6271   11006     465 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.new_neoplasm_event_type:
distant metastasis               NA's 
                 5                465 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.new_tumor_additional_rad_tx:
  no  yes NA's 
   4    1  465 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.new_tumor_additional_rx_tx:
  no  yes NA's 
   2    2  466 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   4    1  465 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.new_tumor_metastasis_anatomic_site:
         brain other, specify           NA's 
             2              3            465 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.7.new_tumor_metastasis_anatomic_site_other_text:
         left anterior chest - skin and soft tissue 
                                                  1 
left occipito-parietal scalp - skin and soft tissue 
                                                  1 
                upper abdomen subcutaneous deposits 
                                                  1 
                                               NA's 
                                                467 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.days_to_new_tumor_event_additional_surgery_procedure:
2455 6505 NA's 
   1    1  468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.days_to_new_tumor_event_after_initial_treatment:
2367 2455 6505 NA's 
   1    1    1  467 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_neoplasm_event_type:
distant metastasis               NA's 
                 3                467 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_tumor_additional_rad_tx:
  no NA's 
   3  467 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_tumor_additional_rx_tx:
  no NA's 
   2  468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    2  467 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_tumor_metastasis_anatomic_site:
         brain other, specify           NA's 
             1              2            467 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.8.new_tumor_metastasis_anatomic_site_other_text:
 ln right neck posterior neck           NA's 
             1              1            468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.9.days_to_new_tumor_event_after_initial_treatment:
2367 6525 NA's 
   1    1  468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.9.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.9.new_tumor_additional_rad_tx:
  no NA's 
   2  468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.9.new_tumor_additional_rx_tx:
  no NA's 
   2  468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.9.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  469 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.9.new_tumor_metastasis_anatomic_site:
other, specify           NA's 
             2            468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.9.new_tumor_metastasis_anatomic_site_other_text:
right head - skin and soft tissue                       small bowel 
                                1                                 1 
                             NA's 
                              468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     37     339    1007    2006    3037   10916     425 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   324.0   880.5  1755.0  2287.8 10888.0     378 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
   extremities           lung other, specify          trunk           NA's 
             2              2              1              1            464 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     47                      16                       2 
    regional lymph node                    NA's 
                     15                     390 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_non_melanoma_event_histologic_type_text:
         adenocarcinoma                     cml                     scc 
                      1                       1                       2 
squamous cell carcinoma                    NA's 
                      1                     465 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_primary_melanoma_anatomic_site:
  extremities head and neck          NA's 
            2             1           467 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_additional_rad_tx:
  no  yes NA's 
  55   18  397 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_additional_rx_tx:
  no  yes NA's 
  40   33  397 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  35   45  390 



patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.primary_anatomic_site_count:
   1 NA's 
   2  468 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 189   93  188 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       182        105        183 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
 218   66  186 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 247   38  185 

patient.follow_ups.follow_up.subsequent_primary_melanoma_during_followup:
  no  yes NA's 
 232    6  232 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  229    63   178 

patient.follow_ups.follow_up.year_of_form_completion:
2012 2013 2014 2015 NA's 
  13   94  175   39  149 

patient.gender:
female   male 
   180    290 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  146.0   163.0   170.0   170.1   178.0   193.0     217 

patient.history_of_neoadjuvant_treatment:
 no yes 
445  25 




patient.informed_consent_verified:
yes 
470 

patient.interferon_90_day_prior_excision_admin_indicator:
  no  yes NA's 
  12   29  429 

patient.lactate_dehydrogenase_result:
elevated   normal     NA's 
       2        5      463 

patient.malignant_neoplasm_mitotic_count_rate:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.000   5.000   6.464   9.000  40.000     298 


patient.melanoma_origin_skin_anatomic_site:
non-glabrous skin              NA's 
              435                35 

patient.melanoma_ulceration_indicator:
  no  yes NA's 
 146  167  157 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   7.017  11.000  12.000 

patient.new_tumor_events.new_tumor_dx_prior_submitted_specimen_dx:
  no  yes NA's 
 140   74  256 

patient.new_tumor_events.new_tumor_event.10.additional_pharmaceutical_therapy:
  no  yes NA's 
   6    6  458 

patient.new_tumor_events.new_tumor_event.10.additional_radiation_therapy:
  no NA's 
  13  457 

patient.new_tumor_events.new_tumor_event.10.days_to_new_tumor_event_additional_surgery_procedure:
1359 1823 4027 NA's 
   1    1    1  467 

patient.new_tumor_events.new_tumor_event.10.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    333    1359    2105    2803    4447    5964     457 

patient.new_tumor_events.new_tumor_event.10.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                     12                       1                     457 

patient.new_tumor_events.new_tumor_event.10.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  10    3  457 

patient.new_tumor_events.new_tumor_event.10.new_tumor_metastasis_anatomic_site:
          bone other, specify           NA's 
             1             11            458 


patient.new_tumor_events.new_tumor_event.11.additional_pharmaceutical_therapy:
  no  yes NA's 
   4    5  461 

patient.new_tumor_events.new_tumor_event.11.additional_radiation_therapy:
  no  yes NA's 
   9    1  460 

patient.new_tumor_events.new_tumor_event.11.days_to_new_tumor_event_additional_surgery_procedure:
1359 1863 4027 NA's 
   1    1    1  467 

patient.new_tumor_events.new_tumor_event.11.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    333     924    1829    2124    3503    4447     460 

patient.new_tumor_events.new_tumor_event.11.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      8                       2                     460 

patient.new_tumor_events.new_tumor_event.11.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   7    3  460 

patient.new_tumor_events.new_tumor_event.11.new_tumor_metastasis_anatomic_site:
         brain          liver other, specify           NA's 
             1              1              6            462 


patient.new_tumor_events.new_tumor_event.12.additional_pharmaceutical_therapy:
  no  yes NA's 
   3    1  466 

patient.new_tumor_events.new_tumor_event.12.additional_radiation_therapy:
  no NA's 
   7  463 

patient.new_tumor_events.new_tumor_event.12.days_to_new_tumor_event_additional_surgery_procedure:
1988 2107 4027 NA's 
   1    1    1  467 

patient.new_tumor_events.new_tumor_event.12.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    779    1892    2105    2741    4036    4447     463 

patient.new_tumor_events.new_tumor_event.12.new_neoplasm_event_type:
     distant metastasis locoregional recurrence     regional lymph node 
                      4                       2                       1 
                   NA's 
                    463 

patient.new_tumor_events.new_tumor_event.12.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   4    3  463 

patient.new_tumor_events.new_tumor_event.12.new_tumor_metastasis_anatomic_site:
          lung other, specify           NA's 
             2              2            466 

patient.new_tumor_events.new_tumor_event.12.new_tumor_metastasis_anatomic_site_other_text:
right scalp      spleen        NA's 
          1           1         468 

patient.new_tumor_events.new_tumor_event.13.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    1  467 

patient.new_tumor_events.new_tumor_event.13.additional_radiation_therapy:
  no NA's 
   4  466 

patient.new_tumor_events.new_tumor_event.13.days_to_new_tumor_event_additional_surgery_procedure:
2023 4187 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.13.days_to_new_tumor_event_after_initial_treatment:
2023 4044 4187 4447 NA's 
   1    1    1    1  466 

patient.new_tumor_events.new_tumor_event.13.new_neoplasm_event_type:
distant metastasis               NA's 
                 4                466 

patient.new_tumor_events.new_tumor_event.13.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    2  466 

patient.new_tumor_events.new_tumor_event.13.new_tumor_metastasis_anatomic_site:
          lung other, specify           NA's 
             1              3            466 

patient.new_tumor_events.new_tumor_event.13.new_tumor_metastasis_anatomic_site_other_text:
          mediastinum right posterior chest          right temple 
                    1                     1                     1 
                 NA's 
                  467 

patient.new_tumor_events.new_tumor_event.14.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    1  467 

patient.new_tumor_events.new_tumor_event.14.additional_radiation_therapy:
  no  yes NA's 
   3    1  466 

patient.new_tumor_events.new_tumor_event.14.days_to_new_tumor_event_additional_surgery_procedure:
2121 4187 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.14.days_to_new_tumor_event_after_initial_treatment:
2121 4163 4187 4447 NA's 
   1    1    1    1  466 

patient.new_tumor_events.new_tumor_event.14.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      3                       1                     466 

patient.new_tumor_events.new_tumor_event.14.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    2  466 

patient.new_tumor_events.new_tumor_event.14.new_tumor_metastasis_anatomic_site:
         brain          liver other, specify           NA's 
             1              1              1            467 

patient.new_tumor_events.new_tumor_event.14.new_tumor_metastasis_anatomic_site_other_text:
right upper scalp              NA's 
                1               469 

patient.new_tumor_events.new_tumor_event.15.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.15.additional_radiation_therapy:
  no NA's 
   3  467 

patient.new_tumor_events.new_tumor_event.15.days_to_new_tumor_event_additional_surgery_procedure:
2151 4187 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.15.days_to_new_tumor_event_after_initial_treatment:
2151 4184 4187 NA's 
   1    1    1  467 

patient.new_tumor_events.new_tumor_event.15.new_neoplasm_event_type:
distant metastasis               NA's 
                 3                467 

patient.new_tumor_events.new_tumor_event.15.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    2  467 

patient.new_tumor_events.new_tumor_event.15.new_tumor_metastasis_anatomic_site:
          bone other, specify           NA's 
             1              2            467 

patient.new_tumor_events.new_tumor_event.15.new_tumor_metastasis_anatomic_site_other_text:
           left thigh right posterior scalp                  NA's 
                    1                     1                   468 

patient.new_tumor_events.new_tumor_event.16.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.16.additional_radiation_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.16.days_to_new_tumor_event_additional_surgery_procedure:
2195 4187 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.16.days_to_new_tumor_event_after_initial_treatment:
2195 4187 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.16.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                468 

patient.new_tumor_events.new_tumor_event.16.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.16.new_tumor_metastasis_anatomic_site:
other, specify           NA's 
             2            468 

patient.new_tumor_events.new_tumor_event.16.new_tumor_metastasis_anatomic_site_other_text:
      left thigh right upper back             NA's 
               1                1              468 

patient.new_tumor_events.new_tumor_event.17.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.17.additional_radiation_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.17.days_to_new_tumor_event_additional_surgery_procedure:
2334 4321 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.17.days_to_new_tumor_event_after_initial_treatment:
2334 4321 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.17.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                468 

patient.new_tumor_events.new_tumor_event.17.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.17.new_tumor_metastasis_anatomic_site:
other, specify           NA's 
             2            468 

patient.new_tumor_events.new_tumor_event.17.new_tumor_metastasis_anatomic_site_other_text:
          base penis right anterior chest                 NA's 
                   1                    1                  468 

patient.new_tumor_events.new_tumor_event.18.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.18.additional_radiation_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.18.days_to_new_tumor_event_additional_surgery_procedure:
2334 4383 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.18.days_to_new_tumor_event_after_initial_treatment:
2388 4370 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.18.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      1                       1                     468 

patient.new_tumor_events.new_tumor_event.18.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.18.new_tumor_metastasis_anatomic_site:
other, specify           NA's 
             1            469 

patient.new_tumor_events.new_tumor_event.18.new_tumor_metastasis_anatomic_site_other_text:
left lower back            NA's 
              1             469 

patient.new_tumor_events.new_tumor_event.19.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.19.additional_radiation_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.19.days_to_new_tumor_event_additional_surgery_procedure:
2388 4410 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.19.days_to_new_tumor_event_after_initial_treatment:
2388 4396 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.19.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                468 

patient.new_tumor_events.new_tumor_event.19.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.19.new_tumor_metastasis_anatomic_site:
         brain other, specify           NA's 
             1              1            468 

patient.new_tumor_events.new_tumor_event.19.new_tumor_metastasis_anatomic_site_other_text:
left anterior abdomen                  NA's 
                    1                   469 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
  85   39  346 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
 101   24  345 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   55.0   757.2  1267.0  1871.6  2682.8  8305.0     406 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     55     389     940    1523    1984    8305     344 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
         colon    extremities other, specify          trunk           NA's 
             1              1              2              1            465 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     79                      28                      10 
    regional lymph node                    NA's 
                      7                     346 


patient.new_tumor_events.new_tumor_event.2.new_primary_melanoma_anatomic_site:
  extremities head and neck         trunk          NA's 
            4             3             3           460 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  64   65  341 



patient.new_tumor_events.new_tumor_event.2.primary_anatomic_site_count:
   1 NA's 
   8  462 

patient.new_tumor_events.new_tumor_event.20.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.20.additional_radiation_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.20.days_to_new_tumor_event_after_initial_treatment:
2388 4396 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.20.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                468 

patient.new_tumor_events.new_tumor_event.20.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.20.new_tumor_metastasis_anatomic_site:
         liver other, specify           NA's 
             1              1            468 

patient.new_tumor_events.new_tumor_event.20.new_tumor_metastasis_anatomic_site_other_text:
abdominal soft tissue                  NA's 
                    1                   469 

patient.new_tumor_events.new_tumor_event.21.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.21.additional_radiation_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.21.days_to_new_tumor_event_after_initial_treatment:
2388 4396 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.21.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                468 

patient.new_tumor_events.new_tumor_event.21.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.21.new_tumor_metastasis_anatomic_site:
other, specify           NA's 
             2            468 

patient.new_tumor_events.new_tumor_event.21.new_tumor_metastasis_anatomic_site_other_text:
 chest wall soft tissue retroperitoneal nodules                    NA's 
                      1                       1                     468 

patient.new_tumor_events.new_tumor_event.22.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.22.additional_radiation_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.22.days_to_new_tumor_event_after_initial_treatment:
2388 4396 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.22.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                468 

patient.new_tumor_events.new_tumor_event.22.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.22.new_tumor_metastasis_anatomic_site:
other, specify           NA's 
             2            468 

patient.new_tumor_events.new_tumor_event.22.new_tumor_metastasis_anatomic_site_other_text:
     mediastinal lymph mediastinal lymph node                   NA's 
                     1                      1                    468 

patient.new_tumor_events.new_tumor_event.23.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.23.additional_radiation_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.23.days_to_new_tumor_event_after_initial_treatment:
2388 4396 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.23.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                468 

patient.new_tumor_events.new_tumor_event.23.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.23.new_tumor_metastasis_anatomic_site:
other, specify           NA's 
             2            468 

patient.new_tumor_events.new_tumor_event.23.new_tumor_metastasis_anatomic_site_other_text:
pancreatic masses     right buttock              NA's 
                1                 1               468 

patient.new_tumor_events.new_tumor_event.24.additional_pharmaceutical_therapy:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.24.additional_radiation_therapy:
  no  yes NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.24.days_to_new_tumor_event_after_initial_treatment:
2388 4441 NA's 
   1    1  468 

patient.new_tumor_events.new_tumor_event.24.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                468 

patient.new_tumor_events.new_tumor_event.24.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.24.new_tumor_metastasis_anatomic_site:
          bone other, specify           NA's 
             1              1            468 

patient.new_tumor_events.new_tumor_event.24.new_tumor_metastasis_anatomic_site_other_text:
left axilla        NA's 
          1         469 

patient.new_tumor_events.new_tumor_event.25.additional_pharmaceutical_therapy:
  no NA's 
   1  469 

patient.new_tumor_events.new_tumor_event.25.additional_radiation_therapy:
  no NA's 
   1  469 

patient.new_tumor_events.new_tumor_event.25.days_to_new_tumor_event_additional_surgery_procedure:
2446 NA's 
   1  469 

patient.new_tumor_events.new_tumor_event.25.days_to_new_tumor_event_after_initial_treatment:
2446 NA's 
   1  469 

patient.new_tumor_events.new_tumor_event.25.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                469 

patient.new_tumor_events.new_tumor_event.25.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  469 

patient.new_tumor_events.new_tumor_event.25.new_tumor_metastasis_anatomic_site:
other, specify           NA's 
             1            469 

patient.new_tumor_events.new_tumor_event.25.new_tumor_metastasis_anatomic_site_other_text:
right hip      NA's 
        1       469 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
  59   30  381 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no  yes NA's 
  81   11  378 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    484    1000    1468    2329    3133    6038     446 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     55     483    1288    1753    2606    8126     377 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     73                      16                       4 
    regional lymph node                    NA's 
                      3                     374 

patient.new_tumor_events.new_tumor_event.3.new_primary_melanoma_anatomic_site:
extremities       trunk        NA's 
          2           2         466 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  69   26  375 



patient.new_tumor_events.new_tumor_event.3.primary_anatomic_site_count:
   1 NA's 
   4  466 

patient.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no  yes NA's 
  52   21  397 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no  yes NA's 
  65   10  395 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  172.0   874.5  1481.5  1961.1  2683.8  7361.0     444 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     55     609    1479    1988    2780    8126     393 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
extremities       trunk        NA's 
          1           1         468 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     60                      10                       4 
    regional lymph node                    NA's 
                      3                     393 

patient.new_tumor_events.new_tumor_event.4.new_non_melanoma_event_histologic_type_text:
   basal cell carcinoma squamous cell carcinoma                    NA's 
                      1                       1                     468 

patient.new_tumor_events.new_tumor_event.4.new_primary_melanoma_anatomic_site:
  extremities head and neck         trunk          NA's 
            2             1             1           466 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  52   27  391 



patient.new_tumor_events.new_tumor_event.4.primary_anatomic_site_count:
   1 NA's 
   4  466 

patient.new_tumor_events.new_tumor_event.5.additional_pharmaceutical_therapy:
  no  yes NA's 
  32   19  419 

patient.new_tumor_events.new_tumor_event.5.additional_radiation_therapy:
  no  yes NA's 
  43    7  420 

patient.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  212.0   735.2  1454.5  1885.0  2470.2  7449.0     452 

patient.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   33.0   490.8  1454.5  1774.5  2391.8  7449.0     418 


patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     40                       7                       2 
                   NA's 
                    421 


patient.new_tumor_events.new_tumor_event.5.new_primary_melanoma_anatomic_site:
extremities       trunk        NA's 
          1           1         468 

patient.new_tumor_events.new_tumor_event.5.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  32   20  418 

patient.new_tumor_events.new_tumor_event.5.new_tumor_metastasis_anatomic_site:
          bone          brain          liver other, specify           NA's 
             3              7              1             29            430 


patient.new_tumor_events.new_tumor_event.5.primary_anatomic_site_count:
   1 NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.6.additional_pharmaceutical_therapy:
  no  yes NA's 
  26   12  432 

patient.new_tumor_events.new_tumor_event.6.additional_radiation_therapy:
  no  yes NA's 
  37    1  432 

patient.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    315    1499    2488    2923    4113    7512     458 

patient.new_tumor_events.new_tumor_event.6.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  250.0   703.8  1689.5  2133.6  3126.5  7512.0     432 

patient.new_tumor_events.new_tumor_event.6.new_neoplasm_event_occurrence_anatomic_site:
head and neck      prostate          NA's 
            1             1           468 

patient.new_tumor_events.new_tumor_event.6.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     26                       8                       2 
    regional lymph node                    NA's 
                      1                     433 

patient.new_tumor_events.new_tumor_event.6.new_non_melanoma_event_histologic_type_text:
      adenocarcinoma basal cell carcinoma                 NA's 
                   1                    1                  468 

patient.new_tumor_events.new_tumor_event.6.new_primary_melanoma_anatomic_site:
extremities       trunk        NA's 
          1           1         468 

patient.new_tumor_events.new_tumor_event.6.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  26   12  432 



patient.new_tumor_events.new_tumor_event.6.primary_anatomic_site_count:
   1 NA's 
   2  468 

patient.new_tumor_events.new_tumor_event.7.additional_pharmaceutical_therapy:
  no  yes NA's 
  20    9  441 

patient.new_tumor_events.new_tumor_event.7.additional_radiation_therapy:
  no  yes NA's 
  28    1  441 

patient.new_tumor_events.new_tumor_event.7.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    657    1697    2587    3067    3902    7652     462 

patient.new_tumor_events.new_tumor_event.7.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    250    1042    1897    2471    3828    7652     442 

patient.new_tumor_events.new_tumor_event.7.new_neoplasm_event_occurrence_anatomic_site:
extremities        NA's 
          1         469 

patient.new_tumor_events.new_tumor_event.7.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     21                       5                       1 
    regional lymph node                    NA's 
                      1                     442 

patient.new_tumor_events.new_tumor_event.7.new_non_melanoma_event_histologic_type_text:
basal cell carcinoma                 NA's 
                   1                  469 

patient.new_tumor_events.new_tumor_event.7.new_primary_melanoma_anatomic_site:
head and neck          NA's 
            1           469 

patient.new_tumor_events.new_tumor_event.7.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  21    8  441 

patient.new_tumor_events.new_tumor_event.7.new_tumor_metastasis_anatomic_site:
          bone          liver other, specify           NA's 
             2              3             16            449 


patient.new_tumor_events.new_tumor_event.7.primary_anatomic_site_count:
   1 NA's 
   1  469 

patient.new_tumor_events.new_tumor_event.8.additional_pharmaceutical_therapy:
  no  yes NA's 
  16    9  445 

patient.new_tumor_events.new_tumor_event.8.additional_radiation_therapy:
  no  yes NA's 
  24    2  444 

patient.new_tumor_events.new_tumor_event.8.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1271    1804    3795    3792    4426    9021     463 

patient.new_tumor_events.new_tumor_event.8.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    252     701    1814    2500    3969    9021     445 

patient.new_tumor_events.new_tumor_event.8.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     21                       3                       1 
    regional lymph node                    NA's 
                      1                     444 

patient.new_tumor_events.new_tumor_event.8.new_primary_melanoma_anatomic_site:
trunk  NA's 
    1   469 

patient.new_tumor_events.new_tumor_event.8.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  19    7  444 

patient.new_tumor_events.new_tumor_event.8.new_tumor_metastasis_anatomic_site:
         brain          liver           lung other, specify           NA's 
             2              2              2             15            449 


patient.new_tumor_events.new_tumor_event.8.primary_anatomic_site_count:
   1 NA's 
   1  469 

patient.new_tumor_events.new_tumor_event.9.additional_pharmaceutical_therapy:
  no  yes NA's 
  12    5  453 

patient.new_tumor_events.new_tumor_event.9.additional_radiation_therapy:
  no  yes NA's 
  15    2  453 

patient.new_tumor_events.new_tumor_event.9.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1359    2225    4217    3646    4504    5921     464 

patient.new_tumor_events.new_tumor_event.9.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    328     780    2009    2587    4390    5921     453 

patient.new_tumor_events.new_tumor_event.9.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     14                       2                       1 
                   NA's 
                    453 

patient.new_tumor_events.new_tumor_event.9.new_primary_melanoma_anatomic_site:
trunk  NA's 
    1   469 

patient.new_tumor_events.new_tumor_event.9.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  11    6  453 

patient.new_tumor_events.new_tumor_event.9.new_tumor_metastasis_anatomic_site:
         brain          liver other, specify           NA's 
             2              2             10            456 


patient.new_tumor_events.new_tumor_event.9.primary_anatomic_site_count:
   1 NA's 
   1  469 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
 134   61  275 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
 159   38  273 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   333.5   858.5  1259.9  1590.8  8113.0     342 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     208     646    1177    1442   10334     247 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
      bladder   extremities head and neck          lung          NA's 
            1             1             2             1           465 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence    new primary melanoma 
                     91                      46                      19 
    regional lymph node                    NA's 
                     44                     270 


patient.new_tumor_events.new_tumor_event.new_primary_melanoma_anatomic_site:
  extremities head and neck         trunk          NA's 
            8             4            10           448 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  76  130  264 



patient.new_tumor_events.new_tumor_event.primary_anatomic_site_count:
   0    1    2 NA's 
   1   12    1  456 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 119  338   13 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       222        220         28 

patient.postoperative_rx_tx:
  no  yes NA's 
 329   92   49 

patient.primary_neoplasm_melanoma_dx:
 no yes 
 47 423 

patient.prior_dx:
 no yes 
448  22 

patient.prior_systemic_therapy_types.prior_systemic_therapy_type:
         chemotherapy immunotherapy/vaccine            interferon 
                    2                     3                    29 
                other                  NA's 
                    1                   435 

patient.race:
                    asian black or african american                     white 
                       12                         1                       447 
                     NA's 
                       10 

patient.radiation_therapy:
  no  yes NA's 
 438   23    9 




patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.50   11.00   12.15   16.00   27.00     450 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  158.0   457.5  1025.5  1191.8  1326.0  4462.0     450 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  158.0   447.8  1018.5  1181.0  1301.2  4458.0     450 

patient.radiations.radiation.2.measure_of_response:
               complete response radiographic progressive disease 
                               3                                2 
                  stable disease                             NA's 
                               2                              463 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.50    7.50    7.65   10.00   12.00     450 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.500   5.000   7.062  10.000  20.000     454 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   20.00   27.50   28.94   33.75   76.00     454 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
  20  450 

patient.radiations.radiation.2.radiation_type:
     external external beam          NA's 
           18             1           451 

patient.radiations.radiation.2.regimen_indication:
progression        NA's 
          1         469 

patient.radiations.radiation.2.units:
  gy NA's 
  16  454 

patient.radiations.radiation.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   2   14    3    1  450 

patient.radiations.radiation.3.anatomic_treatment_site:
 distant site regional site          NA's 
            3             2           465 



patient.radiations.radiation.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0    11.0    14.0    15.2    21.0    27.0     465 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  157.0   231.0  1117.0   969.2  1373.0  1968.0     465 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    140     231    1095     959    1367    1962     465 

patient.radiations.radiation.3.measure_of_response:
complete response    stable disease              NA's 
                1                 1               468 

patient.radiations.radiation.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     5.0     7.0     7.4    10.0    12.0     465 

patient.radiations.radiation.3.numfractions:
   5 NA's 
   2  468 

patient.radiations.radiation.3.radiation_dosage:
  20   30 3600 NA's 
   2    1    1  466 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   5  465 

patient.radiations.radiation.3.radiation_type:
external     NA's 
       5      465 

patient.radiations.radiation.3.units:
  gy NA's 
   4  466 

patient.radiations.radiation.3.year_of_form_completion:
2013 2014 NA's 
   4    1  465 

patient.radiations.radiation.4.anatomic_treatment_site:
distant recurrence       distant site      regional site               NA's 
                 1                  1                  1                467 

patient.radiations.radiation.4.bcr_radiation_barcode:
tcga-ee-a29q-r55047 tcga-ee-a2a5-r45373 tcga-ee-a2mn-r49875                NA's 
                  1                   1                   1                 467 

patient.radiations.radiation.4.bcr_radiation_uuid:
1a5695ee-233a-4bca-abec-997e373ea95a 3a195db3-cb4a-41be-885d-68dbafcef87f 
                                   1                                    1 
c0e58a71-a2dc-45a6-928e-3105aae536ed                                 NA's 
                                   1                                  467 

patient.radiations.radiation.4.day_of_form_completion:
  11   14   27 NA's 
   1    1    1  467 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
1118 1373 1397 NA's 
   1    1    1  467 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
1118 1351 1367 NA's 
   1    1    1  467 

patient.radiations.radiation.4.month_of_form_completion:
   7   10   12 NA's 
   1    1    1  467 

patient.radiations.radiation.4.numfractions:
   1    5   27 NA's 
   1    1    1  467 

patient.radiations.radiation.4.radiation_dosage:
  20   54 NA's 
   1    1  468 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   3  467 

patient.radiations.radiation.4.radiation_type:
external     NA's 
       3      467 

patient.radiations.radiation.4.units:
  gy NA's 
   2  468 

patient.radiations.radiation.4.year_of_form_completion:
2013 NA's 
   3  467 

patient.radiations.radiation.5.anatomic_treatment_site:
distant site         NA's 
           2          468 

patient.radiations.radiation.5.bcr_radiation_barcode:
tcga-ee-a2a5-r45374 tcga-ee-a2mn-r49876                NA's 
                  1                   1                 468 

patient.radiations.radiation.5.bcr_radiation_uuid:
08d402a0-f4b8-4a5c-b430-4a5f90b202fd 7409dc81-953f-4809-bdd4-3e671fa70140 
                                   1                                    1 
                                NA's 
                                 468 

patient.radiations.radiation.5.day_of_form_completion:
  11   14 NA's 
   1    1  468 

patient.radiations.radiation.5.days_to_radiation_therapy_end:
1182 1401 NA's 
   1    1  468 

patient.radiations.radiation.5.days_to_radiation_therapy_start:
1176 1401 NA's 
   1    1  468 

patient.radiations.radiation.5.month_of_form_completion:
   7   10 NA's 
   1    1  468 

patient.radiations.radiation.5.numfractions:
   1    5 NA's 
   1    1  468 

patient.radiations.radiation.5.radiation_dosage:
   8   20 NA's 
   1    1  468 

patient.radiations.radiation.5.radiation_treatment_ongoing:
  no NA's 
   2  468 

patient.radiations.radiation.5.radiation_type:
external     NA's 
       2      468 

patient.radiations.radiation.5.units:
  gy NA's 
   2  468 

patient.radiations.radiation.5.year_of_form_completion:
2013 NA's 
   2  468 

patient.radiations.radiation.6.anatomic_treatment_site:
distant site         NA's 
           1          469 

patient.radiations.radiation.6.bcr_radiation_barcode:
tcga-ee-a2mn-r49877                NA's 
                  1                 469 

patient.radiations.radiation.6.bcr_radiation_uuid:
579f693f-dec9-44ae-9ce1-c5500a0b23d6                                 NA's 
                                   1                                  469 

patient.radiations.radiation.6.day_of_form_completion:
  14 NA's 
   1  469 

patient.radiations.radiation.6.days_to_radiation_therapy_end:
1401 NA's 
   1  469 

patient.radiations.radiation.6.days_to_radiation_therapy_start:
1401 NA's 
   1  469 

patient.radiations.radiation.6.month_of_form_completion:
  10 NA's 
   1  469 

patient.radiations.radiation.6.numfractions:
   1 NA's 
   1  469 

patient.radiations.radiation.6.radiation_dosage:
   8 NA's 
   1  469 

patient.radiations.radiation.6.radiation_treatment_ongoing:
  no NA's 
   1  469 

patient.radiations.radiation.6.radiation_type:
external     NA's 
       1      469 

patient.radiations.radiation.6.units:
  gy NA's 
   1  469 

patient.radiations.radiation.6.year_of_form_completion:
2013 NA's 
   1  469 

patient.radiations.radiation.7.anatomic_treatment_site:
distant site         NA's 
           1          469 

patient.radiations.radiation.7.bcr_radiation_barcode:
tcga-ee-a2mn-r49878                NA's 
                  1                 469 

patient.radiations.radiation.7.bcr_radiation_uuid:
3776468c-3dd1-4183-89a0-b94b08eaafe8                                 NA's 
                                   1                                  469 

patient.radiations.radiation.7.day_of_form_completion:
  14 NA's 
   1  469 

patient.radiations.radiation.7.days_to_radiation_therapy_end:
1401 NA's 
   1  469 

patient.radiations.radiation.7.days_to_radiation_therapy_start:
1401 NA's 
   1  469 

patient.radiations.radiation.7.month_of_form_completion:
  10 NA's 
   1  469 

patient.radiations.radiation.7.numfractions:
   1 NA's 
   1  469 

patient.radiations.radiation.7.radiation_dosage:
   8 NA's 
   1  469 

patient.radiations.radiation.7.radiation_treatment_ongoing:
  no NA's 
   1  469 

patient.radiations.radiation.7.radiation_type:
external     NA's 
       1      469 

patient.radiations.radiation.7.units:
  gy NA's 
   1  469 

patient.radiations.radiation.7.year_of_form_completion:
2013 NA's 
   1  469 




patient.radiations.radiation.course_number:
   1 NA's 
   5  465 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   11.00   13.38   19.00   31.00     355 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0   262.5   766.0  1324.8  1931.5  8137.0     359 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     11     231     736    1291    1865    8128     357 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              18                                6 
radiographic progressive disease                   stable disease 
                              20                                3 
                            NA's 
                             423 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   7.444  10.000  12.000     355 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    9.00   12.41   20.00   33.00     382 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   30.00   48.00  850.37   63.62 5400.00     376 

patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
 114    1  355 

patient.radiations.radiation.radiation_type:
     external external beam          NA's 
           96             9           365 

patient.radiations.radiation.regimen_indication:
   adjuvant progression  recurrence        NA's 
          4           2           5         459 

patient.radiations.radiation.units:
 cgy   gy NA's 
  22   72  376 

patient.radiations.radiation.year_of_form_completion:
2012 2013 2014 2015 NA's 
  34   49   24    8  355 

patient.sites_of_primary_melanomas.primary_melanoma_at_diagnosis_count:
   0    1    2    3 NA's 
   1  164    6    1  298 

patient.sites_of_primary_melanomas.primary_tumor_multiple_present_ind:
  no  yes NA's 
 385   12   73 

patient.sites_of_primary_melanomas.site.2.primary_anatomic_site_count:
   1    2 NA's 
   2    1  467 

patient.sites_of_primary_melanomas.site.2.tumor_tissue_site:
extremities       trunk        NA's 
          3           2         465 

patient.sites_of_primary_melanomas.site.primary_anatomic_site_count:
   1    2    5 NA's 
 188    1    1  280 

patient.sites_of_primary_melanomas.site.tumor_tissue_site:
   extremities  head and neck other  specify          trunk           NA's 
           196             37             13            172             52 






patient.tissue_prospective_collection_indicator:
 no yes 
269 201 

patient.tissue_retrospective_collection_indicator:
 no yes 
201 269 


patient.tumor_tissue_site:
                                                                      distant metastasis 
                                                                                      68 
                                                                           primary tumor 
                                                                                     103 
regional cutaneous or subcutaneous tissue (includes satellite and in-transit metastasis) 
                                                                                      74 
                                                                     regional lymph node 
                                                                                     222 
                                                                                    NA's 
                                                                                       3 

patient.vital_status:
alive  dead 
  313   157 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0    69.0    80.0    81.4    90.0   161.0     212 

patient.year_of_form_completion:
2012 2013 2014 2015 
 225  152   86    7 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1978    2002    2008    2006    2011    2013       3 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
                    case matched expression control 
                                                  2 
metastatic tumor and additional metastatic tumor(s) 
                                                  1 
              primary tumor and metastatic tumor(s) 
                                                  2 
                                               NA's 
                                                465 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control                             NA's 
                               2                              468 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                  2                 468 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             465                5 

patient.biospecimen_cqcf.consent_or_death_status:
  consented    deceased exemption 4        NA's 
        390          57          18           5 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     215    1329    2039    2972   11724      11 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -25.0    23.5   360.0  1032.0  1372.5 10847.0      11 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
447  23 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     6                    160                    304 

patient.biospecimen_cqcf.ffpe_tumor_slide_submitted:
digital image         slide          NA's 
           41           216           213 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
cutaneous or subcutaneous tissue; regional 
                                        68 
                   lymph node(s); regional 
                                       224 
                       metastasis; distant 
                                        67 
                                      skin 
                                       111 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             451 
yes, pharmaceutical treatment prior to resection 
                                              19 

patient.biospecimen_cqcf.method_of_sample_procurement:
     excisional biopsy other (please specify)     surgical resection 
                    41                      9                    420 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    -20      40     426    1390    1746   10847     441 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0190   0.0553   0.1060  12.9643   0.2220 205.0000      454 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
            cdrop          nanodrop spectrophotometer              NA's 
                6                 2                 8               454 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.442  11.625  15.000  36.398  20.400 317.400     454 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   33.0   133.0   176.7   224.7   300.0   600.0     454 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
        blood draw  excisional biopsy surgical resection               NA's 
                25                  1                  4                440 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
other  NA's 
    5   465 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            5           465 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
  27   12  431 


patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
  19   27 1132 NA's 
   1    1    1  467 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_conc:
0.0189   NA's 
     1    469 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qm:
cdrop  NA's 
    1   469 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qty:
3.213  NA's 
    1   469 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_vol:
 170 NA's 
   1  469 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
                 2                  1                467 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
other  skin  NA's 
    2     1   467 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            3           467 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    2 NA's 
   3    2  465 

patient.biospecimen_cqcf.normal_controls.normal_control.4.bcr_sample_uuid:
e54d1844-c1c5-49cb-8417-211d4951fbc2                                 NA's 
                                   1                                  469 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_conc:
0.01346    NA's 
      1     469 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_qm:
cdrop  NA's 
    1   469 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_qty:
2.2882   NA's 
     1    469 

patient.biospecimen_cqcf.normal_controls.normal_control.4.extracted_dna.ncedna_dna_vol:
 170 NA's 
   1  469 

patient.biospecimen_cqcf.normal_controls.normal_control.4.vial_number:
   3 NA's 
   1  469 

patient.biospecimen_cqcf.normal_controls.normal_control.5.bcr_sample_uuid:
c00d38d3-e65e-42d0-a3df-adbfc33e22c2                                 NA's 
                                   1                                  469 

patient.biospecimen_cqcf.normal_controls.normal_control.5.extracted_dna.ncedna_dna_conc:
0.02755    NA's 
      1     469 

patient.biospecimen_cqcf.normal_controls.normal_control.5.extracted_dna.ncedna_dna_qm:
cdrop  NA's 
    1   469 

patient.biospecimen_cqcf.normal_controls.normal_control.5.extracted_dna.ncedna_dna_qty:
4.6835   NA's 
     1    469 

patient.biospecimen_cqcf.normal_controls.normal_control.5.extracted_dna.ncedna_dna_vol:
 170 NA's 
   1  469 

patient.biospecimen_cqcf.normal_controls.normal_control.5.vial_number:
   4 NA's 
   1  469 

patient.biospecimen_cqcf.normal_controls.normal_control.6.bcr_sample_uuid:
71fe001a-1cd9-4cc8-9857-88277a67a91a                                 NA's 
                                   1                                  469 

patient.biospecimen_cqcf.normal_controls.normal_control.6.extracted_dna.ncedna_dna_conc:
0.00544    NA's 
      1     469 

patient.biospecimen_cqcf.normal_controls.normal_control.6.extracted_dna.ncedna_dna_qm:
cdrop  NA's 
    1   469 

patient.biospecimen_cqcf.normal_controls.normal_control.6.extracted_dna.ncedna_dna_qty:
0.37536    NA's 
      1     469 

patient.biospecimen_cqcf.normal_controls.normal_control.6.extracted_dna.ncedna_dna_vol:
  69 NA's 
   1  469 

patient.biospecimen_cqcf.normal_controls.normal_control.6.vial_number:
   5 NA's 
   1  469 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -455.0    14.5   281.0  1047.1  1305.5 10837.0      19 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0140  0.0766  0.1220  0.5627  0.2795 76.0000     271 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   16.00   20.00   20.47   25.00   50.40     271 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   33.0    60.0   160.0   205.8   300.0   600.0     271 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
        blood draw  excisional biopsy surgical resection               NA's 
               457                  2                  1                 10 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
other  skin  NA's 
    1     3   466 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            3           467 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2 NA's 
 459    5    6 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 no yes 
  1 469 

patient.biospecimen_cqcf.path_confirm_report_attached:
 no yes 
 35 435 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
yes 
470 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
yes 
470 

patient.biospecimen_cqcf.race:
asian white  NA's 
    6   155   309 

patient.biospecimen_cqcf.sample_prescreened:
yes 
470 

patient.biospecimen_cqcf.site_of_disease:
cutaneous or subcutaneous tissue; regional 
                                        68 
                    lymph node(s) regional 
                                       224 
                       metastasis; distant 
                                        67 
                                      skin 
                                       103 
                                      NA's 
                                         8 

patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
 213   26  231 

patient.biospecimen_cqcf.top_slide_submitted:
 no yes 
 28 442 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0    5   30 NA's 
   2    4    1  463 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  60   70   80   95 NA's 
   1    2    2    2  463 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   142.5   150.0   162.1   215.0   230.0     463 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   4    3  463 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   4.483   7.000  30.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  20.00   75.00   85.00   81.67   90.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0   160.0   200.0   338.3   345.2  2773.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1   2 
469   1 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                      5                      54                     118 
               cryovial  other (please specify) 
                    138                     155 

patient.days_to_index:
   0 NA's 
 459   11 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
metastatic tumor and additional metastatic tumor(s) 
                                                  1 
              primary tumor and metastatic tumor(s) 
                                                  2 
                                               NA's 
                                                467 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   246.5  1391.0  2110.2  3178.5 11991.0      11 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     70      90     130     170     160     400     465 

patient.samples.sample.2.is_ffpe:
 no 
470 

patient.samples.sample.2.oct_embedded:
false  true 
  466     4 

patient.samples.sample.2.pathology_report_file_name:
tcga-d3-a1q6.7e74d698-ca50-40d9-8a06-6cecfd8580da.pdf 
                                                    1 
tcga-d3-a1qa.0895b97d-2bb1-4e6e-a59f-aae9579b46ef.pdf 
                                                    1 
tcga-er-a19t.c8dbb901-f139-48ad-af1f-7f2ced1e21fc.pdf 
                                                    1 
tcga-er-a2nf.19ec6528-7dce-43ad-a353-3b1ecd4b703a.pdf 
                                                    1 
                                                 NA's 
                                                  466 

patient.samples.sample.2.pathology_report_uuid:
0895b97d-2bb1-4e6e-a59f-aae9579b46ef 19ec6528-7dce-43ad-a353-3b1ecd4b703a 
                                   1                                    1 
7e74d698-ca50-40d9-8a06-6cecfd8580da c8dbb901-f139-48ad-af1f-7f2ced1e21fc 
                                   1                                    1 
                                NA's 
                                 466 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.78 1.82 1.84 NA's 
   1    1    1  467 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-d3-a1qa-07a-11r-a37k-07 tcga-er-a2nf-06a-11r-a18w-13 
                           1                            1 
                        NA's 
                         468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
278783a2-646d-4669-b153-c0ba3deab9cf c0439e75-7e0d-489b-8930-64ff86e38e72 
                                   1                                    1 
                                NA's 
                                 468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
118955268 164104804      NA's 
        1         1       468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.16 NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  25   26 NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   2   10 NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   5   12 NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a18w a37k NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   e    h NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
2.08  3.2 NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2014 NA's 
   1    1  468 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113390451 122847842 122848126 136396332 153889251 175156221        13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   2  454    1   13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.11 0.16  0.5 NA's 
   1    2  454   13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00    8.00   18.00   16.28   18.00   29.00      13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 457   13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    1.00    3.49    7.00   10.00      13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.923  10.000  12.000      13 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
1.43 2.08  3.2   40 NA's 
   1    1    1  454   13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
 457   13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
   2    1  454   13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
   2  268   84  103   13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
 3.26 16.98 40.95  NA's 
    1     1     1   467 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 454                    3                   13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   3  454   13 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
0.11 0.16 NA's 
   1    2  467 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 454   16 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
 454   16 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             457 
                                            NA's 
                                              13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 457   13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
nrna - melanoma protocol                  repli-g                     NA's 
                       3                      454                       13 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
1.36  1.4  1.7 NA's 
   1    1    1  467 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
 7.3  7.6  8.3 NA's 
   1    1    1  467 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      3     467 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
   3    1  466 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-d3-a1qa-07a-11w-a44h-08 tcga-er-a19t-06a-11w-a20k-08 
                           1                            1 
tcga-er-a2nf-06a-11w-a20l-08                         NA's 
                           1                          467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
20ccd4cd-a6b0-4716-a857-3026f15097d8 32bb99c1-bdb5-4266-bf7c-a5b38f0acc42 
                                   1                                    1 
9b233a80-49dd-4b0c-8332-258bbad00201                                 NA's 
                                   1                                  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
122846979 122847874 167803158      NA's 
        1         1         1       467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   8   18 NA's 
   1    2  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   1    7 NA's 
   2    1  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   3    4    7 NA's 
   1    1    1  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a20k a20l a44h NA's 
   1    1    1  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   a    d    e NA's 
   1    1    1  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2014 NA's 
   2    1  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   3                  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-d3-a1qa-07a-11w tcga-er-a19t-06a-11w tcga-er-a2nf-06a-11w 
                   1                    1                    1 
                NA's 
                 467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
040dd709-e8d6-4e2f-b6a0-aaae12def955 97396bb8-f971-4ea4-9244-2687bc85c3c9 
                                   1                                    1 
b0a07b7d-6de8-4f20-817d-7fabe256360f                                 NA's 
                                   1                                  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   3  467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      3     467 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   3  467 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118956878 118958489 122850130 134500919 153879826 167823253         1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
 147  322    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0800  0.1043  0.1500  0.1800       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   25.00   25.00   23.84   26.00   31.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 469    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   9.000   6.721  10.000  12.000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.908  10.000  12.000       1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.140   2.800   2.481   3.200   3.600       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
 469    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 6.7   40 NA's 
 147  322    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 NA's 
 216   69   86   98    1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113385757 113389387 113389436 130250290 167779800 167780153       301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8 NA's 
   1  168  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.07 0.08 0.09 0.17 NA's 
  31   97   40    1  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  25   26   30 NA's 
 119    1   49  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 169  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   2    4   10 NA's 
   1   49  119  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.438   9.000  12.000     301 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.070   3.200   2.622   3.200   3.600     301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 169  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  13 13.4   40 NA's 
   1   49  119  301 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2014 NA's 
 119   50  301 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118967618 167779988 167780016 165659051 167780044 167811834       424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8    9 NA's 
   2   42    2  424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0800  0.0852  0.0900  0.1600     424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  21   25   30 NA's 
   2    2   42  424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  46  424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1    4   10 NA's 
   2   42    2  424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   5.500   5.696   7.000  10.000     424 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.070   1.070   1.139   1.210   2.080     424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
  46  424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  13 13.4 NA's 
   2   44  424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2014 2015 NA's 
   2   42    2  424 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-er-a19t-06a-11d-a38r-08 tcga-er-a2nf-06a-11d-a38r-08 
                           1                            1 
                        NA's 
                         468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
ba51f105-3001-418a-8fb1-412d1088e38a c4792062-3c79-4f7e-b87b-1d4866098cda 
                                   1                                    1 
                                NA's 
                                 468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
167780001 167780038      NA's 
        1         1       468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8 NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.08 NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  30 NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   4 NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   2   10 NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a38r NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   c    f NA's 
   1    1  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
1.07 NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4 NA's 
   2  468 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2014 NA's 
   2  468 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118952647 118967655 122849362 134503400 153879869 167823261         1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    8 NA's 
 322  119   28    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1500  0.1600  0.1536  0.1700  0.1800       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   25.00   25.00   23.84   26.00   31.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 469    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   9.000   6.721  10.000  12.000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.908  10.000  12.000       1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.070   1.318   1.600   3.600       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
 469    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7   10   40 NA's 
 322  119   28    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
 216   69   86   98    1 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.98   11.57   18.18   24.65   34.03  144.40       1 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
 469    1 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
 469    1 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1582  0.1700  0.1800       1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 469    1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
 469    1 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             469 
                                            NA's 
                                               1 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
 469    1 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                              269                               200 
                             NA's 
                                1 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
      469         1 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
   3    1  466 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   17.00   15.96   22.00   31.00       1 

patient.samples.sample.2.portions.portion.is_ffpe:
  no NA's 
 469    1 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   6.759   9.000  12.000       1 

patient.samples.sample.2.portions.portion.portion_number:
   1    2   11   12 NA's 
 462    3    3    1    1 

patient.samples.sample.2.portions.portion.portion_sequence:
   1    2 NA's 
 402    3   65 

patient.samples.sample.2.portions.portion.slides.slide.bcr_slide_barcode:
tcga-d3-a1qa-07a-01-tsa tcga-er-a19t-06a-01-tsa tcga-er-a2nf-06a-01-tsa 
                      1                       1                       1 
tcga-fw-a5dy-11a-01-ts1                    NA's 
                      1                     466 

patient.samples.sample.2.portions.portion.slides.slide.bcr_slide_uuid:
58f948e1-1df0-4e13-a1fa-2ef0eedde354 9d212aba-b116-4d56-9344-302fff842ce4 
                                   1                                    1 
e7d8188d-e774-41f6-93f9-5dad199b67ad f461f575-9aec-4e14-a647-41572f736e88 
                                   1                                    1 
                                NA's 
                                 466 

patient.samples.sample.2.portions.portion.slides.slide.image_file_name:
tcga-d3-a1qa-07a-01-tsa.e7d8188d-e774-41f6-93f9-5dad199b67ad.svs 
                                                               1 
tcga-er-a19t-06a-01-tsa.f461f575-9aec-4e14-a647-41572f736e88.svs 
                                                               1 
tcga-er-a2nf-06a-01-tsa.9d212aba-b116-4d56-9344-302fff842ce4.svs 
                                                               1 
tcga-fw-a5dy-11a-01-ts1.58f948e1-1df0-4e13-a1fa-2ef0eedde354.svs 
                                                               1 
                                                            NA's 
                                                             466 

patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   4  466 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    2    4 NA's 
   1    2    1  466 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1 NA's 
   1    3  466 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0    1    6 NA's 
   2    1    1  466 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1 NA's 
   2    2  466 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0   10 NA's 
   3    1  466 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   0   14   24   90 NA's 
   1    1    1    1  466 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0   70   85  100 NA's 
   1    1    1    1  466 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0   70   85 NA's 
   1    2    1  466 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
   4  466 

patient.samples.sample.2.portions.portion.weight:
   1    2   30 2000 NA's 
   6  215    4   44  201 

patient.samples.sample.2.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2013    2014       1 

patient.samples.sample.2.portions.shipment_portion.bcr_shipment_portion_uuid:
20ee3a64-2e03-4d8b-82b8-57a31d166658 c342a8d0-21ad-49f2-be57-97288146e963 
                                   1                                    1 
                                NA's 
                                 468 

patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
   2  468 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
   2  468 

patient.samples.sample.2.portions.shipment_portion.plate_id:
a20m a241 NA's 
   1    1  468 

patient.samples.sample.2.portions.shipment_portion.portion_number:
  21   41 NA's 
   1    1  468 

patient.samples.sample.2.portions.shipment_portion.portion_sequence:
   2    4 NA's 
   1    1  468 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode:
tcga-d3-a1q6-07a-41-a20m-20 tcga-er-a2nf-06a-21-a241-20 
                          1                           1 
                       NA's 
                        468 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
  14   30 NA's 
   1    1  468 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   1   11 NA's 
   1    1  468 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 NA's 
   2  468 

patient.samples.sample.2.sample_type:
additional metastatic  blood derived normal            metastatic 
                    2                   465                     2 
  solid tissue normal 
                    1 

patient.samples.sample.2.sample_type_id:
  6   7  10  11 
  2   2 465   1 

patient.samples.sample.2.vial_number:
  a   b   f 
442  27   1 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  1                 469 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    246     882    1445    1416    1869    2654     464 

patient.samples.sample.3.initial_weight:
  70  320 NA's 
   1    1  468 

patient.samples.sample.3.is_ffpe:
  no NA's 
   6  464 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
    5     1   464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.82 1.85 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-fw-a3r5-11a-11r-a23a-13 tcga-gn-a4u8-11a-11r-a32p-07 
                           1                            1 
                        NA's 
                         468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
16f17f5b-73fa-48ef-ba5b-311e8a150ab4 ff7431a9-243a-4651-80bb-096379925bbe 
                                   1                                    1 
                                NA's 
                                 468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             469 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
132268586 153876582      NA's 
        1         1       468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.17 0.18 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  25   31 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   7    9 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    3 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a23a a32p NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   b    h NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
2.21  3.6 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 2013 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-fw-a3r5-11a-11r-a466-07 tcga-gn-a4u8-11a-11r-a360-13 
                           1                            1 
                        NA's 
                         468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
711f6c5d-e6f7-42c7-b845-aa02e1f27726 98bbf4f0-6758-485e-b7bb-b40a010157ba 
                                   1                                    1 
                                NA's 
                                 468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
164082184 167812966      NA's 
        1         1       468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7   13 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.17 0.18 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   9   29 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   1   12 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   2    8 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a360 a466 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   c    e NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
1.19 3.96 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6 23.3 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   2  468 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             469 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
122847127 122847834 122847872 129589732 129913386 153879144       464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   1    4    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.17 0.18  0.5 NA's 
   1    1    4  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  18   25   30 NA's 
   4    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   1    7    9 NA's 
   4    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   5.500   5.667   8.250  10.000     464 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   a    b    e    h NA's 
   3    1    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
2.34  3.4   40 NA's 
   1    1    4  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
   1    1    4  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 NA's 
   5    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
 9.91 21.78  NA's 
    1     1   468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   4                    2                  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   2    4  464 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.17 0.18 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   4  466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   4  466 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               6 
                                            NA's 
                                             464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
nrna - melanoma protocol                  repli-g                     NA's 
                       2                        4                      464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
 1.2 1.25 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
 8.4  8.5 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      2     468 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-fw-a3r5-11a-11w-a23h-08 tcga-gn-a4u8-11a-11w-a35w-08 
                           1                            1 
                        NA's 
                         468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
c9be6346-f420-4c13-ba5e-7e2814a85421 cf1e7d1b-0bb5-4071-a5ed-1817015bf60f 
                                   1                                    1 
                                NA's 
                                 468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
132274185 164082101      NA's 
        1         1       468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  22   29 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   1    8 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   6 NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a23h a35w NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   a    d NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2014 NA's 
   1    1  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-fw-a3r5-11a-11w tcga-gn-a4u8-11a-11w                 NA's 
                   1                    1                  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
046b372e-5666-49c7-a0a6-c28554d21767 79e6f5ed-54dc-420d-b5e6-d37e313e3a89 
                                   1                                    1 
                                NA's 
                                 468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     468 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   2  468 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118957448 118957767 118958490 126996839 128941153 153880373       464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
   2    4  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.08 0.17 0.18 NA's 
   4    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  25   31 NA's 
   5    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   7    9   10 NA's 
   1    1    4  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   5    6    9   10 NA's 
   1    3    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a192 a198 a23b a32n NA's 
   2    2    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   a    d    h NA's 
   4    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
1.14 1.21  3.2 NA's 
   1    1    4  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 6.7   40 NA's 
   2    4  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 NA's 
   4    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-er-a19t-10a-01d-a19a-08 tcga-er-a2nf-10a-01d-a19a-08 
                           1                            1 
tcga-fw-a3r5-11a-11d-a23d-05 tcga-gn-a4u8-11a-11d-a32s-05 
                           1                            1 
                        NA's 
                         466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
03a157fc-7ee6-44ef-87d8-f70ad334cfd9 e694c1a2-86f0-43d0-95cc-4cfaf0b04060 
                                   1                                    1 
f0d22e58-5486-47c4-a316-e974e2bcbae6 fd5802ca-8780-4151-aca0-f3bd82297133 
                                   1                                    1 
                                NA's 
                                 466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched methylation control                             NA's 
                               1                              469 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
118957444 118957452 132268741 153879701      NA's 
        1         1         1         1       466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8 NA's 
   2    2  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.08 0.09 0.16 0.17 NA's 
   1    1    1    1  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  25   31 NA's 
   3    1  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   4  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   7    9   10 NA's 
   1    1    2  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   6    9   10 NA's 
   2    1    1  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a19a a23d a32s NA's 
   2    1    1  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   a    d    h NA's 
   2    1    1  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
2.08 2.21  3.2  3.6 NA's 
   1    1    1    1  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   4  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  13   40 NA's 
   2    2  466 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 NA's 
   2    1    1  466 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118958778 118960230 118966126 126998455 128937543 153880085       464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2 NA's 
   4    2  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.16 0.17 0.18 NA's 
   3    2    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  25   31 NA's 
   5    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   7    9   10 NA's 
   1    1    4  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   5    6    9   10 NA's 
   1    3    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a18z a195 a238 a32m NA's 
   2    2    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   a    d    h NA's 
   4    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
1.07 1.14  1.7  1.8 NA's 
   3    1    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7   10 NA's 
   4    2  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 NA's 
   4    1    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.20   14.75   16.17   17.62   21.31   23.88     464 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   6  464 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
0.16 0.17 0.18 NA's 
   3    2    1  464 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               6 
                                            NA's 
                                             464 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                                4                                 2 
                             NA's 
                              464 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        6       464 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   2  468 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   12.50   14.00   17.83   24.50   31.00     464 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
   6  464 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00    6.00    6.50    8.75   11.00     464 

patient.samples.sample.3.portions.portion.portion_number:
   1   11 NA's 
   4    2  464 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
   6  464 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_barcode:
tcga-fw-a3r5-11a-01-ts1 tcga-gn-a4u8-11a-01-tsa                    NA's 
                      1                       1                     468 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_uuid:
1819e2f3-2a33-4dee-a218-efa64bdb8018 dac058b9-0bc2-45d5-b806-4684bd66af91 
                                   1                                    1 
                                NA's 
                                 468 

patient.samples.sample.3.portions.portion.slides.slide.image_file_name:
tcga-fw-a3r5-11a-01-ts1.1819e2f3-2a33-4dee-a218-efa64bdb8018.svs 
                                                               1 
tcga-gn-a4u8-11a-01-tsa.dac058b9-0bc2-45d5-b806-4684bd66af91.svs 
                                                               1 
                                                            NA's 
                                                             468 

patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   2  468 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   2  468 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   2  468 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   2  468 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   2  468 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
 100 NA's 
   2  468 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0 NA's 
   2  468 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   2  468 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   2  468 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
   2  468 

patient.samples.sample.3.portions.portion.weight:
  30 2000 NA's 
   2    2  466 

patient.samples.sample.3.portions.portion.year_of_creation:
2011 2012 NA's 
   4    2  464 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   4                    2                  464 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
   4    2  464 

patient.samples.sample.3.vial_number:
   a NA's 
   6  464 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      9     240    1381    2102    3177   11991      11 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10:
2be51074-7e23-41c0-a3ea-1a84bf812048 35f656a3-d0dd-4860-8348-3b7557821f2a 
                                   1                                    1 
92935375-c146-4e55-b27c-70017227b5a2                                 NA's 
                                   1                                  467 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.11:
70815334-6aee-45ec-a76f-132bd19f4b33 841b85f8-b1fd-4acc-8879-8fcc1f98ffc4 
                                   1                                    1 
c3bc9286-eb9e-4911-a310-357c133897e6                                 NA's 
                                   1                                  467 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.12:
6e09a9e9-82d9-4d29-9e7f-14bc9c5dc105                                 NA's 
                                   1                                  469 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.13:
7dd0ae3f-7305-4ca4-9a8d-118a59de2328                                 NA's 
                                   1                                  469 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.4:
15bb8b97-7227-4520-b257-fcef6e4c984b 16445359-ec88-45a1-821e-093dd8e1e62c 
                                   1                                    1 
7b2d0262-0d5a-4dfa-8454-cce3e2763cd7                                 NA's 
                                   1                                  467 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.5:
0865819a-d6ff-4715-9b37-3b2a3462319d 350f3a9c-29b0-42d6-808f-ec671d923b8d 
                                   1                                    1 
84ccda65-9dc1-4235-900c-f3b54a39a9d7                                 NA's 
                                   1                                  467 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
06f2fbb4-52c0-42c2-a497-3f65eeafe5be 7f36f443-cadb-4146-8b26-82e60dc88e55 
                                   1                                    1 
c13cb8ed-cd54-4a07-aac1-845d7a31cd8b                                 NA's 
                                   1                                  467 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
6c842fea-2df8-4467-984e-c317fe537fc3 7a4fb831-e03e-43b3-9cd9-8441682f146c 
                                   1                                    1 
8f583629-bb0d-46ad-97a7-42c7e871760e                                 NA's 
                                   1                                  467 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.8:
0e6095a7-836e-454f-bb2e-94bab7c7eb4f 16e6e720-aaab-4eb0-9196-ef25cbd71da9 
                                   1                                    1 
78d26246-c824-450a-b055-bdc9bbc38d2b                                 NA's 
                                   1                                  467 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
4dab37e0-79fb-4b8b-ba51-3441a2129f2c b7cc8dff-cb1a-4a9d-a9dc-b1723d90fe7b 
                                   1                                    1 
da2d6ddc-7312-47a8-8aea-e666684e8c69                                 NA's 
                                   1                                  467 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    90.0   160.0   246.3   320.0  1800.0       8 

patient.samples.sample.is_ffpe:
 no 
470 

patient.samples.sample.oct_embedded:
false  true 
   97   373 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.550   1.810   1.840   1.837   1.870   2.100 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
113385717 118955320 122849632 133823610 153878041 167788990 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  7  13 
144 326 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1100  0.1600  0.1700  0.1667  0.1800  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   25.00   25.00   24.11   26.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
470 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000  10.000   7.026  10.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.447   9.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   2.340   3.200   2.973   3.400   3.600 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
 23 
470 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
 13  20 
144 326 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
 218   69   98   85 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
164082148 164082173 164082197 164082197 164082220 164082234       453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
  13 NA's 
  17  453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.16 0.17 0.18 NA's 
   4    8    5  453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  29 NA's 
  17  453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  17  453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   1 NA's 
  17  453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   5    6    8    9 NA's 
   7    6    2    2  453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a360 NA's 
  17  453 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.080   2.210   3.960   3.477   4.190   4.190     453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
  17  453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
  13 23.3 NA's 
   5   12  453 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
  17  453 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
113386745 118954741 122850180 133824316 153878492 167812030 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  7  13 
326 144 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1100  0.1600  0.1700  0.1667  0.1800  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   25.00   25.00   24.06   26.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
 no 
470 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000  10.000   7.026  10.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.447   9.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.430   2.080   2.210   2.527   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
 23 
470 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
 13  20 
326 144 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
 218   69   98   85 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.26   12.24   18.21   20.61   26.63   64.21 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
470 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
  r 
470 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1100  0.1600  0.1700  0.1667  0.1800  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             470 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
 no 
470 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
nrna - melanoma protocol 
                     470 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.490   1.300   1.500   1.551   1.760   3.300 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.800   8.400   8.373   9.000   9.800 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
    470 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 386   66    2   16 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113390400 122847066 122847854 135522306 153889293 175154493        12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
 458   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 458   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00    8.00   18.00   16.24   18.00   29.00      12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 458   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   3.443   7.000  10.000      12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.002   7.000  12.000      12 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 458   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
 458   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 458   12 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
 272   84  102   12 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                 458                   12 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
 458   12 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 458   12 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
 458   12 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             458 
                                            NA's 
                                              12 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 458   12 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
    458      12 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 378   62    2   28 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
113388511 118966066 122850124 134364529 153879819 167823444 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   8 
147 323 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.0800  0.0800  0.1044  0.1500  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   25.00   25.00   23.84   26.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
470 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   9.000   6.745  10.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.949   7.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.140   3.200   2.498   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
470 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.7  40 
147 323 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
 218   69   86   97 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
113385697 118965330 122850230 134233215 153878295 167823357 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  5   8 
351 119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.0900  0.1500  0.1385  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   25.00   25.00   24.11   26.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 no 
470 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000  10.000   7.026  10.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.949   7.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.690   1.950   2.080   2.354   2.800   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
 23 
470 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
 13  40 
351 119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 
 218   69   98   85 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118953065 118958060 118959262 137203228 167779851 167780152       280 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8 NA's 
 119   71  280 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1500  0.1313  0.1600  0.1800     280 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  25   30 NA's 
 119   71  280 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 190  280 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   4   10 NA's 
  71  119  280 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   5.221   8.000  12.000     280 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.070   1.950   1.718   2.080   2.340     280 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 190  280 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  13 13.4 NA's 
 119   71  280 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2014 NA's 
 119   71  280 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167779779 167779813 167779856 167779904 167780016 167780063       397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8 NA's 
  73  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 0.08 0.09 NA's 
   8   50   15  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  30 NA's 
  73  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  73  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   4 NA's 
  73  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   6.849  10.000  11.000     397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a38q a38r NA's 
  43   30  397 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
0.94 1.07 1.21 NA's 
   8   50   15  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
  73  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4 NA's 
  73  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2014 NA's 
  73  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-da-a1i2-06a-21d-a46a-09 tcga-fs-a1zk-06a-11d-a46a-09 
                           1                            1 
                        NA's 
                         468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
538813e2-26d8-43bb-9695-8a73a360b15d 8a82b1e3-b674-4eef-87d7-3e006c9eb681 
                                   1                                    1 
                                NA's 
                                 468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
167811748 167811772      NA's 
        1         1       468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   9 NA's 
   2  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.08 0.09 NA's 
   1    1  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  21 NA's 
   2  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   2  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1 NA's 
   2  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   5 NA's 
   2  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a46a NA's 
   2  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   f    h NA's 
   1    1  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
1.07 1.21 NA's 
   1    1  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   2  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
13.4 NA's 
   2  468 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2015 NA's 
   2  468 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
118957484 118967666 122849356 134377143 153879916 167823453 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   8 
323 119  28 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1500  0.1600  0.1545  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   25.00   25.00   23.84   26.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
470 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   9.000   6.745  10.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.949   7.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.010   1.070   1.329   1.600   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
470 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7  10  40 
323 119  28 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
 218   69   86   97 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.16   32.95   42.94   44.95   57.31   83.73 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
470 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
470 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1591  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
470 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
470 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             470 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
470 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  470 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
      470 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 386   66    2   16 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    6.00   15.50   15.57   25.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
470 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    6.00    8.00    7.67   10.00   12.00 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   11.0    11.0    11.0    12.6    11.0    61.0 

patient.samples.sample.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    1.00    1.16    1.00    6.00      58 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
470 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   1.000   2.551   3.000  90.000 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.4872  0.7500 20.0000 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.855   1.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
  0   1   2   3 
420  39   6   5 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   2.545   0.000  65.000 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   2.500   8.311  15.000  65.000 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  30.00   80.00   90.00   87.29  100.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   80.00   85.00   84.43   90.00   99.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
top 
470 

patient.samples.sample.portions.portion.weight:
 10  20  30 
 14  30 426 

patient.samples.sample.portions.portion.year_of_creation:
2011 2012 2013 2014 
 247   79   94   50 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 352  118 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 352  118 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   21.66   21.00   51.00     118 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.059   2.000   5.000     164 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  14   24   30 NA's 
  42  148  162  118 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   1    6   11 NA's 
 162  148   42  118 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 2014 NA's 
 204  148  118 

patient.samples.sample.sample_type:
   metastatic primary tumor 
          366           104 

patient.samples.sample.sample_type_id:
  1   6 
104 366 

patient.samples.sample.tissue_type:
tumor  NA's 
    1   469 

patient.samples.sample.tumor_descriptor:
metastatic       NA's 
         1        469 

patient.samples.sample.vial_number:
  a   b 
468   2 

}}
\keyword{datasets}
