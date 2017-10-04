\name{ LGG }
\alias{ LGG }
\docType{data}
\title{ Brain Lower Grade Glioma }

\details{
\preformatted{
> experiments( LGG )
ExperimentList class object of length 11: 
 [1] CNASeq-20160128: RaggedExperiment with 6360 rows and 104 columns 
 [2] CNASNP-20160128: RaggedExperiment with 411918 rows and 1015 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 79791 rows and 1015 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 513 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 513 columns 
 [6] Methylation-20160128: SummarizedExperiment with 485577 rows and 530 columns 
 [7] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 526 columns 
 [8] mRNAArray-20160128: SummarizedExperiment with 17814 rows and 27 columns 
 [9] Mutation-20160128: RaggedExperiment with 9885 rows and 286 columns 
 [10] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 530 columns 
 [11] RPPAArray-20160128: SummarizedExperiment with 201 rows and 435 columns 

> rownames( LGG )
CharacterList of length 11
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 PNMA1 ... SLC39A6 SNRPD2 AQP7 CTSC
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
...
<1 more element>

> colnames( LGG )
CharacterList of length 11
[["CNASeq-20160128"]] TCGA-CS-4938-01B-11D-1891-02 ...
[["CNASNP-20160128"]] TCGA-CS-4938-01B-11D-1892-01 ...
[["CNVSNP-20160128"]] TCGA-CS-4938-01B-11D-1892-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-CS-4938-01B-11D-1892-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-CS-4938-01B-11D-1892-01 ...
[["Methylation-20160128"]] TCGA-CS-4938-01B-11D-1894-05 ...
[["miRNASeqGene-20160128"]] TCGA-CS-4938-01B-11R-1895-13 ...
[["mRNAArray-20160128"]] TCGA-CS-4942-01A-01R-1470-07 ...
[["Mutation-20160128"]] TCGA-CS-4938-01B-11D-1893-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-CS-4938-01B-11R-1896-07 ...
...
<1 more element>

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128  0.2 Mb
2                    CNASNP-20160128 11.3 Mb
3                    CNVSNP-20160128  2.4 Mb
4          GISTIC_AllByGene-20160128  4.4 Mb
5  GISTIC_ThresholdedByGene-20160128  4.4 Mb
6               Methylation-20160128 67.5 Mb
7              miRNASeqGene-20160128  0.1 Mb
8                 mRNAArray-20160128    1 Mb
9                  Mutation-20160128    4 Mb
10          RNASeq2GeneNorm-20160128  1.2 Mb
11                RPPAArray-20160128  0.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   391 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 125.00  125.00    2.23    1.87    2.83 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   32.00   41.00   42.93   53.00   86.00       2 

vital_status:
   0    1 NA's 
 389  126    1 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      7     438     814    1219    1547    5166     391 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -1.0   384.0   629.0   880.1  1147.0  6423.0     127 

tumor_tissue_site:
central nervous system                   NA's 
                   515                      1 

gender:
female   male   NA's 
   230    285      1 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2008    2011    2009    2012    2013       1 

radiation_therapy:
  no  yes NA's 
 186  296   34 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   80.00   90.00   86.64  100.00  100.00     209 

histological_type:
      astrocytoma  oligoastrocytoma oligodendroglioma              NA's 
              194               130               191                 1 

race:
american indian or alaska native                            asian 
                               1                                8 
       black or african american                            white 
                              21                              475 
                            NA's 
                              11 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    32                    449                     35 

admin.bcr:
nationwide children's hospital                           NA's 
                           515                              1 

admin.day_of_dcc_upload:
   1 NA's 
 515    1 

admin.disease_code:
 lgg NA's 
 515    1 


admin.month_of_dcc_upload:
  10 NA's 
 515    1 

admin.patient_withdrawal.withdrawn:
false  NA's 
  515     1 

admin.project_code:
tcga NA's 
 515    1 

admin.year_of_dcc_upload:
2015 NA's 
 515    1 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   32.00   41.00   42.94   53.00   87.00       1 

patient.animal_insect_allergy_history:
  no  yes NA's 
 287   15  214 


patient.asthma_history:
  no  yes NA's 
 345   21  150 


patient.clinical_cqcf.consent_or_death_status:
  consented    deceased exemption 4        NA's 
        435          72           8           1 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -120.0    -2.0     0.0   140.3    13.0  6419.0      88 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   23.0   425.8   902.5  1304.6  1936.2  4445.0     444 

patient.clinical_cqcf.frozen_specimen_anatomic_site:
brain; supratentorial       popliteal fossa           spinal cord 
                  454                     3                     1 
              unknown                  NA's 
                   57                     1 

patient.clinical_cqcf.histological_type:
      astrocytoma  oligoastrocytoma oligodendroglioma              NA's 
              193               132               190                 1 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             512 
yes, pharmaceutical treatment prior to resection 
                                               2 
               yes, radiation prior to resection 
                                               1 
                                            NA's 
                                               1 

patient.clinical_cqcf.history_of_prior_malignancy:
                                                     no 
                                                    499 
                       yes, history of prior malignancy 
                                                     15 
yes, history of synchronous and or bilateral malignancy 
                                                      1 
                                                   NA's 
                                                      1 

patient.clinical_cqcf.tumor_type:
primary    NA's 
    512       4 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   15.00   15.36   22.00   31.00       1 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -31776  -19610  -15048  -15863  -11970   -5267       2 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.0   352.8   801.0  1205.8  1793.0  4445.0     424 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 515    1 

patient.days_to_initial_score_performance_status_scale:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -122.0    -3.0     8.0   121.2    53.0  4445.0     234 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -1.0   121.0   302.0   607.8   774.2  6423.0      92 



patient.drugs.drug.10.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   15.25   17.50   19.67   24.25   31.00     510 

patient.drugs.drug.10.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    154    1271    2288    2230    3270    4124     510 

patient.drugs.drug.10.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     48    1198    2198    2107    2987    4081     510 


patient.drugs.drug.10.measure_of_response:
clinical progressive disease                         NA's 
                           2                          514 

patient.drugs.drug.10.month_of_form_completion:
   1    5    8 NA's 
   2    1    3  510 

patient.drugs.drug.10.number_cycles:
   2    4   14 NA's 
   2    1    1  512 

patient.drugs.drug.10.prescribed_dose:
 1.4   10   15 82.5 NA's 
   1    1    1    1  512 

patient.drugs.drug.10.prescribed_dose_units:
mg/kg mg/m2  NA's 
    2     2   512 

patient.drugs.drug.10.regimen_indication:
   adjuvant progression        NA's 
          1           3         512 

patient.drugs.drug.10.regimen_number:
   1    6    8   13 NA's 
   1    1    1    1  512 

patient.drugs.drug.10.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               3                1              512 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
   6  510 

patient.drugs.drug.10.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         3                          1 
targeted molecular therapy                       NA's 
                         2                        510 

patient.drugs.drug.10.total_dose:
    8   280  1172 10192  NA's 
    1     1     1     1   512 

patient.drugs.drug.10.total_dose_units:
  mg NA's 
   4  512 

patient.drugs.drug.10.tx_on_clinical_trial:
  no NA's 
   2  514 

patient.drugs.drug.10.year_of_form_completion:
2011 2014 NA's 
   4    2  510 

patient.drugs.drug.11.bcr_drug_barcode:
tcga-du-6404-d14804  tcga-e1-5302-d7577  tcga-e1-5318-d7622 tcga-e1-a7z4-d61759 
                  1                   1                   1                   1 
               NA's 
                512 

patient.drugs.drug.11.bcr_drug_uuid:
4784091c-7545-4001-bb2f-b2d9920e698b 74468cb9-fcf4-4b97-a28f-164a5d94f934 
                                   1                                    1 
7964078b-3059-4440-8855-5ada2090cfe0 9a3dd9aa-87ee-4227-af43-6166975ee44b 
                                   1                                    1 
                                NA's 
                                 512 

patient.drugs.drug.11.day_of_form_completion:
  16   19   21   26 NA's 
   1    1    1    1  512 

patient.drugs.drug.11.days_to_drug_therapy_end:
 294 1026 4066 4137 NA's 
   1    1    1    1  512 

patient.drugs.drug.11.days_to_drug_therapy_start:
 202  824 4066 4081 NA's 
   1    1    1    1  512 

patient.drugs.drug.11.drug_name:
 bevacizumab procarbazine      temodar temozolomide         NA's 
           1            1            1            1          512 

patient.drugs.drug.11.measure_of_response:
clinical progressive disease                         NA's 
                           1                          515 

patient.drugs.drug.11.month_of_form_completion:
   1    6    8 NA's 
   2    1    1  512 

patient.drugs.drug.11.number_cycles:
   1    2    7 NA's 
   1    1    1  513 

patient.drugs.drug.11.prescribed_dose:
  10   75  150 NA's 
   1    1    1  513 

patient.drugs.drug.11.prescribed_dose_units:
mg/kg mg/m2  NA's 
    1     2   513 

patient.drugs.drug.11.regimen_indication:
   adjuvant progression        NA's 
          2           1         513 

patient.drugs.drug.11.regimen_number:
   3   10 NA's 
   2    1  513 

patient.drugs.drug.11.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                2              513 

patient.drugs.drug.11.therapy_ongoing:
  no NA's 
   4  512 

patient.drugs.drug.11.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         3                          1 
                      NA's 
                       512 

patient.drugs.drug.11.total_dose:
 635 7840 8400 NA's 
   1    1    1  513 

patient.drugs.drug.11.total_dose_units:
  mg NA's 
   3  513 

patient.drugs.drug.11.tx_on_clinical_trial:
  no NA's 
   1  515 

patient.drugs.drug.11.year_of_form_completion:
2011 2014 NA's 
   3    1  512 

patient.drugs.drug.12.bcr_drug_barcode:
 tcga-e1-5302-d7584  tcga-e1-5318-d7629 tcga-e1-a7z4-d61761                NA's 
                  1                   1                   1                 513 

patient.drugs.drug.12.bcr_drug_uuid:
0b29e6cd-3baa-44d6-8169-0fdb64b67e0e 98e606fe-d8e2-423e-a0ef-c9f8d553ed9c 
                                   1                                    1 
c5b80291-66ea-4e01-815c-852f2e3ca534                                 NA's 
                                   1                                  513 

patient.drugs.drug.12.day_of_form_completion:
  16   19   26 NA's 
   1    1    1  513 

patient.drugs.drug.12.days_to_drug_therapy_end:
1434 1904 4336 NA's 
   1    1    1  513 

patient.drugs.drug.12.days_to_drug_therapy_start:
1322 1655 4138 NA's 
   1    1    1  513 

patient.drugs.drug.12.drug_name:
avastin    ccnu  cpt-11    NA's 
      1       1       1     513 

patient.drugs.drug.12.measure_of_response:
clinical progressive disease                         NA's 
                           1                          515 

patient.drugs.drug.12.month_of_form_completion:
   1    8 NA's 
   2    1  513 

patient.drugs.drug.12.number_cycles:
   3 NA's 
   2  514 

patient.drugs.drug.12.prescribed_dose:
 110  340 NA's 
   1    1  514 

patient.drugs.drug.12.prescribed_dose_units:
mg/m2  NA's 
    2   514 

patient.drugs.drug.12.regimen_indication:
progression        NA's 
          2         514 

patient.drugs.drug.12.regimen_number:
   7 NA's 
   2  514 

patient.drugs.drug.12.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                1              514 

patient.drugs.drug.12.therapy_ongoing:
  no NA's 
   3  513 

patient.drugs.drug.12.therapy_types.therapy_type:
 chemotherapy immunotherapy          NA's 
            2             1           513 

patient.drugs.drug.12.total_dose:
 540 5670 NA's 
   1    1  514 

patient.drugs.drug.12.total_dose_units:
  mg NA's 
   2  514 

patient.drugs.drug.12.tx_on_clinical_trial:
  no NA's 
   1  515 

patient.drugs.drug.12.year_of_form_completion:
2011 2014 NA's 
   2    1  513 

patient.drugs.drug.13.bcr_drug_barcode:
 tcga-e1-5302-d7578  tcga-e1-5318-d7636 tcga-e1-a7z4-d61762                NA's 
                  1                   1                   1                 513 

patient.drugs.drug.13.bcr_drug_uuid:
5cd187c5-7576-451c-b84d-341b29cc2732 cd15de85-acfa-4ddb-b91d-ce601ea1fec3 
                                   1                                    1 
feb703c5-9c8f-4b8f-8353-1131d01355ee                                 NA's 
                                   1                                  513 

patient.drugs.drug.13.day_of_form_completion:
  16   19   26 NA's 
   1    1    1  513 

patient.drugs.drug.13.days_to_drug_therapy_end:
 294 2211 4308 NA's 
   1    1    1  513 

patient.drugs.drug.13.days_to_drug_therapy_start:
 202 2127 4138 NA's 
   1    1    1  513 

patient.drugs.drug.13.drug_name:
carboplatin      cpt-11 vincristine        NA's 
          1           1           1         513 

patient.drugs.drug.13.measure_of_response:
clinical progressive disease                         NA's 
                           1                          515 

patient.drugs.drug.13.month_of_form_completion:
   1    8 NA's 
   2    1  513 

patient.drugs.drug.13.number_cycles:
   2 NA's 
   2  514 

patient.drugs.drug.13.prescribed_dose:
 1.4  260 NA's 
   1    1  514 

patient.drugs.drug.13.prescribed_dose_units:
mg/m2  NA's 
    2   514 

patient.drugs.drug.13.regimen_indication:
adjuvant     NA's 
       2      514 

patient.drugs.drug.13.regimen_number:
   3   11 NA's 
   1    1  514 

patient.drugs.drug.13.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              514 

patient.drugs.drug.13.therapy_ongoing:
  no NA's 
   3  513 

patient.drugs.drug.13.therapy_types.therapy_type:
chemotherapy         NA's 
           3          513 

patient.drugs.drug.13.total_dose:
   8 3400 NA's 
   1    1  514 

patient.drugs.drug.13.total_dose_units:
  mg NA's 
   2  514 

patient.drugs.drug.13.tx_on_clinical_trial:
  no NA's 
   1  515 

patient.drugs.drug.13.year_of_form_completion:
2011 2014 NA's 
   2    1  513 

patient.drugs.drug.14.bcr_drug_barcode:
tcga-e1-5318-d7635               NA's 
                 1                515 

patient.drugs.drug.14.bcr_drug_uuid:
40ce51f2-557a-411e-b618-642259b5929d                                 NA's 
                                   1                                  515 

patient.drugs.drug.14.day_of_form_completion:
  26 NA's 
   1  515 

patient.drugs.drug.14.days_to_drug_therapy_end:
2127 NA's 
   1  515 

patient.drugs.drug.14.days_to_drug_therapy_start:
2023 NA's 
   1  515 

patient.drugs.drug.14.drug_name:
cpt-11   NA's 
     1    515 

patient.drugs.drug.14.month_of_form_completion:
   1 NA's 
   1  515 

patient.drugs.drug.14.number_cycles:
   1 NA's 
   1  515 

patient.drugs.drug.14.prescribed_dose:
 350 NA's 
   1  515 

patient.drugs.drug.14.prescribed_dose_units:
mg/m2  NA's 
    1   515 

patient.drugs.drug.14.regimen_indication:
progression        NA's 
          1         515 

patient.drugs.drug.14.regimen_number:
  10 NA's 
   1  515 

patient.drugs.drug.14.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              515 

patient.drugs.drug.14.therapy_ongoing:
  no NA's 
   1  515 

patient.drugs.drug.14.therapy_types.therapy_type:
chemotherapy         NA's 
           1          515 

patient.drugs.drug.14.total_dose:
1710 NA's 
   1  515 

patient.drugs.drug.14.total_dose_units:
  mg NA's 
   1  515 

patient.drugs.drug.14.year_of_form_completion:
2011 NA's 
   1  515 



patient.drugs.drug.2.clinical_trail_drug_classification:
              chemotherapy targeted molecular therapy 
                         1                          1 
                      NA's 
                       514 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    14.0    19.0    18.1    23.0    31.0     363 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -30.0   128.5   288.0   583.7   658.5  4543.0     396 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -49.0    78.5   123.5   421.2   468.5  4500.0     364 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                          22                            5 
            partial response               stable disease 
                           4                           29 
                        NA's 
                         456 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.682   9.000  12.000     362 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.731   6.000  17.000     449 

patient.drugs.drug.2.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    75.0   125.0   152.5   193.8   560.0     454 


patient.drugs.drug.2.regimen_indication:
               adjuvant other: specify in notes             progression 
                     54                       1                      17 
                   NA's 
                    444 

patient.drugs.drug.2.regimen_indication_notes:
neo-adjuvant         NA's 
           1          515 

patient.drugs.drug.2.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.861   2.000   8.000     444 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
    intratumoral intravenous (iv)             oral             NA's 
               1               10               61              444 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
 125   27  364 


patient.drugs.drug.2.therapy_types.therapy_type_notes:
brachytherpy         NA's 
           1          515 

patient.drugs.drug.2.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   249.8  1060.0  4728.8  5880.0 94920.0     460 


patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  74    6  436 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2014    2015     362 



patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    12.0    19.0    17.7    24.0    31.0     423 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   59.0   280.2   505.5   866.0  1204.2  3868.0     438 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   129.0   328.0   727.7   999.0  3282.0     423 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                          20                            1 
            partial response               stable disease 
                           4                           15 
                        NA's 
                         476 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.538   9.000  12.000     423 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   5.108   7.000  19.000     479 

patient.drugs.drug.3.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    75.0   150.0   262.8   330.0  1611.0     483 

patient.drugs.drug.3.prescribed_dose_units:
    mg mg/day  mg/kg  mg/m2   NA's 
     8      7      5     18    478 

patient.drugs.drug.3.regimen_indication:
   adjuvant progression  recurrence        NA's 
         19          23           1         473 

patient.drugs.drug.3.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.674   3.000  12.000     473 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              10               32              474 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  81   10  425 

patient.drugs.drug.3.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                        74                          5 
            other, specify targeted molecular therapy 
                         2                         10 
                      NA's 
                       425 

patient.drugs.drug.3.therapy_types.therapy_type_notes:
nsaid  NA's 
    2   514 

patient.drugs.drug.3.total_dose:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   140.0    787.5   1555.5  26955.2   9371.2 425600.0      488 

patient.drugs.drug.3.total_dose_units:
   mg mg/m2  NA's 
   33     1   482 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  46    4  466 

patient.drugs.drug.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2013    2014    2015     423 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   12.00   17.00   17.06   22.00   31.00     467 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     59     501     752    1154    1234    4152     473 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   28.0   345.5   665.0  1013.5  1104.5  3996.0     468 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease             partial response 
                          22                            1 
              stable disease                         NA's 
                           4                          489 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   6.959   8.000  12.000     467 

patient.drugs.drug.4.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   7.118   6.000  41.000     499 

patient.drugs.drug.4.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0    75.0   150.0   262.9   450.0   800.0     501 

patient.drugs.drug.4.prescribed_dose_units:
{wafer}      mg   mg/kg   mg/m2    NA's 
      1       4       1      11     499 

patient.drugs.drug.4.regimen_indication:
   adjuvant progression        NA's 
          7          13         496 

patient.drugs.drug.4.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.75    3.00    3.20    4.00    8.00     496 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
    intratumoral intravenous (iv)             oral             NA's 
               1                6               12              497 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
  46    2  468 

patient.drugs.drug.4.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                        35                          3 
            other, specify targeted molecular therapy 
                         3                          6 
                      NA's 
                       469 

patient.drugs.drug.4.therapy_types.therapy_type_notes:
  antiongigenic  antiongiogenic radiosensitizer            NA's 
              1               1               1             513 

patient.drugs.drug.4.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   258.2   915.5 17908.9  7805.0 95200.0     502 

patient.drugs.drug.4.total_dose_units:
{wafer}      mg    NA's 
      1      15     500 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
  24    5  487 

patient.drugs.drug.4.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2013    2014    2015     467 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   14.00   15.00   16.88   22.00   31.00     483 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     89     573     781    1157    1347    4207     485 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   82.0   409.8   700.5  1053.2  1263.8  4152.0     484 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease               stable disease 
                          16                            3 
                        NA's 
                         497 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   6.364   8.000  12.000     483 

patient.drugs.drug.5.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   4.636   9.000  12.000     505 

patient.drugs.drug.5.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.2    75.0   110.0   149.2   177.5   560.0     506 

patient.drugs.drug.5.prescribed_dose_units:
   mg mg/m2  NA's 
    1    10   505 

patient.drugs.drug.5.regimen_indication:
   adjuvant progression        NA's 
          3           9         504 

patient.drugs.drug.5.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.417   4.250   8.000     504 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               3                9              504 

patient.drugs.drug.5.route_of_administrations.route_of_administration.2:
oral NA's 
   1  515 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
  32    1  483 

patient.drugs.drug.5.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         1                         28 
             immunotherapy targeted molecular therapy 
                         3                          1 
                      NA's 
                       483 

patient.drugs.drug.5.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     11     165     305    3693    2880   25200     506 

patient.drugs.drug.5.total_dose_units:
  mg NA's 
  11  505 

patient.drugs.drug.5.tx_on_clinical_trial:
  no  yes NA's 
  18    3  495 

patient.drugs.drug.5.year_of_form_completion:
2011 2013 2014 2015 NA's 
  12    1   13    7  483 



patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   15.00   15.50   18.05   23.50   31.00     494 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    160     593    1062    1473    2043    4418     495 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  125.0   393.5   891.0  1345.5  1877.5  4289.0     494 


patient.drugs.drug.6.measure_of_response:
clinical progressive disease               stable disease 
                           9                            2 
                        NA's 
                         505 

patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   8.000   6.227   8.000  12.000     494 

patient.drugs.drug.6.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   4.111   4.000  12.000     507 

patient.drugs.drug.6.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  100.0   125.0   150.0   173.9   200.0   400.0     507 

patient.drugs.drug.6.prescribed_dose_units:
   mg mg/m2  NA's 
    1     8   507 

patient.drugs.drug.6.regimen_indication:
   adjuvant progression        NA's 
          2           7         507 

patient.drugs.drug.6.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.111   6.000   9.000     507 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                8              507 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
  21    1  494 

patient.drugs.drug.6.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                        17                          3 
            other, specify targeted molecular therapy 
                         1                          1 
                      NA's 
                       494 

patient.drugs.drug.6.therapy_types.therapy_type_notes:
nsaids   NA's 
     1    515 

patient.drugs.drug.6.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    140    1620    3000    9602    8250   44800     508 

patient.drugs.drug.6.total_dose_units:
  mg NA's 
   8  508 

patient.drugs.drug.6.tx_on_clinical_trial:
  no  yes NA's 
  11    2  503 

patient.drugs.drug.6.year_of_form_completion:
2011 2013 2014 2015 NA's 
   9    1    9    3  494 



patient.drugs.drug.7.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   15.00   19.00   19.41   24.00   31.00     499 

patient.drugs.drug.7.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    285     848    1276    1480    2006    2799     500 

patient.drugs.drug.7.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    260     846    1266    1341    1861    2615     499 


patient.drugs.drug.7.measure_of_response:
clinical progressive disease               stable disease 
                           4                            3 
                        NA's 
                         509 

patient.drugs.drug.7.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   5.824   8.000   9.000     499 

patient.drugs.drug.7.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.556   6.000  12.000     507 

patient.drugs.drug.7.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    87.5   162.5   135.6   200.0   200.0     508 

patient.drugs.drug.7.prescribed_dose_units:
   mg mg/kg mg/m2  NA's 
    2     1     6   507 

patient.drugs.drug.7.regimen_indication:
   adjuvant progression        NA's 
          3           6         507 

patient.drugs.drug.7.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.444   8.000   9.000     507 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               2                7              507 

patient.drugs.drug.7.therapy_ongoing:
  no  yes NA's 
  16    1  499 

patient.drugs.drug.7.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                        10                          2 
targeted molecular therapy                       NA's 
                         4                        500 

patient.drugs.drug.7.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    134    1437    5950    6342   11325   13200     508 

patient.drugs.drug.7.total_dose_units:
  mg NA's 
   9  507 

patient.drugs.drug.7.tx_on_clinical_trial:
  no  yes NA's 
   6    2  508 

patient.drugs.drug.7.year_of_form_completion:
2011 2014 2015 NA's 
   9    6    2  499 



patient.drugs.drug.8.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   14.50   19.00   19.27   24.00   31.00     501 

patient.drugs.drug.8.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    260     837    1329    1635    2258    3749     501 

patient.drugs.drug.8.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  229.0   773.5  1266.0  1508.9  2230.5  3282.0     501 


patient.drugs.drug.8.measure_of_response:
clinical progressive disease               stable disease 
                           4                            2 
                        NA's 
                         510 

patient.drugs.drug.8.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   5.933   8.000   9.000     501 

patient.drugs.drug.8.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.778   5.000  15.000     507 

patient.drugs.drug.8.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   72.50   91.25   96.56  112.50  200.00     508 

patient.drugs.drug.8.prescribed_dose_units:
    mg mg/day  mg/kg  mg/m2   NA's 
     3      1      1      4    507 

patient.drugs.drug.8.regimen_indication:
   adjuvant progression        NA's 
          2           7         507 

patient.drugs.drug.8.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   6.000   6.444   7.000  12.000     507 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                8              507 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
  15  501 

patient.drugs.drug.8.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                         9                          4 
targeted molecular therapy                       NA's 
                         1                        502 

patient.drugs.drug.8.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    180    1250    3500   13851   14000   84000     507 

patient.drugs.drug.8.total_dose_units:
  mg NA's 
   9  507 

patient.drugs.drug.8.tx_on_clinical_trial:
  no  yes NA's 
   5    1  510 

patient.drugs.drug.8.year_of_form_completion:
2011 2014 NA's 
   9    6  501 



patient.drugs.drug.9.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   14.00   15.50   18.10   22.75   31.00     506 

patient.drugs.drug.9.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    824    1037    1674    2010    2674    3888     506 

patient.drugs.drug.9.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  781.0   979.8  1472.0  1814.5  2472.2  3545.0     506 


patient.drugs.drug.9.measure_of_response:
clinical progressive disease               stable disease 
                           3                            1 
                        NA's 
                         512 

patient.drugs.drug.9.month_of_form_completion:
   1    5    8 NA's 
   2    1    7  506 

patient.drugs.drug.9.number_cycles:
   1    4    6   10 NA's 
   2    2    1    1  510 

patient.drugs.drug.9.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   46.25  142.50  207.50  193.75  730.00     510 

patient.drugs.drug.9.prescribed_dose_units:
  auc    mg mg/m2  NA's 
    1     1     4   510 

patient.drugs.drug.9.regimen_indication:
   adjuvant progression        NA's 
          2           4         510 

patient.drugs.drug.9.regimen_number:
   2    6    8    9 NA's 
   1    2    1    2  510 

patient.drugs.drug.9.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                5              510 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
  10  506 

patient.drugs.drug.9.therapy_types.therapy_type:
 chemotherapy immunotherapy          NA's 
            8             2           506 

patient.drugs.drug.9.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    200    1065    3825   16130    6225   81760     510 

patient.drugs.drug.9.total_dose_units:
  mg NA's 
   6  510 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   4  512 

patient.drugs.drug.9.year_of_form_completion:
2011 2014 NA's 
   6    4  506 



patient.drugs.drug.clinical_trail_drug_classification:
              chemotherapy                      e3fo5 
                         1                          1 
targeted molecular therapy                       NA's 
                         1                        513 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.25   18.00   16.66   22.00   31.00     234 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-227.00   84.75  248.00  465.96  443.00 4901.00     286 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -325.0    28.0    57.0   302.7   209.8  4536.0     236 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          28                           14 
            partial response               stable disease 
                           9                           84 
                        NA's 
                         381 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   6.947  10.000  12.000     231 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   4.402   6.000  13.000     419 



patient.drugs.drug.regimen_indication:
               adjuvant other: specify in notes             progression 
                     79                       2                      26 
             recurrence                    NA's 
                      2                     407 

patient.drugs.drug.regimen_indication_notes:
  neo-adjuvant post-operative           NA's 
             1              1            514 


patient.drugs.drug.route_of_administrations.route_of_administration:
    intratumoral intravenous (iv)             oral             NA's 
               2               11               97              406 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 238   44  234 


patient.drugs.drug.therapy_types.therapy_type_notes:
retinoid     NA's 
       1      515 


patient.drugs.drug.total_dose_units:
{wafer}      mg   mg/m2    NA's 
      1      94       5     416 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
 160   12  344 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2014    2015     231 

patient.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.5000  0.5667  1.0000  4.0000     366 

patient.eczema_history:
  no  yes NA's 
 344   12  160 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    32                    449                     35 

patient.family_history_of_cancer:
  no  yes NA's 
 210  132  174 

patient.family_history_of_primary_brain_tumor:
  no  yes NA's 
 333   14  169 

patient.first_diagnosis_age_asth_ecz_hay_fev_mold_dust:
   < 12 years    > 20 years 12 - 20 years          NA's 
           14            18             3           481 

patient.first_diagnosis_age_of_animal_insect_allergy:
   < 12 years    > 20 years 12 - 20 years          NA's 
            2             6             2           506 

patient.first_diagnosis_age_of_food_allergy:
   > 20 years 12 - 20 years          NA's 
            3             3           510 


patient.first_presenting_symptom_longest_duration:
   > 181 days   0 - 30 days  31 - 90 days 91 - 180 days          NA's 
          111           219            77            37            72 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
  no  yes NA's 
  17   14  485 

patient.follow_ups.follow_up.2.additional_radiation_therapy:
  no  yes NA's 
  28    3  485 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure:
  no  yes NA's 
  15    7  494 

patient.follow_ups.follow_up.2.additional_surgery_metastatic_procedure:
  no  yes NA's 
  13    1  502 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   14.00   20.00   19.44   26.00   31.00     404 

patient.follow_ups.follow_up.2.days_to_additional_surgery_locoregional_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    286     541     826    1346    2070    3091     509 

patient.follow_ups.follow_up.2.days_to_additional_surgery_metastatic_procedure:
1191 NA's 
   1  515 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  492.0   784.8  1113.0  1579.6  1565.0  4695.0     494 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   634.5  1042.5  1142.5  1400.0  3574.0     430 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  218.0   480.5   739.5  1027.7  1111.0  3071.0     488 

patient.follow_ups.follow_up.2.days_to_performance_status_follow_up_score:
 235  405 1099 NA's 
   1    1    1  513 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   1 NA's 
   1  515 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                            19                             79 
                          NA's 
                           418 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response  partial remission/response 
                         14                           4 
        progressive disease              stable disease 
                         26                          41 
                       NA's 
                        431 

patient.follow_ups.follow_up.2.karnofsky_performance_score:
  50   80 NA's 
   1    1  514 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  91   15  410 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.455   9.000  12.000     404 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  58   28  430 

patient.follow_ups.follow_up.2.performance_status_scale_timing:
     adjuvant therapy post-adjuvant therapy                  NA's 
                    1                     2                   513 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        18         71        427 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         23                          18 
        progressive disease              stable disease 
                         24                          32 
                       NA's 
                        419 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  37   58  421 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
  36   59  421 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   85    23   408 

patient.follow_ups.follow_up.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   8   15   38   51  404 

patient.follow_ups.follow_up.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    9  506 

patient.follow_ups.follow_up.3.additional_radiation_therapy:
  no  yes NA's 
   9    2  505 

patient.follow_ups.follow_up.3.additional_surgery_locoregional_procedure:
  no NA's 
   9  507 

patient.follow_ups.follow_up.3.additional_surgery_metastatic_procedure:
  no NA's 
   7  509 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   15.00   21.00   20.28   24.00   31.00     491 

patient.follow_ups.follow_up.3.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    886     987    1209    2121    2835    4445     507 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     62     704    1217    1176    1472    3236     501 

patient.follow_ups.follow_up.3.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  218.0   785.0   918.5  1218.4  1394.8  2789.0     504 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             9                             16 
                          NA's 
                           491 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response  partial remission/response 
                          1                           1 
        progressive disease              stable disease 
                         11                           7 
                       NA's 
                        496 

patient.follow_ups.follow_up.3.lost_follow_up:
  no  yes NA's 
  20    5  491 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.00    5.00    5.56    8.00   12.00     491 

patient.follow_ups.follow_up.3.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  10   12  494 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         2         22        492 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                          2                           4 
        progressive disease              stable disease 
                         11                           8 
                       NA's 
                        491 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
   8   17  491 

patient.follow_ups.follow_up.3.targeted_molecular_therapy:
  no  yes NA's 
   8   17  491 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
   15     9   492 

patient.follow_ups.follow_up.3.year_of_form_completion:
2013 2014 2015 NA's 
   5   10   10  491 

patient.follow_ups.follow_up.4.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    7  508 

patient.follow_ups.follow_up.4.additional_radiation_therapy:
  no  yes NA's 
   7    1  508 

patient.follow_ups.follow_up.4.additional_surgery_locoregional_procedure:
  no  yes NA's 
   6    1  509 

patient.follow_ups.follow_up.4.additional_surgery_metastatic_procedure:
  no NA's 
   5  511 



patient.follow_ups.follow_up.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   14.25   15.00   17.36   24.75   31.00     502 

patient.follow_ups.follow_up.4.days_to_additional_surgery_locoregional_procedure:
 984 NA's 
   1  515 

patient.follow_ups.follow_up.4.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  819.0   911.2  1911.0  2397.3  4017.8  4445.0     510 

patient.follow_ups.follow_up.4.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    623    1285    1356    1935    2231    4113     508 

patient.follow_ups.follow_up.4.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  783.0   906.8  1200.5  1433.8  1424.2  3500.0     508 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             7                              7 
                          NA's 
                           502 

patient.follow_ups.follow_up.4.followup_treatment_success:
progressive disease      stable disease                NA's 
                  6                   4                 506 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
  14  502 

patient.follow_ups.follow_up.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   8.000   7.143   8.000  12.000     502 

patient.follow_ups.follow_up.4.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   3    8  505 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         1         12        503 

patient.follow_ups.follow_up.4.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                          2                           2 
        progressive disease              stable disease 
                          5                           5 
                       NA's 
                        502 

patient.follow_ups.follow_up.4.radiation_therapy:
  no  yes NA's 
   6    8  502 

patient.follow_ups.follow_up.4.targeted_molecular_therapy:
  no  yes NA's 
   5    9  502 

patient.follow_ups.follow_up.4.vital_status:
alive  dead  NA's 
    8     6   502 

patient.follow_ups.follow_up.4.year_of_form_completion:
2013 2014 2015 NA's 
   1    8    5  502 

patient.follow_ups.follow_up.5.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    3  511 

patient.follow_ups.follow_up.5.additional_radiation_therapy:
  no NA's 
   5  511 

patient.follow_ups.follow_up.5.additional_surgery_locoregional_procedure:
  no NA's 
   5  511 

patient.follow_ups.follow_up.5.additional_surgery_metastatic_procedure:
  no NA's 
   4  512 



patient.follow_ups.follow_up.5.day_of_form_completion:
   4   14   15   18 NA's 
   1    1    2    1  511 

patient.follow_ups.follow_up.5.days_to_death:
 886  987 4412 4445 NA's 
   1    1    1    1  512 

patient.follow_ups.follow_up.5.days_to_last_followup:
1329 NA's 
   1  515 

patient.follow_ups.follow_up.5.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    846     948    1195    2123    3485    4143     511 

patient.follow_ups.follow_up.5.followup_case_report_form_submission_reason:
additional new tumor event                       NA's 
                         5                        511 

patient.follow_ups.follow_up.5.followup_treatment_success:
progressive disease                NA's 
                  5                 511 

patient.follow_ups.follow_up.5.lost_follow_up:
  no NA's 
   5  511 

patient.follow_ups.follow_up.5.month_of_form_completion:
   8   12 NA's 
   4    1  511 

patient.follow_ups.follow_up.5.new_tumor_event_after_initial_treatment:
 yes NA's 
   5  511 

patient.follow_ups.follow_up.5.person_neoplasm_cancer_status:
with tumor       NA's 
         5        511 

patient.follow_ups.follow_up.5.primary_therapy_outcome_success:
progressive disease      stable disease                NA's 
                  3                   2                 511 

patient.follow_ups.follow_up.5.radiation_therapy:
  no  yes NA's 
   1    4  511 

patient.follow_ups.follow_up.5.targeted_molecular_therapy:
  no  yes NA's 
   1    4  511 

patient.follow_ups.follow_up.5.vital_status:
alive  dead  NA's 
    1     4   511 

patient.follow_ups.follow_up.5.year_of_form_completion:
2013 2014 NA's 
   1    4  511 

patient.follow_ups.follow_up.6.additional_pharmaceutical_therapy:
 yes NA's 
   3  513 

patient.follow_ups.follow_up.6.additional_radiation_therapy:
  no NA's 
   3  513 

patient.follow_ups.follow_up.6.additional_surgery_locoregional_procedure:
  no NA's 
   3  513 

patient.follow_ups.follow_up.6.additional_surgery_metastatic_procedure:
  no NA's 
   2  514 

patient.follow_ups.follow_up.6.bcr_followup_barcode:
tcga-e1-a7yy-f61637 tcga-e1-a7z4-f61757 tcga-fg-a711-f65436                NA's 
                  1                   1                   1                 513 

patient.follow_ups.follow_up.6.bcr_followup_uuid:
3504aefd-39ba-4f92-aed7-227c3617d0a9 f3e1f0e6-5629-44e0-b482-993e9174a344 
                                   1                                    1 
fef02454-9aa5-4331-9677-685208daeb8d                                 NA's 
                                   1                                  513 

patient.follow_ups.follow_up.6.day_of_form_completion:
  15   18   23 NA's 
   1    1    1  513 

patient.follow_ups.follow_up.6.days_to_death:
1481 4412 4445 NA's 
   1    1    1  513 

patient.follow_ups.follow_up.6.days_to_new_tumor_event_after_initial_treatment:
1277 4047 4266 NA's 
   1    1    1  513 

patient.follow_ups.follow_up.6.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                              1 
                          NA's 
                           513 

patient.follow_ups.follow_up.6.followup_treatment_success:
progressive disease                NA's 
                  3                 513 

patient.follow_ups.follow_up.6.lost_follow_up:
  no NA's 
   3  513 

patient.follow_ups.follow_up.6.month_of_form_completion:
   8    9 NA's 
   2    1  513 

patient.follow_ups.follow_up.6.new_tumor_event_after_initial_treatment:
 yes NA's 
   3  513 

patient.follow_ups.follow_up.6.person_neoplasm_cancer_status:
with tumor       NA's 
         3        513 

patient.follow_ups.follow_up.6.primary_therapy_outcome_success:
progressive disease      stable disease                NA's 
                  2                   1                 513 

patient.follow_ups.follow_up.6.radiation_therapy:
  no  yes NA's 
   1    2  513 

patient.follow_ups.follow_up.6.targeted_molecular_therapy:
  no  yes NA's 
   1    2  513 

patient.follow_ups.follow_up.6.vital_status:
dead NA's 
   3  513 

patient.follow_ups.follow_up.6.year_of_form_completion:
2014 NA's 
   3  513 

patient.follow_ups.follow_up.7.additional_pharmaceutical_therapy:
 yes NA's 
   1  515 

patient.follow_ups.follow_up.7.additional_radiation_therapy:
 yes NA's 
   1  515 

patient.follow_ups.follow_up.7.additional_surgery_locoregional_procedure:
  no NA's 
   1  515 

patient.follow_ups.follow_up.7.additional_surgery_metastatic_procedure:
  no NA's 
   1  515 

patient.follow_ups.follow_up.7.bcr_followup_barcode:
tcga-e1-a7z4-f61760                NA's 
                  1                 515 

patient.follow_ups.follow_up.7.bcr_followup_uuid:
b13971d1-089b-4932-8927-3841c5c6d801                                 NA's 
                                   1                                  515 

patient.follow_ups.follow_up.7.day_of_form_completion:
  18 NA's 
   1  515 

patient.follow_ups.follow_up.7.days_to_death:
4412 NA's 
   1  515 

patient.follow_ups.follow_up.7.days_to_new_tumor_event_after_initial_treatment:
4138 NA's 
   1  515 

patient.follow_ups.follow_up.7.followup_case_report_form_submission_reason:
additional new tumor event                       NA's 
                         1                        515 

patient.follow_ups.follow_up.7.followup_treatment_success:
progressive disease                NA's 
                  1                 515 

patient.follow_ups.follow_up.7.lost_follow_up:
  no NA's 
   1  515 

patient.follow_ups.follow_up.7.month_of_form_completion:
   8 NA's 
   1  515 

patient.follow_ups.follow_up.7.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  515 

patient.follow_ups.follow_up.7.person_neoplasm_cancer_status:
with tumor       NA's 
         1        515 

patient.follow_ups.follow_up.7.primary_therapy_outcome_success:
progressive disease                NA's 
                  1                 515 

patient.follow_ups.follow_up.7.radiation_therapy:
 yes NA's 
   1  515 

patient.follow_ups.follow_up.7.targeted_molecular_therapy:
 yes NA's 
   1  515 

patient.follow_ups.follow_up.7.vital_status:
dead NA's 
   1  515 

patient.follow_ups.follow_up.7.year_of_form_completion:
2014 NA's 
   1  515 

patient.follow_ups.follow_up.8.additional_pharmaceutical_therapy:
  no NA's 
   1  515 

patient.follow_ups.follow_up.8.additional_radiation_therapy:
  no NA's 
   1  515 

patient.follow_ups.follow_up.8.additional_surgery_locoregional_procedure:
  no NA's 
   1  515 

patient.follow_ups.follow_up.8.additional_surgery_metastatic_procedure:
  no NA's 
   1  515 

patient.follow_ups.follow_up.8.bcr_followup_barcode:
tcga-e1-a7z4-f61765                NA's 
                  1                 515 

patient.follow_ups.follow_up.8.bcr_followup_uuid:
e2e2d852-98d1-4de3-b9c2-6c7e3c958a95                                 NA's 
                                   1                                  515 

patient.follow_ups.follow_up.8.day_of_form_completion:
  18 NA's 
   1  515 

patient.follow_ups.follow_up.8.days_to_death:
4412 NA's 
   1  515 

patient.follow_ups.follow_up.8.days_to_new_tumor_event_after_initial_treatment:
4339 NA's 
   1  515 

patient.follow_ups.follow_up.8.followup_case_report_form_submission_reason:
additional new tumor event                       NA's 
                         1                        515 

patient.follow_ups.follow_up.8.followup_treatment_success:
progressive disease                NA's 
                  1                 515 

patient.follow_ups.follow_up.8.lost_follow_up:
  no NA's 
   1  515 

patient.follow_ups.follow_up.8.month_of_form_completion:
   8 NA's 
   1  515 

patient.follow_ups.follow_up.8.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  515 

patient.follow_ups.follow_up.8.person_neoplasm_cancer_status:
with tumor       NA's 
         1        515 

patient.follow_ups.follow_up.8.primary_therapy_outcome_success:
progressive disease                NA's 
                  1                 515 

patient.follow_ups.follow_up.8.radiation_therapy:
 yes NA's 
   1  515 

patient.follow_ups.follow_up.8.targeted_molecular_therapy:
 yes NA's 
   1  515 

patient.follow_ups.follow_up.8.vital_status:
dead NA's 
   1  515 

patient.follow_ups.follow_up.8.year_of_form_completion:
2014 NA's 
   1  515 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
  68   86  362 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
 102   52  362 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure:
  no  yes NA's 
  65   58  393 

patient.follow_ups.follow_up.additional_surgery_metastatic_procedure:
  no  yes NA's 
  68    6  442 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   19.00   17.49   24.00   31.00      30 

patient.follow_ups.follow_up.days_to_additional_surgery_locoregional_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   77.0   381.5   864.0  1073.3  1318.8  4437.0     458 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     77     955    1141    1535    2552    3759     507 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      7     444     821    1332    1915    5166     423 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -10.0   310.5   539.0   776.0   950.0  6423.0     136 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   292.5   508.0   799.7  1075.0  4347.0     386 

patient.follow_ups.follow_up.days_to_performance_status_follow_up_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -23.0   190.0   364.0   748.0   928.2  4671.0     454 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   0    1    2    4 NA's 
   6    9    2    1  498 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                            34                            359 
                          NA's 
                           123 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response  partial remission/response 
                         87                          19 
        progressive disease              stable disease 
                         98                         128 
                       NA's 
                        184 

patient.follow_ups.follow_up.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   80.00   90.00   84.92   90.00  100.00     455 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 271   47  198 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.979   9.000  12.000      30 

patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 278  130  108 

patient.follow_ups.follow_up.performance_status_scale_timing:
     adjuvant therapy post-adjuvant therapy  pre-adjuvant therapy 
                    3                    35                    17 
         preoperative                  NA's 
                    6                   455 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       173        242        101 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        118                          60 
        progressive disease              stable disease 
                         95                         138 
                       NA's 
                        105 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 170  277   69 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
 192  249   75 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  379    93    44 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2014    2013    2014    2015      30 

patient.food_allergy_history:
  no  yes NA's 
 287   20  209 


patient.gender:
female   male   NA's 
   230    285      1 

patient.hay_fever_history:
  no  yes NA's 
 303   37  176 

patient.headache_history:
  no  yes NA's 
 295  170   51 

patient.histological_type:
      astrocytoma  oligoastrocytoma oligodendroglioma              NA's 
              194               130               191                 1 

patient.history_ionizing_rt_to_head:
  no  yes NA's 
 491    4   21 

patient.history_of_neoadjuvant_treatment:
                                              no 
                                             512 
                                             yes 
                                               1 
yes, pharmaceutical treatment prior to resection 
                                               1 
               yes, radiation prior to resection 
                                               1 
                                            NA's 
                                               1 




patient.informed_consent_verified:
 yes NA's 
 515    1 

patient.inherited_genetic_syndrome_found:
  no  yes NA's 
 235    2  279 

patient.inherited_genetic_syndrome_result:
                                       1p and 19q deleted 
                                                        1 
                                          1p19q codeleted 
                                                        1 
                                           1p19q deletion 
                                                        1 
3 x chromosomes (super female) osler-weber-rendu syndrome 
                                                        1 
                                                     NA's 
                                                      512 

patient.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   80.00   90.00   87.47  100.00  100.00     220 

patient.laterality:
   left midline   right    NA's 
    250       7     253       6 

patient.ldh1_mutation_found:
  no  yes NA's 
  34   91  391 

patient.ldh1_mutation_test_method:
              ihc sequence analysis              NA's 
              113                11               392 

patient.ldh1_mutation_tested:
  no  yes NA's 
 296  127   93 

patient.mental_status_changes:
  no  yes NA's 
 345  114   57 

patient.mold_or_dust_allergy_history:
  no  yes NA's 
 311   17  188 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   7.000   6.336   9.000  12.000       1 

patient.motor_movement_changes:
  no  yes NA's 
 347  110   59 

patient.neoplasm_histologic_grade:
  g2   g3 NA's 
 249  265    2 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  23   41  452 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  39   23  454 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   56.0   240.0   455.0   633.3   815.0  3637.0     451 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.additional_surgery_locoregional_procedure:
  no  yes NA's 
  28   33  455 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.days_to_additional_surgery_locoregional_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  102.0   342.0   536.0   814.5   923.0  3637.0     481 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.additional_surgery_metastatic_procedure:
  no  yes NA's 
  28    2  486 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.days_to_additional_surgery_metastatic_procedure:
 237  698 NA's 
   1    1  514 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 189   65  262 


patient.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                   18                    47                    91 
        pre-operative                  NA's 
                  134                   226 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       181        275         60 

patient.preoperative_antiseizure_meds:
  no  yes NA's 
 111  265  140 

patient.preoperative_corticosteroids:
  no  yes NA's 
 216  151  149 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         84                          51 
        progressive disease              stable disease 
                         39                          59 
                       NA's 
                        283 

patient.prior_dx:
                                                     no 
                                                    500 
                                                    yes 
                                                      6 
                       yes, history of prior malignancy 
                                                      8 
yes, history of synchronous and or bilateral malignancy 
                                                      1 
                                                   NA's 
                                                      1 

patient.race:
american indian or alaska native                            asian 
                               1                                8 
       black or african american                            white 
                              21                              475 
                            NA's 
                              11 

patient.radiation_therapy:
  no  yes NA's 
 121  142  253 

patient.radiations.radiation.2.anatomic_treatment_site:
   local recurrence primary tumor field       regional site                NA's 
                 15                  14                   1                 486 



patient.radiations.radiation.2.course_number:
   1    2    5    9 NA's 
   4    8    1    1  502 

patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    8.00   19.00   16.88   26.00   31.00     484 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0   105.5   495.0  1058.0  1151.8  4152.0     486 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     17     120     495    1032    1176    4152     485 

patient.radiations.radiation.2.measure_of_response:
radiographic progressive disease                   stable disease 
                              10                                6 
                            NA's 
                             500 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.500   7.156  10.000  12.000     484 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     6.0    11.6    22.0    30.0     491 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0    40.8  1000.0  1824.4  3300.0  6000.0     489 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
  32  484 

patient.radiations.radiation.2.radiation_type:
               external           external beam other: specify in notes 
                     18                       8                       5 
          radioisotopes                    NA's 
                      1                     484 


patient.radiations.radiation.2.regimen_indication:
   adjuvant progression  recurrence        NA's 
          5           8           1         502 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
  21   10  485 

patient.radiations.radiation.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2011    2012    2012    2014    2015     484 

patient.radiations.radiation.3.anatomic_treatment_site:
   local recurrence primary tumor field                NA's 
                  1                   1                 514 

patient.radiations.radiation.3.bcr_radiation_barcode:
tcga-du-6396-r14925 tcga-du-6404-r14793                NA's 
                  1                   1                 514 

patient.radiations.radiation.3.bcr_radiation_uuid:
0b042829-6f75-49c5-8299-9af5359f0b21 1a641820-2360-4cea-8752-d87697c36cf1 
                                   1                                    1 
                                NA's 
                                 514 

patient.radiations.radiation.3.course_number:
   1    7 NA's 
   1    1  514 

patient.radiations.radiation.3.day_of_form_completion:
  27   31 NA's 
   1    1  514 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
  25 3219 NA's 
   1    1  514 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
  25 3206 NA's 
   1    1  514 

patient.radiations.radiation.3.month_of_form_completion:
   5    7 NA's 
   1    1  514 

patient.radiations.radiation.3.numfractions:
   1    6 NA's 
   1    1  514 

patient.radiations.radiation.3.radiation_dosage:
 800 3600 NA's 
   1    1  514 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   2  514 

patient.radiations.radiation.3.radiation_type:
other: specify in notes                    NA's 
                      2                     514 

patient.radiations.radiation.3.radiation_type_notes:
fractionated stereotactic radiotherapy                    stereo radiosurgery 
                                     1                                      1 
                                  NA's 
                                   514 

patient.radiations.radiation.3.regimen_indication:
   adjuvant progression        NA's 
          1           1         514 

patient.radiations.radiation.3.units:
 cgy NA's 
   2  514 

patient.radiations.radiation.3.year_of_form_completion:
2011 NA's 
   2  514 

patient.radiations.radiation.4.anatomic_treatment_site:
local recurrence             NA's 
               1              515 

patient.radiations.radiation.4.bcr_radiation_barcode:
tcga-du-6396-r14927                NA's 
                  1                 515 

patient.radiations.radiation.4.bcr_radiation_uuid:
157246a0-ede8-49ca-9759-47f82191a245                                 NA's 
                                   1                                  515 

patient.radiations.radiation.4.course_number:
   3 NA's 
   1  515 

patient.radiations.radiation.4.day_of_form_completion:
  27 NA's 
   1  515 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
1825 NA's 
   1  515 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
1825 NA's 
   1  515 

patient.radiations.radiation.4.month_of_form_completion:
   7 NA's 
   1  515 

patient.radiations.radiation.4.numfractions:
   1 NA's 
   1  515 

patient.radiations.radiation.4.radiation_dosage:
1800 NA's 
   1  515 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   1  515 

patient.radiations.radiation.4.radiation_type:
other: specify in notes                    NA's 
                      1                     515 

patient.radiations.radiation.4.radiation_type_notes:
stereotactic radiosurgery                      NA's 
                        1                       515 

patient.radiations.radiation.4.regimen_indication:
progression        NA's 
          1         515 

patient.radiations.radiation.4.units:
 cgy NA's 
   1  515 

patient.radiations.radiation.4.year_of_form_completion:
2011 NA's 
   1  515 

patient.radiations.radiation.5.anatomic_treatment_site:
local recurrence             NA's 
               1              515 

patient.radiations.radiation.5.bcr_radiation_barcode:
tcga-du-6396-r14928                NA's 
                  1                 515 

patient.radiations.radiation.5.bcr_radiation_uuid:
8592a4cc-06e6-4aea-812e-eca28b8e8bfd                                 NA's 
                                   1                                  515 

patient.radiations.radiation.5.course_number:
   4 NA's 
   1  515 

patient.radiations.radiation.5.day_of_form_completion:
  27 NA's 
   1  515 

patient.radiations.radiation.5.days_to_radiation_therapy_end:
2097 NA's 
   1  515 

patient.radiations.radiation.5.days_to_radiation_therapy_start:
2066 NA's 
   1  515 

patient.radiations.radiation.5.month_of_form_completion:
   7 NA's 
   1  515 

patient.radiations.radiation.5.radiation_dosage:
5600 NA's 
   1  515 

patient.radiations.radiation.5.radiation_treatment_ongoing:
  no NA's 
   1  515 

patient.radiations.radiation.5.radiation_type:
other: specify in notes                    NA's 
                      1                     515 

patient.radiations.radiation.5.radiation_type_notes:
intensity modulated radio therapy                              NA's 
                                1                               515 

patient.radiations.radiation.5.regimen_indication:
progression        NA's 
          1         515 

patient.radiations.radiation.5.units:
 cgy NA's 
   1  515 

patient.radiations.radiation.5.year_of_form_completion:
2011 NA's 
   1  515 

patient.radiations.radiation.anatomic_treatment_site:
   local recurrence primary tumor field       regional site                NA's 
                  8                 278                   6                 224 



patient.radiations.radiation.course_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.192   1.000   5.000     396 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   17.00   16.41   22.00   31.00     207 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -4.0    71.0    84.0   230.5   122.0  4543.0     222 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -70.0    27.0    40.0   183.7    81.5  4500.0     217 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              32                               11 
radiographic progressive disease                   stable disease 
                              21                              103 
                            NA's 
                             349 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   6.968  10.000  12.000     207 


patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     14      60    5400    3769    5940    6185     236 

patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
 304    4  208 

patient.radiations.radiation.radiation_type:
               external           external beam other: specify in notes 
                    183                     119                       4 
                   NA's 
                    210 

patient.radiations.radiation.radiation_type_notes:
intensity modulated radiotherapy                     radiosurgery 
                               1                                1 
       stereotactic radiosurgery                              xrt 
                               1                                1 
                            NA's 
                             512 

patient.radiations.radiation.regimen_indication:
   adjuvant progression  recurrence        NA's 
        108          14           1         393 

patient.radiations.radiation.units:
 cgy   gy NA's 
 203   89  224 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2011    2013    2012    2014    2015     207 

patient.seizure_history:
  no  yes NA's 
 177  303   36 

patient.sensory_changes:
  no  yes NA's 
 382   72   62 

patient.supratentorial_localization:
                        cerebral cortex deep gray (e.g.basal ganglia, thalamus) 
                                    142                                       3 
           not listed in medical record                            white matter 
                                    222                                     119 
                                   NA's 
                                     30 

patient.targeted_molecular_therapy:
  no  yes NA's 
 126  138  252 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 331  175   10 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
 176  330   10 



patient.tumor_tissue_site:
central nervous system                   NA's 
                   515                      1 

patient.visual_changes:
  no  yes NA's 
 393   65   58 

patient.vital_status:
alive  dead  NA's 
  423    92     1 

patient.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2012    2013    2013    2013    2015       1 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2008    2011    2009    2012    2013       1 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
             case matched methylation control 
                                            1 
primary tumor and additional primary tumor(s) 
                                            9 
         primary tumor and recurrent tumor(s) 
                                            5 
                                         NA's 
                                          501 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus             NA's 
             500               15                1 

patient.biospecimen_cqcf.consent_or_death_status:
  consented    deceased exemption 4        NA's 
        435          72           8           1 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -13.0    56.0   260.0   881.9  1106.0  6432.0      15 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-118.00    0.00    0.00   18.89    0.00 2737.00       8 

patient.biospecimen_cqcf.digital_image_submitted:
  no  yes NA's 
 415   12   89 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    20                    236                    260 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
brain; supratentorial       popliteal fossa           spinal cord 
                  454                     3                     1 
              unknown                  NA's 
                   57                     1 

patient.biospecimen_cqcf.histological_type:
      astrocytoma  oligoastrocytoma oligodendroglioma              NA's 
              193               132               190                 1 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             512 
yes, pharmaceutical treatment prior to resection 
                                               2 
               yes, radiation prior to resection 
                                               1 
                                            NA's 
                                               1 

patient.biospecimen_cqcf.method_of_sample_procurement:
          biopsy only gross total resection    subtotal resection 
                    8                   306                   190 
                 NA's 
                   12 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
 -12    0 NA's 
   1    4  511 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
0.177  NA's 
    1   515 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
stratagene kit #200600                   NA's 
                     1                    515 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  20 NA's 
   1  515 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
 113 NA's 
   1  515 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
blood draw       NA's 
         5        511 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
   6   11  499 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
1bcdbb2e-a25b-4380-947d-1380c8ae73e3                                 NA's 
                                   1                                  515 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
 138 NA's 
   1  515 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
blood draw       NA's 
         1        515 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1 NA's 
   1  515 

patient.biospecimen_cqcf.normal_controls.normal_control.4.bcr_sample_uuid:
a3c2d018-d116-46c9-af7a-20681e11d0c2                                 NA's 
                                   1                                  515 

patient.biospecimen_cqcf.normal_controls.normal_control.4.days_to_normal_sample_procurement:
 138 NA's 
   1  515 

patient.biospecimen_cqcf.normal_controls.normal_control.4.method_of_normal_sample_procurement:
blood draw       NA's 
         1        515 

patient.biospecimen_cqcf.normal_controls.normal_control.4.vial_number:
   2 NA's 
   1  515 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -31.00    0.00    0.00   87.97    1.25 3637.00      12 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.0231    0.2165    0.3842   62.9864    0.8295 1851.3000       246 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.15   20.00   20.00   29.50   25.00  150.00     246 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.15   44.00   74.00  144.33  125.00 1000.00     247 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
blood draw  mouthwash       NA's 
       509          2          5 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2 NA's 
 507    4    5 


patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
  no  yes NA's 
   6  509    1 

patient.biospecimen_cqcf.path_confirm_report_attached:
  no  yes NA's 
   3  512    1 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 yes NA's 
 515    1 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 yes NA's 
 515    1 

patient.biospecimen_cqcf.race:
american indian or alaska native                            asian 
                               1                                7 
       black or african american                            white 
                              11                              259 
                            NA's 
                             238 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  6 
                                                                                                                               NA's 
                                                                                                                                510 

patient.biospecimen_cqcf.sample_prescreened:
  no  yes NA's 
   2  513    1 

patient.biospecimen_cqcf.site_of_disease:
      brain spinal cord        NA's 
        514           1           1 

patient.biospecimen_cqcf.site_of_disease_description:
posterior fossa  supratentorial            NA's 
              3             454              59 

patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
 227   17  272 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
   9  418   89 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0   10   20 NA's 
  30    1    1  484 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   68.75   85.00   80.31   96.25  100.00     484 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   45.0   125.0   250.0   233.7   300.0   641.0     485 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
  25    7  484 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_necrosis_percent:
   0 NA's 
   6  510 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_nuclei_percent:
  40   60   90  100 NA's 
   1    1    2    2  510 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   41.0   104.2   157.5   160.8   206.2   300.0     510 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.vial_number:
   1    3 NA's 
   5    1  510 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.bcr_sample_uuid:
f89b3845-3d22-4d43-85b9-de584271045c fb8ee996-d991-43d2-87db-fc23d153a3dc 
                                   1                                    1 
                                NA's 
                                 514 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_necrosis_percent:
   0 NA's 
   2  514 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_nuclei_percent:
  60  100 NA's 
   1    1  514 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_weight:
  56  160 NA's 
   1    1  514 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.vial_number:
   1    2 NA's 
   1    1  514 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.9105  0.0000 30.0000       2 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   61.00   75.00   75.69   90.00  100.00       2 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0   150.8   245.0   329.8   400.0  2300.0      12 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1 NA's 
 514    2 


patient.days_to_index:
   0 NA's 
 515    1 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and additional primary tumor(s) 
                                            9 
         primary tumor and recurrent tumor(s) 
                                            5 
                                         NA's 
                                          502 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   115.8   397.5   937.9  1113.0  7912.0     218 


patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   162.5   200.0   227.9   272.5   430.0     502 

patient.samples.sample.2.is_ffpe:
  no NA's 
 515    1 

patient.samples.sample.2.oct_embedded:
false  true  NA's 
  295     3   218 



patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.810   1.823   1.840   1.869   1.885   2.130     502 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132275835 151537770 164053773 155571819 164053802 164081923       502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
  10    4  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.16 0.17 0.18 NA's 
   2    4    4    4  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   8   24   25   29 NA's 
   3    1    1    9  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   1    5    7    9 NA's 
   9    3    1    1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   5.000   8.000   7.071   9.000   9.000     502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a29v a31r a32q a36h NA's 
   3    1    1    9  502 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.080   2.210   2.513   2.835   3.600     502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 NA's 
  10    4  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2013 2014 NA's 
   5    9  502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-tm-a7cf-02a-11r-a360-13                         NA's 
                           1                          515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
28819779-d108-4c2f-ba54-eecc2d40968e                                 NA's 
                                   1                                  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
164082174      NA's 
        1       515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
  13 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.17 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  29 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   1 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   7 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a360 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   f NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
3.96 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
23.3 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
   1  515 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.323e+08 1.603e+08 2.717e+08 1.678e+08 1.062e+09         5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   4  497   10    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.5000  0.5000  0.4909  0.5000  0.5000       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00    8.00   19.00   16.88   24.00   30.00       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 511    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   5.521   7.000  11.000       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.303   8.500  12.000       5 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.95   40.00   40.00   38.99   40.00   40.00       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 170  341    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
   4   10  497    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
 111   59  128  213    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.8   160.0   181.0   167.5   194.0   238.0     332 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 497                   14                    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  14  497    5 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.5000  0.5000  0.4747  0.5000  0.5000     332 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 497   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
 170  327   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             511 
                                            NA's 
                                               5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 511    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                 repli-g x 
                       14                       457                        40 
                     NA's 
                        5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.920   1.272   1.360   1.426   1.597   1.930     502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.100   7.250   7.650   7.829   8.075   9.100     502 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
      119        65       332 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
   9    5  502 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
153887521 156436595 167804376 163562399 167804429 167804438       502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   8   21   24   29 NA's 
   9    1    3    1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   1    7   10 NA's 
   1   12    1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.250  10.500   8.357  11.000  12.000     502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a31c a333 a35v a44b NA's 
   3    1    1    9  502 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2013 2014 NA's 
   4   10  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  14                  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
  14  502 



patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              14 
                                            NA's 
                                             502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  14  502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     14     502 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
   9    5  502 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.700   1.900   1.900   1.891   1.900   2.600     441 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.456e+08 1.604e+08 3.814e+08 1.050e+09 1.076e+09         1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
 115  400    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.07000 0.08000 0.08786 0.09000 0.17000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   19.00   25.00   22.22   26.00   30.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 515    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.353  10.000  12.000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.635   8.000  12.000       1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.800   3.000   2.608   3.200   3.600       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 214  301    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7   40   60 NA's 
 106    9  292  108    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2014       1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.508e+08 1.678e+08 4.857e+08 1.050e+09 1.062e+09       289 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   1    2    5    8 NA's 
  26    8    5  188  289 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.05000 0.07000 0.07762 0.08000 0.17000     289 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0    15.0    26.0    22.3    30.0    30.0     289 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 227  289 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.974  10.000  12.000     289 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.185   8.000  12.000     289 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.070   2.000   1.966   3.000   3.600     289 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 114  113  289 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.40   40.00   32.81   60.00   60.00     289 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2013 2014 NA's 
  30   84   29   84  289 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108684030 164054827 167779724 163676590 167822696 167822774       449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   1    5    8 NA's 
   2    1   64  449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0700  0.0800  0.0864  0.0800  0.5000     449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   26.00   30.00   26.45   30.00   30.00     449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  67  449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   4.000   3.582   4.000  10.000     449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    4.00    5.03    7.00   11.00     449 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.700   0.940   1.070   1.669   1.070  40.000     449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
   1   66  449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.40   13.40   14.51   13.40   80.00     449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2013 2014 NA's 
   1    4   62  449 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-db-5278-10a-01d-a43p-08 tcga-du-5874-10a-01d-a38v-08 
                           1                            1 
tcga-du-7009-10a-01d-a46a-09 tcga-fg-5964-10a-01d-a290-08 
                           1                            1 
                        NA's 
                         512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
084be43b-f9dc-4c97-987c-e9a0a587f348 b012a46a-c845-4b4d-b238-253695eccdb3 
                                   1                                    1 
b0182e1f-1541-4b7f-aaad-d85de75f3c9b c2db3480-95e2-4c78-bbf5-65aa10aeb412 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
132268900 167780131 167804266 167811757      NA's 
        1         1         1         1       512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8    9 NA's 
   3    1  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 0.08 NA's 
   2    2  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   8   21   30 NA's 
   1    1    2  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1    4    7 NA's 
   1    2    1  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    5 NA's 
   3    1  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a290 a38v a43p a46a NA's 
   1    1    1    1  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   a    c    f NA's 
   1    2    1  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
0.94 1.07  1.6 NA's 
   2    1    1  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   4  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4   20 NA's 
   3    1  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2013 2014 2015 NA's 
   1    2    1  512 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-db-5278-10a-01d-a461-08 tcga-fg-5964-10a-01d-a38v-08 
                           1                            1 
                        NA's 
                         514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
089b9f49-f012-41c6-81f5-8cc77f81645d 66bad3bd-28bc-47a5-b361-9fc36300208f 
                                   1                                    1 
                                NA's 
                                 514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
167780107 167814086      NA's 
        1         1       514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8 NA's 
   2  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 0.08 NA's 
   1    1  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   7   30 NA's 
   1    1  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   2  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   4   10 NA's 
   1    1  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   2    5 NA's 
   1    1  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a38v a461 NA's 
   1    1  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   c    e NA's 
   1    1  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
 0.7 1.07 NA's 
   1    1  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   2  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  10 13.4 NA's 
   1    1  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2014 NA's 
   2  514 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-db-5278-10a-01d-a46a-09                         NA's 
                           1                          515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
1f96c9e2-e090-4324-a1de-7d9a28110f33                                 NA's 
                                   1                                  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
167811781      NA's 
        1       515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   9 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.07 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  21 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   1 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
a46a NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   d NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
0.94 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
13.4 NA's 
   1  515 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2015 NA's 
   1  515 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.456e+08 1.539e+08 3.274e+08 1.641e+08 1.076e+09         1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    5 NA's 
 400  106    9    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1450  0.1500  0.1526  0.1600  0.1700       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   19.00   25.00   22.22   26.00   30.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 515    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.353  10.000  12.000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.571   8.000  12.000       1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.800   1.000   1.070   1.191   1.140   2.260       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 214  301    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.670   6.700   6.700   7.816   6.700  13.300       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2014       1 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   14.09   29.01   85.25   90.35 1232.70       1 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
 515    1 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
 515    1 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1450  0.1500  0.1526  0.1600  0.1700       1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 515    1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 217  298    1 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             515 
                                            NA's 
                                               1 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
 515    1 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                              121                               116 
pre-extracted dna received by tss                              NA's 
                              278                                 1 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
      440        75         1 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
   9    5  502 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   15.00   15.39   20.00   31.00       1 

patient.samples.sample.2.portions.portion.is_ffpe:
  no NA's 
 515    1 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   7.235  10.000  12.000       1 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.262   1.000  21.000       1 

patient.samples.sample.2.portions.portion.portion_sequence:
   1    2 NA's 
 281    3  232 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  14  502 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    5 NA's 
  13    1  502 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
  14  502 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
  14  502 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
  14  502 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   10.00   10.00   11.43   10.00   30.00     502 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   12.50   27.50   31.79   45.00   65.00     502 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   45.00   60.00   56.79   70.00   90.00     502 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
  70   80   85   90 NA's 
   6    1    2    5  502 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
  14  502 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   2.000   6.066   2.000  80.000     395 

patient.samples.sample.2.portions.portion.year_of_creation:
2010 2011 2012 2013 NA's 
  34  121   79  281    1 


patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
   6  510 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
   6  510 

patient.samples.sample.2.portions.shipment_portion.plate_id:
a300 a44e NA's 
   2    4  510 

patient.samples.sample.2.portions.shipment_portion.portion_number:
  21 NA's 
   6  510 

patient.samples.sample.2.portions.shipment_portion.portion_sequence:
   2 NA's 
   3  513 


patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
   9   14 NA's 
   4    2  510 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   5    7 NA's 
   2    4  510 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2013 2014 NA's 
   2    4  510 

patient.samples.sample.2.sample_type:
blood derived normal   buccal cell normal      recurrent tumor 
                 499                    2                   14 
                NA's 
                   1 

patient.samples.sample.2.sample_type_id:
   2   10   12 NA's 
  14  499    2    1 

patient.samples.sample.2.vial_number:
   a    b    c    d NA's 
 496   16    2    1    1 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and additional primary tumor(s) 
                                            2 
         primary tumor and recurrent tumor(s) 
                                            1 
                                         NA's 
                                          513 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    661    1308    1453    2122    2274    4843     507 

patient.samples.sample.3.diagnostic_slides.ffpe_slide_uuid:
93c15688-f5b2-40bc-85eb-ff2661e16d4e                                 NA's 
                                   1                                  515 

patient.samples.sample.3.initial_weight:
  60  120  360  390 NA's 
   1    1    1    1  512 

patient.samples.sample.3.is_ffpe:
  no NA's 
  14  502 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
    8     1   507 

patient.samples.sample.3.pathology_report_file_name:
tcga-du-6404.4a27aeba-b999-4f7f-b28f-ccc5ad367ba2.pdf 
                                                    1 
tcga-du-6407.49baf246-ae0a-4627-af91-35932d00f105.pdf 
                                                    1 
tcga-fg-5965.dd2349dd-e128-4c2d-962b-f8644121fb16.pdf 
                                                    1 
tcga-tq-a7rk.5452fdc5-eb15-49de-a0cc-9a9ca7018a5b.pdf 
                                                    1 
                                                 NA's 
                                                  512 

patient.samples.sample.3.pathology_report_uuid:
49baf246-ae0a-4627-af91-35932d00f105 4a27aeba-b999-4f7f-b28f-ccc5ad367ba2 
                                   1                                    1 
5452fdc5-eb15-49de-a0cc-9a9ca7018a5b dd2349dd-e128-4c2d-962b-f8644121fb16 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.84 1.87 1.88 1.89 NA's 
   1    1    1    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-du-6404-02b-11r-a36h-07 tcga-du-6407-02b-11r-a36h-07 
                           1                            1 
tcga-fg-5965-02b-11r-a29v-13 tcga-tq-a7rk-02b-11r-a40a-07 
                           1                            1 
                        NA's 
                         512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
1cd76b6a-a698-46cd-9594-decc4ebf24e4 25aa5be1-62ec-4bd7-9509-bafbcd1adddb 
                                   1                                    1 
54f3eba4-0024-4176-af00-07d378266f7b af6191bb-b4c3-42ea-adc9-f6d45b00f0b0 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
132275852 164053817 164053818 167788991      NA's 
        1         1         1         1       512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
   3    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.16 0.17 0.18 NA's 
   2    1    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   8   14   29 NA's 
   1    1    2  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   1    5 NA's 
   2    2  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    4    9 NA's 
   1    1    2  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a29v a36h a40a NA's 
   1    2    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   a    e    g NA's 
   1    2    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
2.08 2.21 2.34  3.2 NA's 
   1    1    1    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   4  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 NA's 
   3    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2013 2014 NA's 
   1    3  512 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
107043895 110530940 152296486 141278739 164053798 167812031       502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   1   10    3  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.16 0.17 0.18  0.5 NA's 
   2    1    1   10  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0    12.5    23.0    20.0    27.5    30.0     502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  14  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.571   5.750  10.000     502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.25    6.00    6.00    8.00   11.00     502 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.08   12.70   40.00   29.45   40.00   40.00     502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
   5    9  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
   1    3   10  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
   4    1    3    6  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  17.15   31.03  187.00  122.94  202.00  210.00     507 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                  10                    4                  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   4   10  502 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.16 0.17 0.18  0.5 NA's 
   2    1    1    5  507 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  10  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
   5    5  506 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              14 
                                            NA's 
                                             502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  14  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                        4                        10                       502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
1.51 1.55 1.61 1.85 NA's 
   1    1    1    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
 7.9  8.6  8.9  9.8 NA's 
   1    1    1    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
        1         8       507 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   4  512 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-du-6404-02b-11w-a44b-08 tcga-du-6407-02b-11w-a44b-08 
                           1                            1 
tcga-fg-5965-02b-11w-a31c-08                         NA's 
                           1                          513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
6e4751dd-1c78-43b3-a470-4d229495ae72 8d1c69fd-86de-48fd-98b9-61be875893fb 
                                   1                                    1 
b2efdc94-3fda-4001-ba4b-cceb30753550                                 NA's 
                                   1                                  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
153887906 167804390 174165408      NA's 
        1         1         1       513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   8   24 NA's 
   2    1  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   7 NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   3   11 NA's 
   1    2  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a31c a44b NA's 
   1    2  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   e    g    h NA's 
   1    1    1  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2013 2014 NA's 
   1    2  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   3                  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-du-6404-02b-11w tcga-du-6407-02b-11w tcga-fg-5965-02b-11w 
                   1                    1                    1 
                NA's 
                 513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
41739b05-7587-4d5b-b205-f0f046dd63ed 97492779-f99f-4158-afaa-00e6a3e68e18 
                                   1                                    1 
bad4b47f-2e47-4d8c-bd16-bade99418b5e                                 NA's 
                                   1                                  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      3     513 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   3  513 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
 1.9 NA's 
   4  512 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108686897 111773029 149775626 140128164 163129749 167780324       502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
   6    8  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0800  0.0850  0.1036  0.1375  0.1600     502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   24.25   25.00   23.50   26.50   29.00     502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  14  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     2.5     4.0     5.0     6.5    12.0     502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   6.071   7.750  11.000     502 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   0.950   2.800   2.191   3.200   3.600     502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
   5    9  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7   40   60 NA's 
   4    2    7    1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2013 2014 NA's 
   5    5    4  502 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108686417 108686453 132270082 136743632 164053815 167789543       507 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8 NA's 
   2    7  507 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 0.07 0.08 0.16 NA's 
   4    2    1    2  507 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   25.00   25.00   23.33   29.00   30.00     507 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   9  507 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   1    4    5 NA's 
   2    5    2  507 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.889   7.000  11.000     507 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.080   3.000   2.567   3.000   3.200     507 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
   4    5  507 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  13 13.4   40   60 NA's 
   2    1    2    4  507 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2013 2014 NA's 
   4    1    4  507 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108686636 145635555 164054786 149297337 164054834 164054875       511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   1    5    8 NA's 
   1    1    3  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.07 0.13 0.16 NA's 
   3    1    1  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   3   25   26 NA's 
   1    1    3  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   5  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   2    4    9 NA's 
   3    1    1  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   3    4    5    8 NA's 
   2    1    1    1  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
1706 a323 a37s NA's 
   1    1    3  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   a    d    e    h NA's 
   1    1    1    2  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
0.94 1.28 3.47 NA's 
   3    1    1  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
   1    4  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   8 13.4 26.7 NA's 
   1    3    1  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2013 2014 NA's 
   1    1    3  511 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-du-5870-10a-01d-a43p-08 tcga-du-5872-10a-01d-a465-08 
                           1                            1 
tcga-du-6397-10a-01d-a43p-08 tcga-fg-5963-10a-01d-a37s-08 
                           1                            1 
                        NA's 
                         512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
24e6e15e-8d1b-4302-8e0c-da1158c5ee5c 2749b3ce-c9cb-4a0a-b5d9-d6051046b1f5 
                                   1                                    1 
b5c9f7f2-3341-49cb-aee5-b0838180af09 ef52238c-1fb1-4ba7-a943-76e849f8145f 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
164054848 167804263 167804265 167812138      NA's 
        1         1         1         1       512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8 NA's 
   4  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 NA's 
   4  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   2    8   26 NA's 
   1    2    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2    7   12 NA's 
   1    2    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    2    8 NA's 
   2    1    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a37s a43p a465 NA's 
   1    2    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   a    c    d    f NA's 
   1    1    1    1  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
0.92 0.94 NA's 
   1    3  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   4  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.2 13.4 NA's 
   1    3  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2014 NA's 
   4  512 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-du-5870-10a-01d-a461-08 tcga-du-6397-10a-01d-a461-08 
                           1                            1 
                        NA's 
                         514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
1a48ebf2-931b-435c-9efd-021e0568e375 5d762eaa-d788-47e4-83f7-e00b7a2d44b4 
                                   1                                    1 
                                NA's 
                                 514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
167811274 167814106      NA's 
        1         1       514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8 NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   7 NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
  10 NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   1 NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a461 NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   c    f NA's 
   1    1  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
 0.7 NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  10 NA's 
   2  514 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2014 NA's 
   2  514 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108686513 111772669 149777882 140127806 163129514 167780271       502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    5 NA's 
   8    4    2  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.13    0.14    0.15    0.15    0.16    0.17     502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   24.25   25.00   23.50   26.50   29.00     502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  14  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     2.5     4.0     5.0     6.5    12.0     502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   6.071   7.750  11.000     502 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.070   1.140   1.399   1.827   2.080     502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
   5    9  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7   13 13.3 NA's 
   1    7    2    4  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2013 2014 NA's 
   5    5    4  502 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.27   19.70   40.91   55.64   64.29  168.40     502 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  14  502 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  14  502 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.13    0.14    0.15    0.15    0.16    0.17     502 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  14  502 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
   5    9  502 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              14 
                                            NA's 
                                             502 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  14  502 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                                6                                 8 
                             NA's 
                              502 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       10         4       502 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   4  512 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   12.25   18.00   17.14   22.75   26.00     502 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  14  502 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.250   8.000   6.571   9.750  12.000     502 

patient.samples.sample.3.portions.portion.portion_number:
   1   11 NA's 
  10    4  502 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
   6  510 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_barcode:
tcga-du-6404-02b-01-ts1 tcga-du-6407-02b-01-ts1 tcga-fg-5965-02b-01-ts1 
                      1                       1                       1 
tcga-tq-a7rk-02b-01-ts1                    NA's 
                      1                     512 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_uuid:
1c087e11-5130-4095-979f-3d949c41ef15 90e66b88-254f-4cad-8415-662a50022438 
                                   1                                    1 
9b51dcdd-bfc9-4471-b2fd-21ab570809bd f1fb2086-7951-4603-a4b1-0e254cd49350 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.3.portions.portion.slides.slide.image_file_name:
tcga-du-6404-02b-01-ts1.f1fb2086-7951-4603-a4b1-0e254cd49350.svs 
                                                               1 
tcga-du-6407-02b-01-ts1.90e66b88-254f-4cad-8415-662a50022438.svs 
                                                               1 
tcga-fg-5965-02b-01-ts1.1c087e11-5130-4095-979f-3d949c41ef15.svs 
                                                               1 
tcga-tq-a7rk-02b-01-ts1.9b51dcdd-bfc9-4471-b2fd-21ab570809bd.svs 
                                                               1 
                                                            NA's 
                                                             512 

patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   4  512 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   4  512 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0    5   20 NA's 
   2    1    1  512 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   4  512 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
   5   10   20 NA's 
   2    1    1  512 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0    5   45   50 NA's 
   1    1    1    1  512 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
  40   50   60   85 NA's 
   1    1    1    1  512 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
  70   80   85   90 NA's 
   1    1    1    1  512 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
   4  512 

patient.samples.sample.3.portions.portion.weight:
   2   30 NA's 
   2    4  510 

patient.samples.sample.3.portions.portion.year_of_creation:
2011 2012 2013 2014 NA's 
   5    2    6    1  502 

patient.samples.sample.3.sample_type:
blood derived normal      recurrent tumor                 NA's 
                  10                    4                  502 

patient.samples.sample.3.sample_type_id:
   2   10 NA's 
   4   10  502 

patient.samples.sample.3.vial_number:
   a    b NA's 
  10    4  502 

patient.samples.sample.4.bcr_sample_barcode:
tcga-du-6404-10a tcga-du-6407-10a tcga-fg-5965-10a tcga-tq-a7rk-10a 
               1                1                1                1 
            NA's 
             512 

patient.samples.sample.4.bcr_sample_uuid:
1cd7f0e4-4680-470b-b23d-7ff2ff16ea6e 4035393d-d331-4d0c-95c7-5b00acd26035 
                                   1                                    1 
8469a6ea-9610-4b3d-8caa-48e503b0fd2e b7b08d55-fe27-4261-a8c2-e29dbc658e74 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.4.days_to_collection:
1057 NA's 
   1  515 

patient.samples.sample.4.is_ffpe:
  no NA's 
   4  512 

patient.samples.sample.4.oct_embedded:
false  NA's 
    1   515 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-du-6404-10a-01w-1780-08 tcga-du-6407-10a-01w-1780-08 
                           1                            1 
tcga-fg-5965-10a-01w-1930-08 tcga-tq-a7rk-10a-01w-a453-08 
                           1                            1 
                        NA's 
                         512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
0488a7e4-bd8b-4f6d-b818-8c02ae9b274c 624a3f58-2fc4-45f4-82e2-409e4c871ba5 
                                   1                                    1 
78731e5c-53c8-4086-8710-e88283917159 81b20b65-784d-4062-9bc7-b5bb455f8fe1 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
107043640 107043958 113627634 175155747      NA's 
        1         1         1         1       512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   8 NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
 0.5 NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   8   23 NA's 
   2    2  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   5    7    8 NA's 
   2    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   4    6    8 NA's 
   1    1    2  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
1780 1930 a453 NA's 
   2    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   b    e    g    h NA's 
   1    1    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
  40 NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  80 NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2014 NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.amount:
 180  206  213 NA's 
   1    1    1  513 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                 NA's 
                   4                  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   w NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-du-6404-10a-01w tcga-du-6407-10a-01w tcga-fg-5965-10a-01w 
                   1                    1                    1 
tcga-tq-a7rk-10a-01w                 NA's 
                   1                  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
19e49677-b6e1-4fc2-9b44-1f064f8c2596 b902b8b3-6e1b-48c3-ab50-cfde84e42f01 
                                   1                                    1 
e6f69d7d-6ee3-4107-99c2-c5283e1f1001 f2fd42bb-267f-4eaa-bb83-2cc0ea2c0b35 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.concentration:
 0.5 NA's 
   3  513 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      4     512 

patient.samples.sample.4.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
        1         2       513 

patient.samples.sample.4.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9 NA's 
   2    1  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-du-6404-10a-01d-1704-01 tcga-du-6407-10a-01d-1704-01 
                           1                            1 
tcga-fg-5965-10a-01d-1892-01 tcga-tq-a7rk-10a-01d-a33w-08 
                           1                            1 
                        NA's 
                         512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
4a8c0837-95d6-428f-af78-778185c83361 bf658a6a-86df-4667-b089-d2f86f9ff2dc 
                                   1                                    1 
c3ebdf51-7993-4c69-b3d5-38be928a2d8e ed3e7b50-72a7-4dd3-90ba-87583c1a65da 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
 108686921  108686958  160357155 1053935695       NA's 
         1          1          1          1        512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.09 0.14 0.16 NA's 
   1    2    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  20   25   30 NA's 
   1    2    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   4    7   10 NA's 
   2    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   4    6    7 NA's 
   1    1    2  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
1704 1892 a33w NA's 
   2    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   a    b    e    h NA's 
   1    1    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
0.93 1.07  3.6 NA's 
   2    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67   40 NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2013 NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-du-6404-10a-01d-1705-08 tcga-du-6407-10a-01d-1705-08 
                           1                            1 
tcga-fg-5965-10a-01d-1893-08                         NA's 
                           1                          513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
1895ec32-d356-44f2-94e4-7e43bf8ea156 4a5655ba-0f40-4c74-9534-23eb5df3ad4d 
                                   1                                    1 
6f6eeb7c-c029-419f-a597-d0f72fd8ae69                                 NA's 
                                   1                                  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
 108686441  108686478 1053935791       NA's 
         1          1          1        513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   8 NA's 
   3  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 NA's 
   3  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  20   25 NA's 
   1    2  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   4    7 NA's 
   2    1  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   6    7 NA's 
   1    2  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
1705 1893 NA's 
   2    1  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   a    b    e NA's 
   1    1    1  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   3 NA's 
   3  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   3  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  60 NA's 
   3  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   3  513 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-du-6404-10a-01d-a37s-08 tcga-du-6407-10a-01d-a290-08 
                           1                            1 
                        NA's 
                         514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
44e8a999-8454-437f-9db0-10fa2366af5e 5d1741ca-e214-4ce2-9af2-09020e99d837 
                                   1                                    1 
                                NA's 
                                 514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
132268906 164054861      NA's 
        1         1       514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   8 NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.07 NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  26   30 NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   2    4 NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    6 NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a290 a37s NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   b    f NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
0.94  1.4 NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
13.4   20 NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2013 2014 NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-du-6404-10a-01d-a43p-08 tcga-du-6407-10a-01d-a38t-08 
                           1                            1 
                        NA's 
                         514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
779a8023-24d8-4afc-94d1-2973af3badf8 a2b77766-4e40-43a4-8c29-c557f9044d11 
                                   1                                    1 
                                NA's 
                                 514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
167779682 167804261      NA's 
        1         1       514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8 NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   8   30 NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   4    7 NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    3 NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a38t a43p NA's 
   1    1  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   h NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
0.94 NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4 NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2014 NA's 
   2  514 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-du-6404-10a-01d-a461-08                         NA's 
                           1                          515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
934d95ec-4ae8-48af-acc1-87dad8b41f8c                                 NA's 
                                   1                                  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
167811265      NA's 
        1       515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   7 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
  10 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   2 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a461 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   d NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
 0.7 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  10 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2014 NA's 
   1  515 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-du-6404-10a-01d-1703-02 tcga-du-6407-10a-01d-1703-02 
                           1                            1 
tcga-fg-5965-10a-01d-1891-02 tcga-tq-a7rk-10a-01d-a33v-01 
                           1                            1 
                        NA's 
                         512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
51702b0a-b663-4083-b9b0-5bd1bbdee263 70176976-e09d-4991-967d-28987a5cfd67 
                                   1                                    1 
7f70b31d-d062-4b09-80ca-f7eee520a0ea eb380229-1c8c-4751-b42f-30df444f67e5 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
 108686537  108686574  160357251 1053966688       NA's 
         1          1          1          1        512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2 NA's 
   1    3  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.16 0.17 NA's 
   2    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  20   25   30 NA's 
   1    2    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   4    7   10 NA's 
   2    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   4    5    7 NA's 
   1    1    2  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
1703 1891 a33v NA's 
   2    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   b    e    h NA's 
   1    2    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
1.14  1.6 1.86 NA's 
   1    1    2  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7   10 13.3 NA's 
   1    1    2  512 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2013 NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
9.57 53.3   58   85 NA's 
   1    1    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-du-6404-10a-01d tcga-du-6407-10a-01d tcga-fg-5965-10a-01d 
                   1                    1                    1 
tcga-tq-a7rk-10a-01d                 NA's 
                   1                  512 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid:
07aaf4e6-f601-47e0-b99a-ec64d7a4b6c3 3e72463f-b67b-424a-9de6-8fa700ae34ac 
                                   1                                    1 
633ef1a1-0ab3-4c83-bbd2-a2b815c9106c cc382d56-e7dc-4326-b475-18e3044615ee 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
0.14 0.16 0.17 NA's 
   2    1    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
   3    1  512 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             512 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
pre-extracted dna received by tss                              NA's 
                                4                               512 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
        1         3       512 

patient.samples.sample.4.portions.portion.bcr_portion_barcode:
tcga-du-6404-10a-01 tcga-du-6407-10a-01 tcga-fg-5965-10a-01 tcga-tq-a7rk-10a-01 
                  1                   1                   1                   1 
               NA's 
                512 

patient.samples.sample.4.portions.portion.bcr_portion_uuid:
30cc85b5-fd57-4765-8c59-f2bff954be8f 58af8efe-e700-4e8b-b5c5-e4159b18a181 
                                   1                                    1 
66258208-b4a3-4c43-aec9-3a2acb96d561 c52a42af-03bc-4b94-816b-29c668af1ebc 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.4.portions.portion.day_of_creation:
  11   16   19 NA's 
   1    2    1  512 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   4  512 

patient.samples.sample.4.portions.portion.month_of_creation:
   2    3    9 NA's 
   1    2    1  512 

patient.samples.sample.4.portions.portion.portion_number:
   1 NA's 
   4  512 

patient.samples.sample.4.portions.portion.portion_sequence:
   1 NA's 
   1  515 

patient.samples.sample.4.portions.portion.year_of_creation:
2011 2013 NA's 
   3    1  512 

patient.samples.sample.4.sample_type:
blood derived normal                 NA's 
                   4                  512 

patient.samples.sample.4.sample_type_id:
  10 NA's 
   4  512 

patient.samples.sample.4.vial_number:
   a NA's 
   4  512 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   114.0   375.0   865.3   961.0  7912.0     223 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10:
088a6643-8205-45a9-af02-8c039d9f653e b88fa0f5-7626-46c8-b81c-48ac95e4af63 
                                   1                                    1 
cd6e7624-8ec3-4431-a3dc-2739419f9e12                                 NA's 
                                   1                                  513 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.11:
874632d2-dd16-441a-927d-df54c5ed35f2 e06e482d-dbb9-4bbe-bd34-de76ac8bfd56 
                                   1                                    1 
f02c56e8-9793-4272-b14e-031a06ee2c8f                                 NA's 
                                   1                                  513 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.12:
484f328c-f9e9-4970-88cd-9c931ba53bba 9a0644c6-3bad-4dee-bd7d-190588fc4213 
                                   1                                    1 
f741de76-6e81-4dcf-a99f-659c1e398fb0                                 NA's 
                                   1                                  513 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.13:
26ca7de4-febc-4544-829f-163a7fb04da7 3bed3f8d-421d-40fd-91af-38ad307639d0 
                                   1                                    1 
d62ddc11-431f-41a4-9f9f-da33e90d9dc4                                 NA's 
                                   1                                  513 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.14:
035d15f7-4627-463b-bca0-293d3980dbe9 7888c0c3-818c-4c36-9ec6-f708b257872a 
                                   1                                    1 
8f2ae4ba-d2b0-406c-8624-7b3d840e67b2                                 NA's 
                                   1                                  513 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.15:
13ea5176-70ac-420d-9e05-ebed338b1abc f23d5126-9d7d-436e-9565-5724cdfdd79e 
                                   1                                    1 
                                NA's 
                                 514 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.16:
3b3e33bb-2b4a-4600-8474-62afcc9f99a4                                 NA's 
                                   1                                  515 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.17:
4ea311da-943a-415a-b2c6-19b7d20231af                                 NA's 
                                   1                                  515 









patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   100.0   160.0   231.1   290.0  2980.0     224 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.8000  0.9000  0.8856  1.0000  1.8000     294 

patient.samples.sample.is_ffpe:
  no NA's 
 515    1 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   1.000   1.100   1.313   1.500   3.700     294 

patient.samples.sample.oct_embedded:
false  true  NA's 
  179   114   223 



patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio:
 1.7  1.8 NA's 
   1    1  514 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-db-5279-01a-03r-1470-07 tcga-du-6395-01a-13r-1708-07 
                           1                            1 
tcga-fg-7638-01b-12r-2090-07                         NA's 
                           1                          513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
11b3ce08-d237-4e4e-8656-a235d3964f8f a6296495-9708-414d-a797-80e9fba7be77 
                                   1                                    1 
c1c9f44f-935f-47b9-80cd-5116d52001c3                                 NA's 
                                   1                                  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
 108686041  120998281 1049951397       NA's 
         1          1          1        513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   7 NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.15 0.16 NA's 
   2    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  15   19   25 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   4   12 NA's 
   1    2  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   2    3 NA's 
   2    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
1470 1708 2090 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   a    b    f NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
   2 4.01 4.27 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
13.3 26.7 NA's 
   1    2  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   1    2  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-db-5279-01a-03r-1762-13                         NA's 
                           1                          515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
26f3783b-6fef-492c-a308-0cd363873d58                                 NA's 
                                   1                                  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
1053960440       NA's 
         1        515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
  13 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
0.15 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  16 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   5 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   5 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
1762 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_row:
   h NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
   3 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
  20 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-db-5279-01a-03r-2347-07                         NA's 
                           1                          515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
4af34323-4cc9-4b69-b0cf-558bb9d8ee1a                                 NA's 
                                   1                                  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
1076438310       NA's 
         1        515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id:
   7 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration:
0.15 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  30 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   7 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column:
   1 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id:
2347 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_row:
   a NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity:
   2 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume:
13.3 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2012 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-db-5279-01a-03r-a32z-07                         NA's 
                           1                          515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
4aaa36fe-91d2-41f2-a957-4d7457af781f                                 NA's 
                                   1                                  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
150757350      NA's 
        1       515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.center_id:
   7 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.concentration:
0.15 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   2 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
  10 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_column:
   3 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_id:
a32z NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_row:
   h NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.quantity:
1.95 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.volume:
  13 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2013 NA's 
   1  515 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-db-5279-01a-03r-1469-13 tcga-du-6395-01a-13r-1707-13 
                           1                            1 
tcga-fg-7638-01b-12r-2089-13                         NA's 
                           1                          513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
6a036235-0d95-43f8-9543-ab77dc0df8f1 7c5a455f-6076-41a0-bc4a-ca26068a8571 
                                   1                                    1 
ea7ddef7-db96-4965-a660-8dcd97d5b306                                 NA's 
                                   1                                  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
 108685849  120998377 1049951205       NA's 
         1          1          1        513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
  13 NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.15 0.16 NA's 
   2    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  15   19   25 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   4   12 NA's 
   1    2  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   2    3 NA's 
   2    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
1469 1707 2089 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   a    b    f NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
   3  3.2 NA's 
   2    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  20 NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   1    2  513 

patient.samples.sample.portions.portion.2.analytes.analyte.amount:
 3.8   12 96.2 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
 rna NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   r NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.bcr_analyte_barcode:
tcga-db-5279-01a-03r tcga-du-6395-01a-13r tcga-fg-7638-01b-12r 
                   1                    1                    1 
                NA's 
                 513 

patient.samples.sample.portions.portion.2.analytes.analyte.bcr_analyte_uuid:
0406e40b-6523-47ea-91ee-3f9aafb35774 3f31aea6-c4a9-473e-b4bd-5471bae7894f 
                                   1                                    1 
63a075f7-6c07-4224-8942-ab4600ada49d                                 NA's 
                                   1                                  513 

patient.samples.sample.portions.portion.2.analytes.analyte.concentration:
0.15 0.16 NA's 
   2    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             513 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   3  513 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        3                       513 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
0.98 1.11  1.2 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.rinvalue:
 7.1  7.4  7.8 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      3     513 

patient.samples.sample.portions.portion.2.analytes.analyte.subportion_sequence:
   2    3 NA's 
   1    1  514 

patient.samples.sample.portions.portion.2.bcr_portion_barcode:
tcga-db-5279-01a-03 tcga-du-6395-01a-13 tcga-fg-7638-01b-12                NA's 
                  1                   1                   1                 513 

patient.samples.sample.portions.portion.2.bcr_portion_uuid:
3133e972-4cc5-4771-90fe-3590f810ccaa 82ff0574-c153-477c-b763-575feff5113d 
                                   1                                    1 
ce7ed07d-47b3-4232-af7b-6148a9ca61fe                                 NA's 
                                   1                                  513 

patient.samples.sample.portions.portion.2.day_of_creation:
   8   18   21 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
   3  513 

patient.samples.sample.portions.portion.2.month_of_creation:
   3   10   11 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.portion_number:
   3   12   13 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_barcode:
tcga-fg-7638-01b-01-ts1                    NA's 
                      1                     515 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_uuid:
ef29b78f-ee6a-4ef0-bc7b-aaa0a51dd1f9                                 NA's 
                                   1                                  515 

patient.samples.sample.portions.portion.2.slides.slide.2.image_file_name:
tcga-fg-7638-01b-01-ts1.ef29b78f-ee6a-4ef0-bc7b-aaa0a51dd1f9.svs 
                                                               1 
                                                            NA's 
                                                             515 

patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1  515 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_necrosis:
   0 NA's 
   1  515 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_normal_cells:
   0 NA's 
   1  515 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_stromal_cells:
   0 NA's 
   1  515 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_cells:
 100 NA's 
   1  515 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
  75 NA's 
   1  515 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location:
 top NA's 
   1  515 

patient.samples.sample.portions.portion.2.slides.slide.bcr_slide_barcode:
tcga-db-5279-01a-01-bs1 tcga-du-6395-01a-01-ts1 tcga-fg-7638-01b-01-bs1 
                      1                       1                       1 
                   NA's 
                    513 

patient.samples.sample.portions.portion.2.slides.slide.bcr_slide_uuid:
6f74a1a4-f73a-4e06-9151-a27bfc664e13 a0ae13d6-9400-45a0-86d6-4087824a3868 
                                   1                                    1 
f207492e-bdcd-4f30-bbcf-b3a3c92cc82e                                 NA's 
                                   1                                  513 

patient.samples.sample.portions.portion.2.slides.slide.image_file_name:
tcga-db-5279-01a-01-bs1.6f74a1a4-f73a-4e06-9151-a27bfc664e13.svs 
                                                               1 
tcga-du-6395-01a-01-ts1.a0ae13d6-9400-45a0-86d6-4087824a3868.svs 
                                                               1 
tcga-fg-7638-01b-01-bs1.f207492e-bdcd-4f30-bbcf-b3a3c92cc82e.svs 
                                                               1 
                                                            NA's 
                                                             513 

patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   3  513 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   0 NA's 
   3  513 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   0 NA's 
   3  513 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
   0 NA's 
   3  513 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
 100 NA's 
   3  513 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
  60   65   80 NA's 
   1    1    1  513 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
bottom    top   NA's 
     1      2    513 

patient.samples.sample.portions.portion.2.weight:
  34   36 NA's 
   1    1  514 

patient.samples.sample.portions.portion.2.year_of_creation:
2010 2011 NA's 
   1    2  513 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   1.800   1.850   1.844   1.900   2.270     131 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.323e+08 1.603e+08 3.476e+08 1.641e+08 1.076e+09         4 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
 377  135    4 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1579  0.1700  0.1800       4 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   19.00   25.00   22.27   26.00   30.00       4 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 512    4 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.426  10.000  12.000       4 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.865   5.000  12.000       4 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.280   2.000   2.210   2.593   3.400   4.270       4 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
 219  293    4 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   13.00   13.30   16.42   20.00   26.70       4 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2014       4 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
164079919 164082163 164082194 164082093 164082216 164082237       493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
  13 NA's 
  23  493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 0.16 0.17 0.18 NA's 
   4   12    5    2  493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  29 NA's 
  23  493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  23  493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   1 NA's 
  23  493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   4.000   4.478   6.000   8.000     493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a360 NA's 
  23  493 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   3.730   3.730   3.641   3.960   4.190     493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
  23  493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
  13 23.3 NA's 
   2   21  493 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
  23  493 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.323e+08 1.604e+08 3.820e+08 1.050e+09 1.076e+09         1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
 135    3  377    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1599  0.1700  0.5000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   19.00   25.00   22.28   26.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 515    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.396  10.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.804   5.000   9.000       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.340   3.000   3.063   3.200  40.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 222  293    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   18   20   80 NA's 
 135    2  375    3    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2014       1 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   14.15   20.68   38.31   44.51  402.20       1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   3                  512                    1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
 512    3    1 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1599  0.1700  0.5000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   3  513 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   3  513 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             515 
                                            NA's 
                                               1 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 515    1 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                      512                         3                         1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.230   1.390   1.388   1.550   2.290       4 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.400   8.000   8.034   8.600   9.900       4 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
        1       514         1 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 417   55    3   41 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.323e+08 1.641e+08 2.700e+08 1.678e+08 1.062e+09         6 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
 510    6 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 510    6 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00    8.00   19.00   16.69   24.00   30.00       6 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 510    6 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    7.00    5.61    7.00   11.00       6 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.573   5.000  12.000       6 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 510    6 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
 176  334    6 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 510    6 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
 117   59  126  208    6 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  124.0   161.5   174.0   170.1   184.2   213.0     340 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                 510                    6 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
 510    6 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
 176  340 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 510    6 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
 176  334    6 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             510 
                                            NA's 
                                               6 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 510    6 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
      469        41         6 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
      120        56       340 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 416   54    3   43 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9    2 NA's 
   3   74    3  436 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.323e+08 1.604e+08 3.825e+08 1.050e+09 1.076e+09         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
 163  352    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.07000 0.08000 0.09513 0.14000 0.17000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   19.00   25.00   22.25   26.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 515    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.437  10.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.781   5.000  12.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.070   3.000   2.426   3.200   3.600       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 222  293    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7   40   60 NA's 
 113   50  243  109    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2014       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.317e+08 1.539e+08 3.078e+08 1.641e+08 1.062e+09         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8 NA's 
 382  133    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.090   0.150   0.129   0.160   0.170       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   19.00   25.00   22.25   26.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 515    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.437  10.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.274   4.000   9.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   1.950   2.080   2.386   3.000   4.540       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 222  293    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   13.00   13.30   24.04   40.00   60.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2014       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.507e+08 1.678e+08 4.861e+08 1.050e+09 1.062e+09       287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   1    2    5    8 NA's 
  25    8   83  113  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0700  0.1300  0.1099  0.1500  0.1700     287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   15.00   26.00   22.34   30.00   30.00     287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 229  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   7.000   7.105  10.000  12.000     287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   3.572   5.000  12.000     287 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.700   1.070   1.500   1.767   2.000   4.540     287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
 121  108  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.30   13.40   17.68   26.70   40.00     287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2013 2014 NA's 
  30   91   24   84  287 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132268898 164054838 167780065 164468956 167822692 167822783       443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8 NA's 
  73  443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 0.08 0.09 NA's 
  37   33    3  443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   7   26   30 NA's 
   4   27   42  443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  73  443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2    4   10 NA's 
  27   42    4  443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    4.00    4.37    6.00   11.00     443 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.800   0.940   0.940   1.023   1.070   1.800     443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
  73  443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
  10 13.4   20 NA's 
   4   65    4  443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2013 2014 NA's 
   4   69  443 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167779706 167780128 167804262 167798089 167811270 167812137       505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8    9 NA's 
  10    1  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 0.08 0.09 NA's 
   4    5    2  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    7.50    8.00   16.45   30.00   30.00     505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  11  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.364   8.500  12.000     505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.364   3.500   5.000     505 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.8000  0.9400  0.9400  0.9973  1.0700  1.2100     505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
  11  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  10 13.2 13.4 NA's 
   2    1    8  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2014 2015 NA's 
  10    1  505 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-db-5278-01a-01d-a46a-09 tcga-du-5870-01a-11d-a461-08 
                           1                            1 
tcga-du-6397-01a-11d-a461-08 tcga-du-6404-01a-11d-a461-08 
                           1                            1 
                        NA's 
                         512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
77909413-9323-4916-bb9f-a8e578342333 8efa4e9e-f222-4ccb-bf34-32443183bd7f 
                                   1                                    1 
a3dce351-5357-42b1-af0d-9865b33a9608 e24bc524-4f82-4fc8-aa20-0b64c9b74762 
                                   1                                    1 
                                NA's 
                                 512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
167811270 167811272 167811802 167814084      NA's 
        1         1         1         1       512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   8    9 NA's 
   3    1  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.07 0.08 NA's 
   2    2  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   7   21 NA's 
   3    1  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   4  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1   10 NA's 
   1    3  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   1 NA's 
   4  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
a461 a46a NA's 
   3    1  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   a    c    g    h NA's 
   1    1    1    1  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
 0.7  0.8 1.07 NA's 
   2    1    1  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
   4  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
  10 13.4 NA's 
   3    1  512 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2014 2015 NA's 
   3    1  512 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.456e+08 1.539e+08 3.285e+08 1.641e+08 1.076e+09         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    5 NA's 
 352  113   50    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1524  0.1600  0.1700       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   19.00   25.00   22.25   26.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 515    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.437  10.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.767   5.000  12.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.070   1.279   1.500   2.260       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 222  293    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.670   6.700   6.700   8.399  10.000  13.300       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2014       1 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   16.49   29.86   74.50   99.85  638.00       1 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
 515    1 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
 515    1 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1524  0.1600  0.1700       1 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 515    1 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 222  293    1 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             515 
                                            NA's 
                                               1 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
 515    1 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                  515                     1 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
      435        80         1 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 418   55    3   40 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   18.00   16.22   22.00   30.00       1 

patient.samples.sample.portions.portion.is_ffpe:
  no NA's 
 515    1 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.569  10.000  12.000       1 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   11.00   11.36   11.00   51.00       1 

patient.samples.sample.portions.portion.portion_sequence:
   1    2    3    5 NA's 
 260   30    1    1  224 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 195  321 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   0 NA's 
 195  321 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5567  0.0000 30.0000     322 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   0    5   10   12 NA's 
 188    4    1    1  322 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  70.00  100.00  100.00   99.22  100.00  100.00     323 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  55.00   65.00   75.00   72.95   80.00   85.00     321 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
     3    192    321 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
 514    2 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    2    5 NA's 
 281    2    9  224 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    2    5 NA's 
 287    1    4  224 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.1465  0.0000 20.0000       4 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
 292  224 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   6.433  10.000  85.000       6 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   15.00   21.53   40.00   85.00       6 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   50.00   75.00   71.83  100.00  100.00       7 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   70.00   80.00   77.42   85.00   95.00       4 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top   NA's 
   192    322      2 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.00   30.00   31.72   34.00   46.00       4 

patient.samples.sample.portions.portion.year_of_creation:
2010 2011 2012 2013 NA's 
  34  145   61  275    1 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 428   88 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 428   88 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   20.82   21.00   61.00      88 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    3    6 NA's 
  22  397    3    1   93 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   9   14 NA's 
 170  258   88 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   5    7 NA's 
 258  170   88 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2013 2014 NA's 
 258  170   88 

patient.samples.sample.sample_type:
primary tumor          NA's 
          515             1 

patient.samples.sample.sample_type_id:
   1 NA's 
 515    1 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.4000  0.6000  0.5689  0.7000  1.0000     294 

patient.samples.sample.vial_number:
   a    b NA's 
 504   11    1 


Study:
Brain Lower Grade Glioma 
                     516 

BCR:
IGC NCH 
352 164 

Whole.exome:
Yes 
516 

Whole.genome:
 No Yes 
426  90 

RNAseq:
 No Yes 
  3 513 

SNP6:
 No Yes 
  3 513 

U133a:
 No 
516 

HM450:
Yes 
516 

HM27:
 No 
516 

RPPA:
 No Yes 
258 258 

Histology:
      astrocytoma  oligoastrocytoma oligodendroglioma              NA's 
              169               114               174                59 

Grade:
  G2   G3 NA's 
 216  241   59 

Age..years.at.diagnosis.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   33.00   41.00   43.16   53.00   87.00      59 

Gender:
female   male   NA's 
   201    256     59 

Survival..months.:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 -0.03285   5.88103  14.68614  24.70609  30.06223 211.02702        59 

Vital.status..1.dead.:
   0    1 NA's 
 377   80   59 

Karnofsky.Performance.Score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   80.00   90.00   87.71  100.00  100.00     254 

Mutation.Count:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    0.00    18.00    25.00    52.79    37.00 12255.00        3 

Percent.aneuploidy:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
0.000006 0.054296 0.102943 0.143381 0.170786 0.947728       16 

IDH.status:
Mutant     WT   NA's 
   419     94      3 

X1p.19q.codeletion:
    codel non-codel 
      169       347 

IDH.codel.subtype:
    IDHmut-codel IDHmut-non-codel            IDHwt             NA's 
             169              250               94                3 

MGMT.promoter.status:
  Methylated Unmethylated 
         425           91 

Chr.7.gain.Chr.10.loss:
Gain chr 7 & loss chr 10          No combined CNA                     NA's 
                      55                      458                        3 

Chr.19.20.co.gain:
   Gain chr 19/20 No chr 19/20 gain              NA's 
               12               501                 3 

TERT.promoter.status:
Mutant     WT   NA's 
   130    162    224 

TERT.expression..log2.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   1.957   3.700   9.077       3 

TERT.expression.status:
    Expressed Not expressed          NA's 
          224           289             3 

ATRX.status:
Mutant     WT   NA's 
   188    325      3 

DAXX.status:
Mutant     WT   NA's 
     1    512      3 

Telomere.Maintenance:
 -/- ATRX TERT NA's 
  52  110  126  228 

BRAF.V600E.status:
Mutant     WT   NA's 
     1    512      3 

BRAF.KIAA1549.fusion:
Fusion     WT   NA's 
     1    512      3 

ABSOLUTE.purity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1600  0.6100  0.7600  0.7241  0.8600  1.0000      98 

ABSOLUTE.ploidy:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.490   1.950   2.050   2.816   3.860   8.330      98 

ESTIMATE.stromal.score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -617.9   102.3   418.4   503.1   821.6  3945.0       3 

ESTIMATE.immune.score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1122.4   131.0   723.4   832.6  1464.6  4314.7       3 

ESTIMATE.combined.score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1670.0   235.8  1135.0  1335.8  2214.0  7626.8       3 

Original.Subtype:
    IDHmut-codel IDHmut-non-codel            IDHwt             NA's 
             169              249               95                3 

Transcriptome.Subtype:
  CL   ME   NE   PN NA's 
  37   31  105  220  123 

Pan.Glioma.RNA.Expression.Cluster:
LGr1 LGr2 LGr3 LGr4 NA's 
 122   86  228   77    3 






RPPA.cluster:
  K1   K2 NA's 
  26  232  258 

Telomere.length.estimate.in.blood.normal..Kb.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.419   4.184   5.139   5.286   5.957  10.640     427 

Telomere.length.estimate.in.tumor..Kb.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.4822  2.6696  5.1528  6.6422  8.9216 29.2683     429 

}}
\keyword{datasets}
