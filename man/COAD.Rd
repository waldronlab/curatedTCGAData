\name{ COAD }
\alias{ COAD }
\docType{data}
\title{ Colon adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( COAD )
ExperimentList class object of length 13: 
 [1] CNASeq-20160128: RaggedExperiment with 40530 rows and 136 columns 
 [2] CNASNP-20160128: RaggedExperiment with 459490 rows and 914 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 90534 rows and 914 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 449 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 449 columns 
 [6] Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 202 columns 
 [7] Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 333 columns 
 [8] miRNASeqGene-20160128: SummarizedExperiment with 705 rows and 221 columns 
 [9] mRNAArray-20160128: SummarizedExperiment with 17814 rows and 172 columns 
 [10] Mutation-20160128: RaggedExperiment with 62530 rows and 154 columns 
 [11] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 191 columns 
 [12] RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 10 columns 
 [13] RPPAArray-20160128: SummarizedExperiment with 208 rows and 360 columns 

> rownames( COAD )
CharacterList of length 13
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation_methyl27-20160128"]] cg00000292 cg00002426 ... cg27665659
[["Methylation_methyl450-20160128"]] cg00000029 cg00000108 ... rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 PNMA1 ... SLC39A6 SNRPD2 AQP7 CTSC
[["Mutation-20160128"]] character(0)
...
<3 more elements>

> colnames( COAD )
CharacterList of length 13
[["CNASeq-20160128"]] TCGA-A6-2671-01A-01D-1405-02 ...
[["CNASNP-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["CNVSNP-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-3L-AA1B-01A-11D-A36W-01 ...
[["Methylation_methyl27-20160128"]] TCGA-A6-2672-11A-01D-1551-05 ...
[["Methylation_methyl450-20160128"]] TCGA-3L-AA1B-01A-11D-A36Y-05 ...
[["miRNASeqGene-20160128"]] TCGA-A6-2675-01A-02T-1722-13 ...
[["mRNAArray-20160128"]] TCGA-A6-2671-11A-01R-1758-07 ...
[["Mutation-20160128"]] TCGA-A6-2672-01A-01W-0833-10 ...
...
<3 more elements>

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128  1.1 Mb
2                    CNASNP-20160128 12.5 Mb
3                    CNVSNP-20160128  2.7 Mb
4          GISTIC_AllByGene-20160128  4.4 Mb
5  GISTIC_ThresholdedByGene-20160128  4.4 Mb
6      Methylation_methyl27-20160128  4.4 Mb
7     Methylation_methyl450-20160128 67.4 Mb
8              miRNASeqGene-20160128  0.1 Mb
9                 mRNAArray-20160128    1 Mb
10                 Mutation-20160128 22.7 Mb
11          RNASeq2GeneNorm-20160128  1.1 Mb
12               RNASeqGene-20160128  1.1 Mb
13                RPPAArray-20160128    0 Mb


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   58.00   68.00   66.95   77.00   90.00       2 

vital_status.x:
  0   1 
355 102 

days_to_death.x:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   176.8   404.0   683.1  1063.5  3042.0     355 

days_to_last_followup.x:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -2.0   426.2   730.0   911.2  1092.0  4502.0     103 

tumor_tissue_site:
colon  NA's 
  456     1 





gender.x:
female   male 
   216    241 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1998    2007    2009    2008    2010    2013 

days_to_last_known_alive.x:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   624.2  2169.5  2155.3  3745.8  3924.0     447 

radiation_therapy:
  no  yes NA's 
 378    9   70 

histological_type.x:
         colon adenocarcinoma colon mucinous adenocarcinoma 
                          390                            62 
                         NA's 
                            5 

residual_tumor.x:
  r0   r1   r2   rx NA's 
 329    4   26   25   73 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.058   2.000  50.000      25 

race:
american indian or alaska native                            asian 
                               1                               11 
       black or african american                            white 
                              59                              213 
                            NA's 
                             173 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     4                    270                    183 

admin.bcr:
nationwide children's hospital 
                           457 

admin.day_of_dcc_upload:
  1  16 
456   1 

admin.disease_code:
coad 
 457 


admin.month_of_dcc_upload:
  4  10 
  1 456 

admin.patient_withdrawal.withdrawn:
false 
  457 

admin.project_code:
tcga 
 457 

admin.year_of_dcc_upload:
2014 2015 
   1  456 

patient.additional_studies.additional_study.disease_code:
fppp NA's 
  10  447 

patient.additional_studies.additional_study.project_code:
tcga NA's 
  10  447 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  31.00   58.00   68.00   66.95   77.00   90.00 



patient.braf_gene_analysis_performed:
  no  yes NA's 
 379   28   50 

patient.braf_gene_analysis_result:
abnormal   normal     NA's 
       3       25      429 

patient.circumferential_resection_margin:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    4.00   20.00   29.31   40.00  165.00     387 

patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      436        21 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-374.00    0.00    0.00   12.90   24.25 1247.00      21 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   176.0   427.0   754.8   899.0  3042.0     436 


patient.clinical_cqcf.histological_type:
         colon adenocarcinoma colon mucinous adenocarcinoma 
                          392                            64 
                         NA's 
                            1 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                               no yes, radiation prior to resection 
                              456                                 1 

patient.clinical_cqcf.history_of_prior_malignancy:
                                                     no 
                                                    395 
                       yes, history of prior malignancy 
                                                     59 
yes, history of synchronous and or bilateral malignancy 
                                                      3 


patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           80           377 

patient.clinical_cqcf.other_anatomic_site_normal_tissue:
blood  NA's 
   17   440 

patient.clinical_cqcf.tumor_type:
primary 
    457 

patient.colon_polyps_present:
  no  yes NA's 
 146   80  231 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   15.00   18.00   16.93   19.00   31.00      25 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32873  -28444  -25143  -24623  -21519  -11391       2 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   115.8   362.5   630.5   818.2  3042.0     401 

patient.days_to_initial_pathologic_diagnosis:
  0 
457 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -2       0      42     360     296    4122      46 

patient.days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   624.2  2169.5  2155.3  3745.8  3924.0     447 



patient.drugs.drug.10.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    14.5    18.5    17.0    19.0    23.0     447 

patient.drugs.drug.10.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  178.0   564.2   729.5   774.0  1003.2  1443.0     449 

patient.drugs.drug.10.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   24.0   375.2   596.0   574.2   738.2  1041.0     447 


patient.drugs.drug.10.measure_of_response:
clinical progressive disease             partial response 
                           1                            1 
                        NA's 
                         455 

patient.drugs.drug.10.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     1.0     3.0     4.5     7.0    11.0     447 

patient.drugs.drug.10.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    7.75   11.00    9.50   12.00   13.00     451 


patient.drugs.drug.10.prescribed_dose_units:
   mg mg/m2  NA's 
    5     1   451 

patient.drugs.drug.10.regimen_indication:
   adjuvant progression  recurrence        NA's 
          1           5           1         450 

patient.drugs.drug.10.regimen_number:
   1    2    3 NA's 
   2    4    1  450 

patient.drugs.drug.10.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               5              452 

patient.drugs.drug.10.therapy_ongoing:
  no  yes NA's 
   8    2  447 

patient.drugs.drug.10.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         9                          1 
                      NA's 
                       447 

patient.drugs.drug.10.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    715    2981    6536    6215    9025   11850     451 

patient.drugs.drug.10.total_dose_units:
   mg mg/m2  NA's 
    5     1   451 

patient.drugs.drug.10.tx_on_clinical_trial:
  no NA's 
   3  454 

patient.drugs.drug.10.year_of_form_completion:
2010 2011 2012 2014 NA's 
   1    2    6    1  447 



patient.drugs.drug.11.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   12.50   18.00   15.86   19.00   23.00     450 

patient.drugs.drug.11.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  350.0   474.8   740.0   737.3   758.5  1443.0     451 

patient.drugs.drug.11.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  301.0   443.5   641.0   588.6   697.5   896.0     450 


patient.drugs.drug.11.measure_of_response:
clinical progressive disease             partial response 
                           1                            1 
                        NA's 
                         455 

patient.drugs.drug.11.month_of_form_completion:
   1    6    7   10 NA's 
   3    1    2    1  450 

patient.drugs.drug.11.number_cycles:
   3   23 NA's 
   2    1  454 

patient.drugs.drug.11.prescribed_dose:
 140  500  760 NA's 
   1    1    1  454 

patient.drugs.drug.11.prescribed_dose_units:
  mg NA's 
   3  454 

patient.drugs.drug.11.regimen_indication:
progression        NA's 
          4         453 

patient.drugs.drug.11.regimen_number:
   2 NA's 
   4  453 

patient.drugs.drug.11.route_of_administrations.route_of_administration:
intravenous (iv)     intravesical             NA's 
               2                1              454 

patient.drugs.drug.11.therapy_ongoing:
  no  yes NA's 
   6    1  450 

patient.drugs.drug.11.therapy_types.therapy_type:
              chemotherapy             other, specify 
                         5                          1 
targeted molecular therapy                       NA's 
                         1                        450 

patient.drugs.drug.11.therapy_types.therapy_type_notes:
ancillary      NA's 
        1       456 

patient.drugs.drug.11.total_dose:
  420  1500 11210  NA's 
    1     1     1   454 

patient.drugs.drug.11.total_dose_units:
  mg NA's 
   3  454 

patient.drugs.drug.11.tx_on_clinical_trial:
  no NA's 
   3  454 

patient.drugs.drug.11.year_of_form_completion:
2010 2011 2012 2014 NA's 
   1    1    4    1  450 



patient.drugs.drug.12.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.0     9.0    16.0    14.8    19.0    23.0     452 

patient.drugs.drug.12.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    635     756     883    1010    1181    1596     452 

patient.drugs.drug.12.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  301.0   551.0   760.0   819.2  1041.0  1443.0     452 


patient.drugs.drug.12.measure_of_response:
clinical progressive disease             partial response 
                           1                            1 
                        NA's 
                         455 

patient.drugs.drug.12.month_of_form_completion:
   1    6    7 NA's 
   3    1    1  452 

patient.drugs.drug.12.number_cycles:
  10   13 NA's 
   1    1  455 

patient.drugs.drug.12.prescribed_dose:
4300-4325       445      NA's 
        1         1       455 

patient.drugs.drug.12.prescribed_dose_units:
  mg NA's 
   2  455 

patient.drugs.drug.12.regimen_indication:
progression  recurrence        NA's 
          2           1         454 

patient.drugs.drug.12.regimen_number:
   2    3 NA's 
   1    2  454 

patient.drugs.drug.12.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              455 

patient.drugs.drug.12.therapy_ongoing:
  no NA's 
   5  452 

patient.drugs.drug.12.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           1            4          452 

patient.drugs.drug.12.total_dose:
 4310 56000  NA's 
    1     1   455 

patient.drugs.drug.12.total_dose_units:
  mg NA's 
   2  455 

patient.drugs.drug.12.tx_on_clinical_trial:
  no NA's 
   2  455 

patient.drugs.drug.12.year_of_form_completion:
2011 2012 2014 NA's 
   1    3    1  452 



patient.drugs.drug.13.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      7      16      19      19      23      30     452 

patient.drugs.drug.13.days_to_drug_therapy_end:
 635  739 1596 1636 NA's 
   1    1    1    1  453 

patient.drugs.drug.13.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  301.0   551.0  1161.0   966.6  1377.0  1443.0     452 


patient.drugs.drug.13.measure_of_response:
clinical progressive disease                         NA's 
                           1                          456 

patient.drugs.drug.13.month_of_form_completion:
   1    6    7    9 NA's 
   2    1    1    1  452 

patient.drugs.drug.13.number_cycles:
  12   13 NA's 
   1    1  455 

patient.drugs.drug.13.prescribed_dose:
300-325    3600    NA's 
      1       1     455 

patient.drugs.drug.13.prescribed_dose_units:
    mg mg/day   NA's 
     1      1    455 

patient.drugs.drug.13.regimen_indication:
progression        NA's 
          3         454 

patient.drugs.drug.13.regimen_number:
   2    3    4 NA's 
   1    1    1  454 

patient.drugs.drug.13.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                1              455 

patient.drugs.drug.13.therapy_ongoing:
  no  yes NA's 
   4    1  452 

patient.drugs.drug.13.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         4                          1 
                      NA's 
                       452 

patient.drugs.drug.13.total_dose:
  3775 655200   NA's 
     1      1    455 

patient.drugs.drug.13.total_dose_units:
  mg NA's 
   2  455 

patient.drugs.drug.13.tx_on_clinical_trial:
  no NA's 
   2  455 

patient.drugs.drug.13.year_of_form_completion:
2011 2012 2014 NA's 
   1    3    1  452 



patient.drugs.drug.14.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      7      16      19      19      23      30     452 

patient.drugs.drug.14.days_to_drug_therapy_end:
 103  637  711 1994 NA's 
   1    1    1    1  453 

patient.drugs.drug.14.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   33.0   301.0   551.0   777.2  1161.0  1840.0     452 


patient.drugs.drug.14.measure_of_response:
clinical progressive disease                         NA's 
                           1                          456 

patient.drugs.drug.14.month_of_form_completion:
   1    6    7    9 NA's 
   2    1    1    1  452 

patient.drugs.drug.14.number_cycles:
   9   10 NA's 
   1    1  455 

patient.drugs.drug.14.prescribed_dose:
300-320     325       5    NA's 
      1       1       1     454 

patient.drugs.drug.14.prescribed_dose_units:
   mg mg/kg  NA's 
    2     1   454 

patient.drugs.drug.14.regimen_indication:
   adjuvant progression        NA's 
          1           2         454 

patient.drugs.drug.14.regimen_number:
   1    2    5 NA's 
   1    1    1  454 

patient.drugs.drug.14.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               3              454 

patient.drugs.drug.14.therapy_ongoing:
  no  yes NA's 
   4    1  452 

patient.drugs.drug.14.therapy_types.therapy_type:
chemotherapy         NA's 
           5          452 

patient.drugs.drug.14.total_dose:
2890 3240 NA's 
   1    1  455 

patient.drugs.drug.14.total_dose_units:
  mg NA's 
   2  455 

patient.drugs.drug.14.tx_on_clinical_trial:
  no NA's 
   2  455 

patient.drugs.drug.14.year_of_form_completion:
2011 2012 2014 NA's 
   1    3    1  452 

patient.drugs.drug.15.bcr_drug_barcode:
tcga-a6-2671-d12770 tcga-f4-6806-d65813 tcga-g4-6297-d22071 tcga-g4-6303-d21794 
                  1                   1                   1                   1 
               NA's 
                453 

patient.drugs.drug.15.bcr_drug_uuid:
3bae1404-d8d7-407f-b349-e6868293cd6b 5ceb94a7-d009-4796-a6b3-e5d560cc9b81 
                                   1                                    1 
8425101a-756c-410d-81a8-2076b80de3bb 9859896f-c740-4094-96ad-102b312c4b6b 
                                   1                                    1 
                                NA's 
                                 453 

patient.drugs.drug.15.day_of_form_completion:
   7   19   23   30 NA's 
   1    1    1    1  453 

patient.drugs.drug.15.days_to_drug_therapy_end:
 103  754 1994 NA's 
   1    1    1  454 

patient.drugs.drug.15.days_to_drug_therapy_start:
  33  551 1161 1840 NA's 
   1    1    1    1  453 

patient.drugs.drug.15.drug_name:
       5 fu        5-fu bevacizumab oxaliplatin        NA's 
          1           1           1           1         453 

patient.drugs.drug.15.month_of_form_completion:
   1    6    9 NA's 
   2    1    1  453 

patient.drugs.drug.15.number_cycles:
  10   13 NA's 
   1    1  455 

patient.drugs.drug.15.prescribed_dose:
  85  725 NA's 
   1    1  455 

patient.drugs.drug.15.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1   455 

patient.drugs.drug.15.regimen_indication:
   adjuvant progression        NA's 
          1           2         454 

patient.drugs.drug.15.regimen_number:
   1    2    5 NA's 
   1    1    1  454 

patient.drugs.drug.15.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               3              454 

patient.drugs.drug.15.therapy_ongoing:
  no  yes NA's 
   3    1  453 

patient.drugs.drug.15.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         3                          1 
                      NA's 
                       453 

patient.drugs.drug.15.total_dose:
9425 NA's 
   1  456 

patient.drugs.drug.15.total_dose_units:
  mg NA's 
   1  456 

patient.drugs.drug.15.tx_on_clinical_trial:
  no NA's 
   1  456 

patient.drugs.drug.15.year_of_form_completion:
2011 2012 2014 NA's 
   1    2    1  453 

patient.drugs.drug.16.bcr_drug_barcode:
tcga-a6-2671-d31097                NA's 
                  1                 456 

patient.drugs.drug.16.bcr_drug_uuid:
3254ea81-298b-461f-9e51-2daa00fb9c60                                 NA's 
                                   1                                  456 

patient.drugs.drug.16.day_of_form_completion:
  24 NA's 
   1  456 

patient.drugs.drug.16.days_to_drug_therapy_end:
 984 NA's 
   1  456 

patient.drugs.drug.16.days_to_drug_therapy_start:
 844 NA's 
   1  456 

patient.drugs.drug.16.drug_name:
irinotecan       NA's 
         1        456 

patient.drugs.drug.16.measure_of_response:
clinical progressive disease                         NA's 
                           1                          456 

patient.drugs.drug.16.month_of_form_completion:
   4 NA's 
   1  456 

patient.drugs.drug.16.therapy_ongoing:
  no NA's 
   1  456 

patient.drugs.drug.16.therapy_types.therapy_type:
chemotherapy         NA's 
           1          456 

patient.drugs.drug.16.tx_on_clinical_trial:
  no NA's 
   1  456 

patient.drugs.drug.16.year_of_form_completion:
2012 NA's 
   1  456 

patient.drugs.drug.17.bcr_drug_barcode:
tcga-a6-2671-d31177                NA's 
                  1                 456 

patient.drugs.drug.17.bcr_drug_uuid:
28b4628d-26ff-4ffe-998d-3a1de2be56b8                                 NA's 
                                   1                                  456 

patient.drugs.drug.17.day_of_form_completion:
  24 NA's 
   1  456 

patient.drugs.drug.17.days_to_drug_therapy_end:
 844 NA's 
   1  456 

patient.drugs.drug.17.days_to_drug_therapy_start:
 844 NA's 
   1  456 

patient.drugs.drug.17.drug_name:
5-fu NA's 
   1  456 

patient.drugs.drug.17.measure_of_response:
clinical progressive disease                         NA's 
                           1                          456 

patient.drugs.drug.17.month_of_form_completion:
   4 NA's 
   1  456 

patient.drugs.drug.17.therapy_ongoing:
  no NA's 
   1  456 

patient.drugs.drug.17.therapy_types.therapy_type:
chemotherapy         NA's 
           1          456 

patient.drugs.drug.17.tx_on_clinical_trial:
  no NA's 
   1  456 

patient.drugs.drug.17.year_of_form_completion:
2012 NA's 
   1  456 

patient.drugs.drug.18.bcr_drug_barcode:
tcga-a6-2671-d31178                NA's 
                  1                 456 

patient.drugs.drug.18.bcr_drug_uuid:
9ef7c5d5-470f-4586-8ff2-adb6b1db8667                                 NA's 
                                   1                                  456 

patient.drugs.drug.18.day_of_form_completion:
  24 NA's 
   1  456 

patient.drugs.drug.18.days_to_drug_therapy_end:
 844 NA's 
   1  456 

patient.drugs.drug.18.days_to_drug_therapy_start:
 844 NA's 
   1  456 

patient.drugs.drug.18.drug_name:
leucovorin       NA's 
         1        456 

patient.drugs.drug.18.measure_of_response:
clinical progressive disease                         NA's 
                           1                          456 

patient.drugs.drug.18.month_of_form_completion:
   4 NA's 
   1  456 

patient.drugs.drug.18.therapy_ongoing:
  no NA's 
   1  456 

patient.drugs.drug.18.therapy_types.therapy_type:
ancillary      NA's 
        1       456 

patient.drugs.drug.18.tx_on_clinical_trial:
  no NA's 
   1  456 

patient.drugs.drug.18.year_of_form_completion:
2012 NA's 
   1  456 

patient.drugs.drug.19.bcr_drug_barcode:
tcga-a6-2671-d31179                NA's 
                  1                 456 

patient.drugs.drug.19.bcr_drug_uuid:
0b9b3648-918f-4f14-a9c8-5cd58d935ed7                                 NA's 
                                   1                                  456 

patient.drugs.drug.19.day_of_form_completion:
  24 NA's 
   1  456 

patient.drugs.drug.19.days_to_drug_therapy_end:
 846 NA's 
   1  456 

patient.drugs.drug.19.days_to_drug_therapy_start:
 844 NA's 
   1  456 

patient.drugs.drug.19.drug_name:
5-fu NA's 
   1  456 

patient.drugs.drug.19.measure_of_response:
clinical progressive disease                         NA's 
                           1                          456 

patient.drugs.drug.19.month_of_form_completion:
   4 NA's 
   1  456 

patient.drugs.drug.19.therapy_ongoing:
  no NA's 
   1  456 

patient.drugs.drug.19.therapy_types.therapy_type:
chemotherapy         NA's 
           1          456 

patient.drugs.drug.19.tx_on_clinical_trial:
  no NA's 
   1  456 

patient.drugs.drug.19.year_of_form_completion:
2012 NA's 
   1  456 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00   11.50   12.89   19.75   31.00     335 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   41.0   189.0   221.0   280.9   258.0  1838.0     344 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    40.0    61.0   117.1    92.0  1596.0     336 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                          14                           36 
            partial response               stable disease 
                           7                            1 
                        NA's 
                         399 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   6.975  12.000  12.000     335 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.250  12.000   9.037  12.000  16.000     403 


patient.drugs.drug.2.prescribed_dose_units:
g/day    mg mg/m2    ug  NA's 
    1    45     6     1   404 

patient.drugs.drug.2.regimen_indication:
               adjuvant other: specify in notes              palliative 
                     47                       2                       3 
            progression                    NA's 
                      4                     401 

patient.drugs.drug.2.regimen_indication_notes:
metastatic       NA's 
         2        455 

patient.drugs.drug.2.regimen_number:
   1    2    3    4 NA's 
  49    2    2    1  403 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
 intravenous (iv)              oral subcutaneous (sc)              NA's 
               51                 2                 1               403 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
 116    6  335 



patient.drugs.drug.2.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     40    1595    6482   11786   10122   60300     409 

patient.drugs.drug.2.total_dose_units:
g/day    mg mg/m2    ug  NA's 
    1    45     1     1   409 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  64    2  391 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     335 

patient.drugs.drug.20.bcr_drug_barcode:
tcga-a6-2671-d31180                NA's 
                  1                 456 

patient.drugs.drug.20.bcr_drug_uuid:
386e2918-33c5-4df0-a196-28864d9a87e3                                 NA's 
                                   1                                  456 

patient.drugs.drug.20.day_of_form_completion:
  24 NA's 
   1  456 

patient.drugs.drug.20.days_to_drug_therapy_end:
 984 NA's 
   1  456 

patient.drugs.drug.20.days_to_drug_therapy_start:
 844 NA's 
   1  456 

patient.drugs.drug.20.drug_name:
avastin    NA's 
      1     456 

patient.drugs.drug.20.measure_of_response:
clinical progressive disease                         NA's 
                           1                          456 

patient.drugs.drug.20.month_of_form_completion:
   4 NA's 
   1  456 

patient.drugs.drug.20.therapy_ongoing:
  no NA's 
   1  456 

patient.drugs.drug.20.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        456 

patient.drugs.drug.20.tx_on_clinical_trial:
  no NA's 
   1  456 

patient.drugs.drug.20.year_of_form_completion:
2012 NA's 
   1  456 

patient.drugs.drug.21.bcr_drug_barcode:
tcga-a6-2671-d31182                NA's 
                  1                 456 

patient.drugs.drug.21.bcr_drug_uuid:
b57bfe0a-f932-417b-b510-dc6c1800c1a7                                 NA's 
                                   1                                  456 

patient.drugs.drug.21.day_of_form_completion:
  24 NA's 
   1  456 

patient.drugs.drug.21.days_to_drug_therapy_end:
1105 NA's 
   1  456 

patient.drugs.drug.21.days_to_drug_therapy_start:
1041 NA's 
   1  456 

patient.drugs.drug.21.drug_name:
irinotecan       NA's 
         1        456 

patient.drugs.drug.21.measure_of_response:
clinical progressive disease                         NA's 
                           1                          456 

patient.drugs.drug.21.month_of_form_completion:
   4 NA's 
   1  456 

patient.drugs.drug.21.therapy_ongoing:
  no NA's 
   1  456 

patient.drugs.drug.21.therapy_types.therapy_type:
chemotherapy         NA's 
           1          456 

patient.drugs.drug.21.tx_on_clinical_trial:
  no NA's 
   1  456 

patient.drugs.drug.21.year_of_form_completion:
2012 NA's 
   1  456 

patient.drugs.drug.22.bcr_drug_barcode:
tcga-a6-2671-d31184                NA's 
                  1                 456 

patient.drugs.drug.22.bcr_drug_uuid:
f8603c4a-b9ed-4efa-a9d1-07b84ae5c330                                 NA's 
                                   1                                  456 

patient.drugs.drug.22.day_of_form_completion:
  24 NA's 
   1  456 

patient.drugs.drug.22.days_to_drug_therapy_end:
1091 NA's 
   1  456 

patient.drugs.drug.22.days_to_drug_therapy_start:
1041 NA's 
   1  456 

patient.drugs.drug.22.drug_name:
erbitux    NA's 
      1     456 

patient.drugs.drug.22.measure_of_response:
clinical progressive disease                         NA's 
                           1                          456 

patient.drugs.drug.22.month_of_form_completion:
   4 NA's 
   1  456 

patient.drugs.drug.22.therapy_ongoing:
  no NA's 
   1  456 

patient.drugs.drug.22.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        456 

patient.drugs.drug.22.tx_on_clinical_trial:
  no NA's 
   1  456 

patient.drugs.drug.22.year_of_form_completion:
2012 NA's 
   1  456 



patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0    12.0    13.4    21.0    31.0     344 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   54.0   196.8   232.5   292.6   276.8  1369.0     353 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   41.75   61.00  141.69  112.50 1377.00     345 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                          13                           30 
            partial response               stable disease 
                           5                            3 
                        NA's 
                         406 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   6.956  12.000  12.000     344 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   11.00   10.11   12.00   61.00     402 


patient.drugs.drug.3.prescribed_dose_units:
    mg mg/day  mg/kg  mg/m2   NA's 
    47      1      1      5    403 

patient.drugs.drug.3.regimen_indication:
               adjuvant other: specify in notes              palliative 
                     45                       2                       2 
            progression                    NA's 
                      7                     401 

patient.drugs.drug.3.regimen_indication_notes:
metastatic       NA's 
         2        455 

patient.drugs.drug.3.regimen_number:
   1    2    3    4 NA's 
  46    5    2    1  403 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
     intrahepatic  intravenous (iv) subcutaneous (sc)              NA's 
                1                51                 3               402 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
 106    7  344 

patient.drugs.drug.3.therapy_types.therapy_type:
     ancillary   chemotherapy  immunotherapy other, specify           NA's 
             5             85              4             19            344 

patient.drugs.drug.3.therapy_types.therapy_type_notes:
               ancillary          ancillary agent phase iii clinical trial 
                      14                        3                        1 
                    NA's 
                     439 

patient.drugs.drug.3.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     42    1140    3455    9947    9934  101059     407 

patient.drugs.drug.3.total_dose_units:
    mg mg/day  mg/kg  mg/m2   NA's 
    47      1      1      1    407 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  55    2  400 

patient.drugs.drug.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     344 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.25   15.00   15.33   23.25   31.00     393 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   76.0   209.5   240.0   339.1   303.0  1994.0     402 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    49.5    76.0   184.3   139.0  1840.0     394 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                          11                            5 
            partial response               stable disease 
                           3                            1 
                        NA's 
                         437 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.750   8.500   7.141  11.000  12.000     393 

patient.drugs.drug.4.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.25   12.00   10.11   12.00   23.00     419 


patient.drugs.drug.4.prescribed_dose_units:
   mg mg/m2  NA's 
   35     1   421 

patient.drugs.drug.4.regimen_indication:
               adjuvant other: specify in notes              palliative 
                     33                       2                       2 
            progression                    NA's 
                      2                     418 

patient.drugs.drug.4.regimen_indication_notes:
metastatic       NA's 
         2        455 

patient.drugs.drug.4.regimen_number:
   1    2    5 NA's 
  35    3    1  418 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
              38              419 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
  56    8  393 


patient.drugs.drug.4.therapy_types.therapy_type_notes:
               ancillary          ancillary agent           clinical trial 
                       6                        2                        1 
phase iii clinical trial                     NA's 
                       1                      447 

patient.drugs.drug.4.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    185    1137    1865    8051    7935   64992     423 

patient.drugs.drug.4.total_dose_units:
  mg NA's 
  35  422 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
  23    2  432 

patient.drugs.drug.4.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     393 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    8.00   16.50   15.79   22.50   31.00     423 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  106.0   237.0   273.0   500.4   699.0  1443.0     428 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0    86.0   203.0   366.1   358.0  2450.0     423 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease            complete response 
                           6                            4 
            partial response               stable disease 
                           2                            2 
                        NA's 
                         443 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   8.000   6.882  10.750  12.000     423 

patient.drugs.drug.5.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.500  10.000   9.733  12.000  23.000     442 


patient.drugs.drug.5.prescribed_dose_units:
  mg NA's 
  14  443 

patient.drugs.drug.5.regimen_indication:
               adjuvant other: specify in notes              palliative 
                      9                       1                       2 
            progression                    NA's 
                      4                     441 

patient.drugs.drug.5.regimen_indication_notes:
metastatic       NA's 
         1        456 

patient.drugs.drug.5.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    1.00    1.75    2.00    6.00     441 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
              15              442 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
  29    5  423 

patient.drugs.drug.5.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                        19                          2 
            other, specify targeted molecular therapy 
                         5                          8 
                      NA's 
                       423 

patient.drugs.drug.5.therapy_types.therapy_type_notes:
      ancillary ancillary agent            NA's 
              3               2             452 

patient.drugs.drug.5.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    300    1860    3875    6125    6974   21800     443 

patient.drugs.drug.5.total_dose_units:
  mg NA's 
  14  443 

patient.drugs.drug.5.tx_on_clinical_trial:
  no  yes NA's 
  16    2  439 

patient.drugs.drug.5.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     423 



patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     8.0    15.0    14.7    20.0    29.0     434 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  113.0   248.5   350.0   507.7   721.5  1369.0     438 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   125.5   301.0   377.8   533.5  1246.0     434 


patient.drugs.drug.6.measure_of_response:
clinical progressive disease            complete response 
                           4                            2 
            partial response               stable disease 
                           1                            2 
                        NA's 
                         448 

patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.500   7.000   6.348  10.500  12.000     434 

patient.drugs.drug.6.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   7.154  10.000  20.000     444 


patient.drugs.drug.6.prescribed_dose_units:
  mg NA's 
  11  446 


patient.drugs.drug.6.regimen_indication_notes:
metastatic       NA's 
         1        456 

patient.drugs.drug.6.regimen_number:
   1    2    3 NA's 
   5    6    2  444 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              11                1              445 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
  19    4  434 

patient.drugs.drug.6.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         1                         16 
            other, specify targeted molecular therapy 
                         1                          5 
                      NA's 
                       434 

patient.drugs.drug.6.therapy_types.therapy_type_notes:
ancillary      NA's 
        1       456 

patient.drugs.drug.6.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  339.0   720.8  1320.0  1546.2  1706.8  5075.0     447 

patient.drugs.drug.6.total_dose_units:
  mg NA's 
  11  446 

patient.drugs.drug.6.tx_on_clinical_trial:
  no NA's 
  10  447 

patient.drugs.drug.6.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     434 



patient.drugs.drug.7.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   13.00   19.00   18.33   24.00   31.00     442 

patient.drugs.drug.7.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   76.0   277.8   373.5   742.6   969.2  2450.0     445 

patient.drugs.drug.7.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     34     231     337     605     790    2450     442 


patient.drugs.drug.7.measure_of_response:
clinical progressive disease            complete response 
                           2                            1 
              stable disease                         NA's 
                           1                          453 

patient.drugs.drug.7.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   4.267   7.500  11.000     442 

patient.drugs.drug.7.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    5.50    6.60   10.75   12.00     447 


patient.drugs.drug.7.prescribed_dose_units:
  mg NA's 
   9  448 

patient.drugs.drug.7.regimen_indication:
   adjuvant  palliative progression  recurrence        NA's 
          3           1           5           1         447 

patient.drugs.drug.7.regimen_number:
   1    2    4    6 NA's 
   3    3    3    1  447 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               9              448 

patient.drugs.drug.7.therapy_ongoing:
  no  yes NA's 
  13    2  442 

patient.drugs.drug.7.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         1                         10 
            other, specify targeted molecular therapy 
                         1                          3 
                      NA's 
                       442 

patient.drugs.drug.7.therapy_types.therapy_type_notes:
ancillary      NA's 
        1       456 

patient.drugs.drug.7.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    792    1085    2093    3669    3200   17812     448 

patient.drugs.drug.7.total_dose_units:
  mg NA's 
   9  448 

patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   5  452 

patient.drugs.drug.7.year_of_form_completion:
2010 2011 2012 2014 NA's 
   1    4    8    2  442 



patient.drugs.drug.8.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   19.00   17.23   21.00   31.00     444 

patient.drugs.drug.8.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  108.0   273.8   480.5   519.0   701.5  1181.0     445 

patient.drugs.drug.8.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    96.0   366.0   379.5   516.0  1041.0     444 


patient.drugs.drug.8.measure_of_response:
clinical progressive disease             partial response 
                           1                            1 
              stable disease                         NA's 
                           1                          454 

patient.drugs.drug.8.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   4.154   7.000  11.000     444 

patient.drugs.drug.8.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000  10.000   9.667  12.000  20.000     448 


patient.drugs.drug.8.prescribed_dose_units:
  mg NA's 
   8  449 

patient.drugs.drug.8.regimen_indication:
   adjuvant  palliative progression  recurrence        NA's 
          2           2           3           2         448 

patient.drugs.drug.8.regimen_number:
   1    2    3 NA's 
   5    1    3  448 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               9              448 

patient.drugs.drug.8.therapy_ongoing:
  no  yes NA's 
  12    1  444 

patient.drugs.drug.8.therapy_types.therapy_type:
  chemotherapy other, specify           NA's 
            12              1            444 

patient.drugs.drug.8.therapy_types.therapy_type_notes:
ancillary      NA's 
        1       456 

patient.drugs.drug.8.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     16    5793    8168   15314   18003   59112     449 

patient.drugs.drug.8.total_dose_units:
  mg NA's 
   8  449 

patient.drugs.drug.8.tx_on_clinical_trial:
  no NA's 
   4  453 

patient.drugs.drug.8.year_of_form_completion:
2010 2011 2012 2014 NA's 
   1    4    6    2  444 



patient.drugs.drug.9.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    15.0    19.0    18.5    20.5    31.0     447 

patient.drugs.drug.9.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  272.0   289.0   699.0   858.2   944.0  2373.0     448 

patient.drugs.drug.9.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   48.0   257.5   465.5   617.5   731.0  1687.0     447 


patient.drugs.drug.9.measure_of_response:
partial response   stable disease             NA's 
               1                1              455 

patient.drugs.drug.9.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    3.00    4.60    7.75   11.00     447 

patient.drugs.drug.9.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   12.00   12.50   17.00   15.25   45.00     451 


patient.drugs.drug.9.prescribed_dose_units:
  mg NA's 
   5  452 

patient.drugs.drug.9.regimen_indication:
   adjuvant  palliative progression  recurrence        NA's 
          1           2           3           1         450 

patient.drugs.drug.9.regimen_number:
   1    2    3    5 NA's 
   3    2    1    1  450 

patient.drugs.drug.9.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               5              452 

patient.drugs.drug.9.therapy_ongoing:
  no  yes NA's 
   9    1  447 

patient.drugs.drug.9.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           2            8          447 

patient.drugs.drug.9.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    600    9852   13386   25463   42950   60525     452 

patient.drugs.drug.9.total_dose_units:
  mg NA's 
   5  452 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   3  454 

patient.drugs.drug.9.year_of_form_completion:
2010 2011 2012 2014 NA's 
   1    3    5    1  447 



patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00   12.00   13.16   20.00   31.00     304 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0   189.8   221.0   275.0   273.0  1994.0     321 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   37.25   60.00  102.33   85.00 1840.00     307 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          17                           45 
            partial response               stable disease 
                          10                            3 
                        NA's 
                         382 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   6.634  11.000  12.000     304 





patient.drugs.drug.regimen_indication_notes:
metastatic       NA's 
         2        455 


patient.drugs.drug.route_of_administrations.route_of_administration:
 intravenous (iv)      intravesical              oral subcutaneous (sc) 
               62                 1                 5                 1 
             NA's 
              388 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 143   10  304 

patient.drugs.drug.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         3                        136 
            other, specify targeted molecular therapy 
                        11                          3 
                      NA's 
                       304 

patient.drugs.drug.therapy_types.therapy_type_notes:
               ancillary          ancillary agent phase iii clinical trial 
                       6                        1                        1 
   protocol amg 20060464                     NA's 
                       1                      448 


patient.drugs.drug.total_dose_units:
   mg mg/kg mg/m2  NA's 
   52     1     4   400 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  79    4  374 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2012    2012    2012    2015     304 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     4                    270                    183 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   18.00   17.45   25.00   31.00     328 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  153.0   446.2   741.5   926.3  1087.5  2532.0     447 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -9.0   457.0   704.0   919.7  1009.0  4502.0     338 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             9                            109 
                          NA's 
                           339 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response  partial remission/response 
                         60                           1 
        progressive disease              stable disease 
                          5                           2 
                       NA's 
                        389 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  73    8  376 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.636   7.000  12.000     328 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   6   11  440 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  16    4  437 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0   244.0   948.0   927.4   990.0  2394.0     452 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   492.2   628.0   799.6   955.0  2394.0     437 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
locoregional disease           metastatic                 NA's 
                   2                    3                  452 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  16    5  436 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   2  455 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.site_of_additional_surgery_new_tumor_event_mets:
liver other  NA's 
    2     1   454 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 100   20  337 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        96         20        341 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  49   51  357 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                         58                           1 
             stable disease                        NA's 
                          1                         397 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  98    2  357 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
  117    10   330 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2012    2013    2014    2015     328 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   16.50   27.00   21.87   29.00   30.00     434 

patient.follow_ups.follow_up.3.days_to_death:
 405  743 1305 NA's 
   1    1    1  454 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    405     915    1260    1264    1387    3324     436 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             3                             19 
                          NA's 
                           435 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response                        NA's 
                         11                         446 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
  19  438 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   5.348   9.500  12.000     434 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    5  450 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   9  448 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   80.0   661.0   970.0   847.7  1132.0  1219.0     448 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no NA's 
   9  448 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.site_of_additional_surgery_new_tumor_event_mets:
lung NA's 
   1  456 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  14    9  434 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        16          5        436 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no  yes NA's 
   9   10  438 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response                        NA's 
                         14                         443 

patient.follow_ups.follow_up.3.radiation_therapy:
  no NA's 
  19  438 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
   20     3   434 

patient.follow_ups.follow_up.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2013    2013    2013    2015     434 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-a6-2671-f36756                NA's 
                  1                 456 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
1d1f813a-9e27-4121-9e12-6be5520aad63                                 NA's 
                                   1                                  456 

patient.follow_ups.follow_up.4.day_of_form_completion:
  29 NA's 
   1  456 

patient.follow_ups.follow_up.4.days_to_death:
1331 NA's 
   1  456 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
additional new tumor event                       NA's 
                         1                        456 

patient.follow_ups.follow_up.4.followup_treatment_success:
progressive disease                NA's 
                  1                 456 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   1  456 

patient.follow_ups.follow_up.4.month_of_form_completion:
  11 NA's 
   1  456 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   1  456 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   1  456 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
1215 NA's 
   1  456 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  456 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  456 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
with tumor       NA's 
         1        456 

patient.follow_ups.follow_up.4.vital_status:
dead NA's 
   1  456 

patient.follow_ups.follow_up.4.year_of_form_completion:
2012 NA's 
   1  456 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00   10.00   13.61   24.00   31.00      46 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   29.0   239.0   426.0   726.5  1094.5  3042.0     378 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -2.0   362.0   552.0   818.6  1035.0  4270.0     112 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             7                            323 
                          NA's 
                           127 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response  partial remission/response 
                        123                          11 
        progressive disease              stable disease 
                         32                           3 
                       NA's 
                        288 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 213    6  238 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   9.000   7.874  12.000  12.000      36 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no NA's 
   1  456 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   1  456 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 514 NA's 
   1  456 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 493 NA's 
   1  456 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
metastatic       NA's 
         1        456 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  456 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.site_of_additional_surgery_new_tumor_event_mets:
liver  NA's 
    1   456 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  20   27  410 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  44    9  404 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0   367.8   482.0   619.6   605.0  2270.0     443 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0   280.2   396.0   561.0   732.0  2564.0     381 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
locoregional disease           metastatic                 NA's 
                   7                    7                  443 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  63   14  380 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r2   rx NA's 
   4    3    2  448 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.site_of_additional_surgery_new_tumor_event_mets:
liver other  NA's 
    9     3   445 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 308   74   75 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       275         76        106 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
 245  143   69 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        135                          11 
        progressive disease              stable disease 
                         24                           3 
                       NA's 
                        284 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 377    9   71 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  339    80    38 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015      36 

patient.gender:
female   male 
   216    241 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   80.3   162.0   170.0   168.4   176.0   193.0     225 

patient.histological_type:
         colon adenocarcinoma colon mucinous adenocarcinoma 
                          390                            62 
                         NA's 
                            5 

patient.history_of_colon_polyps:
  no  yes NA's 
 251  137   69 

patient.history_of_neoadjuvant_treatment:
 no yes 
454   3 




patient.informed_consent_verified:
yes 
457 

patient.kras_gene_analysis_performed:
  no  yes NA's 
 372   48   37 

patient.kras_mutation_codon:
  12   13 NA's 
  17    4  436 

patient.kras_mutation_found:
  no  yes NA's 
  24   22  411 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc:
  no  yes NA's 
 285   56  116 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc_results.loss_expression_of_mismatch_repair_proteins_by_ihc_result:
    mlh1 expressed mlh1 not expressed               NA's 
                36                 22                399 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc_results.loss_expression_of_mismatch_repair_proteins_by_ihc_result.2:
    msh2 expressed msh2 not expressed     pms2 expressed               NA's 
                39                 18                  1                399 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc_results.loss_expression_of_mismatch_repair_proteins_by_ihc_result.3:
    msh6 expressed msh6 not expressed     pms2 expressed pms2 not expressed 
                 1                  2                 28                 25 
              NA's 
               401 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc_results.loss_expression_of_mismatch_repair_proteins_by_ihc_result.4:
    msh6 expressed msh6 not expressed               NA's 
                36                 16                405 

patient.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   14.00   19.50   22.39   27.25  109.00      21 

patient.lymphatic_invasion:
  no  yes NA's 
 249  164   44 

patient.microsatellite_instability:
  no  yes NA's 
  81   11  365 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   6.000   5.832   7.000  12.000 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    7  448 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   6    2  449 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 463  560  686 NA's 
   1    1    1  454 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  106.0   256.0   276.0   371.4   506.0   810.0     448 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
locoregional disease           metastatic                 NA's 
                   1                    2                  454 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   6    3  448 

patient.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r2 NA's 
   1  456 

patient.new_tumor_events.new_tumor_event.site_of_additional_surgery_new_tumor_event_mets:
liver  NA's 
    2   455 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  27    9  421 

patient.non_nodal_tumor_deposits:
  no  yes NA's 
 187   39  231 

patient.number_of_abnormal_loci:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.7778  0.0000  9.0000     403 

patient.number_of_first_degree_relatives_with_cancer_diagnosis:
   0    1    2    3 NA's 
 332   47    9    2   67 

patient.number_of_loci_tested:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   5.000   5.000   5.019   5.000  10.000     403 

patient.number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.058   2.000  50.000      25 

patient.number_of_lymphnodes_positive_by_ihc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    0.68    0.00   12.00     407 


patient.perineural_invasion_present:
  no  yes NA's 
 133   46  278 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       199        214         44 

patient.postoperative_rx_tx:
  no  yes NA's 
  16   18  423 

patient.preoperative_pretreatment_cea_level:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   0.000    1.700    3.000   37.440    7.525 2614.000      170 

patient.primary_lymph_node_presentation_assessment:
  no  yes NA's 
  15  432   10 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         17                           3 
        progressive disease              stable disease 
                          6                           1 
                       NA's 
                        430 

patient.prior_dx:
                                                     no 
                                                    395 
                                                    yes 
                                                     61 
yes, history of synchronous and or bilateral malignancy 
                                                      1 

patient.race:
american indian or alaska native                            asian 
                               1                               11 
       black or african american                            white 
                              59                              213 
                            NA's 
                             173 

patient.radiation_therapy:
  no NA's 
  34  423 

patient.radiations.radiation.2.anatomic_treatment_site:
distant recurrence               NA's 
                 3                454 

patient.radiations.radiation.2.bcr_radiation_barcode:
tcga-aa-3692-r38346 tcga-aa-3713-r38350 tcga-g4-6314-r21829                NA's 
                  1                   1                   1                 454 

patient.radiations.radiation.2.bcr_radiation_uuid:
0a2c52c9-f534-4b59-8257-9908132d9638 1081c34f-da75-4856-966c-8f9b10e784aa 
                                   1                                    1 
58a7c745-c9f0-4ca4-9e8c-1880ea0e319c                                 NA's 
                                   1                                  454 

patient.radiations.radiation.2.course_number:
   4 NA's 
   1  456 

patient.radiations.radiation.2.day_of_form_completion:
   9   13 NA's 
   1    2  454 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
 306  761 1093 NA's 
   1    1    1  454 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
 275  365 1093 NA's 
   1    1    1  454 

patient.radiations.radiation.2.measure_of_response:
                partial response radiographic progressive disease 
                               1                                1 
                            NA's 
                             455 

patient.radiations.radiation.2.month_of_form_completion:
   2   12 NA's 
   1    2  454 

patient.radiations.radiation.2.radiation_dosage:
  38   50 NA's 
   1    1  455 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   3  454 

patient.radiations.radiation.2.radiation_type:
               external other: specify in notes                    NA's 
                      2                       1                     454 

patient.radiations.radiation.2.radiation_type_notes:
gamma knife stereotactic radiosurgery                                  NA's 
                                    1                                   456 

patient.radiations.radiation.2.regimen_indication:
progression        NA's 
          1         456 

patient.radiations.radiation.2.units:
  gy NA's 
   2  455 

patient.radiations.radiation.2.year_of_form_completion:
2012 NA's 
   3  454 

patient.radiations.radiation.3.anatomic_treatment_site:
distant recurrence               NA's 
                 1                456 

patient.radiations.radiation.3.bcr_radiation_barcode:
tcga-g4-6314-r21823                NA's 
                  1                 456 

patient.radiations.radiation.3.bcr_radiation_uuid:
41e91ef0-9f0d-4ff5-8a49-5eacb7820a13                                 NA's 
                                   1                                  456 

patient.radiations.radiation.3.course_number:
   1 NA's 
   1  456 

patient.radiations.radiation.3.day_of_form_completion:
   9 NA's 
   1  456 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
 622 NA's 
   1  456 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
 609 NA's 
   1  456 

patient.radiations.radiation.3.month_of_form_completion:
   2 NA's 
   1  456 

patient.radiations.radiation.3.numfractions:
  10 NA's 
   1  456 

patient.radiations.radiation.3.radiation_dosage:
3000 NA's 
   1  456 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   1  456 

patient.radiations.radiation.3.radiation_type:
external beam          NA's 
            1           456 

patient.radiations.radiation.3.regimen_indication:
recurrence       NA's 
         1        456 

patient.radiations.radiation.3.units:
 cgy NA's 
   1  456 

patient.radiations.radiation.3.year_of_form_completion:
2012 NA's 
   1  456 

patient.radiations.radiation.4.anatomic_treatment_site:
distant recurrence               NA's 
                 1                456 

patient.radiations.radiation.4.bcr_radiation_barcode:
tcga-g4-6314-r21828                NA's 
                  1                 456 

patient.radiations.radiation.4.bcr_radiation_uuid:
9bd482d5-a6c4-4389-99ff-503503c1b98f                                 NA's 
                                   1                                  456 

patient.radiations.radiation.4.course_number:
   3 NA's 
   1  456 

patient.radiations.radiation.4.day_of_form_completion:
   9 NA's 
   1  456 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
 911 NA's 
   1  456 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
 898 NA's 
   1  456 

patient.radiations.radiation.4.month_of_form_completion:
   2 NA's 
   1  456 

patient.radiations.radiation.4.numfractions:
  10 NA's 
   1  456 

patient.radiations.radiation.4.radiation_dosage:
3000 NA's 
   1  456 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   1  456 

patient.radiations.radiation.4.radiation_type:
external beam          NA's 
            1           456 

patient.radiations.radiation.4.regimen_indication:
palliative       NA's 
         1        456 

patient.radiations.radiation.4.units:
 cgy NA's 
   1  456 

patient.radiations.radiation.4.year_of_form_completion:
2012 NA's 
   1  456 

patient.radiations.radiation.anatomic_treatment_site:
 distant recurrence        distant site primary tumor field       regional site 
                  5                   2                   6                   1 
               NA's 
                443 



patient.radiations.radiation.course_number:
   1    2 NA's 
   2    1  454 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   11.50   13.00   16.27   23.50   29.00     442 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   49.0   193.8   425.0   467.2   716.8  1126.0     443 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   19.0    98.0   275.0   392.7   638.0  1126.0     442 

patient.radiations.radiation.measure_of_response:
                partial response radiographic progressive disease 
                               1                                8 
                  stable disease                             NA's 
                               1                              447 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.5     9.0     7.8    12.0    12.0     442 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     7.5    10.0    14.0    20.0    30.0     450 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    37.0   309.1  1973.1  4625.0  5400.0     445 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  15  442 

patient.radiations.radiation.radiation_type:
               external           external beam                internal 
                      9                       3                       1 
other: specify in notes                    NA's 
                      1                     443 

patient.radiations.radiation.radiation_type_notes:
gamma knife stereotactic radiosurgery                                  NA's 
                                    1                                   456 

patient.radiations.radiation.regimen_indication:
  adjuvant recurrence       NA's 
         4          1        452 

patient.radiations.radiation.units:
 cgy   gy NA's 
   5    7  445 

patient.radiations.radiation.year_of_form_completion:
2012 2013 2014 2015 NA's 
   9    2    2    2  442 

patient.residual_tumor:
  r0   r1   r2   rx NA's 
 329    4   26   25   73 


patient.stage_event.system_version:
 2nd  5th  6th  7th NA's 
   1   64  206  132   54 




patient.synchronous_colon_cancer_present:
  no  yes NA's 
 388   25   44 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 281  162   14 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
 162  281   14 


patient.tumor_tissue_site:
colon  NA's 
  456     1 

patient.venous_invasion:
  no  yes NA's 
 300   96   61 

patient.vital_status:
alive  dead  NA's 
  400    56     1 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  34.00   65.10   80.00   80.93   93.00  175.30     207 

patient.year_of_form_completion:
2010 2011 2013 2014 2015 
 141  280   21   11    4 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1998    2007    2009    2008    2010    2013 


patient.microsatellite_instability_test_results.microsatellite_instability_test_result.mononucleotide_and_dinucleotide_marker_panel_analysis_status:
indeterminate         msi-h         msi-l           mss 
            2            83            82           290 


patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              39                               22 
                            NA's 
                             396 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                 22                 435 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             358               99 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased      NA's 
      435        21         1 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -245.0    63.0   291.0   807.8  1105.5  4533.0      46 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -14.00    0.00    0.00   11.85   27.00  120.00       1 

patient.biospecimen_cqcf.digital_image_submitted:
  no  yes NA's 
 438   18    1 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     3                     29                    425 


patient.biospecimen_cqcf.histological_type:
         colon adenocarcinoma colon mucinous adenocarcinoma 
                          392                            64 
                         NA's 
                            1 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                               no yes, radiation prior to resection 
                              455                                 1 
                             NA's 
                                1 



patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -26.00    0.00   16.00   57.05   31.00 1429.00     346 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0170  0.1220  0.1660  0.1722  0.2220  0.3340     436 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
picogreen      NA's 
       21       436 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  20 NA's 
  21  436 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0    90.0   121.0   185.4   165.0  1207.0     436 



patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           15           442 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2    3 NA's 
  41   70    3  343 


patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
   0   31   92 NA's 
   4    6    1  446 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
blood draw       NA's 
         6        451 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
 ascending colon  hepatic flexure    sigmoid colon transverse colon 
               1                2                3                1 
            NA's 
             450 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    3 NA's 
   7    4  446 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -90.00    0.00    3.00   25.79   29.00 1247.00      74 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0092   0.0810   0.3022  40.3444   0.9201 852.1000      377 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   15.00   20.00   20.06   20.00   85.21     377 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   39.97  118.05  303.81  438.25 1200.00     377 



patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           60           397 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2    3 NA's 
 344   62   16   35 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 yes NA's 
 328  129 

patient.biospecimen_cqcf.path_confirm_report_attached:
  no  yes NA's 
  21  435    1 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
  no  yes NA's 
   1  327  129 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
  no  yes NA's 
   1  327  129 

patient.biospecimen_cqcf.race:
black or african american                      NA's 
                       36                       421 

patient.biospecimen_cqcf.sample_prescreened:
 yes NA's 
 456    1 

patient.biospecimen_cqcf.site_of_disease:
colon  NA's 
  455     2 


patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
  30    6  421 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
 184  272    1 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    4.25    7.50   10.00   13.75   30.00     447 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   71.25   77.50   77.50   83.75   90.00     447 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  200.0   238.8   346.0   485.0   682.8  1118.0     447 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   7    3  447 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00   10.00   11.07   20.00   30.00       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   70.00   70.00   74.57   80.00  100.00       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0   100.0   200.0   267.7   306.2  1657.0       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1    2 NA's 
 455    1    1 

patient.biospecimen_cqcf.vessel_used:
              cassette               cryomold               cryovial 
                    20                     53                    338 
other (please specify)                   NA's 
                    45                      1 

patient.days_to_index:
  0 
457 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and metastatic tumor(s)  primary tumor and recurrent tumor(s) 
                                    1                                     1 
                                 NA's 
                                  455 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     14     326     843    1424    2312    4563     340 


patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    120     200     250     240     300     330     444 

patient.samples.sample.2.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.200   0.800   1.000   1.033   1.075   2.000     427 

patient.samples.sample.2.is_ffpe:
 no yes 
444  13 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.700   1.100   1.200   1.457   1.925   2.500     427 

patient.samples.sample.2.oct_embedded:
false  NA's 
  117   340 



patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   1.710   1.800   1.761   1.800   1.900     428 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   4  453 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   4  453 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             454 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97514926  97515266  97518839 102067706  97556718 167814212       330 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   9   10   13   36 NA's 
 130   17    7    5  298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4745  0.5000  0.5000     298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    7.00   12.00   11.75   14.00   31.00     298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
 147   12  298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   4.000   4.541   5.000   8.000     298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   7.214  10.000  12.000     298 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.80   40.00   40.00   37.14   40.00   40.00     298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
 118   41  298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   5   20   80 NA's 
   5    7  147  298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2012 2013 2015 NA's 
 147    3    4    5  298 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.888e+07 1.543e+08 1.054e+09 7.578e+08 1.054e+09 1.054e+09       413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
  10   36 NA's 
  41    3  413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.16 0.17  0.5 NA's 
   2    1   41  413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  15   21   26   29 NA's 
   8    1    3   32  413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
  41    3  413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   7    8 NA's 
   1   43  413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    5.75    7.00    7.00    8.00   11.00     413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
1133 1953 1982 a46x NA's 
   1    8   32    3  413 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
 0.8 0.85    4   10 NA's 
   2    1   40    1  413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22   23 NA's 
  41    3  413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   5    8   20 NA's 
   3   40    1  413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2010 2011 2015 NA's 
   1   40    3  413 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
  10  447 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
  10  447 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             454 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 9.756e+07 1.070e+08 2.690e+08 1.456e+08 1.054e+09        43 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    9   10   13 NA's 
  30   17  397    2   11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4751  0.5000  0.5000      11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   12.00   14.50   16.93   24.00   31.00      11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
 433   13   11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.787   7.000  12.000      11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   7.339  10.000  12.000      11 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.73   40.00   40.00   37.32   40.00   40.00      11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 339  107   11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   80.00   80.00   75.54   80.00   80.00      11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014      11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    60.0   155.0   123.3   176.8   400.8     107 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 414                   32                   11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r    w NA's 
   2   30  414   11 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4683  0.5000  0.5000     107 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 414   43 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
 318   96   43 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             446 
                                            NA's 
                                              11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no  yes NA's 
 433   13   11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
               allprep rna extraction highpure mirna (allprep dna) ffpe rna 
                                   17                                    13 
            mirvana (allprep dna) rna                               repli-g 
                                    2                                   414 
                                 NA's 
                                   11 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.255   1.296   1.495  12.930     425 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.400   7.850   5.994   8.525   9.600     425 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
      106       244       107 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  10    2  445 

patient.samples.sample.2.portions.portion.analytes.analyte.3.a260_a280_ratio:
 1.7 1.92 NA's 
   1    1  455 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 9.754e+07 9.754e+07 4.167e+08 1.054e+09 1.054e+09       430 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   9   10 NA's 
  32   27  398 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
  59  398 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   5   11   12   29 NA's 
  17   15   13   14  398 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  59  398 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   4    5    6    8 NA's 
  17   28    4   10  398 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    5.00    5.78    6.00   12.00     398 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
   4   40 NA's 
  10   49  398 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22   23 NA's 
  27   32  398 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
   8   80 NA's 
  10   49  398 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
  49   10  398 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-aa-3971-10a-01x-1982-10 tcga-aa-3977-10a-01x-1982-10 
                           1                            1 
tcga-aa-3979-10a-01x-1982-10                         NA's 
                           1                          454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
6e29a8c6-f984-46dd-9c64-aa8ec8caf016 9088cdbd-75a1-4c44-b77d-3d9f2760a28b 
                                   1                                    1 
a5b07325-74be-4b4b-b304-c1b15ef28783                                 NA's 
                                   1                                  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
1053967638 1053967661 1053967662       NA's 
         1          1          1        454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
  10 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
 0.5 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
  29 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
  12 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
1982 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   d    e    f NA's 
   1    1    1  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
   4 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
   8 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   3  454 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 9.756e+07 9.902e+07 3.319e+08 1.476e+08 1.054e+09       358 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7    9   10 NA's 
   2   17  112  326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
0.14 0.16  0.5 NA's 
   1    1  129  326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   12.00   12.00   17.05   23.00   30.00     326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 131  326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.00    5.00    5.87    7.50   11.00     326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   6.962  10.000  12.000     326 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
1.86 2.08    4   40 NA's 
   1    1   19  110  326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
  85   46  326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
   8   13 13.3   80 NA's 
  19    1    1  110  326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2014 NA's 
  93   37    1  326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   29.0    65.0   103.5   115.9   180.0   220.0     371 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                    17                    112                      2 
                  NA's 
                   326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   r    w    x NA's 
   2   17  112  326 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration:
0.14 0.16  0.5 NA's 
   1    1   84  371 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 129  328 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
  84   45  328 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             131 
                                            NA's 
                                             326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 131  326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g              repli-g x 
                     2                     17                    112 
                  NA's 
                   326 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
1.27 1.88 NA's 
   1    1  455 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.rinvalue:
 7.2  8.8 NA's 
   1    1  455 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
       12        74       371 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
  10    2  445 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-g4-6317-02a-11w-2108-10                         NA's 
                           1                          456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
4a52e9c7-ee44-47ed-a8de-d050b71c80a0                                 NA's 
                                   1                                  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
121029564      NA's 
        1       456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   4 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   1 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   1 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
2108 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   b NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2012 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.amount:
 201 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.bcr_analyte_barcode:
tcga-g4-6317-02a-11w                 NA's 
                   1                  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.bcr_analyte_uuid:
76e148a6-6a81-4222-862b-f210c6b51b85                                 NA's 
                                   1                                  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen      NA's 
        1       456 

patient.samples.sample.2.portions.portion.analytes.analyte.4.subportion_sequence:
   1 NA's 
   1  456 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.710   1.800   1.900   1.871   1.900   2.130      63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   9  448 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   9  448 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.885e+07 1.087e+08 2.734e+08 1.456e+08 1.054e+09        34 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   3.738   5.000  10.000       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1397  0.1500  0.1800       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   14.00   22.00   18.22   26.00   31.00       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
 443   12    2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.149   6.000  12.000       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.374   9.000  12.000       2 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.530   0.940   1.070   1.729   2.800   4.540       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 341  114    2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67   13.39   26.70   40.00       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   9  448 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   9  448 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.885e+07 1.087e+08 2.837e+08 1.136e+08 1.054e+09        69 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000  10.000  10.000   9.152  10.000  23.000      36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.01000 0.05000 0.05000 0.09007 0.14000 0.18000      36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   10.00   22.00   18.78   27.00   31.00      36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
 409   12   36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    4.00    5.00    5.43    6.00   11.00      36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.418   9.000  12.000      36 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.330   1.010   2.000   1.973   3.000   4.540      37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 340   81   36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.20    6.70   26.70   31.59   60.00   60.00      37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014      36 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             456 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.648e+07 9.906e+07 1.070e+08 3.770e+08 1.053e+09 1.054e+09        74 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   23.00   16.59   23.00   24.00      58 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1363  0.1500  0.1700      58 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   22.00   28.00   22.82   29.00   31.00      58 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no  yes NA's 
 390    9   58 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   6.000   5.672   6.000  11.000      58 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.500   8.000   7.657   9.000  12.000      58 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   0.940   1.010   1.435   1.950   4.270     228 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
 165  234   58 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.67    6.70   13.18   13.34   60.00     228 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2012 2015 NA's 
 129  261    1    8   58 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.750e+07 1.005e+08 1.053e+09 6.880e+08 1.053e+09 1.054e+09       246 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   23.00   23.00   19.25   24.00   24.00     241 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1432  0.1500  0.1700     241 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   22.00   28.50   25.56   31.00   31.00     241 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 216  241 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   4.139   6.000   9.000     241 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   6.958   9.000  12.000     241 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.260   0.930   1.000   1.327   1.860  12.900     322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  91  125  241 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.67    6.67   10.58   13.00  258.00     322 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 2012 2015 NA's 
  35  179    1    1  241 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.134e+08 1.053e+09 6.933e+08 1.053e+09 1.053e+09       286 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   23.00   16.81   23.00   24.00     285 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1400  0.1355  0.1500  0.1700     285 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   22.00   27.00   25.68   29.00   31.00     285 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
 172  285 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   4.128   6.000   8.000     285 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   7.000   7.436   9.000  12.000     285 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.380   1.000   1.860   3.054   5.600  30.000     370 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
  52  120  285 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.55    6.67   13.30   35.67   80.00  600.00     370 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 2011 NA's 
   6  166  285 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.244e+08 1.053e+09 7.188e+08 1.053e+09 1.054e+09       342 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   2   10   23   24 NA's 
  34   53   24    7  339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1300  0.1400  0.1253  0.1500  0.1700     339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   22.50   28.00   24.36   29.00   31.00     339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
 118  339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   3    4    6    8 NA's 
  69    2    8   39  339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    4.67    7.00   12.00     339 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.895   2.775   2.801   3.000   6.400     363 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
  82   36  339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.30   20.00   29.46   60.00   80.00     363 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 2012 NA's 
 104   14  339 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.136e+08 1.136e+08 2.709e+08 1.163e+08 1.054e+09       409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
  10   23 NA's 
  42    6  409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1400  0.1379  0.1500  0.1700     409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   15.00   15.00   18.54   29.00   29.00     409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
  48  409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   3    4    5    8 NA's 
   8    2    2   36  409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    7.00    6.75    9.25   12.00     409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1618    1953    1953    1933    1981    2188     409 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   2.800   2.800   2.821   3.000   3.400     413 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
  44    4  409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
6.67   20   60 NA's 
   2   38    4  413 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 2012 NA's 
  44    4  409 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-aa-3549-10a-01d-1953-10 tcga-aa-3553-10a-01d-1953-10 
                           1                            1 
tcga-aa-3715-10a-01d-1981-10                         NA's 
                           1                          454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
20135e82-ea86-466d-8418-bc8aacaa321a 86b1fdb7-60ed-46fe-9889-cc0ec33e3bff 
                                   1                                    1 
ab36c6b3-5814-42e4-adff-33b85176bb5b                                 NA's 
                                   1                                  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
113627941 113628352 113628376      NA's 
        1         1         1       454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
  10 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.14 0.16 NA's 
   2    1  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
  15   29 NA's 
   2    1  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   8 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   8   11 NA's 
   2    1  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
1953 1981 NA's 
   2    1  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   a    c    e NA's 
   1    1    1  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
 2.8  3.2 NA's 
   2    1  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
  20 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 NA's 
   3  454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
  10  447 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
  10  447 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             454 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 9.885e+07 1.087e+08 2.725e+08 1.322e+08 1.062e+09        33 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2   10   24 NA's 
 184  250   10   12    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1465  0.1600  0.1800       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   22.00   18.21   26.00   31.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
 443   13    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   4.000   5.099   6.000  12.000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.397   9.000  12.000       1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.800   1.010   1.400   1.444   1.860   2.260       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 340  116    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.600   6.700  10.000   9.978  13.300  20.000       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       1 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   16.85   51.83  139.96  193.28 1113.40       1 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
 456    1 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
 456    1 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1400  0.1500  0.1483  0.1600  0.1800       1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 456    1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 220  236    1 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             456 
                                            NA's 
                                               1 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no  yes NA's 
 443   13    1 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type                  allprep ffpe dna 
                               97                                13 
    chemical lysis dna extraction pre-extracted dna received by tss 
                              240                               106 
                             NA's 
                                1 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       62       394         1 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  10    2  445 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     9.0    14.0    15.5    22.0    31.0       1 

patient.samples.sample.2.portions.portion.is_ffpe:
  no  yes NA's 
 443   13    1 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.671   6.000  12.000       1 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.445   1.000  31.000       1 

patient.samples.sample.2.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.298   1.000   8.000     353 




patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no  yes NA's 
  12   10  435 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   25.00   21.62   40.00   50.00     436 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   4.952  10.000  10.000     436 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   5.364  10.000  15.000     446 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.429   0.000  15.000     436 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   57.50   51.64  100.00  100.00     435 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   20.00   19.09   32.50   50.00     446 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   17.50   36.14   72.25  100.00     435 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   69.00   80.00   77.45   85.00   92.00     446 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    11     11    435 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no  yes NA's 
  42   10  405 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.75   14.00   19.05   35.00   50.00     435 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   4.364   8.000  10.000     435 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    7.50    7.25   12.75   18.00     445 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.546   0.000  20.000     435 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   20.00   49.61  100.00  100.00     434 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.25   25.00   22.67   35.00   55.00     445 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   30.00   34.78   69.00  100.00     434 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   65.00   77.50   73.50   81.25   92.00     445 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
bottom    top   NA's 
    11     41    405 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    30.0    95.5   106.2   137.2   400.0     359 

patient.samples.sample.2.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2010    2011    2011    2014       1 

patient.samples.sample.2.portions.shipment_portion.bcr_shipment_portion_uuid:
05de831d-b33d-415e-a2f8-258c44ef5abd 31b50118-e859-434a-8f13-f210778079b6 
                                   1                                    1 
9a7ed113-6c6e-4b34-9799-ba3cf36d9a41                                 NA's 
                                   1                                  454 

patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
   3  454 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
   3  454 

patient.samples.sample.2.portions.shipment_portion.plate_id:
2189 a45f NA's 
   1    2  454 

patient.samples.sample.2.portions.shipment_portion.portion_number:
  11   13   21 NA's 
   1    1    1  454 

patient.samples.sample.2.portions.shipment_portion.portion_sequence:
   1    2 NA's 
   1    1  455 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode:
tcga-g4-6317-02a-13-2189-20 tcga-nh-a6gc-06a-11-a45f-20 
                          1                           1 
tcga-nh-a8f7-06a-21-a45f-20                        NA's 
                          1                         454 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
   5   30 NA's 
   1    2  454 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   3    7 NA's 
   1    2  454 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 2014 NA's 
   1    2  454 

patient.samples.sample.2.sample_type:
blood derived normal           metastatic        primary tumor 
                 401                    2                   13 
     recurrent tumor  solid tissue normal 
                   1                   40 

patient.samples.sample.2.sample_type_id:
  1   2   6  10  11 
 13   1   2 401  40 

patient.samples.sample.2.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.3000  0.3667  0.5000  0.8000     427 

patient.samples.sample.2.vial_number:
  a   b   c 
442  14   1 



patient.samples.sample.3.days_to_collection:
  14   48 NA's 
   1    1  455 

patient.samples.sample.3.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.6000  0.8000  0.7614  0.9000  1.0000     413 

patient.samples.sample.3.is_ffpe:
  no NA's 
  60  397 

patient.samples.sample.3.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.000   1.200   1.264   1.500   2.200     413 

patient.samples.sample.3.oct_embedded:
false  NA's 
    2   455 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.7  1.8 NA's 
   1   21  435 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.751e+07 1.054e+09 1.054e+09 8.547e+08 1.054e+09 1.054e+09       433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    9   13 NA's 
  18    5    1  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.2212  0.1600  0.5000     433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   7   11   12 NA's 
   3   19    2  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  24  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   4    5 NA's 
   3   21  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   4.000   4.292   5.000   8.000     433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    896    1758    1758    1587    1758    1758     433 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.680   2.000   2.000   9.930   2.397  40.000     433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
  24  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  12 13.3   20   80 NA's 
   1   17    1    5  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   5   19  433 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.508e+08 1.508e+08 1.508e+08 2.009e+08 1.508e+08 1.054e+09       439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
  18  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.13 0.14 0.15 0.16 NA's 
   1    3   12    2  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2   11 NA's 
  17    1  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  18  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   5   10 NA's 
   1   17  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.25    4.50    4.00    5.00    7.00     439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
1758 a32z NA's 
   1   17  439 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.690   1.950   1.950   1.931   1.950   2.130     439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22   23 NA's 
   1   17  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
  13 13.3 NA's 
  17    1  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2011 2013 NA's 
   1   17  439 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.070e+08 1.087e+08 5.385e+08 1.054e+09 1.054e+09       411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   10   13 NA's 
   4   24   18  411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.5000  0.3313  0.5000  0.5000     411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   11.00   11.00   14.76   24.00   27.00     411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  46  411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   5.000   5.043   5.000  12.000     411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   7.000   6.283   8.750  10.000     411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    831    1727    1757    1666    1782    1967     411 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.52    3.00   40.00   22.36   40.00   40.00     411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
  46  411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  18   20 26.7   80 NA's 
   1   17    4   24  411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   6   40  411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   93.45  151.50  137.70  171.00  386.20     411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                  24                   22                  411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  22   24  411 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.5000  0.3313  0.5000  0.5000     411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  24  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
  24  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              46 
                                            NA's 
                                             411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  46  411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                       21                         1                        24 
                     NA's 
                      411 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.130   1.385   1.470   1.484   1.520   2.010     435 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.625   7.950   7.882   8.100   8.600     435 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
       11        35       411 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97556374  97556383 107042902 103577058 107042911 108686719       452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   4   12   25 NA's 
   1    2    2  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5 NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   8 NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
 997 1727 1791 NA's 
   2    1    2  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   a    b    f NA's 
   2    2    1  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    3  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     46      59     148     119     166     176     452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                     3                      2                    452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
   3    2  452 



patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   5  452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
        3         2       452 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
        2         3       452 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9    2 NA's 
   3   54    1  399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.additional_studies.additional_study.disease_code:
fppp NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.additional_studies.additional_study.project_code:
tcga NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_barcode:
tcga-a6-2677-10a-01d-a274-01 tcga-a6-2684-10a-01d-a46w-08 
                           1                            1 
                        NA's 
                         455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_uuid:
0cb8495e-97cc-4059-bc3f-2faa3fe5fab1 45315948-76f1-42ac-bacd-db8b92243d09 
                                   1                                    1 
                                NA's 
                                 455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
132179140 145675741      NA's 
        1         1       455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
   1    8 NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
0.07 0.14 NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
   3   14 NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
   2  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   3    8 NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   5    7 NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_id:
a274 a46w NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_row:
   a    d NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
0.94  1.4 NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  23 NA's 
   2  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
 6.7   20 NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2013 2015 NA's 
   1    1  455 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_barcode:
tcga-a6-2677-10a-01d-a46w-08                         NA's 
                           1                          456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_uuid:
4115dbf1-eaba-4493-ac43-355e0b463a4d                                 NA's 
                                   1                                  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.biospecimen_barcode_bottom:
132179155      NA's 
        1       456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.center_id:
   8 NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.concentration:
0.07 NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.day_of_shipment:
   3 NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.is_derived_from_ffpe:
  no NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.month_of_shipment:
   8 NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_column:
   5 NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_id:
a46w NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_row:
   a NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.quantity:
 1.4 NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.source_center:
  23 NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.volume:
  20 NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.11.year_of_shipment:
2015 NA's 
   1  456 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.087e+08 1.050e+09 5.963e+08 1.050e+09 1.054e+09       397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2    5   10 NA's 
  52    1    5    2  397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1400  0.1500  0.1487  0.1600  0.1700     397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   14.00   16.68   23.00   28.00     397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  60  397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   2.000   3.567   4.000  12.000     397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.467  10.000  12.000     397 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.87    1.00    1.00    1.35    1.07    4.27     397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
  58    2  397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67 13.3 26.7   40 NA's 
  52    1    5    2  397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2014 NA's 
   8   50    2  397 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.087e+08 1.050e+09 6.109e+08 1.050e+09 1.054e+09       399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   1    5   10 NA's 
   1   27   30  399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 0.14 0.15 0.16 NA's 
  25    4   16   13  399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0    14.0    14.0    16.6    26.0    28.0     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  58  399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   2.000   3.276   4.000  11.000     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.535  10.000  12.000     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    959    1551    1551    1561    1702    1924     399 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.670   2.000   3.000   3.112   4.010   4.270     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
  58  399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   26.70   26.70   35.08   55.00   60.00     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
   6   52  399 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.087e+08 1.050e+09 6.108e+08 1.050e+09 1.054e+09       399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    6.00   10.00   10.62   10.00   24.00     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0900  0.0991  0.1500  0.1700     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   14.00   18.26   27.00   31.00     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  58  399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   2.000   3.293   4.000   8.000     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   7.017   8.000  11.000     399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    960    1554    1554    1581    1721    1925     399 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   2.000   2.000   2.454   3.740   4.270     407 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
  50    8  399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
6.67  6.7 26.7   40 NA's 
   1    5   15   29  407 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
   5   53  399 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   7  450 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   7  450 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              7             450 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.070e+08 1.050e+09 6.069e+08 1.050e+09 1.053e+09       402 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   2    9   10   23 NA's 
   5   27    8   15  402 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0889  0.1400  0.1600     402 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   14.00   14.00   17.56   25.50   29.00     402 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  55  402 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2    3    6    8 NA's 
  28    8   14    5  402 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   8.000   8.146  10.000  12.000     402 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   2.000   2.000   1.926   2.000   2.130     417 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  33   22  402 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.3   20   40 NA's 
   5    7   28  417 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 2013 NA's 
   5   43    7  402 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   7  450 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   7  450 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              7             450 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.070e+08 1.457e+08 4.065e+08 1.051e+09 1.053e+09       434 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       1      23      15      23      24     434 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1461  0.1550  0.1700     434 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  14   28   29   31 NA's 
   8    1    9    5  434 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  23  434 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   2    3    6 NA's 
   1   13    9  434 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   6.217   7.000  11.000     434 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   0.955   1.010   1.161   1.123   2.130     443 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
   7   16  434 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
6.67  6.7 13.3   40 NA's 
   5    7    1    1  443 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 2013 NA's 
  16    7  434 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.244e+08 1.322e+08 1.322e+08 5.151e+08 1.053e+09 1.053e+09       445 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   8   10   23 NA's 
   6    1    5  445 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0775  0.0850  0.1050  0.1425  0.1600     445 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   3    5   22 NA's 
   6    1    5  445 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  12  445 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   3    8 NA's 
   6    6  445 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    6.00    5.75    6.25    9.00     445 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
1617 1619 2188 a46w NA's 
   3    2    1    6  445 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
 1.4  1.6  1.8    3 NA's 
   2    3    1    1  450 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
   1   11  445 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
  20   60 NA's 
   6    1  450 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 2012 2015 NA's 
   5    1    6  445 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.disease_code:
fppp NA's 
   1  456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.project_code:
tcga NA's 
   1  456 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             456 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113627199 113627474 113628304 116716366 113628327 132158859       451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
  10 NA's 
   6  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.07 0.14 0.15 0.16 NA's 
   1    2    2    1  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  13   15   29 NA's 
   1    3    2  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   6  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   3    8 NA's 
   1    5  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   5    7   12 NA's 
   3    1    2  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
1953 1981 a270 NA's 
   3    2    1  451 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
 1.4  2.8    3  3.2 NA's 
   1    2    2    1  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
   5    1  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
  20 NA's 
   6  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 2013 NA's 
   5    1  451 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.additional_studies.additional_study.disease_code:
fppp NA's 
   3  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.additional_studies.additional_study.project_code:
tcga NA's 
   3  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-a6-2674-10a-01d-a270-10 tcga-a6-2677-10a-01d-a270-10 
                           1                            1 
tcga-a6-2684-10a-01d-a274-01                         NA's 
                           1                          454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
0a0099f8-5b98-475a-aad5-5e1547cb42aa d4a0dce0-027b-4f41-8ef2-9dc7ec32def2 
                                   1                                    1 
ee2a141e-21a2-43c7-88a1-90bebac74611                                 NA's 
                                   1                                  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
132158813 132158839 145675740      NA's 
        1         1         1       454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   1   10 NA's 
   1    2  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.07 0.08 0.14 NA's 
   1    1    1  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
  13   14 NA's 
   2    1  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   3 NA's 
   3  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   7    8 NA's 
   2    1  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
a270 a274 NA's 
   2    1  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   a    b NA's 
   1    2  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
0.94  1.4  1.6 NA's 
   1    1    1  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  23 NA's 
   3  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
 6.7   20 NA's 
   1    2  454 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2013 NA's 
   3  454 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 1.087e+08 1.050e+09 5.963e+08 1.050e+09 1.054e+09       397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    5 NA's 
   7   52    1  397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1512  0.1600  0.1700     397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   14.00   16.85   24.50   28.00     397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  60  397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    2.00    3.55    4.00   12.00     397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.500   7.283  10.000  12.000     397 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.860   2.000   2.003   2.130   9.990     397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
  58    2  397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7   10 13.3 66.6 NA's 
   7    3   49    1  397 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2014 NA's 
   8   50    2  397 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.93  161.91  279.23  287.87  371.96  845.73     397 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  60  397 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  60  397 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1512  0.1600  0.1700     397 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  60  397 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
  50   10  397 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              60 
                                            NA's 
                                             397 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  60  397 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                               46                                13 
pre-extracted dna received by tss                              NA's 
                                1                               397 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
        2        58       397 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   15.00   16.00   17.62   25.00   30.00     397 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  60  397 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     5.5     7.0    12.0    12.0     397 

patient.samples.sample.3.portions.portion.portion_number:
   1 NA's 
  60  397 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
   3  454 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-a6-3807-11a-01-ts1 tcga-a6-3808-11a-01-ts1 tcga-a6-4107-11a-01-ts1 
                      1                       1                       1 
                   NA's 
                    454 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_uuid:
8425e9ee-5910-48cd-aeb4-430e0401ce73 de8d5f36-febb-43d5-8f5c-9bc3842e53ff 
                                   1                                    1 
e73c0645-13ac-43fd-b8be-05036fa31fea                                 NA's 
                                   1                                  454 

patient.samples.sample.3.portions.portion.slides.slide.2.image_file_name:
tcga-a6-3807-11a-01-ts1.e73c0645-13ac-43fd-b8be-05036fa31fea.svs 
                                                               1 
tcga-a6-3808-11a-01-ts1.de8d5f36-febb-43d5-8f5c-9bc3842e53ff.svs 
                                                               1 
tcga-a6-4107-11a-01-ts1.8425e9ee-5910-48cd-aeb4-430e0401ce73.svs 
                                                               1 
                                                            NA's 
                                                             454 

patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location:
 top NA's 
   3  454 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  44  413 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     3     41    413 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   57.00   62.50   57.96   66.75  164.00     411 

patient.samples.sample.3.portions.portion.year_of_creation:
2010 2011 2012 2013 NA's 
  35   22    1    2  397 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                  16                   44                  397 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
  16   44  397 

patient.samples.sample.3.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.4000  0.4568  0.6000  0.9000     413 

patient.samples.sample.3.vial_number:
   a NA's 
  60  397 



patient.samples.sample.4.intermediate_dimension:
 0.7  0.8  0.9 NA's 
   3    3    3  448 

patient.samples.sample.4.is_ffpe:
  no NA's 
   9  448 

patient.samples.sample.4.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.900   1.300   1.600   1.644   2.000   2.400     448 

patient.samples.sample.4.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.7  1.8 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-a6-2684-11a-01r-1758-07 tcga-a6-3809-11a-01w-0996-09 
                           1                            1 
tcga-a6-3810-11a-01w-0996-09                         NA's 
                           1                          454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
26535485-3597-44fe-ae01-63491d87f86b 389b9900-ba9c-48b5-a90e-25982084c4c2 
                                   1                                    1 
745579ae-d038-45f9-a314-e10aef18aaa6                                 NA's 
                                   1                                  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
  97555490   97556734 1053958210       NA's 
         1          1          1        454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    9 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14  0.5 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  11   12 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   5 NA's 
   3  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   3    8 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
 996 1758 NA's 
   2    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   c    d    g NA's 
   1    1    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
1.86   40 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   3  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3   80 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   2    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-a6-2684-11a-01r-a32z-07                         NA's 
                           1                          456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
47adb423-3e97-4d73-86ab-3daafc157f5f                                 NA's 
                                   1                                  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
150757401      NA's 
        1       456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.14 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  10 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   4 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a32z NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   d NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
1.82 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
  13 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2013 NA's 
   1  456 



patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.756e+07 1.003e+08 1.087e+08 2.625e+08 1.087e+08 1.054e+09       451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   10   13 NA's 
   2    3    1  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.14 0.15  0.5 NA's 
   1    2    3  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   4    6   11   12 NA's 
   1    2    1    2  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   6  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   4    5 NA's 
   2    4  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   3    6    8 NA's 
   1    2    3  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
 995 1653 1727 1757 NA's 
   2    2    1    1  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   c    d    e    g NA's 
   2    1    2    1  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
 2.8 4.01   40 NA's 
   1    2    3  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   6  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20 26.7   80 NA's 
   1    2    3  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    4  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   64.0   113.0   143.9   135.1   148.6   206.2     451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   3                    3                  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   3    3  451 



patient.samples.sample.4.portions.portion.analytes.analyte.2.concentration:
0.14 0.15  0.5 NA's 
   1    2    3  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   3  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   3  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               6 
                                            NA's 
                                             451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   6  451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g                   NA's 
                     3                      3                    451 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
1.39 1.42 1.58 NA's 
   1    1    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.rinvalue:
 7.1  7.3    8 NA's 
   1    1    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      6     451 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a6-3809-11a-01x-0997-10 tcga-a6-3810-11a-01x-0997-10 
                           1                            1 
tcga-a6-5659-11a-01w-1727-10 tcga-a6-5665-11a-01w-1727-10 
                           1                            1 
                        NA's 
                         453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
45b049e6-18f5-4368-85f6-7522587b5e52 4f2ab556-8516-4d54-88db-d89c2899b710 
                                   1                                    1 
5d0c5e3a-35dc-43ff-9bf4-f7f9c9fd63b0 a93d34aa-ed82-41c7-a0f5-bbf769388cfe 
                                   1                                    1 
                                NA's 
                                 453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
 97556350  97556359 108686704 108686752      NA's 
        1         1         1         1       453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   4   12 NA's 
   2    2  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5 NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   8 NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
 997 1727 NA's 
   2    2  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   c    d    g NA's 
   2    1    1  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    2  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.amount:
  50   99  144  145 NA's 
   1    1    1    1  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                     2                      2                    453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
   2    2  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-a6-3809-11a-01x tcga-a6-3810-11a-01x tcga-a6-5659-11a-01w 
                   1                    1                    1 
tcga-a6-5665-11a-01w                 NA's 
                   1                  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
4b8035b2-9e5a-40c4-a609-4a03c20f49f5 9ac5366a-df17-4e63-8be2-d72f2c251f92 
                                   1                                    1 
dcc03ee5-00c7-4992-9da8-8782c187fe6e ee6edae6-907a-472e-b669-9afaa64af220 
                                   1                                    1 
                                NA's 
                                 453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   4  453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
        2         2       453 

patient.samples.sample.4.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      4     453 

patient.samples.sample.4.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9 NA's 
   1    8  448 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 1.087e+08 1.087e+08 5.248e+08 1.050e+09 1.050e+09       448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    5 NA's 
   7    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1500  0.1500  0.1444  0.1600  0.1700     448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   6   14   26 NA's 
   3    4    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   2    4    5 NA's 
   4    3    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   7    9   10   11 NA's 
   5    2    1    1  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
1020 1549 1649 NA's 
   2    4    3  448 


patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.470   1.000   1.000   1.677   1.070   4.540     448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67 26.7 NA's 
   7    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   2    7  448 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 1.087e+08 1.087e+08 5.248e+08 1.050e+09 1.050e+09       448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5   10 NA's 
   4    5  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.05    0.05    0.14    0.11    0.15    0.17     448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   6   14   26 NA's 
   3    4    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   2    4    5 NA's 
   4    3    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   7    9   10   11 NA's 
   5    2    1    1  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
1024 1551 1650 NA's 
   2    4    3  448 


patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   1.870   2.000   2.449   3.740   4.270     448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
 6.7 26.7   40 NA's 
   2    4    3  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
   2    7  448 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 1.087e+08 1.087e+08 5.249e+08 1.050e+09 1.050e+09       448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    9   10 NA's 
   3    2    4  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.05 0.15 0.16 0.17 NA's 
   4    3    1    1  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   6   14   26 NA's 
   3    4    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   2    4    5 NA's 
   4    3    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   6    7 NA's 
   6    3  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
1026 1554 1651 NA's 
   2    4    3  448 


patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   2.000   2.000   2.493   4.010   4.270     448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
 6.7 26.7   40 NA's 
   2    3    4  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
   2    7  448 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.070e+08 1.070e+08 5.247e+08 1.050e+09 1.050e+09       448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   2    9   23 NA's 
   2    4    3  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.05 0.15 0.16 0.17 NA's 
   4    3    1    1  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  11   14   29 NA's 
   2    4    3  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2    6    8 NA's 
   4    3    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   5    6    9   10 NA's 
   1    4    3    1  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
1167 1555 1862 NA's 
   2    4    3  448 


patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   2 2.26 NA's 
   5    1  451 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
   6    3  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.3   40 NA's 
   2    4  451 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
   2    7  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-a6-2684-11a-01d-1861-23 tcga-a6-3809-11a-01d-1614-24 
                           1                            1 
tcga-a6-3810-11a-01d-1614-24                         NA's 
                           1                          454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
058bbd88-69ec-4c80-9722-b69f4f32c757 30a2cfc3-bb39-448c-8481-34331700cc25 
                                   1                                    1 
35525a5a-4bb5-48e8-bd73-2f3c621fbfef                                 NA's 
                                   1                                  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
 107030443 1052625145 1052625156       NA's 
         1          1          1        454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
  23   24 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.15 0.17 NA's 
   2    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  29   31 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   3    6 NA's 
   2    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   5    7 NA's 
   1    2  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
1614 1861 NA's 
   2    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   a    b    h NA's 
   1    1    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   1 1.13 NA's 
   1    1  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
   2    1  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
6.67 NA's 
   2  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 NA's 
   3  454 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-a6-3809-11a-01d-1619-23 tcga-a6-3810-11a-01d-1619-23 
                           1                            1 
                        NA's 
                         455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
5c89fee3-7fdb-4a16-bcf3-c3a61d2ea7a2 60136d2c-6477-4e8b-aab4-29698b38d0c3 
                                   1                                    1 
                                NA's 
                                 455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
1052626690 1052626701       NA's 
         1          1        455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
  23 NA's 
   2  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.15 0.17 NA's 
   1    1  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  22 NA's 
   2  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   2  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   3 NA's 
   2  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   7 NA's 
   2  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
1619 NA's 
   2  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   a    b NA's 
   1    1  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
   2  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   2  455 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-a6-3810-11a-01d-1981-10                         NA's 
                           1                          456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
0e8a979b-a4b6-4af8-ae3e-f34587979605                                 NA's 
                                   1                                  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
113627963      NA's 
        1       456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
  10 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.15 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  29 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   8 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
  12 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
1981 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   d NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   3 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
  20 NA's 
   1  456 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 NA's 
   1  456 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 1.087e+08 1.087e+08 5.248e+08 1.050e+09 1.050e+09       448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2 NA's 
   2    7  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1500  0.1500  0.1444  0.1600  0.1700     448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   6   14   26 NA's 
   3    4    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   2    4    5 NA's 
   4    3    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   7    9   10   11 NA's 
   5    2    1    1  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
1018 1547 1648 NA's 
   2    4    3  448 


patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.140   2.000   1.689   2.000   2.130     448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7 13.3 NA's 
   2    7  448 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    7  448 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  16.73   95.73  316.70  291.75  468.86  593.73     448 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   9  448 



patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1500  0.1500  0.1444  0.1600  0.1700     448 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
   7    2  448 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               9 
                                            NA's 
                                             448 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   9  448 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    9                   448 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      9     448 



patient.samples.sample.4.portions.portion.day_of_creation:
  10   11   16 NA's 
   3    2    4  448 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   9  448 

patient.samples.sample.4.portions.portion.month_of_creation:
   1    3   12 NA's 
   3    2    4  448 

patient.samples.sample.4.portions.portion.portion_number:
   1 NA's 
   9  448 

patient.samples.sample.4.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-a6-3809-11a-01-ts1 tcga-a6-3810-11a-01-ts1                    NA's 
                      1                       1                     455 

patient.samples.sample.4.portions.portion.slides.slide.2.bcr_slide_uuid:
b729304b-9abb-4c03-a0da-efabd8d79587 ca0ed29f-23cc-476a-b69c-52b4077da741 
                                   1                                    1 
                                NA's 
                                 455 

patient.samples.sample.4.portions.portion.slides.slide.2.image_file_name:
tcga-a6-3809-11a-01-ts1.ca0ed29f-23cc-476a-b69c-52b4077da741.svs 
                                                               1 
tcga-a6-3810-11a-01-ts1.b729304b-9abb-4c03-a0da-efabd8d79587.svs 
                                                               1 
                                                            NA's 
                                                             455 

patient.samples.sample.4.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   2  455 

patient.samples.sample.4.portions.portion.slides.slide.2.section_location:
 top NA's 
   2  455 




patient.samples.sample.4.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   9  448 

patient.samples.sample.4.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     2      7    448 

patient.samples.sample.4.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  56.00   60.00   63.00   80.11   69.00  159.00     448 

patient.samples.sample.4.portions.portion.year_of_creation:
2010 2011 NA's 
   6    3  448 

patient.samples.sample.4.sample_type:
solid tissue normal                NA's 
                  9                 448 

patient.samples.sample.4.sample_type_id:
  11 NA's 
   9  448 

patient.samples.sample.4.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.4000  0.5000  0.7000  0.6444  0.8000  0.9000     448 

patient.samples.sample.4.vial_number:
   a NA's 
   9  448 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   280.2   912.0  1491.3  2230.5  4563.0     351 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
1de7d5ed-60f7-4645-8243-ab0c027b3ed7                                 NA's 
                                   1                                  456 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   150.0   270.0   326.3   435.0  1160.0     351 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.6000  0.8000  0.8063  1.0000  2.5000     107 

patient.samples.sample.is_ffpe:
 no 
457 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.30    0.90    1.00    1.24    1.50    3.00     107 

patient.samples.sample.oct_embedded:
false  true  NA's 
   96    10   351 



patient.samples.sample.portions.portion.2.analytes.analyte.2.a260_a280_ratio:
 1.8 1.87 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a6-2675-01a-02r-1723-07 tcga-a6-4105-01a-02r-1774-07 
                           1                            1 
tcga-aa-a02k-01a-21r-a082-13                         NA's 
                           1                          454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
02425503-99c9-4a4e-841f-5b8b4de64148 1493b166-ffc2-4689-8b77-a8b972b6dbc0 
                                   1                                    1 
3aa5c179-5ff2-4883-80f7-84c592bc4916                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
  99015484  108687647 1053959487       NA's 
         1          1          1        454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.center_id:
   7   13 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.concentration:
0.14 0.16 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  18   22   27 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   4    5    9 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_column:
   1 NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_id:
1723 1774 a082 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_row:
   a    d NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.quantity:
2.13  2.8 3.74 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.volume:
13.3   20 26.7 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.amount:
68.85   350 725.2  NA's 
    1     1     1   454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type:
 rna NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type_id:
   r NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.bcr_analyte_barcode:
tcga-a6-2675-01a-02r tcga-a6-4105-01a-02r tcga-aa-a02k-01a-21r 
                   1                    1                    1 
                NA's 
                 454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.bcr_analyte_uuid:
18ff7709-5366-41d3-81ff-e22156fc61e6 86ed1071-2966-4d54-a73d-2078f5bf308a 
                                   1                                    1 
a0dc0a57-a808-4c4f-b075-15d4b31d3a39                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.concentration:
0.14 0.16 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                      NA's 
                        2                         1                       454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.ratio_28s_18s:
1.59  1.6  1.7 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.rinvalue:
 7.1  8.8  9.2 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      3     454 

patient.samples.sample.portions.portion.2.analytes.analyte.2.subportion_sequence:
   1 NA's 
   1  456 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a6-2675-01a-02t-1722-13 tcga-a6-4105-01a-02t-1773-13 
                           1                            1 
tcga-aa-a02k-01a-21w-a096-10                         NA's 
                           1                          454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
05be0bc8-b645-401b-9639-cde6c2086d15 0f5bddc4-4222-4964-8593-75ab9e6a5756 
                                   1                                    1 
e7c44cf0-9970-48b7-8fab-ffdbbb0dd946                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
 100489864  108687071 1053959199       NA's 
         1          1          1        454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.center_id:
  10   13 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1  0.5 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   6   18   27 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   4    5   10 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_column:
   1    3 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_id:
1722 1773 a096 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_row:
   a    b NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.quantity:
   3   40 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.volume:
  30   80 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.amount:
   50 687.5  NA's 
    1     1   455 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna            total rna                 NA's 
                   1                    2                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type_id:
   t    w NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.bcr_analyte_barcode:
tcga-a6-2675-01a-02t tcga-a6-4105-01a-02t tcga-aa-a02k-01a-21w 
                   1                    1                    1 
                NA's 
                 454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.bcr_analyte_uuid:
12c60b42-7cb1-421e-8258-0bd91aa77c51 270d4198-43ab-47d6-9acb-1cc9d145174e 
                                   1                                    1 
774f3102-6fd7-4093-9246-6a242e11b524                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.concentration:
 0.1 NA's 
   2  455 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   1  456 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   1  456 

patient.samples.sample.portions.portion.2.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g total rna      NA's 
        1         2       454 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.ratio_28s_18s:
1.34  1.5 NA's 
   1    1  455 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.rinvalue:
 7.4  8.1 NA's 
   1    1  455 

patient.samples.sample.portions.portion.2.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      2     455 

patient.samples.sample.portions.portion.2.analytes.analyte.3.subportion_sequence:
   1 NA's 
   1  456 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a6-2675-01a-02w-1791-10 tcga-a6-4105-01a-02w-1811-10 
                           1                            1 
tcga-aa-a02k-01a-21x-a095-10                         NA's 
                           1                          454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
265fbb5b-6953-4da6-81aa-4ffbaf5f7e9e a54b0fc6-9fce-49a3-a788-2fd200194e4c 
                                   1                                    1 
d6012a99-5ca9-46a9-ba05-8e7ed35172b7                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
 99029378 107042319 107042895      NA's 
        1         1         1       454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   6   13   25 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   5    6   10 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_column:
   1    3 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_id:
1791 1811 a095 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_row:
   b NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.source_center:
  22   23 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.amount:
 158  163 NA's 
   1    1  455 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                     2                      1                    454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type_id:
   w    x NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_barcode:
tcga-a6-2675-01a-02w tcga-a6-4105-01a-02w tcga-aa-a02k-01a-21x 
                   1                    1                    1 
                NA's 
                 454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_uuid:
6bc58c12-7591-42b0-81ae-bb4b847cf4c3 a515ff54-04a1-4029-bb8c-1c1361ffd5e4 
                                   1                                    1 
e3a7a32c-2e46-44c7-a16e-209cf994c7cb                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.concentration:
 0.5 NA's 
   2  455 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.pcr_amplification_successful:
  no  yes NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
        2         1       454 

patient.samples.sample.portions.portion.2.analytes.analyte.4.spectrophotometer_method:
picogreen   uv spec      NA's 
        1         1       455 

patient.samples.sample.portions.portion.2.analytes.analyte.4.subportion_sequence:
   1 NA's 
   1  456 

patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.640   1.855   1.900   1.884   1.930   2.020     446 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-a6-2675-01a-02d-1717-01 tcga-a6-4105-01a-02d-1770-01 
                           1                            1 
tcga-aa-a02k-01a-21d-a079-02                         NA's 
                           1                          454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
20214bc7-e1bd-4420-bdca-0e60abd62fd7 5f36d619-2676-4d0c-ad6f-f61d582bb418 
                                   1                                    1 
7cc17f3d-27f5-484a-8e22-c9d0b5b27836                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
  99027149  108687455 1053959103       NA's 
         1          1          1        454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.15 0.16 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  18   22   27 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   4    5    9 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   1    3 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
1717 1770 a079 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   a NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
   1 1.07 1.95 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
6.67   13 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-a6-2675-01a-02d-1719-10 tcga-a6-4105-01a-02d-1771-10 
                           1                            1 
tcga-aa-a02k-01a-21d-a080-01                         NA's 
                           1                          454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
37c42da1-2d9b-479e-8160-9da868137b4c 500069b9-de7c-453c-8da2-7cf1794487bc 
                                   1                                    1 
c4559d43-5aa7-456a-9e1f-eae286a1de16                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
  99015453  108687359 1053959967       NA's 
         1          1          1        454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
   1   10 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 0.15 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  18   22   27 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   4    5    9 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   1    3 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
1719 1771 a080 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_row:
   a NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
   1    3 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
6.67   60 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-a6-2675-01a-02d-1721-05 tcga-a6-4105-01a-02d-1772-05 
                           1                            1 
tcga-aa-a02k-01a-21d-a081-05                         NA's 
                           1                          454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
39ffa27b-c99f-43cb-949a-eaa258bb7122 47f1a96a-6eaa-4501-8aa7-3d3e30396c76 
                                   1                                    1 
a8474d93-7a90-4d6f-aeb0-6ea3cddbddd0                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
  99029402  108687743 1053945481       NA's 
         1          1          1        454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id:
   5 NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration:
0.15 0.16 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  18   22   27 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   4    5    9 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    3 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id:
1721 1772 a081 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_row:
   a NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity:
2.13 4.01 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume:
13.3 26.7 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-a6-2675-01a-02d-1864-23 tcga-a6-4105-01a-02d-1865-23 
                           1                            1 
tcga-aa-a02k-01a-21d-a094-10                         NA's 
                           1                          454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
294d7bb5-f128-4e7c-81da-3f420186b860 6129db00-21c5-4967-980e-a686ced4e164 
                                   1                                    1 
e6593429-a434-4238-a468-1b1e0f90e2ba                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
 99015400 107030246 107030436      NA's 
        1         1         1       454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.center_id:
  10   23 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.concentration:
0.08 0.15 0.16 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   6   29 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   6   10 NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    3 NA's 
   1    2  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_id:
1864 1865 a094 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_row:
   a    b NA's 
   2    1  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.quantity:
2.08 NA's 
   1  456 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   3  454 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.volume:
  26 NA's 
   1  456 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
   1    2  454 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.949e+07 9.903e+07 9.904e+07 1.960e+08 1.063e+08 1.054e+09       447 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   2    5   13   23 NA's 
   2    1    7    1  446 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.13 0.15 0.16 NA's 
   1    7    2  447 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
   8   18   27   31 NA's 
   7    1    1    1  447 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  10  447 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   4    5    8    9 NA's 
   1    1    1    7  447 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   1    3    8 NA's 
   7    2    1  447 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.248   3.000   2.688   3.000   3.200     447 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
   2    9  446 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  13 13.3   20 NA's 
   1    2    7  447 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   7    3  447 

patient.samples.sample.portions.portion.2.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  22.95   44.77   65.70  164.16   75.81  921.40     446 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
 dna  rna NA's 
   4    7  446 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   d    r NA's 
   4    7  446 



patient.samples.sample.portions.portion.2.analytes.analyte.concentration:
0.13 0.15 0.16 NA's 
   1    8    2  446 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   4  453 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
   1    3  453 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              11 
                                            NA's 
                                             446 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  11  446 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
    adna preparation type mirvana (allprep dna) rna                      NA's 
                        4                         7                       446 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.600   1.800   1.729   1.850   2.000     450 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  8.100   8.400   8.600   8.643   8.700   9.600     450 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
     11     446 

patient.samples.sample.portions.portion.2.analytes.analyte.subportion_sequence:
   1    2 NA's 
   8    1  448 



patient.samples.sample.portions.portion.2.day_of_creation:
   1   15   26   30 NA's 
   7    1    1    2  446 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
  11  446 

patient.samples.sample.portions.portion.2.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.500   7.000   6.636   7.000  12.000     446 

patient.samples.sample.portions.portion.2.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   16.00   21.00   18.55   21.50   31.00     446 

patient.samples.sample.portions.portion.2.portion_sequence:
   1    2    3 NA's 
   1    6    2  448 




patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  10  447 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.75    8.50    9.00   12.75   22.00     449 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_monocyte_infiltration:
   0    1    2 NA's 
   3    3    2  449 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    3.00    5.90    8.25   25.00     447 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_neutrophil_infiltration:
   0    1    2    6 NA's 
   2    3    2    1  449 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.5     4.0     8.8     9.5    40.0     447 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     7.0    10.0    12.1    16.0    37.0     447 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   56.25   74.50   73.20   85.50   99.00     447 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0    70.0    75.0    74.5    80.0    85.0     447 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location:
bottom    top   NA's 
     6      4    447 




patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
  11  446 

patient.samples.sample.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.000   3.000   2.778   4.000   6.000     448 

patient.samples.sample.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   0    1    2    4 NA's 
   2    4    2    1  448 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.000   2.636   4.000  10.000     446 

patient.samples.sample.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.000   2.556   2.000  10.000     448 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   0   10   15   50 NA's 
   8    1    1    1  446 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.000   5.000   7.727  11.500  20.000     446 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   78.50   90.00   82.82   95.00  100.00     446 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  65.00   70.00   85.00   80.91   87.50   95.00     446 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
bottom    top   NA's 
     4      7    446 

patient.samples.sample.portions.portion.2.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.00   30.00   45.73   35.00  150.00     446 

patient.samples.sample.portions.portion.2.year_of_creation:
2009 2010 2011 NA's 
   1    9    1  446 

patient.samples.sample.portions.portion.3.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.780   1.875   1.900   1.911   1.950   2.050     446 



patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
99016765 99029312 99051418 99045442 99065457 99066342      446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.center_id:
  13 NA's 
  11  446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.145   0.150   0.150   0.155   0.170     446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.day_of_shipment:
   8 NA's 
  11  446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  11  446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.month_of_shipment:
   9 NA's 
  11  446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_column:
   1    3    4 NA's 
   3    6    2  446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_id:
a076 NA's 
  11  446 


patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.6     2.9     3.0     3.0     3.1     3.4     446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
  11  446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.volume:
  20 NA's 
  11  446 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 NA's 
  11  446 

patient.samples.sample.portions.portion.3.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  46.43   58.63   65.20   67.14   76.75   86.16     446 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type:
 rna NA's 
  11  446 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type_id:
   r NA's 
  11  446 



patient.samples.sample.portions.portion.3.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.145   0.150   0.150   0.155   0.170     446 

patient.samples.sample.portions.portion.3.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              11 
                                            NA's 
                                             446 

patient.samples.sample.portions.portion.3.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  11  446 

patient.samples.sample.portions.portion.3.analytes.analyte.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                       11                       446 

patient.samples.sample.portions.portion.3.analytes.analyte.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.600   1.800   1.764   1.900   2.200     446 

patient.samples.sample.portions.portion.3.analytes.analyte.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.550   7.900   8.191   8.850   9.800     446 

patient.samples.sample.portions.portion.3.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
     11     446 

patient.samples.sample.portions.portion.3.analytes.analyte.subportion_sequence:
   1    2    4 NA's 
   9    1    1  446 



patient.samples.sample.portions.portion.3.day_of_creation:
   1   27 NA's 
  10    1  446 

patient.samples.sample.portions.portion.3.is_ffpe:
  no NA's 
  11  446 

patient.samples.sample.portions.portion.3.month_of_creation:
   7    9 NA's 
  10    1  446 

patient.samples.sample.portions.portion.3.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0    21.0    21.0    25.0    26.5    54.0     446 

patient.samples.sample.portions.portion.3.portion_sequence:
   1    2    3    5 NA's 
   1    7    2    1  446 




patient.samples.sample.portions.portion.3.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  11  446 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   8.000   8.091  11.500  18.000     446 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_monocyte_infiltration:
   0    1    2    5 NA's 
   4    5    1    1  446 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.500   2.000   2.364   3.000   5.000     446 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.000   1.636   2.000   5.000     446 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   9.727  17.500  35.000     446 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   4.500   8.000   8.636  10.500  28.000     446 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  55.00   69.50   80.00   79.27   92.50   97.00     446 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  65.00   70.00   75.00   75.45   80.00   90.00     446 

patient.samples.sample.portions.portion.3.slides.slide.2.section_location:
bottom    top   NA's 
     7      4    446 




patient.samples.sample.portions.portion.3.slides.slide.is_derived_from_ffpe:
  no NA's 
  11  446 

patient.samples.sample.portions.portion.3.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   8.000   8.364  11.500  15.000     446 

patient.samples.sample.portions.portion.3.slides.slide.percent_monocyte_infiltration:
   0    1    2    3 NA's 
   2    6    1    2  446 

patient.samples.sample.portions.portion.3.slides.slide.percent_necrosis:
   1    2    3   10 NA's 
   4    3    2    2  446 

patient.samples.sample.portions.portion.3.slides.slide.percent_neutrophil_infiltration:
   0    1    2    5 NA's 
   2    3    5    1  446 

patient.samples.sample.portions.portion.3.slides.slide.percent_normal_cells:
   0    2   10   12 NA's 
   8    1    1    1  446 

patient.samples.sample.portions.portion.3.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    9.50   14.00   14.73   17.50   39.00     446 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   75.00   80.00   79.82   86.50   95.00     446 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   72.50   80.00   78.18   82.50   95.00     446 

patient.samples.sample.portions.portion.3.slides.slide.section_location:
bottom    top   NA's 
     4      7    446 

patient.samples.sample.portions.portion.3.weight:
  20   30 NA's 
   3    8  446 

patient.samples.sample.portions.portion.3.year_of_creation:
2010 NA's 
  11  446 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.800   1.800   1.791   1.800   2.210       3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
  10  447 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
  10  447 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
             10             447 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 9.902e+07 1.012e+08 2.025e+08 1.456e+08 1.054e+09       413 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
  40    4  413 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1527  0.1600  0.1800     413 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   11.50   12.41   14.00   29.00     413 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  44  413 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   6.000   6.409   9.000  10.000     413 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.386   5.000   7.000     413 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   1.988   3.100   3.053   4.010   4.810     413 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
  10   34  413 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13 13.3   20 26.7 NA's 
  12    8    4   20  413 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 2013 NA's 
  12   16   16  413 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.134e+08 1.678e+08 1.678e+08 4.045e+08 6.109e+08 1.054e+09       446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7   36 NA's 
   6    5  446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.13 0.14 0.15 0.16 NA's 
   1    2    5    3  446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   5    6    9   26 NA's 
   1    3    2    5  446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  11  446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   7    8   12 NA's 
   4    5    2  446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   6.000   5.273   6.000  10.000     446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
1873 a16w a466 a46x NA's 
   3    1    2    5  446 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.700   0.775   0.920   1.224   1.840   2.000     446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22   23 NA's 
   3    8  446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   5  6.6   13 13.3 NA's 
   5    2    1    3  446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2011 2014 2015 NA's 
   4    2    5  446 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-aa-3685-01a-02r-a32z-07                         NA's 
                           1                          456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
92962fd5-2a14-4e63-ae3f-97cdb83934ab                                 NA's 
                                   1                                  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
150757382      NA's 
        1       456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   7 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.15 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
   2 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
  10 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   4 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
a32z NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   c NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
1.95 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  23 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
  13 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2013 NA's 
   1  456 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.756e+07 1.070e+08 1.740e+08 1.134e+08 1.054e+09        35 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   13 NA's 
 220  234    3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1500  0.1500  0.1525  0.1600  0.1800       3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   20.00   17.76   26.00   31.00       3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 454    3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.132   6.000  11.000       3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.333   4.000  12.000       3 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.60    3.00    3.47    3.54    4.01    4.54       3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 349  105    3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20 26.7 NA's 
 234  220    3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2013 2014 NA's 
 200  218   15   21    3 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.50   71.51  188.32  238.63  356.55  903.30       3 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
 454    3 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r NA's 
 222  232    3 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1500  0.1500  0.1525  0.1600  0.1800       3 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             454 
                                            NA's 
                                               3 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 454    3 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                      NA's 
                      220                       234                         3 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    1.44    1.60    1.60    1.80    5.17       3 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   8.200   8.800   8.691   9.200  10.000       3 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    454       3 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 219   10    3  225 

patient.samples.sample.portions.portion.analytes.analyte.3.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    1.70    1.80    1.61    1.80    2.23     191 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   4  453 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   4  453 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             453 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 98848238 113422312 145632036 132424203 150757012 150757432       438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   7   13 NA's 
  17    2  438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1500  0.1500  0.1547  0.1750  0.1800     438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   2    5   14 NA's 
   7    6    6  438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  19  438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   3    7   10 NA's 
   4    8    7  438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    3.00    5.21   10.00   11.00     438 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.690   1.950   2.080   2.191   2.340   3.000     438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22   23 NA's 
   2   17  438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  13   30 NA's 
  17    2  438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2011 2013 NA's 
   2    6   11  438 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-a6-5656-01a-21r-a46x-36 tcga-a6-6650-01a-11r-a46x-36 
                           1                            1 
tcga-a6-6780-01a-11r-a46x-36                         NA's 
                           1                          454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
5bc6cb0a-7247-4dd1-a063-60b6d446e6dc 7d8c05ef-acec-4a12-9191-ea451feb6227 
                                   1                                    1 
7f493927-0fdb-436d-94db-a422c6fbaadb                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
167814146 167814170 167814235      NA's 
        1         1         1       454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
  36 NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
0.15 NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
  26 NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   3    5 NA's 
   2    1  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
a46x NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   a    f    h NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
0.75 NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  23 NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
   5 NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2015 NA's 
   3  454 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.885e+07 1.070e+08 1.736e+08 1.134e+08 1.054e+09        35 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7   13 NA's 
 234  220    3 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1000  0.1400  0.1283  0.1500  0.1800       3 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   20.00   17.76   26.00   31.00       3 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 454    3 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.132   6.000  11.000       3 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.396   4.000  12.000       3 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.060   2.210   3.000   2.874   3.000   4.810       3 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
 349  105    3 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   13.30   26.70   23.72   30.00   30.00       3 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2013 2014 NA's 
 200  218   15   21    3 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   37.24   76.50   98.23  132.55  498.00       3 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
      rna total rna      NA's 
      234       220         3 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   r    t NA's 
 234  220    3 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1000  0.1400  0.1283  0.1500  0.1800       3 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             454 
                                            NA's 
                                               3 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 454    3 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction              total rna                   NA's 
                   234                    220                      3 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.365   1.520   1.500   1.695   2.380     166 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.800   8.500   8.425   9.100  10.000     166 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
    454       3 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 219   10    3  225 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97514919 97515086 97518832 97577524 97556727 98890464      335 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   9   10 NA's 
 137   17  303 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
 154  303 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0     7.0    12.0    10.8    12.0    29.0     303 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 154  303 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   4    5    6 NA's 
  87   63    4  303 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.208   4.000  11.000     303 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
  40 NA's 
 154  303 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22   23 NA's 
 122   32  303 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  80 NA's 
 154  303 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
 154  303 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-aa-3522-01a-01w-1953-10                         NA's 
                           1                          456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
83437a9b-5fa0-4bec-8883-1ef424c8375b                                 NA's 
                                   1                                  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.biospecimen_barcode_bottom:
113628369      NA's 
        1       456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
  10 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  15 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
  10 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
1953 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   b NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
 3.5 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
   7 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   1  456 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 9.756e+07 1.070e+08 2.798e+08 1.134e+08 1.054e+09        45 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   9   10 NA's 
  17  427   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
 444   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0    12.0    14.0    16.7    24.0    29.0      13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 444   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    5.00    5.00    5.86    7.00   12.00      13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    3.23    4.00   11.00      13 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
 444   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22   23 NA's 
 349   95   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
 444   13 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 2011 2013 2014 NA's 
 200  218   15   11   13 

patient.samples.sample.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    79.0   153.0   126.7   168.0   203.0     108 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                 444                   13 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
 444   13 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
 349  108 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
 444   13 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no  yes NA's 
 349   95   13 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             444 
                                            NA's 
                                              13 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 444   13 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
    444      13 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen   uv spec      NA's 
      122       227       108 

patient.samples.sample.portions.portion.analytes.analyte.4.subportion_sequence:
   1    2    3 NA's 
 212    7    3  235 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.biospecimen_barcode_bottom:
97528983 97529452 97529473 97529500     NA's 
       1        1        1        1      453 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id:
   9   10 NA's 
  19   17  421 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration:
 0.5 NA's 
  36  421 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment:
   5   11   29 NA's 
  17   15    4  421 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  36  421 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment:
   4    5    6 NA's 
  17   15    4  421 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column:
   1    2    3   11 NA's 
  12   11    9    4  421 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id:
1074 a00i a00l NA's 
   4   17   15  421 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity:
  40 NA's 
  36  421 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center:
  22   23 NA's 
   4   32  421 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume:
  80 NA's 
  36  421 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  36  421 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97528967  97556322  97556348  97965961  97556384 100489888       393 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   9   10 NA's 
  17   79  361 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
  96  361 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   11.00   12.00   13.75   12.00   29.00     361 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  96  361 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
   3    5    6   10 NA's 
  17   63    4   12  361 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   3.354   5.000  11.000     361 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
  40 NA's 
  96  361 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  22   23 NA's 
  52   44  361 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
  80 NA's 
  96  361 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2010 NA's 
  96  361 

patient.samples.sample.portions.portion.analytes.analyte.5.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  41.00   61.50   89.00   82.35  102.25  117.00     405 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type:
repli-g x (qiagen) dna                   NA's 
                    96                    361 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type_id:
   x NA's 
  96  361 



patient.samples.sample.portions.portion.analytes.analyte.5.concentration:
 0.5 NA's 
  52  405 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
  96  361 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful:
  no  yes NA's 
  52   44  361 

patient.samples.sample.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              96 
                                            NA's 
                                             361 

patient.samples.sample.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
  96  361 

patient.samples.sample.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
       96       361 

patient.samples.sample.portions.portion.analytes.analyte.5.spectrophotometer_method:
uv spec    NA's 
     52     405 

patient.samples.sample.portions.portion.analytes.analyte.5.subportion_sequence:
   1    3 NA's 
  10    2  445 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.640   1.900   1.900   1.886   1.900   2.210      54 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.additional_studies.additional_study.disease_code:
fppp NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.additional_studies.additional_study.project_code:
tcga NA's 
   1  456 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.050e+09 1.053e+09 9.041e+08 1.053e+09 1.053e+09       419 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   10.00   24.00   18.59   24.00   24.00     418 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1300  0.1300  0.1226  0.1500  0.1700     419 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
  13   14   29   31 NA's 
   1   10    4   23  419 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
  38  419 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   2    3    4    8 NA's 
   9   24    1    4  419 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.132   4.000  11.000     419 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.260   0.870   1.070   1.419   2.000   3.470     419 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  22   23 NA's 
  37    2  418 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.67    6.67   15.60   20.00   40.00     419 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2011 2013 NA's 
  37    1  419 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.additional_studies.additional_study.disease_code:
fppp NA's 
   2  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.additional_studies.additional_study.project_code:
tcga NA's 
   2  455 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.322e+08 1.053e+09 1.053e+09 9.988e+08 1.053e+09 1.053e+09       423 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   23.00   23.00   21.18   23.00   24.00     423 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1300  0.1450  0.1403  0.1575  0.1700     423 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   22.00   22.00   23.29   22.00   31.00     423 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.is_derived_from_ffpe:
  no NA's 
  34  423 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.month_of_shipment:
   2    3 NA's 
   1   33  423 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     1.0     1.5     2.5     2.0    11.0     423 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   0.970   1.000   1.206   1.365   2.000     446 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.source_center:
  22   23 NA's 
   9   25  423 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67   11.52   10.00   40.00     446 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.year_of_shipment:
2011 2013 NA's 
  32    2  423 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.additional_studies.additional_study.disease_code:
fppp NA's 
   2  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.additional_studies.additional_study.project_code:
tcga NA's 
   2  455 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.070e+08 1.053e+09 7.001e+08 1.053e+09 1.053e+09       425 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00   23.00   13.33   23.00   23.00     424 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1300  0.1500  0.1394  0.1600  0.1700     424 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   22.00   28.00   25.79   29.00   30.00     424 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.is_derived_from_ffpe:
  no NA's 
  33  424 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.month_of_shipment:
   2    3    6    8 NA's 
   1   22    9    1  424 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.697   2.000   5.000     424 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.730   1.730   1.778   2.000   2.600     441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.source_center:
  22   23 NA's 
  14   19  424 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.volume:
 6.7 13.3   20   40 NA's 
   2   12    1    1  441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.year_of_shipment:
2011 2013 NA's 
  31    2  424 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.additional_studies.additional_study.disease_code:
fppp NA's 
   2  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.additional_studies.additional_study.project_code:
tcga NA's 
   2  455 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.136e+08 1.136e+08 1.551e+08 1.136e+08 1.053e+09       433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.center_id:
   5    8   10   24 NA's 
   1    1   21    1  433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1300  0.1300  0.1358  0.1500  0.1700     433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.day_of_shipment:
   3   13   15   31 NA's 
   1    2   20    1  433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.is_derived_from_ffpe:
  no NA's 
  24  433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.month_of_shipment:
   3    8 NA's 
   3   21  433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.208   3.000   9.000     433 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.26    2.60    2.60    2.53    3.00    3.40     433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.source_center:
  22   23 NA's 
  21    3  433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.volume:
   2  6.7   20 NA's 
   1    1   22  433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.year_of_shipment:
2011 2013 2015 NA's 
  21    2    1  433 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.additional_studies.additional_study.disease_code:
fppp NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.additional_studies.additional_study.project_code:
tcga NA's 
   3  454 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.322e+08 1.322e+08 1.322e+08 3.190e+08 1.456e+08 1.053e+09       452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.center_id:
   1    8   10   23 NA's 
   1    1    2    1  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.concentration:
0.07 0.08 0.13 0.15 NA's 
   1    2    1    1  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.day_of_shipment:
   3   13   14   22 NA's 
   1    2    1    1  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.is_derived_from_ffpe:
  no NA's 
   5  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.month_of_shipment:
   3    8 NA's 
   4    1  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.plate_column:
   1    3    5    9 NA's 
   2    1    1    1  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.plate_id:
1618 a270 a274 a46w NA's 
   1    2    1    1  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.plate_row:
   a    b    f    h NA's 
   1    2    1    1  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.quantity:
1.01  1.4  1.6 NA's 
   1    1    2  453 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.source_center:
  23 NA's 
   5  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.volume:
 6.7   20 NA's 
   1    3  453 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.year_of_shipment:
2011 2013 2015 NA's 
   1    3    1  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.additional_studies.additional_study.disease_code:
fppp NA's 
   2  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.additional_studies.additional_study.project_code:
tcga NA's 
   2  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.bcr_aliquot_barcode:
tcga-a6-2677-01a-01d-a274-01 tcga-a6-2684-01a-01d-a27a-05 
                           1                            1 
tcga-aa-3518-01a-02d-1953-10                         NA's 
                           1                          454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.bcr_aliquot_uuid:
5edb7fd3-9802-4c1e-9956-b5fcfb900f28 8c9ff2b3-a02f-4ec1-b8c8-863e17447a23 
                                   1                                    1 
a2528ea7-21ad-492b-b661-7dbfda61253e                                 NA's 
                                   1                                  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.biospecimen_barcode_bottom:
113628361 132159975 145632063      NA's 
        1         1         1       454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.center_id:
   1    5   10 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.concentration:
0.13 0.15 NA's 
   1    2  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.day_of_shipment:
  13   14   15 NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.is_derived_from_ffpe:
  no NA's 
   3  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.month_of_shipment:
   3    8 NA's 
   2    1  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.plate_column:
   1    2 NA's 
   2    1  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.plate_id:
1953 a274 a27a NA's 
   1    1    1  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.plate_row:
   a    b NA's 
   1    2  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.quantity:
1.01  2.6 NA's 
   2    1  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.source_center:
  22   23 NA's 
   1    2  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.volume:
 6.7   20 NA's 
   2    1  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.year_of_shipment:
2011 2013 NA's 
   1    2  454 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.additional_studies.additional_study.disease_code:
fppp NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.additional_studies.additional_study.project_code:
tcga NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.bcr_aliquot_barcode:
tcga-a6-2677-01a-01d-a27a-05 tcga-a6-2684-01a-01d-a46w-08 
                           1                            1 
                        NA's 
                         455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.bcr_aliquot_uuid:
5894c3e4-d97b-4ec5-8cb1-c233060e93a1 ec4ec0ce-81cb-4338-9ef0-47f42a969300 
                                   1                                    1 
                                NA's 
                                 455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.biospecimen_barcode_bottom:
132159779 132179227      NA's 
        1         1       455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.center_id:
   5    8 NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.concentration:
0.08 0.15 NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.day_of_shipment:
   3   13 NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.is_derived_from_ffpe:
  no NA's 
   2  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.month_of_shipment:
   3    8 NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.plate_column:
   1    5 NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.plate_id:
a27a a46w NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.plate_row:
   a    b NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.quantity:
1.01  1.6 NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.source_center:
  23 NA's 
   2  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.volume:
 6.7   20 NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.16.year_of_shipment:
2013 2015 NA's 
   1    1  455 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.bcr_aliquot_barcode:
tcga-a6-2677-01a-01d-a46w-08                         NA's 
                           1                          456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.bcr_aliquot_uuid:
a07309f8-b9e9-41b6-ab8e-ad6d4f45344c                                 NA's 
                                   1                                  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.biospecimen_barcode_bottom:
132179229      NA's 
        1       456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.center_id:
   8 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.concentration:
0.08 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.day_of_shipment:
   3 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.is_derived_from_ffpe:
  no NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.month_of_shipment:
   8 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.plate_column:
   4 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.plate_id:
a46w NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.plate_row:
   g NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.quantity:
 1.6 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.source_center:
  23 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.volume:
  20 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.17.year_of_shipment:
2015 NA's 
   1  456 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.949e+07 9.885e+07 1.087e+08 2.766e+08 1.322e+08 1.054e+09        34 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   3.015   5.000  24.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1400  0.1500  0.1521  0.1500  2.1000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   14.00   22.00   18.23   26.00   31.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 455    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   4.000   5.084   6.000  11.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    3.00    2.91    4.00   10.00       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.87    1.00    1.07    2.05    3.74   27.30       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 349  106    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.60    6.67    6.67   13.62   26.70   40.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2013 2014 NA's 
 200  218   16   21    2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.885e+07 1.087e+08 2.779e+08 1.456e+08 1.054e+09        36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000  10.000  10.000   8.733  10.000  23.000       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.01000 0.05000 0.08000 0.09617 0.15000 0.18000       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   17.00   22.00   19.69   27.00   30.00       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 454    3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.295   6.000  11.000       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   2.729   4.000   6.000       3 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.010   2.340   2.219   3.000   4.540       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 348  106    3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   40.00   32.46   60.00   60.00       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2013 2014 NA's 
 199  219   15   21    3 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.949e+07 9.885e+07 1.087e+08 2.783e+08 1.457e+08 1.054e+09        38 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   9.000   9.718  10.000  23.000       6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.140   0.150   0.134   0.150   0.180       6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   11.00   22.00   18.94   27.00   31.00       6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 451    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.415   6.000  11.000       6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   2.736   4.000   6.000       6 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.670   1.010   1.140   2.009   3.740   4.540      81 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
 273  178    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.60    6.70   13.30   15.55   26.70   40.00      81 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2013 2014 NA's 
 199  218   15   19    6 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.751e+07 1.070e+08 1.070e+08 4.712e+08 1.053e+09 1.054e+09        53 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00   10.00   13.98   23.00   24.00      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1392  0.1500  0.1700      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   14.00   28.00   21.78   29.00   31.00      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 416   41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   6.000   5.692   7.000  11.000      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    3.18    4.00   10.00      41 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.260   1.000   1.860   1.763   2.000  30.000     161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
 227  189   41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.67   13.30   15.12   13.30  600.00     161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
  83  333   41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   5  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   5  452 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5             452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.648e+07 1.005e+08 1.050e+09 6.963e+08 1.053e+09 1.053e+09       236 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   23.00   16.93   23.00   24.00     236 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.140   0.150   0.143   0.150   0.180     236 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   21.00   22.00   22.26   29.00   31.00     236 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
 221  236 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    3.00    3.38    3.00    9.00     236 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.909   5.000  10.000     236 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.280   1.000   1.070   1.839   2.000  17.330     325 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
  71  150  236 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.67    6.67   14.75   20.00  133.30     325 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2011    2015     236 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.disease_code:
fppp NA's 
   5  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.project_code:
tcga NA's 
   5  452 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5             452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.134e+08 1.050e+09 6.440e+08 1.053e+09 1.053e+09       268 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00   10.00   10.85   23.00   24.00     259 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1300  0.1400  0.1308  0.1500  0.1700     265 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   22.00   28.00   23.77   28.00   31.00     265 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
 192  265 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   2    3    6    8 NA's 
  36   89   25   42  265 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   3.984   5.000   9.000     265 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.860   2.000   3.307   4.270  27.000     313 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
 101   97  259 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   34.07   40.00  540.00     313 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2010 2011 2013 NA's 
   1  186    5  265 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.disease_code:
fppp NA's 
   5  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.project_code:
tcga NA's 
   5  452 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5             452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.136e+08 1.136e+08 4.323e+08 1.050e+09 1.053e+09       333 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   10.00   11.97   10.00   24.00     314 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0800  0.1400  0.1243  0.1500  0.1700     333 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   14.00   24.00   21.87   29.00   31.00     333 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
 124  333 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   8.000   5.677   8.000   8.000     333 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.137   6.000  11.000     333 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.000   2.800   2.791   3.000   6.400     336 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
 109   34  314 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   20.00   20.00   26.39   40.00   80.00     336 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 2012 2013 2015 NA's 
 116    2    5    1  333 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.050e+09 1.050e+09 8.371e+08 1.050e+09 1.053e+09       409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00    9.00   10.98   10.00   24.00     409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0775  0.1300  0.1600     409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      3      14      14      15      14      29     409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
  48  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   2.000   3.271   3.000   8.000     409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   3.812   5.250  10.000     409 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.000   2.000   2.051   2.000   6.000     414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22   23 NA's 
  38   10  409 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
6.67   20   40 NA's 
   3    8   32  414 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 2012 2015 NA's 
  43    1    4  409 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 9.885e+07 1.087e+08 3.054e+08 1.136e+08 1.054e+09       103 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   7  23 
148 231   1  77 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1531  0.1500  2.1000      77 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   22.00   17.93   26.00   28.00      77 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 380   77 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    4.00    5.00    5.15    6.00   11.00      77 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   2.861   4.000   6.000      77 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.500   1.636   2.000  56.070      78 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
350 107 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.6     6.7    10.0    10.1    13.3    26.7      78 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2014 NA's 
 185  193    2   77 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.86  127.70  302.70  321.19  458.59 1409.00 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna rna 
456   1 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d   r 
456   1 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0900  0.1400  0.1500  0.1524  0.1500  2.1000 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 456    1 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 233  223    1 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             457 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
457 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
 adna preparation type allprep rna extraction 
                   456                      1 

patient.samples.sample.portions.portion.analytes.analyte.rna.ratio_28s_18s:
 1.2 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.rna.rinvalue:
 7.6 NA's 
   1  456 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
       54       403 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 219   10    3  225 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    4.00   15.00   14.18   23.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
457 

patient.samples.sample.portions.portion.lce:
 lce NA's 
   8  449 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.538   6.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000  11.000   7.571  11.000  72.000 

patient.samples.sample.portions.portion.portion_sequence:
   1    2    3    4 NA's 
  69   22    5    2  359 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 362   95 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   3.000   5.000   7.525  10.000  40.000     335 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   1.408   2.000  10.000     354 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   3.828   5.000  20.000      96 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   2.000   4.009   5.000  50.000     349 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   5.493   8.000  60.000      98 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    7.00   10.00   13.97   20.00   70.00     102 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   70.00   80.00   77.06   87.00  100.00     100 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   75.00   80.00   77.96   80.00   96.00      96 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    55    307     95 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
457 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.750   5.000   7.378  10.000  40.000     261 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   1.322   2.000   8.000     280 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   3.000   4.615   8.000  25.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   2.000   3.368   4.000  50.000     275 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   6.398  10.000 100.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    6.00   12.00   15.05   20.00   76.00       4 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   65.00   75.00   74.21   85.00  100.00       5 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   70.00   80.00   75.75   80.00   96.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
   307    150 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      9      30      40      72     114     183       6 

patient.samples.sample.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2009    2010    2011    2011    2011    2014 


patient.samples.sample.portions.shipment_portion.2.center_id:
  20   27 NA's 
  13   41  403 

patient.samples.sample.portions.shipment_portion.2.is_ffpe:
  no NA's 
  54  403 

patient.samples.sample.portions.shipment_portion.2.plate_id:
2150 a17m a20o NA's 
  21   13   20  403 

patient.samples.sample.portions.shipment_portion.2.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   22.00   22.00   25.52   31.00   41.00     403 

patient.samples.sample.portions.shipment_portion.2.portion_sequence:
   1    2    3    4 NA's 
   3    6   21    3  424 


patient.samples.sample.portions.shipment_portion.2.shipment_portion_day_of_shipment:
  17   25 NA's 
  13   41  403 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_month_of_shipment:
   1    8 NA's 
  41   13  403 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_year_of_shipment:
2011 2012 NA's 
  13   41  403 

patient.samples.sample.portions.shipment_portion.3.bcr_shipment_portion_uuid:
b6abe5cf-db70-41ff-a1e1-9b0663a1d9cd e6208375-5da0-4a01-9886-837378cf267b 
                                   1                                    1 
f821666b-504c-4946-b5fc-c5c9914b37bd fd9fcdb4-e3bb-4f85-8ae2-aff9e77cbb68 
                                   1                                    1 
                                NA's 
                                 453 

patient.samples.sample.portions.shipment_portion.3.center_id:
  27 NA's 
   4  453 

patient.samples.sample.portions.shipment_portion.3.is_ffpe:
  no NA's 
   4  453 

patient.samples.sample.portions.shipment_portion.3.plate_id:
a20o NA's 
   4  453 

patient.samples.sample.portions.shipment_portion.3.portion_number:
  31   41 NA's 
   2    2  453 

patient.samples.sample.portions.shipment_portion.3.portion_sequence:
   3    4 NA's 
   2    2  453 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_bcr_aliquot_barcode:
tcga-aa-a00a-01a-41-a20o-27 tcga-aa-a00k-01a-31-a20o-27 
                          1                           1 
tcga-aa-a00n-01a-41-a20o-27 tcga-aa-a00r-01a-31-a20o-27 
                          1                           1 
                       NA's 
                        453 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_day_of_shipment:
  25 NA's 
   4  453 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_month_of_shipment:
   1 NA's 
   4  453 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_year_of_shipment:
2012 NA's 
   4  453 


patient.samples.sample.portions.shipment_portion.center_id:
  20   27 NA's 
 345   19   93 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 364   93 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   13.00   21.00   19.04   21.00   81.00      93 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.048   2.000   5.000     374 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  16   17   25   30 NA's 
 269   49   19   27   93 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   1    7    8 NA's 
  19   27  318   93 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2011 2012 2014 NA's 
 318   19   27   93 

patient.samples.sample.sample_type:
primary tumor 
          457 

patient.samples.sample.sample_type_id:
  1 
457 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.4000  0.4211  0.5000  1.0000     107 

patient.samples.sample.vial_number:
  a   b 
454   3 

sequenced:
   0    1 NA's 
  47  154  256 

microRNA:
   0    1 NA's 
  15  186  256 

SNP6:
   0    1 NA's 
  16  185  256 

WGS:
   0    1 NA's 
 139   62  256 

RNAseq:
   0    1 NA's 
  53  148  256 

Agilent.expression:
   0    1 NA's 
  50  151  256 

methylation:
   0    1 NA's 
  35  166  256 

normal_meth:
   0    1 NA's 
 164   37  256 

complete:
   0    1 NA's 
  68  133  256 

MSI_status:
MSI-H MSI-L   MSS  NA's 
   37    40   124   256 

methylation_subtype:
  CIMP.H   CIMP.L Cluster3 Cluster4     NA's 
      33       40       47       46      291 

MLH1_silencing:
   0    1 NA's 
 138   28  291 

expression_subtype:
     CIN Invasive MSI/CIMP     NA's 
      56       37       58      306 

hypermutated:
   0    1 NA's 
 122   32  303 

silent_mutrate:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2140  0.6745  0.9585  3.6433  1.7100 67.5000     303 

nonsilent_mutrate:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.860   2.095   3.000  11.555   4.930 262.000     303 

cancer:
Colon  NA's 
  201   256 


tumor_site:
     1 - right colon 2 - transverse colon       3 - left colon 
                  92                   18                   85 
          4 - rectum                 NA's 
                   1                  261 

age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  34.00   63.00   71.00   69.73   80.00   90.00     256 

days_to_death.y:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   37.75  137.00  295.86  478.25 1188.00     435 

days_to_last_followup.y:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    30.0   247.6   437.0  2180.0     256 

days_to_last_known_alive.y:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    31.0   253.6   437.0  2180.0     256 

distant_metastasis_pathologic_spread:
  M0   M1  M1a NA's 
 165   30    2  260 

gender.y:
FEMALE   MALE   NA's 
    99    102    256 

histological_type.y:
         Colon Adenocarcinoma Colon Mucinous Adenocarcinoma 
                          171                            27 
                         NA's 
                          259 

history_of_colon_polyps:
  NO  YES NA's 
  92   97  268 

icd_o_3_histology:
 8010/3 8140/11  8140/3  8480/3    NA's 
      1       1     174      24     257 


lymphatic_invasion_present:
  NO  YES NA's 
  83  100  274 


lymphnodes_examined:
  NO  YES NA's 
   2  198  257 

number_of_first_degree_relatives_with_cancer_diagnosis:
   0    1    2    3 NA's 
 168   22    4    2  261 

number_of_lymphnodes_examined:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   16.00   22.00   23.21   29.00   55.00     259 

number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    2.03    2.00   32.00     260 

person_neoplasm_cancer_status:
TUMOR FREE WITH TUMOR       NA's 
        30        170        257 

preoperative_pretreatment_cea_level:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.20    1.51    2.80   50.51    8.76 2614.00     328 


prior_diagnosis:
  NO  YES NA's 
 162   39  256 

residual_tumor.y:
  R0   R1   R2   RX NA's 
 167    1   23    2  264 

synchronous_colon_cancer_present:
  NO  YES NA's 
 179   17  261 


vascular_invasion_present:
  NO  YES NA's 
 124   44  289 

vital_status.y:
DECEASED   LIVING     NA's 
      22      179      256 

}}
\keyword{datasets}
