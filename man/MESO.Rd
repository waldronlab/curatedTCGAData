\name{ MESO }
\alias{ MESO }
\docType{data}
\title{ Mesothelioma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( MESO )
ExperimentList class object of length 8: 
 [1] CNASNP-20160128: RaggedExperiment with 90003 rows and 173 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 18335 rows and 172 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 87 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 87 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 87 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 87 columns 
 [7] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 87 columns 
 [8] RPPAArray-20160128: SummarizedExperiment with 193 rows and 63 columns 

> rownames( MESO )
CharacterList of length 8
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363

> colnames( MESO )
CharacterList of length 8
[["CNASNP-20160128"]] TCGA-3H-AB3K-01A-11D-A39Q-01 ...
[["CNVSNP-20160128"]] TCGA-3H-AB3K-01A-11D-A39Q-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-3H-AB3K-01A-11D-A39Q-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-3H-AB3K-01A-11D-A39Q-01 ...
[["Methylation-20160128"]] TCGA-3H-AB3K-01A-11D-A39S-05 ...
[["miRNASeqGene-20160128"]] TCGA-3H-AB3K-01A-11R-A404-13 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-3H-AB3K-01A-11R-A40A-07 ...
[["RPPAArray-20160128"]] TCGA-3H-AB3K-01A-21-A45O-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  2.5 Mb
2                   CNVSNP-20160128  0.6 Mb
3         GISTIC_AllByGene-20160128  4.3 Mb
4 GISTIC_ThresholdedByGene-20160128  4.3 Mb
5              Methylation-20160128 67.4 Mb
6             miRNASeqGene-20160128  0.1 Mb
7          RNASeq2GeneNorm-20160128  1.1 Mb
8                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   14 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  73.00   73.00    1.25    1.11    1.67 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  28.00   57.00   64.00   62.99   69.00   81.00 

vital_status:
 0  1 
13 74 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   253.0   457.0   584.1   789.0  2790.0      14 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -8     499    1168    1091    1490    2359      74 

tumor_tissue_site:
pleura 
    87 



pathology_N_stage:
n0 n1 n2 n3 nx 
44 10 26  3  4 

pathology_M_stage:
m0 m1 mx 
57  3 27 

gender:
female   male 
    16     71 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1999    2008    2010    2009    2012    2013 

radiation_therapy:
  no  yes NA's 
  62   24    1 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   80.00   90.00   77.65   90.00  100.00      70 

histological_type:
               biphasic mesothelioma diffuse malignant mesothelioma - nos 
                                  23                                    5 
            epithelioid mesothelioma             sarcomatoid mesothelioma 
                                  57                                    2 

residual_tumor:
  r0   r1   r2   rx NA's 
  17    3   15   11   41 

race:
                    asian black or african american                     white 
                        1                         1                        85 

ethnicity:
not hispanic or latino                   NA's 
                    73                     14 

admin.bcr:
nationwide children's hospital 
                            87 

admin.day_of_dcc_upload:
 1 
87 

admin.disease_code:
meso 
  87 


admin.month_of_dcc_upload:
10 
87 

admin.patient_withdrawal.withdrawn:
false 
   87 

admin.project_code:
tcga 
  87 

admin.year_of_dcc_upload:
2015 
  87 

patient.asbestos_exposure_age:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   15.75   17.00   20.42   24.25   37.00      75 

patient.asbestos_exposure_age_last:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  21.00   25.00   32.00   33.89   35.00   58.00      78 

patient.asbestos_exposure_source:
occupational    secondary         NA's 
          33            5           49 

patient.asbestos_exposure_type:
    amosite crocidolite        NA's 
          1           1          85 

patient.asbestos_exposure_years:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.75   13.00   15.25   24.75   34.00      71 


patient.blood_relative_cancer_history_list.blood_relative_cancer_history.2.family_history_cancer_type:
      melanoma   mesothelioma other, specify           NA's 
             1              2             15             69 


patient.blood_relative_cancer_history_list.blood_relative_cancer_history.2.family_medical_history_relative_family_member_relationship_type:
      child grandparent      parent     sibling        NA's 
          1           2           5          10          69 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.3.family_history_cancer_type:
  mesothelioma other, specify           NA's 
             1              3             83 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.3.family_history_cancer_type_other:
pancreatic cancer       skin cancer              NA's 
                2                 1                84 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.3.family_medical_history_relative_family_member_relationship_type:
grandparent      parent     sibling        NA's 
          1           1           2          83 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.family_history_cancer_type:
other, specify           NA's 
             1             86 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.family_history_cancer_type_other:
colon cancer         NA's 
           1           86 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.4.family_medical_history_relative_family_member_relationship_type:
grandparent        NA's 
          1          86 

patient.blood_relative_cancer_history_list.blood_relative_cancer_history.family_history_cancer_type:
  mesothelioma other, specify           NA's 
             2             28             57 


patient.blood_relative_cancer_history_list.blood_relative_cancer_history.family_medical_history_relative_family_member_relationship_type:
      child grandparent      parent     sibling        NA's 
          2           2          21           5          57 

patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
       65        22 

patient.clinical_cqcf.country:
     australia         brazil united kingdom  united states 
            12              3              5             67 

patient.clinical_cqcf.days_to_consent:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1501.00    -3.00    27.00    59.05    55.00  1404.00       22 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  107.0   208.8   491.5   650.4   747.5  2790.0      65 

patient.clinical_cqcf.histological_type:
   diffuse malignant mesothelioma - biphasic 
                                          23 
diffuse malignant mesothelioma - epithelioid 
                                          58 
        diffuse malignant mesothelioma - nos 
                                           4 
 diffuse malignant mesothelioma -sarcomatoid 
                                           2 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                               no yes, radiation prior to resection 
                               86                                 1 

patient.clinical_cqcf.normal_tissue_anatomic_site:
peritoneum       NA's 
         1         86 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
            1            86 

patient.clinical_cqcf.tumor_type:
primary 
     87 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   10.00   14.71   25.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -29905  -25384  -23591  -23188  -20999  -10510 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   39.0   208.5   445.0   578.1   781.0  2790.0      29 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -8.0   256.0   463.0   553.3   739.0  1575.0      58 



patient.drugs.drug.2.clinical_trail_drug_classification:
anti-ctla4 monoclonal antibody                           NA's 
                             1                             86 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     9.0    12.0    15.9    23.0    30.0      45 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   144.5   180.5   256.7   277.2   919.0      49 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0    56.5   128.0   171.9   174.0   898.0      49 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                          10                            3 
            partial response               stable disease 
                           5                            9 
                        NA's 
                          60 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.250   6.000   6.071   8.000  12.000      45 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  41    1   45 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                        37                          5 
                      NA's 
                        45 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  33    9   45 

patient.drugs.drug.2.year_of_form_completion:
2013 2014 2015 NA's 
  14   20    8   45 



patient.drugs.drug.3.clinical_trail_drug_classification:
anti-ctla4 monoclonal antibody                           NA's 
                             1                             86 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   11.00   15.65   22.50   30.00      64 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  161.0   177.0   275.0   346.8   481.5   672.0      67 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   56.0   103.8   156.5   260.5   424.0   627.0      65 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                           6                            2 
            partial response               stable disease 
                           4                            3 
                        NA's 
                          72 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   6.000   5.174   7.000  12.000      64 

patient.drugs.drug.3.therapy_ongoing:
  no NA's 
  23   64 

patient.drugs.drug.3.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                        21                          1 
                   vaccine                       NA's 
                         1                         64 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  17    6   64 

patient.drugs.drug.3.year_of_form_completion:
2013 2014 2015 NA's 
   6   12    5   64 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    7.50   11.00   14.18   22.00   28.00      76 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  215.0   280.0   429.0   485.9   625.5  1060.0      76 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   74.0   192.0   308.0   355.9   522.5   802.0      76 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease             partial response 
                           3                            4 
              stable disease                         NA's 
                           2                           78 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   6.000   5.909   8.000  12.000      76 

patient.drugs.drug.4.therapy_ongoing:
  no NA's 
  11   76 

patient.drugs.drug.4.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         8                          3 
                      NA's 
                        76 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
   8    3   76 

patient.drugs.drug.4.year_of_form_completion:
2013 2014 2015 NA's 
   3    5    3   76 

patient.drugs.drug.5.bcr_drug_barcode:
tcga-3h-ab3m-d60592 tcga-3h-ab3o-d60486 tcga-3u-a98e-d65109 tcga-sc-a6lm-d49075 
                  1                   1                   1                   1 
               NA's 
                 83 

patient.drugs.drug.5.bcr_drug_uuid:
271365e2-1da4-4b0e-8c8d-9e06ee0d87e3 323c388c-ac8e-4f48-bd0c-feb1b39330a1 
                                   1                                    1 
5f4453b4-63a8-49fa-ad6b-64f199844556 abdbef67-cbed-4b4d-abed-8b589e7758ab 
                                   1                                    1 
                                NA's 
                                  83 

patient.drugs.drug.5.day_of_form_completion:
  10   11   22   23 NA's 
   1    1    1    1   83 

patient.drugs.drug.5.days_to_drug_therapy_end:
 386  634 1265 1430 NA's 
   1    1    1    1   83 

patient.drugs.drug.5.days_to_drug_therapy_start:
 120  571 1084 1286 NA's 
   1    1    1    1   83 

patient.drugs.drug.5.drug_name:
         carboplatin            cediranib           everolimus 
                   1                    1                    1 
tivantinib (arq-197)                 NA's 
                   1                   83 

patient.drugs.drug.5.measure_of_response:
clinical progressive disease             partial response 
                           2                            1 
              stable disease                         NA's 
                           1                           83 

patient.drugs.drug.5.month_of_form_completion:
   1    6   10 NA's 
   1    2    1   83 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
   4   83 

patient.drugs.drug.5.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         1                          3 
                      NA's 
                        83 

patient.drugs.drug.5.tx_on_clinical_trial:
  no  yes NA's 
   1    3   83 

patient.drugs.drug.5.year_of_form_completion:
2013 2014 2015 NA's 
   1    2    1   83 

patient.drugs.drug.6.bcr_drug_barcode:
tcga-3h-ab3m-d60593 tcga-3h-ab3o-d60489 tcga-3u-a98e-d65113 tcga-sc-a6lm-d49076 
                  1                   1                   1                   1 
               NA's 
                 83 

patient.drugs.drug.6.bcr_drug_uuid:
2031fb30-84aa-472d-ad8b-f8b00fa789c7 56020589-2bbe-4362-b806-2f93399c47a2 
                                   1                                    1 
5babfacf-c446-4759-a245-852bfa171478 81c15bbb-ee44-4c1a-a028-ab336bb1a0d9 
                                   1                                    1 
                                NA's 
                                  83 

patient.drugs.drug.6.day_of_form_completion:
  10   11   22   23 NA's 
   1    1    1    1   83 

patient.drugs.drug.6.days_to_drug_therapy_end:
 760  800 1265 1489 NA's 
   1    1    1    1   83 

patient.drugs.drug.6.days_to_drug_therapy_start:
 736  739 1084 1446 NA's 
   1    1    1    1   83 

patient.drugs.drug.6.drug_name:
carboplatin   cisplatin    ds-8273a  pemetrexed        NA's 
          1           1           1           1          83 

patient.drugs.drug.6.measure_of_response:
clinical progressive disease             partial response 
                           2                            1 
              stable disease                         NA's 
                           1                           83 

patient.drugs.drug.6.month_of_form_completion:
   1    6   10 NA's 
   1    2    1   83 

patient.drugs.drug.6.therapy_ongoing:
  no NA's 
   4   83 

patient.drugs.drug.6.therapy_types.therapy_type:
chemotherapy         NA's 
           3           84 

patient.drugs.drug.6.tx_on_clinical_trial:
  no  yes NA's 
   1    3   83 

patient.drugs.drug.6.year_of_form_completion:
2013 2014 2015 NA's 
   1    2    1   83 

patient.drugs.drug.7.bcr_drug_barcode:
tcga-3h-ab3m-d60595 tcga-3h-ab3o-d60490 tcga-sc-a6lm-d49077                NA's 
                  1                   1                   1                  84 

patient.drugs.drug.7.bcr_drug_uuid:
8017f9c5-2f43-47ae-bc78-5d2b7a4dede3 8165b981-d8b6-4cca-80aa-652fcc9ed909 
                                   1                                    1 
d5babb86-cc75-4990-bf03-8bfd62f5ff35                                 NA's 
                                   1                                   84 

patient.drugs.drug.7.day_of_form_completion:
  10   11   23 NA's 
   1    1    1   84 

patient.drugs.drug.7.days_to_drug_therapy_end:
 604  816 1265 NA's 
   1    1    1   84 

patient.drugs.drug.7.days_to_drug_therapy_start:
 471  790 1084 NA's 
   1    1    1   84 

patient.drugs.drug.7.drug_name:
doxorubicin  pemetrexed vinorelbine        NA's 
          1           1           1          84 

patient.drugs.drug.7.measure_of_response:
partial response   stable disease             NA's 
               1                2               84 

patient.drugs.drug.7.month_of_form_completion:
   6   10 NA's 
   2    1   84 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   3   84 

patient.drugs.drug.7.therapy_types.therapy_type:
chemotherapy         NA's 
           3           84 

patient.drugs.drug.7.tx_on_clinical_trial:
  no  yes NA's 
   1    2   84 

patient.drugs.drug.7.year_of_form_completion:
2013 2014 NA's 
   1    2   84 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-3h-ab3m-d60596                NA's 
                  1                  86 

patient.drugs.drug.8.bcr_drug_uuid:
3db085c4-dc87-4e33-86d3-1618dc6adf39                                 NA's 
                                   1                                   86 

patient.drugs.drug.8.day_of_form_completion:
  11 NA's 
   1   86 

patient.drugs.drug.8.days_to_drug_therapy_end:
 604 NA's 
   1   86 

patient.drugs.drug.8.days_to_drug_therapy_start:
 471 NA's 
   1   86 

patient.drugs.drug.8.drug_name:
bevacizumab        NA's 
          1          86 

patient.drugs.drug.8.measure_of_response:
stable disease           NA's 
             1             86 

patient.drugs.drug.8.month_of_form_completion:
   6 NA's 
   1   86 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   1   86 

patient.drugs.drug.8.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                         86 

patient.drugs.drug.8.tx_on_clinical_trial:
 yes NA's 
   1   86 

patient.drugs.drug.8.year_of_form_completion:
2014 NA's 
   1   86 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-3h-ab3m-d60597                NA's 
                  1                  86 

patient.drugs.drug.9.bcr_drug_uuid:
c621d9b9-8ccc-4b79-9194-7959179b3814                                 NA's 
                                   1                                   86 

patient.drugs.drug.9.day_of_form_completion:
  11 NA's 
   1   86 

patient.drugs.drug.9.days_to_drug_therapy_end:
 604 NA's 
   1   86 

patient.drugs.drug.9.days_to_drug_therapy_start:
 471 NA's 
   1   86 

patient.drugs.drug.9.drug_name:
temsirolimus         NA's 
           1           86 

patient.drugs.drug.9.measure_of_response:
stable disease           NA's 
             1             86 

patient.drugs.drug.9.month_of_form_completion:
   6 NA's 
   1   86 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   1   86 

patient.drugs.drug.9.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                         86 

patient.drugs.drug.9.tx_on_clinical_trial:
 yes NA's 
   1   86 

patient.drugs.drug.9.year_of_form_completion:
2014 NA's 
   1   86 



patient.drugs.drug.clinical_trail_drug_classification:
anti-ctla4 monoclonal antibody           copa chelating agent 
                             1                              1 
            defactinib/placebo                        vaccine 
                             1                              1 
                          NA's 
                            83 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00    9.50   14.62   23.00   30.00      23 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    58.0    96.0   180.7   228.0  1056.0      28 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   35.25   56.00  113.36  136.75  910.00      29 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          17                            3 
            partial response               stable disease 
                           5                            7 
                        NA's 
                          55 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.594   8.000  12.000      23 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
  63    1   23 

patient.drugs.drug.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                        12                         35 
targeted molecular therapy                    vaccine 
                        11                          1 
                      NA's 
                        28 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  31   33   23 

patient.drugs.drug.year_of_form_completion:
2013 2014 2015 NA's 
  18   40    6   23 

patient.eastern_cancer_oncology_group:
   0    1    2    3 NA's 
  12   15    6    1   53 

patient.ethnicity:
not hispanic or latino                   NA's 
                    73                     14 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0    11.0    20.0    16.4    21.0    24.0      82 

patient.follow_ups.follow_up.2.days_to_death:
 789  863 NA's 
   1    1   85 

patient.follow_ups.follow_up.2.days_to_last_followup:
 602  651  756 NA's 
   1    1    1   84 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   0    1 NA's 
   1    2   84 

patient.follow_ups.follow_up.2.karnofsky_performance_score:
 100 NA's 
   1   86 

patient.follow_ups.follow_up.2.lost_follow_up:
  no NA's 
   5   82 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      3       4       6       7      10      12      82 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    1   84 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   3   84 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
  85  479  591  716 NA's 
   1    1    1    1   83 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             1              2             84 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
      distant metastasis intrapleural progression                     NA's 
                       1                        3                       83 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
left sided drainage tract, chest wall soft tissue on the left, right sided subpleural fat and in periumbilical region. 
                                                                                                                     1 
                                                                                  mediastinum and contralateral pleura 
                                                                                                                     1 
                                                                                                                  NA's 
                                                                                                                    85 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no NA's 
   4   83 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
 yes NA's 
   4   83 

patient.follow_ups.follow_up.2.performance_status_scale_timing:
pre-adjuvant therapy         preoperative                 NA's 
                   2                    1                   84 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
with tumor       NA's 
         5         82 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
   1    4   82 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
   1    3   83 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
    3     2    82 

patient.follow_ups.follow_up.2.year_of_form_completion:
2014 2015 NA's 
   3    2   82 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    12.0    18.0    17.5    22.0    30.0      51 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   368.5   449.0   502.3   674.5  1156.0      68 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -8.0   656.8   936.0  1037.6  1444.8  2024.0      73 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   0    1    2    3 NA's 
   4    6    4    1   72 

patient.follow_ups.follow_up.karnofsky_performance_score:
   0   50   90  100 NA's 
   1    1    3    1   81 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
  32    4   51 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   6.000   5.278   8.000  11.000      51 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    2   83 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   2    1   84 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  130.0   217.0   463.0   567.0   758.5  1425.0      80 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
         liver other, specify           NA's 
             1              2             84 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
      distant metastasis intrapleural progression                     NA's 
                       2                        3                       82 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
         ipsilateral subcutaneous nodules                 NA's 
                   1                    1                   85 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no NA's 
   7   80 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no NA's 
   1   86 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   1   86 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 237 1038 NA's 
   1    1   85 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1             86 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                 86 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
contralateral pleura                 NA's 
                   1                   86 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2   85 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    5   80 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   7   80 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   47.0   247.5   479.0   536.8   709.0  1299.0      72 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             2              4             81 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
      distant metastasis intrapleural progression                     NA's 
                       1                       12                       74 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
                             abdomen                          ipsilateral 
                                   1                                    1 
mediastinum and contralateral pleura                               pleura 
                                   1                                    1 
                                NA's 
                                  83 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no NA's 
  15   72 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  12   18   57 

patient.follow_ups.follow_up.performance_status_assessment_timepoint_category_other_text:
post surgery and vaccine            postoperative                     NA's 
                       1                        1                       85 

patient.follow_ups.follow_up.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                    2                     9                     1 
         preoperative                  NA's 
                    6                    69 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         7         24         56 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  10   22   55 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
  16   15   56 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
   14    19    54 

patient.follow_ups.follow_up.year_of_form_completion:
2014 2015 NA's 
  19   17   51 

patient.gender:
female   male 
    16     71 

patient.history_asbestos_exposure:
  no  yes NA's 
  14   56   17 

patient.history_of_neoadjuvant_treatment:
 no yes 
 86   1 

patient.icd_10:
c34.90  c38.4  c45.0 
     1      1     85 

patient.icd_o_3_histology:
9050/3 9051/3 9052/3 9053/3 
     6      1     58     22 

patient.icd_o_3_site:
c34.9 c38.4 
    1    86 

patient.informed_consent_verified:
yes 
 87 

patient.karnofsky_performance_score:
   0   80   90  100 NA's 
   1    4    4    3   75 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   4.000   6.000   6.161   8.000  12.000 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   3    4   80 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   6    2   79 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
1244 NA's 
   1   86 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  113.0   236.0   404.0   559.9   883.0  1425.0      74 


patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
      distant metastasis intrapleural progression                     NA's 
                       5                        9                       73 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
                         contralateral effusion 
                                              1 
                                    pericardial 
                                              1 
posterolateral right chest wall to skin surface 
                                              1 
                           subcutaneous nodules 
                                              1 
                                           NA's 
                                             83 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  10    1   76 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    1   85 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   2   85 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
1574 NA's 
   1   86 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 420  442 1038 1574 NA's 
   1    1    1    1   83 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
           abdomen ipsilateral pleura               lung               NA's 
                 1                  1                  1                 84 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
      distant metastasis intrapleural progression                     NA's 
                       1                        3                       83 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   3    1   83 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 724 NA's 
   1   86 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
lung NA's 
   1   86 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
intrapleural progression                     NA's 
                       1                       86 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1   86 

patient.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
 822 NA's 
   1   86 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site:
abdomen    NA's 
      1      86 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                 86 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   6   12   69 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  16    3   68 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   0  132  485  588 NA's 
   1    1    1    1   83 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -30.0   119.0   309.0   382.4   519.0  1257.0      55 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
      distant metastasis intrapleural progression                     NA's 
                       8                       25                       54 


patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  23    4   60 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  36   33   18 


patient.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                    2                     6                     2 
         preoperative                  NA's 
                   31                    46 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        16         50         21 

patient.pleurodesis_performed_90_days:
  no  yes NA's 
   4    5   78 

patient.pleurodesis_performed_prior:
  no  yes NA's 
  78    8    1 

patient.postoperative_rx_tx:
  no  yes NA's 
  32   50    5 

patient.primary_occupation:
                 automotive construction and extraction 
                          1                           7 
      other, please specify                     welding 
                         49                           1 
                       NA's 
                         29 


patient.primary_occupation_years_worked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0    19.0    30.0    28.4    39.5    50.0      72 

patient.primary_pathology.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  28.00   57.00   64.00   62.99   69.00   81.00 

patient.primary_pathology.creatinine_norm_range_lower:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.340   0.500   0.600   4.748   0.790  35.000      42 

patient.primary_pathology.creatinine_norm_range_upper:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.90    1.24    1.30   17.45    1.40  125.00      42 

patient.primary_pathology.creatinine_prior_tx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.530   0.837   0.900  12.503   1.029 126.000      42 

patient.primary_pathology.days_to_initial_pathologic_diagnosis:
 0 
87 

patient.primary_pathology.histological_type:
               biphasic mesothelioma diffuse malignant mesothelioma - nos 
                                  23                                    5 
            epithelioid mesothelioma             sarcomatoid mesothelioma 
                                  57                                    2 

patient.primary_pathology.laterality:
bilateral      left     right 
        3        30        54 

patient.primary_pathology.mesothelioma_detection_method:
       biopsy      cytology thoracentesis    thorascopy          NA's 
           52             4             1            15            15 

patient.primary_pathology.residual_tumor:
  r0   r1   r2   rx NA's 
  17    3   15   11   41 

patient.primary_pathology.serum_mesothelin_lower_limit:
   0  0.3 NA's 
  10    6   71 

patient.primary_pathology.serum_mesothelin_prior_tx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.329   2.300   3.458   4.145  13.421      72 

patient.primary_pathology.serum_mesothelin_upper_limit:
 1.5  2.5 NA's 
  10    6   71 

patient.primary_pathology.suv_of_pleura_max:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.600   5.700   8.450   8.218   9.575  16.700      65 

patient.primary_pathology.tumor_tissue_site:
pleura 
    87 

patient.primary_pathology.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1999    2008    2010    2009    2012    2013 

patient.prior_dx:
 no yes 
 78   9 

patient.race:
                    asian black or african american                     white 
                        1                         1                        85 

patient.radiation_therapy:
  no  yes NA's 
  58   23    6 

patient.radiations.radiation.2.anatomic_treatment_site:
primary tumor field       regional site                NA's 
                  2                   3                  82 



patient.radiations.radiation.2.day_of_form_completion:
   6    9   10   22 NA's 
   1    1    2    1   82 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0   152.0   232.0   537.2   735.0  1551.0      82 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0   117.0   189.0   516.2   717.0  1543.0      82 

patient.radiations.radiation.2.measure_of_response:
               complete response                 partial response 
                               1                                1 
radiographic progressive disease                   stable disease 
                               1                                1 
                            NA's 
                              83 

patient.radiations.radiation.2.month_of_form_completion:
   1    6    8 NA's 
   1    3    1   82 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     7.0    10.0    14.8    25.0    30.0      82 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   15.40   50.40   39.56   54.00   70.00      82 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   5   82 

patient.radiations.radiation.2.radiation_type:
external     NA's 
       4       83 

patient.radiations.radiation.2.units:
  gy NA's 
   5   82 

patient.radiations.radiation.2.year_of_form_completion:
2014 2015 NA's 
   4    1   82 

patient.radiations.radiation.3.anatomic_treatment_site:
regional site          NA's 
            1            86 

patient.radiations.radiation.3.bcr_radiation_barcode:
tcga-3h-ab3t-r60429                NA's 
                  1                  86 

patient.radiations.radiation.3.bcr_radiation_uuid:
aa8b80b9-ec9f-48c9-a832-948e5b97de85                                 NA's 
                                   1                                   86 

patient.radiations.radiation.3.day_of_form_completion:
   9 NA's 
   1   86 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
 152 NA's 
   1   86 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
 117 NA's 
   1   86 

patient.radiations.radiation.3.measure_of_response:
radiographic progressive disease                             NA's 
                               1                               86 

patient.radiations.radiation.3.month_of_form_completion:
   6 NA's 
   1   86 

patient.radiations.radiation.3.numfractions:
  25 NA's 
   1   86 

patient.radiations.radiation.3.radiation_dosage:
  45 NA's 
   1   86 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   1   86 

patient.radiations.radiation.3.radiation_type:
external     NA's 
       1       86 

patient.radiations.radiation.3.units:
  gy NA's 
   1   86 

patient.radiations.radiation.3.year_of_form_completion:
2014 NA's 
   1   86 

patient.radiations.radiation.anatomic_treatment_site:
 distant recurrence    local recurrence primary tumor field       regional site 
                  1                   2                  13                   7 
               NA's 
                 64 



patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    9.00   11.00   15.52   23.00   30.00      62 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  120.0   163.5   225.5   318.4   303.2  1551.0      63 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  110.0   132.0   175.5   277.9   264.5  1524.0      65 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                               4                                7 
radiographic progressive disease                   stable disease 
                               3                                3 
                            NA's 
                              70 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00    6.00    6.84    9.00   12.00      62 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    10.0    25.0    18.1    26.0    30.0      66 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   23.25   50.00 1375.45 3525.00 5220.00      65 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  25   62 

patient.radiations.radiation.radiation_type:
external     NA's 
      22       65 

patient.radiations.radiation.units:
 cgy   gy NA's 
   6   16   65 

patient.radiations.radiation.year_of_form_completion:
2013 2014 2015 NA's 
   8   14    3   62 


patient.stage_event.system_version:
5th 6th 7th 
  5  38  44 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
m0 m1 mx 
57  3 27 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
n0 n1 n2 n3 nx 
44 10 26  3  4 


patient.tissue_prospective_collection_indicator:
 no yes 
 76  11 

patient.tissue_retrospective_collection_indicator:
 no yes 
 11  76 


patient.vital_status:
alive  dead 
   29    58 

patient.year_of_form_completion:
2013 2014 
  31   56 

patient.bcr_canonical_check.bcr_patient_canonical_status:
canonical 
       87 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
       65        22 

patient.biospecimen_cqcf.country:
     australia         brazil united kingdom  united states 
            12              3              5             67 

patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    167     614    1640    2184    3566    4870      70 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-147.00    0.00   28.00   46.85   59.00  796.00 

patient.biospecimen_cqcf.ethnicity:
not hispanic or latino                   NA's 
                    63                     24 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
pleura 
    87 

patient.biospecimen_cqcf.histological_type:
   diffuse malignant mesothelioma - biphasic 
                                          23 
diffuse malignant mesothelioma - epithelioid 
                                          58 
        diffuse malignant mesothelioma - nos 
                                           4 
 diffuse malignant mesothelioma -sarcomatoid 
                                           2 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                               no yes, radiation prior to resection 
                               86                                 1 

patient.biospecimen_cqcf.method_of_sample_procurement:
                 open surgery other method (please specify) 
                           45                            12 
         thoracoscopic biopsy 
                           30 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   0 NA's 
   6   81 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  110.0   110.5   141.0   155.4   172.6   257.0      81 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
nano drop      NA's 
        6        81 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13200   13260   16920   18648   20706   30840      81 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
 120 NA's 
   6   81 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
blood draw       NA's 
         5         82 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1 NA's 
   9   78 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-147.00    0.00   36.00   52.11   66.50  490.00 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.0700    0.2867    0.3715  143.0107  135.1643 1487.0000        65 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm:
                          nanodrop          oragene saliva extraction 
                                12                                  2 
qiagen genomic dna preparation kit                  spectrophotometry 
                                 1                                  7 
                              NA's 
                                65 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   11.09    20.00    22.00  9468.09    30.10 60000.00       66 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0    66.0    75.0    87.5   110.8   300.0      65 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
     blood draw tumor resection 
             86               1 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
peritoneum       NA's 
         1         86 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            1            86 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
 1 
87 


patient.biospecimen_cqcf.other_vessel_used:
     nalgene bottle        plastic wrap universal container                NA's 
                  7                   7                   5                  68 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
  no  yes NA's 
   3   76    8 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
 87 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 yes NA's 
  79    8 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 yes NA's 
  79    8 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                        1                         1                        77 
                     NA's 
                        8 

patient.biospecimen_cqcf.sample_prescreened:
 no yes 
  8  79 

patient.biospecimen_cqcf.site_of_disease:
pleura 
    87 

patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
  13    4   70 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0 NA's 
   5   82 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  80   90  100 NA's 
   2    1    2   82 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    28.0    36.0    82.8   140.0   200.0      82 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1 NA's 
   5   82 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.278   0.000  15.000       8 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   60.00   70.00   72.03   85.00  100.00       8 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0   116.5   200.0   266.9   400.0   885.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
 1 
87 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                      9                       7                      31 
               cryovial  other (please specify) 
                     21                      19 

patient.days_to_index:
 0 
87 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   91.0   439.5  1087.0  1414.8  1916.0  5157.0 

patient.samples.sample.2.initial_weight:
 160 NA's 
   1   86 

patient.samples.sample.2.is_ffpe:
no 
87 

patient.samples.sample.2.oct_embedded:
false  true 
   86     1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160356672 160356826 167779878 164623673 167779928 167789531 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
32 
87 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.14 0.15 0.16 0.17 
  23   20   35    9 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
14 25 
50 37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
87 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 5 11 
50 37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.241   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a34c a39u 
  37   50 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8   3 3.2 3.4 
 23  20  35   9 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
23 
87 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
20 
87 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2013 2014 
  37   50 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167823456 167823481 167823504 167823505 167823528 167823551        17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  26 NA's 
  70   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 NA's 
  19   14   31    6   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  14 NA's 
  70   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  70   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   5 NA's 
  70   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.886   7.000   9.000      17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a40g NA's 
  70   17 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
 1.4  1.5  1.6  1.7 NA's 
  19   14   31    6   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
  70   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10 NA's 
  70   17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
  70   17 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160356685 160356734 167789472 164628288 167789494 167789517 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
87 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  23   20   35    9 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
14 25 
50 37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
87 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 5 11 
50 37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.241   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a34b a39t 
  37   50 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  23   20   35    9 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
23 
87 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
 87 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 2014 
  37   50 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.20   12.88   20.68   30.55   42.38  157.62 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
 87 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
 d 
87 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  23   20   35    9 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 87 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
 87 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              87 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
87 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                               59                                28 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       87 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   3 NA's 
   1   86 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    9.00   16.00   15.05   21.00   30.00 

patient.samples.sample.2.portions.portion.is_ffpe:
no 
87 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   5.023   9.000  11.000 

patient.samples.sample.2.portions.portion.portion_number:
 1 
87 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
  44   43 

patient.samples.sample.2.portions.portion.slides.slide.bcr_slide_barcode:
tcga-xt-aasu-11a-01-tsa                    NA's 
                      1                      86 

patient.samples.sample.2.portions.portion.slides.slide.bcr_slide_uuid:
c37ff230-ea8c-4f8f-aed0-914e482f3da6                                 NA's 
                                   1                                   86 

patient.samples.sample.2.portions.portion.slides.slide.image_file_name:
tcga-xt-aasu-11a-01-tsa.c37ff230-ea8c-4f8f-aed0-914e482f3da6.svs 
                                                               1 
                                                            NA's 
                                                              86 

patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   1   86 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   5 NA's 
   1   86 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   1   86 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   1   86 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   1   86 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   1 NA's 
   1   86 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
  99 NA's 
   1   86 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   1   86 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   1   86 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
   1   86 

patient.samples.sample.2.portions.portion.weight:
   2   90 NA's 
  58    1   28 

patient.samples.sample.2.portions.portion.year_of_creation:
2012 2013 2014 
  10   34   43 

patient.samples.sample.2.sample_type:
blood derived normal  solid tissue normal 
                  86                    1 

patient.samples.sample.2.sample_type_id:
10 11 
86  1 

patient.samples.sample.2.vial_number:
 a  b 
78  9 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     91     442    1087    1399    1916    5129 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
11c557f2-220f-4200-8003-90cafcd5e1fc a761e40b-50c2-4157-be06-52095fd4faf6 
                                   1                                    1 
b727fae0-4562-4976-a896-6dab791045ae bc2dbeea-1c7c-41ad-97d4-92fbf0483200 
                                   1                                    1 
                                NA's 
                                  83 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.4:
70bc2caa-e576-4d03-b996-b67f6abda950                                 NA's 
                                   1                                   86 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   20.0    90.0   230.0   281.8   415.0   810.0 

patient.samples.sample.is_ffpe:
no 
87 

patient.samples.sample.oct_embedded:
false  true 
   13    74 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.760   1.820   1.850   1.856   1.890   2.040 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160355114 160356010 167788912 164627911 167788948 167788989 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
 7 13 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 0.16 0.17 0.18 
   1    4   31   31   20 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
14 25 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
no 
87 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
 5 11 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.218   6.000   8.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a34g a40a 
  37   50 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   2.080   2.210   2.681   3.400   3.600 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
23 
87 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13 20 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2013 2014 
  37   50 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160356852 160357012 167811942 164641444 167811988 167812029 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
 7 13 
37 50 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 0.18 
   1    4   31   31   20 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
14 25 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
no 
87 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
 5 11 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.218   6.000   8.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a34f a404 
  37   50 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.820   2.210   3.200   2.846   3.400   3.600 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
23 
87 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
13 20 
37 50 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2013 2014 
  37   50 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.42   20.07   32.60   33.65   43.73   71.89 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
 87 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
 r 
87 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
0.14 0.15 0.16 0.17 0.18 
   1    4   31   31   20 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              87 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
no 
87 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                       87 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.660   1.320   1.460   1.461   1.650   2.200 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.800   8.300   8.291   8.800   9.600 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
     87 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  76    7    4 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160356714 160357736 167780357 164624129 167780400 167789527 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
32 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.14 0.15 0.16 0.17 
  12   24   35   16 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
14 25 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 5 11 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    2.00    4.00    3.69    5.00    8.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a34c a39r 
  37   50 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8   3 3.2 3.4 
 12  24  35  16 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
23 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
20 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2013 2014 
  37   50 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160355070 160355964 167780375 164622834 167780412 167780447 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
 5 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 
  12   24   35   16 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
14 25 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
no 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
 5 11 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    2.00    4.00    3.69    5.00    8.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a34e a39s 
  37   50 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.82 1.95 2.08 2.21 
  12   24   35   16 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
23 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
13 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2013 2014 
  37   50 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167778013 167789028 167789196 167789044 167789404 167789462        17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
  26 NA's 
  70   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.14 0.15 0.16 0.17 NA's 
   8   21   29   12   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  14 NA's 
  70   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  70   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   5 NA's 
  70   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.886   7.000   9.000      17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a40f NA's 
  70   17 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 1.4  1.5  1.6  1.7 NA's 
   8   21   29   12   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
  70   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10 NA's 
  70   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2014 NA's 
  70   17 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
160355029 160356740 167779895 164622895 167779932 167779967 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  12   24   35   16 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
14 25 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 5 11 
50 37 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    2.00    4.00    3.69    5.00    8.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a34b a39q 
  37   50 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  12   24   35   16 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
23 
87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
 87 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 2014 
  37   50 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   8.29   13.48   20.95   22.14   29.32   61.13 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
 87 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
 d 
87 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  12   24   35   16 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 87 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
 87 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              87 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
87 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                   87 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       87 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  76    7    4 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   14.00   19.00   19.28   27.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
no 
87 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   4.000   5.356  10.000  12.000 

patient.samples.sample.portions.portion.portion_number:
11 12 21 31 41 
69  8  8  1  1 

patient.samples.sample.portions.portion.portion_sequence:
   1    2    3    4 NA's 
  39    4    1    1   42 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
no 
87 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    1.50    2.00   11.64    4.50   70.00 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
 0  1  2 10 20 
54 13  3  7 10 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.862   0.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   3.667   4.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
 0 10 15 20 30 
83  1  1  1  1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    0.00   12.85   20.00   85.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.00   70.00   90.00   84.43  100.00  100.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   70.00   75.00   76.01   80.00  100.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
top 
 87 

patient.samples.sample.portions.portion.weight:
10 20 30 
 1  7 79 

patient.samples.sample.portions.portion.year_of_creation:
2012 2013 2014 
  16   29   42 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
  63   24 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
  63   24 

patient.samples.sample.portions.shipment_portion.plate_id:
a45o NA's 
  63   24 

patient.samples.sample.portions.shipment_portion.portion_number:
  11   21   51 NA's 
   6   56    1   24 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    5 NA's 
   2   29    1   55 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  30 NA's 
  63   24 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   7 NA's 
  63   24 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
  63   24 

patient.samples.sample.sample_type:
primary tumor 
           87 

patient.samples.sample.sample_type_id:
 1 
87 

patient.samples.sample.vial_number:
 a  b 
82  5 

}}
\keyword{datasets}
