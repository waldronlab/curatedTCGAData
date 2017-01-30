\name{ PAAD }
\alias{ PAAD }
\docType{data}
\title{ PAAD }

\details{
\preformatted{
> PAAD
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 183 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 183 columns 
 [3] CNASNP: RangedRaggedAssay with 203871 rows and 368 columns 
 [4] CNVSNP: RangedRaggedAssay with 34808 rows and 365 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 195 columns 
 [6] RPPAArray: ExpressionSet with 195 rows and 123 columns 
 [7] Mutations: RangedRaggedAssay with 37705 rows and 146 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 184 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 184 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( PAAD )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-625470 1:707099-6670395 ... 24:28457337-59018259
[["CNVSNP"]] 1:3218610-6670395 1:6688858-32711360 ... 23:3157107-154905589
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] SMPD1 EXPH5 MBD6 LCE2A ... ERLIN2 KCNMA1 RP11-812E19.9 FAT1
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( PAAD )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-2J-AAB1-01A-11R-A41B-07 ...
[["miRNASeqGene"]] TCGA-2J-AAB1-01A-11R-A41G-13 ...
[["CNASNP"]] TCGA-2J-AAB1-01A-11D-A40V-01 ... TCGA-Z5-AAPL-10A-01D-A40V-01
[["CNVSNP"]] TCGA-2J-AAB1-01A-11D-A40V-01 ... TCGA-Z5-AAPL-10A-01D-A40V-01
[["Methylation"]] TCGA-2J-AAB1-01A-11D-A40Y-05 ... TCGA-Z5-AAPL-01A-12D-A40Y-05
[["RPPAArray"]] TCGA-2J-AAB4-01A-21-A43K-20 ... TCGA-Z5-AAPL-01A-21-A43K-20
[["Mutations"]] TCGA-2J-AAB1-01A-11D-A40W-08 ... TCGA-Z5-AAPL-01A-12D-A40W-08
[["gistica"]] TCGA-2J-AAB1-01A-11D-A40V-01 ... TCGA-Z5-AAPL-01A-12D-A40V-01
[["gistict"]] TCGA-2J-AAB1-01A-11D-A40V-01 ... TCGA-Z5-AAPL-01A-12D-A40V-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.2 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP 16.1 Mb
4          CNVSNP  3.1 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7       Mutations 52.1 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   85 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
100.000 100.000   1.079   0.967   1.315 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  35.00   57.00   65.00   64.86   73.00   88.00 

vital_status:
  0   1 
 85 100 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   228.5   394.0   459.5   596.5  2182.0      85 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   186.5   416.0   553.9   718.0  2741.0      66 

tumor_tissue_site:
pancreas 
     185 



pathology_N_stage:
  n0   n1  n1b   nx NA's 
  50  126    4    4    1 

pathology_M_stage:
m0 m1 mx 
85  5 95 

gender:
female   male 
    83    102 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2001    2010    2012    2011    2012    2013       1 

radiation_therapy:
  no  yes NA's 
 125   45   15 

histological_type:
             pancreas-adenocarcinoma ductal type 
                                             154 
           pancreas-adenocarcinoma-other subtype 
                                              25 
pancreas-colloid (mucinous non-cystic) carcinoma 
                                               4 
             pancreas-undifferentiated carcinoma 
                                               1 
                                            NA's 
                                               1 

number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.30   15.00   25.00   26.84   40.00   75.00     128 

year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1948    1960    1971    1971    1982    1993     138 

residual_tumor:
  r0   r1   r2   rx NA's 
 111   53    5    4   12 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   2.989   4.000  16.000       4 

race:
                    asian black or african american                     white 
                       11                         7                       162 
                     NA's 
                        5 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     5                    137                     43 

admin.bcr:
nationwide children's hospital 
                           185 

admin.day_of_dcc_upload:
  1 
185 

admin.disease_code:
paad 
 185 


admin.month_of_dcc_upload:
 10 
185 

admin.patient_withdrawal.withdrawn:
false 
  185 

admin.project_code:
tcga 
 185 

admin.year_of_dcc_upload:
2015 
 185 

patient.adenocarcinoma_invasion:
 no yes 
  1 184 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  35.00   57.00   65.00   64.86   73.00   88.00 

patient.alcohol_history_documented:
  no  yes NA's 
  65  102   18 


patient.amt_alcohol_consumption_per_day:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   1.000   1.000   1.581   2.000   4.000     154 

patient.anatomic_neoplasm_subdivision:
      body of pancreas       head of pancreas other (please specify) 
                    14                    146                     11 
      tail of pancreas 
                    14 



patient.clinical_cqcf.anatomic_neoplasm_subdivision:
other (please specify)                   NA's 
                     4                    181 

patient.clinical_cqcf.anatomic_neoplasm_subdivision_other:
 distal pancreas             duct         pancreas uncinate process 
               1                1                1                1 
            NA's 
             181 

patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      176         9 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-167.00   -4.00    0.00   32.23   14.50 2072.00      10 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     61     334     541     526     702    1101     176 

patient.clinical_cqcf.histological_type:
             pancreas-adenocarcinoma ductal type 
                                             154 
           pancreas-adenocarcinoma-other subtype 
                                              26 
pancreas-colloid (mucinous non-cystic) carcinoma 
                                               4 
             pancreas-undifferentiated carcinoma 
                                               1 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                               no yes, radiation prior to resection 
                              184                                 1 

patient.clinical_cqcf.history_of_prior_malignancy:
                              no yes, history of prior malignancy 
                             166                               19 

patient.clinical_cqcf.normal_tissue_anatomic_site:
other  NA's 
   17   168 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           45           140 

patient.clinical_cqcf.other_anatomic_site:
 distal pancreas             duct         pancreas uncinate process 
               1                1                1                1 
            NA's 
             181 

patient.clinical_cqcf.tumor_type:
primary 
    185 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   13.00   14.09   22.00   30.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -32475  -26936  -24007  -23884  -20984  -13127 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   153.5   444.0   456.6   616.0  2182.0     119 

patient.days_to_diabetes_onset:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-9070.00  -163.00   -52.00  -706.79   -17.75   504.00      171 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 184    1 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    31.0   194.0   377.7   396.5  2558.0      66 

patient.days_to_pancreatitis_onset:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-18029.0   -231.5    -71.0  -1744.2    -38.0      1.0      174 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     7.0    21.0    17.3    26.0    31.0     138 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0   147.0   205.0   338.7   442.0  1789.0     144 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    97.5   135.0   256.0   403.0  1332.0     138 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                          10                            9 
            partial response               stable disease 
                           1                            6 
                        NA's 
                         159 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   7.362  10.000  12.000     138 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  42    5  138 

patient.drugs.drug.2.therapy_types.therapy_type:
      ancillary    chemotherapy hormone therapy            NA's 
              2              44               1             138 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  44    3  138 

patient.drugs.drug.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   3    9   32    3  138 



patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00   14.00   14.94   24.00   31.00     152 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0   136.8   306.5   413.4   544.5  1789.0     163 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0   115.0   280.0   336.5   457.0  1332.0     152 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                           8                            4 
            partial response               stable disease 
                           1                            4 
                        NA's 
                         168 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   7.364  10.000  12.000     152 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  23   10  152 

patient.drugs.drug.3.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           3           30          152 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  32    1  152 

patient.drugs.drug.3.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    4   24    4  152 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.75   11.00   13.45   24.00   31.00     165 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   81.0   259.0   424.0   429.6   620.8   726.0     171 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   38.0   184.8   386.0   356.3   518.8   698.0     165 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                           5                            2 
              stable disease                         NA's 
                           3                          175 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.00    8.50    7.80   10.25   12.00     165 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
  14    6  165 

patient.drugs.drug.4.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           2           18          165 

patient.drugs.drug.4.tx_on_clinical_trial:
  no NA's 
  20  165 

patient.drugs.drug.4.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    3   13    3  165 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00   11.00   12.93   22.50   31.00     170 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  142.0   300.8   512.0   547.2   658.0  1377.0     175 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   73.0   236.5   421.0   451.1   546.0  1309.0     170 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease            complete response 
                           3                            1 
              stable disease                         NA's 
                           3                          178 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     5.5     7.0     7.4    10.0    12.0     170 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
  10    5  170 

patient.drugs.drug.5.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           2           13          170 

patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
  15  170 

patient.drugs.drug.5.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    2    9    3  170 



patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00   14.00   14.56   24.00   31.00     176 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  485.0   533.8   579.5   752.2   875.0  1377.0     179 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   81.0   280.0   421.0   511.2   539.0  1309.0     176 


patient.drugs.drug.6.measure_of_response:
clinical progressive disease               stable disease 
                           3                            2 
                        NA's 
                         180 

patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  5.000   6.000   7.000   7.111   8.000  11.000     176 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
   7    2  176 

patient.drugs.drug.6.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           1            8          176 

patient.drugs.drug.6.tx_on_clinical_trial:
  no NA's 
   9  176 

patient.drugs.drug.6.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    2    4    2  176 



patient.drugs.drug.7.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   14.25   19.50   19.67   28.50   31.00     179 

patient.drugs.drug.7.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  485.0   620.0   859.0   830.8   960.0  1230.0     180 

patient.drugs.drug.7.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  413.0   445.2   541.0   675.0   823.5  1224.0     179 


patient.drugs.drug.7.measure_of_response:
clinical progressive disease               stable disease 
                           2                            2 
                        NA's 
                         181 

patient.drugs.drug.7.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    6.25    7.00    7.50    8.50   11.00     179 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   6  179 

patient.drugs.drug.7.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           2            4          179 

patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   6  179 

patient.drugs.drug.7.year_of_form_completion:
2013 2014 2015 NA's 
   2    3    1  179 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-hz-7918-d72672 tcga-ib-7645-d44154                NA's 
                  1                   1                 183 

patient.drugs.drug.8.bcr_drug_uuid:
1f781a65-c73d-45bd-a881-b782d63903c2 662af449-a9e9-4fc3-b8da-c34cfbaf6cee 
                                   1                                    1 
                                NA's 
                                 183 

patient.drugs.drug.8.day_of_form_completion:
  14   31 NA's 
   1    1  183 

patient.drugs.drug.8.days_to_drug_therapy_end:
 960 1230 NA's 
   1    1  183 

patient.drugs.drug.8.days_to_drug_therapy_start:
 910 1224 NA's 
   1    1  183 

patient.drugs.drug.8.drug_name:
  5-fu gemzar   NA's 
     1      1    183 

patient.drugs.drug.8.measure_of_response:
clinical progressive disease               stable disease 
                           1                            1 
                        NA's 
                         183 

patient.drugs.drug.8.month_of_form_completion:
   6    7 NA's 
   1    1  183 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   2  183 

patient.drugs.drug.8.therapy_types.therapy_type:
chemotherapy         NA's 
           2          183 

patient.drugs.drug.8.tx_on_clinical_trial:
  no NA's 
   2  183 

patient.drugs.drug.8.year_of_form_completion:
2013 2015 NA's 
   1    1  183 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-hz-7918-d72673                NA's 
                  1                 184 

patient.drugs.drug.9.bcr_drug_uuid:
3d4abd41-5041-4da3-af5c-790cee5dcc4c                                 NA's 
                                   1                                  184 

patient.drugs.drug.9.day_of_form_completion:
  31 NA's 
   1  184 

patient.drugs.drug.9.days_to_drug_therapy_end:
 960 NA's 
   1  184 

patient.drugs.drug.9.days_to_drug_therapy_start:
 910 NA's 
   1  184 

patient.drugs.drug.9.drug_name:
leucovorin       NA's 
         1        184 

patient.drugs.drug.9.measure_of_response:
stable disease           NA's 
             1            184 

patient.drugs.drug.9.month_of_form_completion:
   7 NA's 
   1  184 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   1  184 

patient.drugs.drug.9.therapy_types.therapy_type:
ancillary      NA's 
        1       184 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   1  184 

patient.drugs.drug.9.year_of_form_completion:
2015 NA's 
   1  184 



patient.drugs.drug.clinical_trail_drug_classification:
immunization         NA's 
           1          184 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00   14.00   15.52   26.00   31.00      64 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0   140.5   204.0   247.9   252.2  1895.0      81 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   55.75   82.50  131.23  116.00 1834.00      69 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          40                           29 
            partial response               stable disease 
                           4                            3 
                        NA's 
                         109 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   6.769   9.000  12.000      64 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 113    8   64 

patient.drugs.drug.therapy_types.therapy_type:
      ancillary    chemotherapy hormone therapy         vaccine            NA's 
              2             117               1               1              64 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
 113    6   66 

patient.drugs.drug.year_of_form_completion:
2012 2013 2014 2015 NA's 
   6   23   87    5   64 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     5                    137                     43 

patient.family_history_of_cancer:
  no  yes NA's 
  47   66   72 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   11.25   15.00   16.28   22.50   30.00     167 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  239.0   498.0   592.0   594.8   732.0   913.0     180 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   462.0   525.0   680.6  1100.0  1287.0     172 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             3                             15 
                          NA's 
                           167 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response         progressive disease 
                          6                           6 
                       NA's 
                        173 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  17    1  167 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.750   9.000   7.722  10.750  11.000     167 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   3    4  178 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   7  178 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   83.0   251.0   330.0   527.7   746.0  1287.0     178 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
         liver           lung other, specify           NA's 
             1              3              2            179 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      5                       1                       1 
                   NA's 
                    178 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
           colon perigastric mass             NA's 
               1                1              183 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no NA's 
   6  179 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                  1                                   6 
                               NA's 
                                178 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by.2:
positive biomarker (s)                   NA's 
                     1                    184 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   9    8  168 

patient.follow_ups.follow_up.2.patient_death_reason:
pancreatic cancer              NA's 
                4               181 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         9          8        168 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         10                           3 
        progressive disease                        NA's 
                          2                         170 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  13    3  169 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
   5   10  170 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   13     5   167 

patient.follow_ups.follow_up.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    1   13    3  167 

patient.follow_ups.follow_up.3.bcr_followup_barcode:
tcga-s4-a8ro-f67257 tcga-us-a776-f69517                NA's 
                  1                   1                 183 

patient.follow_ups.follow_up.3.bcr_followup_uuid:
013de073-0a40-4e33-a174-5bbe965cb6cd 44158fe4-d564-4c50-9443-81288f382885 
                                   1                                    1 
                                NA's 
                                 183 

patient.follow_ups.follow_up.3.day_of_form_completion:
   3   12 NA's 
   1    1  183 

patient.follow_ups.follow_up.3.days_to_last_followup:
 525 1216 NA's 
   1    1  183 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                              1 
                          NA's 
                           183 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response                        NA's 
                          1                         184 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
   2  183 

patient.follow_ups.follow_up.3.month_of_form_completion:
   1   11 NA's 
   1    1  183 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   1  184 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   1  184 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 291 NA's 
   1  184 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
non-regional / distant lymph nodes                               NA's 
                                 1                                184 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                184 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  184 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by:
convincing imaging               NA's 
                 1                184 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   1    1  183 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         1          1        183 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          1                         184 

patient.follow_ups.follow_up.3.radiation_therapy:
  no NA's 
   1  184 

patient.follow_ups.follow_up.3.targeted_molecular_therapy:
  no NA's 
   1  184 

patient.follow_ups.follow_up.3.vital_status:
alive  NA's 
    2   183 

patient.follow_ups.follow_up.3.year_of_form_completion:
2014 2015 NA's 
   1    1  183 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   18.00   18.18   29.00   31.00      30 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   41.0   236.0   393.0   452.9   545.0  2036.0     112 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   382.0   525.0   707.6   882.0  2741.0     106 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             3                            149 
                          NA's 
                            33 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response  partial remission/response 
                         24                           4 
        progressive disease              stable disease 
                         48                           8 
                       NA's 
                        101 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 146    8   31 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.500   8.000   7.697  10.000  12.000      30 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  22   33  130 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  53    2  130 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 133  213  439  555 NA's 
   1    1    1    1  181 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   74.0   192.0   343.0   398.1   510.8  1511.0     127 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     45                      13                       1 
                   NA's 
                    126 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type.2:
distant metastasis               NA's 
                 1                184 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  51    4  130 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                 18                                  36 
             positive biomarker (s)                                NA's 
                                  1                                 130 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by.2:
    convincing imaging positive biomarker (s)                   NA's 
                     4                      6                    175 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by.3:
positive biomarker (s)                   NA's 
                     1                    184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r2 NA's 
   1    1  183 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  76   59   50 

patient.follow_ups.follow_up.patient_death_reason:
            other malignancy other, non-malignant disease 
                           2                            5 
           pancreatic cancer       surgical complications 
                          47                            1 
                        NA's 
                         130 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        45         87         53 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         58                           8 
        progressive disease              stable disease 
                         45                           6 
                       NA's 
                         68 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 106   37   42 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
  41  104   40 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
   79    74    32 

patient.follow_ups.follow_up.year_of_form_completion:
2012 2013 2014 2015 NA's 
   4   30  112    9   30 

patient.frequency_of_alcohol_consumption:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   3.000   6.500   4.812   7.000   7.000     153 

patient.gender:
female   male 
    83    102 

patient.histologic_grading_tier_category:
 four tier three tier       NA's 
        74         31         80 

patient.histological_type:
             pancreas-adenocarcinoma ductal type 
                                             154 
           pancreas-adenocarcinoma-other subtype 
                                              25 
pancreas-colloid (mucinous non-cystic) carcinoma 
                                               4 
             pancreas-undifferentiated carcinoma 
                                               1 
                                            NA's 
                                               1 


patient.history_of_chronic_pancreatitis:
  no  yes NA's 
 130   13   42 

patient.history_of_diabetes:
  no  yes NA's 
 111   38   36 

patient.history_of_neoadjuvant_treatment:
                               no yes, radiation prior to resection 
                              184                                 1 

patient.icd_10:
c25.0 c25.1 c25.2 c25.8 c25.9 
  141    15    11     2    16 


patient.icd_o_3_site:
c25.0 c25.1 c25.2 c25.8 c25.9 
  141    15    11     2    16 

patient.informed_consent_verified:
yes 
185 

patient.init_pathology_dx_method_other:
endoscopic ultrasound-guided fine needle aspiration 
                                                  1 
                             fine needle aspiration 
                                                  1 
                      fine needle aspiration biopsy 
                                                  2 
                                               NA's 
                                                181 

patient.initial_pathologic_diagnosis_method:
cytology (e.g. peritoneal or pleural fluid) 
                                         30 
                     other method, specify: 
                                          4 
                              tissue biopsy 
                                         38 
                            tumor resection 
                                        108 
                                       NA's 
                                          5 

patient.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   17.00   17.22   22.00   57.00       4 

patient.maximum_tumor_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   2.925   3.500   3.840   4.500  14.000      15 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.292   9.000  12.000 

patient.neoplasm_histologic_grade:
g1 g2 g3 g4 gx 
32 97 51  2  3 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
  24   30  131 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  51    4  130 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  107.0   150.0   324.0   404.3   525.0   987.0     179 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.0   154.0   342.0   388.3   492.0  1819.0     132 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     38                      14                       2 
                   NA's 
                    131 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type.2:
distant metastasis               NA's 
                 1                184 


patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  45    6  134 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by:
biopsy with histologic confirmation                  convincing imaging 
                                 13                                  34 
             positive biomarker (s)                                NA's 
                                  3                                 135 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by.2:
    convincing imaging positive biomarker (s)                   NA's 
                     4                      6                    175 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by.3:
positive biomarker (s)                   NA's 
                     1                    184 

patient.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r2   rx NA's 
   1    1    1  182 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 110   54   21 

patient.number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   2.989   4.000  16.000       4 

patient.number_of_lymphnodes_positive_by_ihc:
   0 NA's 
  55  130 

patient.number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.30   15.00   25.00   26.84   40.00   75.00     128 



patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        56         77         52 

patient.primary_lymph_node_presentation_assessment:
  no  yes NA's 
   4  180    1 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         43                           8 
        progressive disease              stable disease 
                         46                           8 
                       NA's 
                         80 

patient.prior_dx:
                              no yes, history of prior malignancy 
                             166                               19 

patient.race:
                    asian black or african american                     white 
                       11                         7                       162 
                     NA's 
                        5 

patient.radiation_therapy:
  no  yes NA's 
 108   33   44 

patient.radiations.radiation.2.anatomic_treatment_site:
primary tumor field       regional site                NA's 
                  2                   1                 182 

patient.radiations.radiation.2.bcr_radiation_barcode:
tcga-fb-a545-r50069 tcga-oe-a75w-r66434 tcga-yy-a8lh-r58240                NA's 
                  1                   1                   1                 182 

patient.radiations.radiation.2.bcr_radiation_uuid:
1f42a1e5-9307-4c7b-bd3d-626f869eeae9 405523fd-07ac-4ea7-9520-8523377f77fa 
                                   1                                    1 
63e87529-bce7-4100-8f19-b4daa63587dc                                 NA's 
                                   1                                  182 

patient.radiations.radiation.2.day_of_form_completion:
   3   15   16 NA's 
   1    1    1  182 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
 135  153  154 NA's 
   1    1    1  182 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
  99  118  150 NA's 
   1    1    1  182 

patient.radiations.radiation.2.measure_of_response:
               complete response radiographic progressive disease 
                               2                                1 
                            NA's 
                             182 

patient.radiations.radiation.2.month_of_form_completion:
   4   10 NA's 
   1    2  182 

patient.radiations.radiation.2.numfractions:
   3   25   27 NA's 
   1    1    1  182 

patient.radiations.radiation.2.radiation_dosage:
 5.4 4500 5400 NA's 
   1    1    1  182 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   3  182 

patient.radiations.radiation.2.radiation_type:
external     NA's 
       3      182 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
   2    1  182 

patient.radiations.radiation.2.year_of_form_completion:
2013 2014 NA's 
   1    2  182 

patient.radiations.radiation.anatomic_treatment_site:
       distant site primary tumor field       regional site                NA's 
                  2                  34                   6                 143 



patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     4.0    19.0    17.3    28.0    31.0     141 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   77.0   119.5   144.0   179.7   202.5   565.0     142 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   44.0    77.0   105.0   133.2   143.5   540.0     142 

patient.radiations.radiation.measure_of_response:
               complete response radiographic progressive disease 
                              14                               12 
                  stable disease                             NA's 
                               2                              157 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.75    8.00    7.50    9.00   11.00     141 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   25.00   27.50   25.42   28.00   37.00     149 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     20    4635    5040    4462    5040    5580     145 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  43  142 

patient.radiations.radiation.radiation_type:
external     NA's 
      41      144 

patient.radiations.radiation.units:
 cgy   gy NA's 
  35    4  146 

patient.radiations.radiation.year_of_form_completion:
2012 2013 2014 2015 NA's 
   3    4   35    2  141 

patient.relative_cancer_types.relative_cancer_type:
  breast melanoma    other pancreas     NA's 
      10        2       40       12      121 

patient.relative_cancer_types.relative_cancer_type.2:
other  NA's 
    8   177 

patient.residual_tumor:
  r0   r1   r2   rx NA's 
 111   53    5    4   12 

patient.source_of_patient_death_reason:
          autopsy death certificate    medical record              NA's 
                1                21                41               122 


patient.stage_event.system_version:
5th 6th 7th 
  1  26 158 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
m0 m1 mx 
85  5 95 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
  n0   n1  n1b   nx NA's 
  50  126    4    4    1 


patient.stopped_smoking_year:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1952    1980    1988    1990    2004    2013     135 

patient.surgery_performed_type:
        distal pancreatectomy other method (please specify) 
                           23                            14 
         total pancreatectomy                       whipple 
                            3                           143 
                         NA's 
                            2 


patient.targeted_molecular_therapy:
  no  yes NA's 
  49   94   42 

patient.tissue_prospective_collection_indicator:
 no yes 
100  85 

patient.tissue_retrospective_collection_indicator:
 no yes 
 85 100 



patient.tumor_tissue_site:
pancreas 
     185 

patient.tumor_type:
primary 
    185 

patient.vital_status:
alive  dead 
  119    66 

patient.year_of_form_completion:
2012 2013 2014 2015 
  23   63   93    6 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2001    2010    2012    2011    2012    2013       1 

patient.year_of_tobacco_smoking_onset:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1948    1960    1971    1971    1982    1993     138 


patient.microsatellite_instability_test_results.microsatellite_instability_test_result.mononucleotide_and_dinucleotide_marker_panel_analysis_status:
indeterminate         msi-l           mss    not tested 
           28             9           146             2 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
      case matched expression control      case matched methylation control 
                                    4                                     3 
                  germline comparison primary tumor and metastatic tumor(s) 
                                    2                                     1 
                                 NA's 
                                  175 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control                             NA's 
                               4                              181 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                  4                 181 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             175               10 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      176         9 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    59.5   133.5   454.3   669.8  4126.0       3 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -31.00    0.00    0.00   14.46   23.00  171.00       1 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
155  30 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     4                     88                     93 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
   other pancreas 
       4      181 

patient.biospecimen_cqcf.histological_type:
             pancreas-adenocarcinoma ductal type 
                                             154 
           pancreas-adenocarcinoma-other subtype 
                                              26 
pancreas-colloid (mucinous non-cystic) carcinoma 
                                               4 
             pancreas-undifferentiated carcinoma 
                                               1 


patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                               no yes, radiation prior to resection 
                              184                                 1 

patient.biospecimen_cqcf.method_of_sample_procurement:
surgical resection 
               185 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -1.0     0.0     0.0     5.6     0.0    36.0     170 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
                 8                  7                170 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
other  NA's 
    3   182 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            7           178 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1 NA's 
  16  169 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -31.00    0.00    4.50   79.91   28.00 2072.00       3 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0513   0.1400   0.2319  42.3044   0.4450 250.0000       96 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   15.00   20.00   20.63   20.00   75.00      96 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.102  60.000  94.000 123.072 150.000 640.000      96 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
               145                 38                  2 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
other  NA's 
   14   171 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           38           147 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2 NA's 
 183    1    1 

patient.biospecimen_cqcf.other_vessel_used:
50ml conical      polycon         NA's 
           1           17          167 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 no yes 
  1 184 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
185 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
yes 
185 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 no yes 
  1 184 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                       10                         3                       112 
                     NA's 
                       60 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  1 
                                                                                                                               NA's 
                                                                                                                                184 

patient.biospecimen_cqcf.sample_prescreened:
 no yes 
  3 182 

patient.biospecimen_cqcf.site_of_disease:
   other pancreas     NA's 
       4      171       10 

patient.biospecimen_cqcf.site_of_disease_description:
          other pancreatic body pancreatic head pancreatic tail            NA's 
              4              13             144              14              10 

patient.biospecimen_cqcf.site_of_disease_text:
 distal pancreas             duct         pancreas uncinate process 
               1                1                1                1 
            NA's 
             181 

patient.biospecimen_cqcf.submitted_for_lce:
 no yes 
139  46 

patient.biospecimen_cqcf.top_slide_submitted:
 no yes 
 28 157 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid:
6330144b-5365-400b-a0ca-fbb5c86a3759                                 NA's 
                                   1                                  184 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0 NA's 
   1  184 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  30 NA's 
   1  184 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
  59 NA's 
   1  184 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1 NA's 
   1  184 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.103   1.250  30.000       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00   25.00   55.00   54.03   75.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   15.0   116.0   187.0   222.9   250.0  1654.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1 
185 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                     31                      23                      47 
               cryovial  other (please specify) 
                     66                      18 

patient.days_to_index:
   0 NA's 
 184    1 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and metastatic tumor(s)                                  NA's 
                                    1                                   184 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   87.75  318.50  598.77  943.75 4142.00      51 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   200.0   490.0   487.6   690.0  1320.0     160 

patient.samples.sample.2.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.5000  0.8000  1.0000  0.9714  1.0000  1.7000     178 

patient.samples.sample.2.is_ffpe:
 no 
185 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.900   1.150   1.400   1.386   1.550   2.000     178 

patient.samples.sample.2.oct_embedded:
false  true  NA's 
  117    18    50 

patient.samples.sample.2.pathology_report_file_name:
tcga-hz-a9tj.97e6278d-5acd-470c-847d-d882ea709b28.pdf 
                                                    1 
                                                 NA's 
                                                  184 

patient.samples.sample.2.pathology_report_uuid:
97e6278d-5acd-470c-847d-d882ea709b28                                 NA's 
                                   1                                  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.83 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-hz-a9tj-06a-11r-a41g-13                         NA's 
                           1                          184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
58e3274d-fd8e-4db0-8080-e2ddd6b6cb4d                                 NA's 
                                   1                                  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
174189848      NA's 
        1       184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  13 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  28 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   5 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   9 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a41g NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   d NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   3 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  20 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2014 NA's 
   1  184 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
121011665 132269579 164082113 153560518 167804687 175156219        44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8 NA's 
   1  140   44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.15  0.5 NA's 
   1  140   44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.00    8.00   15.45   27.00   29.00      44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 141   44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   6.000   4.943   7.000   7.000      44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    6.00    5.78    8.00   12.00      44 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
1.95   40 NA's 
   1  140   44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
  34  107   44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   80 NA's 
   1  140   44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
  34   25   82   44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  39.27  165.50  177.00  170.89  181.50  215.00     150 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 140                    1                   44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   1  140   44 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
0.15  0.5 NA's 
   1   34  150 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 140   45 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
  34  106   45 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             141 
                                            NA's 
                                              44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 141   44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                 repli-g x 
                        1                       131                         9 
                     NA's 
                       44 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
1.48 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
 8.6 NA's 
   1  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
       34         1       150 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
  14  171 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
 1.9    2 NA's 
   8    1  176 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.210e+08 1.457e+08 1.641e+08 2.341e+08 1.742e+08 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   8 
 38 147 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05000 0.07000 0.08000 0.08903 0.08000 0.17000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   17.00   26.00   21.76   28.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   4.297   5.000  10.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.562   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   2.800   3.000   2.644   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 22  23 
 50 135 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7   40   60 
  31    7  128   19 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
   7   43   42   93 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
107018680 124428708 150705835 143270561 164053620 174190187        62 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   2    5    8   26 NA's 
   4    3   38   78   62 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0600  0.1400  0.1227  0.1600  0.1700      62 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     7.0    26.0    18.5    27.0    30.0      62 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 123   62 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.163   5.500  10.000      62 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   5.000   5.415   6.000  12.000      62 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.130   1.500   1.600   2.008   3.000   3.200      62 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
  46   77   62 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.50   10.00   10.00   24.45   50.00   60.00      62 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 NA's 
   6   40   34   43   62 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
107018631 107018654 107018701 120645903 107018704 175154826       180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8 NA's 
   4    1  180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.08 0.14 0.15 NA's 
   1    3    1  180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   1    4 NA's 
   1    4  180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   5  180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   5   10 NA's 
   4    1  180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    2    6 NA's 
   2    2    1  180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
1744 a45x NA's 
   4    1  180 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
1.12 1.86    2 NA's 
   1    3    1  180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
   4    1  180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
13.3   14 NA's 
   4    1  180 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2014 NA's 
   4    1  180 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.210e+08 1.457e+08 1.641e+08 2.341e+08 1.742e+08 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1  26 
147  38 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.13 0.14 0.15 0.16 0.17 
   6   43   50   58   28 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   17.00   26.00   22.54   28.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   3.000   4.427   5.000  10.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.476   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.010   1.070   1.127   1.140   1.700 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
 50 135 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7   10 
  19  128   38 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
   7   43   42   93 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   15.47   23.81   77.30   63.00 1119.00 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
185 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
   6   43   50   58   28 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
 50 135 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             185 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                               76                                20 
pre-extracted dna received by tss 
                               89 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      176         9 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  24    1  160 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    6.00   18.00   15.85   25.00   30.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no 
185 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   5.032   8.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
  1   2  11  12 
159   1  24   1 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
  49  136 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  32  153 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.00    5.00   25.36   50.00   70.00     160 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    2.00    4.52   10.00   40.00     160 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
  25  160 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    1.00    3.56    6.00   10.00     160 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    50.0    80.0    70.2   100.0   100.0     160 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     0.0    25.8    40.0   100.0     160 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0  100 NA's 
  24    1  160 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0   70 NA's 
  24    1  160 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
  32  153 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    2.00   13.95   30.00   39.00     109 

patient.samples.sample.2.portions.portion.year_of_creation:
2011 2012 2013 2014 
  29   31   46   79 

patient.samples.sample.2.sample_type:
blood derived normal           metastatic  solid tissue normal 
                 153                    1                   31 

patient.samples.sample.2.sample_type_id:
  6  10  11 
  1 153  31 

patient.samples.sample.2.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.2500  0.3000  0.3143  0.4000  0.5000     178 

patient.samples.sample.2.vial_number:
  a   b 
182   3 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  3                 182 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  38.00   64.75  121.00  126.17  184.00  226.00     179 

patient.samples.sample.3.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     30      80     180     330     480     880     180 

patient.samples.sample.3.intermediate_dimension:
 0.5 NA's 
   1  184 

patient.samples.sample.3.is_ffpe:
  no NA's 
   7  178 

patient.samples.sample.3.longest_dimension:
 1.3 NA's 
   1  184 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
    4     2   179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.81 1.86 1.91 NA's 
   1    1    1  182 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-h6-8124-11a-01r-2404-07 tcga-h6-a45n-11a-12r-a26y-13 
                           1                            1 
tcga-hv-a5a3-11a-11r-a26y-13 tcga-yb-a89d-11a-11r-a36g-07 
                           1                            1 
                        NA's 
                         181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
09edca5b-dedb-4dad-b774-309cde4b7bf1 23314827-f763-46da-89b7-3edbd20ff6e4 
                                   1                                    1 
9d65f639-b6cd-43f5-8cb4-a2a9be0aafe9 f542b8ee-9eec-4229-a25c-ba4d9d51723f 
                                   1                                    1 
                                NA's 
                                 181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             184 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
131702274 145675890 145675914 164080737      NA's 
        1         1         1         1       181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
   2    2  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.17 0.18 NA's 
   2    2  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  10   27   29 NA's 
   1    2    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   1    2    9 NA's 
   1    2    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   2    6 NA's 
   1    3  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
2404 a26y a36g NA's 
   1    2    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   c    e    g NA's 
   2    1    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
2.26 2.34  3.4  3.6 NA's 
   1    1    1    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
   1    3  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13 13.3   20 NA's 
   1    1    2  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 NA's 
   1    2    1  181 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched expression control                            NA's 
                              1                             184 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.457e+08 1.503e+08 1.641e+08 3.106e+08 1.669e+08 1.076e+09       179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8   13 NA's 
   2    2    2  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.17 0.18  0.5 NA's 
   2    2    2  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   8   10   27   29 NA's 
   1    1    2    2  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   6  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   1    2    7    9 NA's 
   2    2    1    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   2    6    9   12 NA's 
   1    3    1    1  179 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.210   2.605   3.500  15.258  30.900  40.000     179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
   1    5  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
   2    2    2  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
   1    2    3  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
 9.82 46.83 61.05 209.2  NA's 
    1     1     1     1   181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   2                    4                  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   4    2  179 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.17 0.18 NA's 
   2    2  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   2  183 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   2  183 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               6 
                                            NA's 
                                             179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   6  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                        4                         2                       179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
1.11 1.31 1.35 1.36 NA's 
   1    1    1    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   7  7.2  7.4  7.5 NA's 
   1    1    1    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      4     181 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
   4    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-h6-8124-11a-01w-a29g-08 tcga-h6-a45n-11a-12w-a30m-08 
                           1                            1 
tcga-hv-a5a3-11a-11w-a30m-08 tcga-yb-a89d-11a-11w-a44b-08 
                           1                            1 
                        NA's 
                         181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
0b067ab7-5a31-46bc-a5e2-255d46e033b1 27617aed-52bc-420c-9670-2a3481697c4e 
                                   1                                    1 
82e7d1cc-2e60-4ab5-bfa0-62eebe08f51c 8d5a184d-a8d8-4338-a87b-848ece855e2f 
                                   1                                    1 
                                NA's 
                                 181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
132271619 150703887 150703908 167804384      NA's 
        1         1         1         1       181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   8   12 NA's 
   2    2  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5    6    7 NA's 
   1    2    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   2    3    9 NA's 
   2    1    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a29g a30m a44b NA's 
   1    2    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   c    d    f    g NA's 
   1    1    1    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2013 2014 NA's 
   3    1  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   4                  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-h6-8124-11a-01w tcga-h6-a45n-11a-12w tcga-hv-a5a3-11a-11w 
                   1                    1                    1 
tcga-yb-a89d-11a-11w                 NA's 
                   1                  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
02c4db00-c738-4dae-bc53-de7addbc99bd 1173ba34-feae-45d7-bea0-6c0a2fd3e89b 
                                   1                                    1 
7c43c7ab-1df2-43ef-846a-8f5982e9b83d a806bd87-11b5-4089-bbb3-5d3ad756ee0e 
                                   1                                    1 
                                NA's 
                                 181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   4  181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
        3         1       181 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
   2    1  182 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.457e+08 1.498e+08 1.641e+08 2.891e+08 1.691e+08 1.076e+09       178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8 NA's 
   1    6  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.05 0.07 0.08 0.14 NA's 
   1    2    3    1  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    26.0    27.0    25.0    28.5    29.0     178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   1    2    5    9 NA's 
   2    2    1    2  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.500   9.000   7.286  10.000  11.000     178 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   d    e    f    g NA's 
   3    1    2    1  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
0.94  2.8    3  3.2 NA's 
   1    2    1    3  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
   1    6  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 6.7   40   60 NA's 
   1    5    1  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 NA's 
   1    3    3  178 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
128925398 131702120 132155047 141508399 150705860 164053571       180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8   26 NA's 
   3    1    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.07 0.15 0.16 NA's 
   1    2    2  180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  10   25   27   29 NA's 
   1    1    2    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   1    2    9 NA's 
   1    2    2  180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   2    3    4    9 NA's 
   1    1    1    2  180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
2399 a26q a32u a36o NA's 
   1    2    1    1  180 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   1.950   2.080   2.092   2.130   2.800     180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
   1    4  180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10   13 13.3   40 NA's 
   1    2    1    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2012 2013 2014 NA's 
   1    3    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-h6-8124-11a-01d-2400-26 tcga-h6-a45n-11a-12d-a26z-26 
                           1                            1 
tcga-hv-a5a3-11a-11d-a26z-26                         NA's 
                           1                          182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
08d62516-167e-4c6b-b69d-83f6bc5742b4 29208e6a-d515-404b-b91a-6adc72a56906 
                                   1                                    1 
2aaa2bc5-2daa-4185-988a-ebb121a29549                                 NA's 
                                   1                                  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
128925379 128925403 131702020      NA's 
        1         1         1       182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
  26 NA's 
   3  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.15 0.16 NA's 
   1    2  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  10   27 NA's 
   1    2  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   3  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   2    9 NA's 
   2    1  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   2    3    5 NA's 
   1    1    1  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
2400 a26z NA's 
   1    2  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   d    g    h NA's 
   1    1    1  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 1.5  1.6 NA's 
   1    2  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
   1    2  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10 NA's 
   3  182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2012 2013 NA's 
   1    2  182 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched methylation control                             NA's 
                               1                              184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.457e+08 1.498e+08 1.641e+08 2.891e+08 1.691e+08 1.076e+09       178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    5 NA's 
   6    1  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 NA's 
   2    3    2  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    26.0    27.0    25.0    28.5    29.0     178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   1    2    5    9 NA's 
   2    2    1    2  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.500   9.000   7.286  10.000  11.000     178 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   d    e    f    g NA's 
   3    1    2    1  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.82 NA's 
   1    3    2    1  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
   1    6  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7   13 NA's 
   1    5    1  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
   1    3    3  178 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.84   16.61   32.27   60.12   38.27  271.00     178 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   7  178 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 NA's 
   2    3    2  178 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
   1    6  178 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             178 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    7                   178 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        7       178 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
   4    1  180 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   14.50   21.00   18.71   23.50   28.00     178 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
   7  178 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   3.000   5.571   9.500  11.000     178 

patient.samples.sample.3.portions.portion.portion_number:
   1   11   12 NA's 
   2    4    1  178 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
   5  180 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   6  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.250   5.000   4.833   8.000  10.000     179 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1    3 NA's 
   4    1    1  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   5  180 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1 NA's 
   5    1  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
  88  100 NA's 
   1    4  180 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0   12 NA's 
   4    1  180 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   5  180 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   5  180 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
   6  179 

patient.samples.sample.3.portions.portion.weight:
   2   30   48 NA's 
   1    5    1  178 

patient.samples.sample.3.portions.portion.year_of_creation:
2012 2013 2014 NA's 
   2    4    1  178 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   1                    6                  178 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
   1    6  178 

patient.samples.sample.3.shortest_dimension:
 0.3 NA's 
   1  184 

patient.samples.sample.3.vial_number:
   a NA's 
   7  178 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   87.75  318.50  597.74  943.75 4142.00      51 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
2f9df4d3-64ab-4d64-9bdb-ab8321fb048f 4388a2a2-b109-4577-93a6-f3a190378f97 
                                   1                                    1 
935d958b-b47a-48c0-98ad-f79a49786b7c bfd443a8-20f9-4fe7-b8b7-5c96fad34c68 
                                   1                                    1 
                                NA's 
                                 181 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.4:
2392d167-7db6-4dc0-9e17-aa825adac18a 722a2015-9286-49c2-8df4-177a74be9c74 
                                   1                                    1 
b0fa6a94-3167-4da6-9d10-f79ef1b2522d                                 NA's 
                                   1                                  182 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.5:
0d1407be-75f7-4827-98c8-0da395ee4f96 ba7e6eed-124e-4a52-b97e-1bc8f16ef7b5 
                                   1                                    1 
c7876997-f7d0-48ec-8869-ab9457fff6eb                                 NA's 
                                   1                                  182 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
3fb953fd-6867-4ba0-9fe6-c7db07003e90                                 NA's 
                                   1                                  184 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
9a3db7ff-b6be-419f-856b-eb922ac99248                                 NA's 
                                   1                                  184 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    80.0   150.0   244.6   285.0  1550.0      50 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.200   0.525   0.700   0.736   0.900   2.000     135 

patient.samples.sample.is_ffpe:
 no 
185 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   1.000   1.200   1.234   1.475   2.300     135 

patient.samples.sample.oct_embedded:
false  true  NA's 
   34   101    50 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.690   1.820   1.850   1.851   1.880   2.090      48 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
121012512 145675877 164080728 154123751 167788085 174189882         7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
  82   96    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1624  0.1700  0.1800       7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   25.00   27.00   21.95   28.00   30.00       7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 178    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.500   4.348   5.000  10.000       7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.719   6.000  10.000       7 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   2.092   3.000   2.746   3.400   3.600       7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
  43  135    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  10   13 13.3   20 NA's 
   1   39   42   96    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 NA's 
  43   42   93    7 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.457e+08 1.641e+08 2.017e+08 1.742e+08 1.076e+09         7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   13 NA's 
  96   82    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1624  0.1700  0.1800       7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   25.00   27.00   21.95   28.00   30.00       7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 178    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.500   4.348   5.000  10.000       7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.006   7.000  11.000       7 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.080   2.340   2.618   3.000   3.600       7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
  43  135    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   15   20 NA's 
  96    1   81    7 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
  43   42   93    7 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.20   15.81   26.66   41.02   47.81  359.20       7 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
 178    7 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   r NA's 
 178    7 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1624  0.1700  0.1800       7 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             178 
                                            NA's 
                                               7 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 178    7 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                      178                         7 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.680   1.170   1.350   1.354   1.498   2.270       7 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.300   7.600   7.746   8.075   9.200       7 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    178       7 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 139   35    3    8 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
121011669 132271268 164082118 153412779 167804382 175155447        45 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8 NA's 
 140   45 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 140   45 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.00    8.00   15.36   27.00   29.00      45 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 140   45 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   6.000   4.943   7.000   7.000      45 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.779   5.000  12.000      45 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 140   45 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
  34  106   45 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 140   45 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
  34   25   81   45 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  139.0   159.2   166.0   165.9   178.2   188.0     151 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                 140                   45 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
 140   45 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
  34  151 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 140   45 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
  34  106   45 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             140 
                                            NA's 
                                              45 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 140   45 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
      131         9        45 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen      NA's 
       34       151 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 113   24    3   45 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
 1.9    2 NA's 
   8    1  176 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.210e+08 1.457e+08 1.641e+08 2.341e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   5   8  26 
126   7  20  32 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0500  0.1400  0.1500  0.1435  0.1600  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   17.00   26.00   22.38   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
185 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0     3.0     3.0     4.4     5.0    10.0 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   3.886   5.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.010   1.070   1.362   1.400   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 22  23 
 50 135 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67    6.70    6.70   11.54   10.00   60.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
   7   43   42   93 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.070e+08 1.457e+08 1.641e+08 2.185e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  1   5   8 
 39  20 126 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05000 0.07000 0.08000 0.09714 0.14000 0.17000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   17.00   26.00   21.62   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 no 
185 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   4.319   5.000  10.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   3.000   4.016   5.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.950   2.800   2.513   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
 22  23 
 50 135 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   13.00   40.00   32.33   40.00   60.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 
   7   43   42   93 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
107018871 131702155 164053608 151905985 167823352 175154812         7 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    5    8   26 NA's 
   4  117   41   16    7 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1324  0.1600  0.1700       7 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   26.00   21.38   28.00   30.00       7 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 178    7 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.421   5.000  10.000       7 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   3.837   5.000  12.000       7 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.660   1.950   2.080   2.184   2.210   3.600       7 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
  46  132    7 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.40   13.00   13.00   21.47   13.30   60.00       7 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 2013 2014 NA's 
   4   42   41   91    7 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
107016734 124429149 150705875 147352145 164105329 175154879        72 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   5    8   26 NA's 
  36   15   62   72 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.070   0.140   0.150   0.141   0.160   0.170      72 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00   25.00   16.89   27.00   30.00      72 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 113   72 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.885   9.000  10.000      72 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.743   4.000  11.000      72 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.75    1.50    1.60    1.59    1.86    2.26      72 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  37   76   72 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.40   10.00   10.00   11.44   13.30   14.00      72 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 2012 2013 2014 NA's 
   4   33   28   48   72 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
174186442 174431138 175154814 174906181 175154850 175154877       119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8 NA's 
  66  119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 0.08 0.09 NA's 
  33   28    5  119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   1   28 NA's 
  49   17  119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  66  119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   5   10 NA's 
  17   49  119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.803   6.000   8.000     119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a40k a45x NA's 
  17   49  119 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.6600  0.8075  0.9800  0.9736  1.1200  1.2600     119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
  66  119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
 9.4   14 NA's 
  17   49  119 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2014 NA's 
  66  119 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.457e+08 1.641e+08 2.417e+08 1.641e+08 1.076e+09       155 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   5  23  26 
 20   4 155   6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 NA's 
   8   10    9    3  155 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   11.75   26.00   21.23   27.75   30.00     155 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  30  155 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.767   3.750  10.000     155 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     1.0     2.0     3.6     4.0    12.0     155 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.010   1.070   1.248   1.475   2.210     155 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
 10 175 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7   10   13 NA's 
   4   16    6    4  155 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
   2    8    7   13  155 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.50   11.37   18.66   40.55   36.00  366.70 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
185 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
185 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
   6   38   64   61   16 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
185 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
 50 135 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             185 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
185 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  185 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      176         9 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 143   37    4    1 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    6.00   14.00   14.25   21.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
185 

patient.samples.sample.portions.portion.lce:
 lce NA's 
   1  184 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   5.546   9.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   11.00   13.39   12.00   61.00 

patient.samples.sample.portions.portion.portion_sequence:
   1    2    3 NA's 
  47    3    2  133 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  36  149 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   5.000   5.000   9.077  15.000  20.000     172 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0    1    5 NA's 
   8    1    1  175 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.7778  0.0000 10.0000     149 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0    1    5 NA's 
   8    1    2  174 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   7.323   4.500  90.000     154 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    8.75   27.50   28.69   40.00   85.00     153 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   40.00   67.50   63.47   86.25  100.00     153 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   37.50   67.50   55.06   71.00   97.00     149 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
 top NA's 
  36  149 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
185 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.00    8.00   17.49   32.50  100.00      34 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   4.812   8.000  20.000      36 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    1.92    0.00   35.00       9 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   5.389  10.000  20.000      36 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   11.94   20.00   98.00      11 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   30.00   30.12   50.00   80.00      10 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   30.00   60.00   56.07   80.00  100.00      10 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   35.00   60.00   52.43   70.00  100.00       9 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
    36    149 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.00   30.00   30.59   30.00   80.00       1 

patient.samples.sample.portions.portion.year_of_creation:
2011 2012 2013 2014 
  29   34   46   76 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 123   62 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 123   62 

patient.samples.sample.portions.shipment_portion.plate_id:
a39l a39m a43k NA's 
  81   25   17   62 

patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   21.09   21.00   51.00      62 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    3 NA's 
   2   69    1  113 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   7   24 NA's 
 106   17   62 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   5    6 NA's 
 106   17   62 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
 123   62 

patient.samples.sample.sample_type:
primary tumor 
          185 

patient.samples.sample.sample_type_id:
  1 
185 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.200   0.225   0.300   0.336   0.400   0.600     135 

patient.samples.sample.vial_number:
  a 
185 

}}
\keyword{datasets}
