\name{ STAD }
\alias{ STAD }
\docType{data}
\title{ STAD }

\details{
\preformatted{
> STAD
A MultiAssayExperiment object of 7 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 7: 
 [1] RNASeqGene: ExpressionSet with 26120 rows and 36 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 41 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 430 columns 
 [4] Methylation: SummarizedExperiment with 485577 rows and 397 columns 
 [5] RPPAArray: ExpressionSet with 195 rows and 357 columns 
 [6] gistica: SummarizedExperiment with 24776 rows and 441 columns 
 [7] gistict: SummarizedExperiment with 24776 rows and 441 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( STAD )
CharacterList of length 7
[["RNASeqGene"]] AADACL3 AADACL4 AB007962 AB074166 AB1 ... VCY1B VCY XKRY ZFY
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( STAD )
CharacterList of length 7
[["RNASeqGene"]] TCGA-BR-4191-01A-02R-1131-13 ... TCGA-CG-4477-01A-01R-1157-13
[["RNASeq2GeneNorm"]] TCGA-B7-5816-01A-21R-1602-13 ...
[["miRNASeqGene"]] TCGA-3M-AB46-01A-11R-A415-13 ...
[["Methylation"]] TCGA-3M-AB46-01A-11D-A411-05 ... TCGA-ZQ-A9CR-01A-11D-A398-05
[["RPPAArray"]] TCGA-3M-AB47-01A-11-A43D-20 ... TCGA-ZA-A8F6-01A-11-A43D-20
[["gistica"]] TCGA-3M-AB46-01A-11D-A40Z-01 ... TCGA-ZQ-A9CR-01A-11D-A396-01
[["gistict"]] TCGA-3M-AB46-01A-11D-A40Z-01 ... TCGA-ZQ-A9CR-01A-11D-A396-01

Sizes of each ExperimentList element:

            assay size.Mb
1      RNASeqGene  1.5 Mb
2 RNASeq2GeneNorm  1.1 Mb
3    miRNASeqGene  0.1 Mb
4     Methylation 67.4 Mb
5       RPPAArray  0.1 Mb
6         gistica  4.4 Mb
7         gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   272 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
171.000 169.000   0.953   0.792   1.090 


--------------------------- 
Available sample meta-data: 
--------------------------- 


years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   58.00   67.00   65.73   73.00   90.00       9 

vital_status: 
  0   1 
270 173 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   196.0   348.0   423.7   553.0  2197.0     272 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    89.0   409.5   546.2   761.8  3720.0      87 

tumor_tissue_site: 
stomach 
    443 




pathology_M_stage: 
 m0  m1  mx 
391  30  22 

gender: 
female   male 
   158    285 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1996    2010    2011    2010    2012    2013       6 

radiation_therapy: 
  no  yes NA's 
 323   77   43 


residual_tumor: 
  r0   r1   r2   rx NA's 
 350   18   19   25   31 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   5.635   8.000  57.000      51 

race: 
                                    asian 
                                       89 
                black or african american 
                                       13 
native hawaiian or other pacific islander 
                                        1 
                                    white 
                                      278 
                                     NA's 
                                       62 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     5                    318                    120 

admin.bcr: 
nationwide children's hospital 
                           443 

admin.day_of_dcc_upload: 
  1 
443 

admin.disease_code: 
stad 
 443 


admin.month_of_dcc_upload: 
 10 
443 

admin.patient_withdrawal.withdrawn: 
false 
  443 

admin.project_code: 
tcga 
 443 

admin.year_of_dcc_upload: 
2015 
 443 

patient.age_at_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   58.00   67.00   65.68   73.00   90.00       5 



patient.antireflux_treatment: 
  no  yes NA's 
 176   44  223 

patient.antireflux_treatment_types.antireflux_treatment_type: 
              antacids            h2 blockers proton pump inhibitors 
                     4                      7                     33 
                  NA's 
                   399 

patient.antireflux_treatment_types.antireflux_treatment_type.2: 
antacids     NA's 
       5      438 

patient.barretts_esophagus: 
  no  yes NA's 
 233   17  193 



patient.clinical_cqcf.consent_or_death_status: 
consented  deceased    waiver 
      428        14         1 


patient.clinical_cqcf.days_to_consent: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
-2309.000    -1.000     0.000    -0.673    11.750  1124.000        21 

patient.clinical_cqcf.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   76.0   288.0   491.5   682.6   769.0  2197.0     429 



patient.clinical_cqcf.history_of_neoadjuvant_treatment: 
 no 
443 

patient.clinical_cqcf.history_of_prior_malignancy: 
                                                     no 
                                                    429 
                       yes, history of prior malignancy 
                                                     13 
yes, history of synchronous and or bilateral malignancy 
                                                      1 


patient.clinical_cqcf.normal_tissue_proximity: 
distal (>2cm)          NA's 
          108           335 

patient.clinical_cqcf.other_anatomic_site: 
   gastric stump         incisura lesser curvature             NA's 
               2                1                1              439 


patient.clinical_cqcf.tumor_type: 
primary 
    443 


patient.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   17.00   16.93   23.00   31.00       2 

patient.days_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32873  -26992  -24661  -24188  -21421  -10963       9 

patient.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     140     282     416     543    2197     358 

patient.days_to_initial_pathologic_diagnosis: 
   0 NA's 
 437    6 

patient.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    12.0    42.5   266.0   268.5  3720.0      91 

patient.drugs.drug.10.bcr_drug_barcode: 
tcga-sw-a7eb-d56033                NA's 
                  1                 442 

patient.drugs.drug.10.bcr_drug_uuid: 
8be1d762-19e4-4265-8de6-427f23a9bcd6                                 NA's 
                                   1                                  442 

patient.drugs.drug.10.day_of_form_completion: 
  25 NA's 
   1  442 

patient.drugs.drug.10.days_to_drug_therapy_end: 
 141 NA's 
   1  442 

patient.drugs.drug.10.days_to_drug_therapy_start: 
 136 NA's 
   1  442 

patient.drugs.drug.10.drug_name: 
fluorouracil         NA's 
           1          442 

patient.drugs.drug.10.measure_of_response: 
stable disease           NA's 
             1            442 

patient.drugs.drug.10.month_of_form_completion: 
   1 NA's 
   1  442 

patient.drugs.drug.10.therapy_ongoing: 
  no NA's 
   1  442 

patient.drugs.drug.10.therapy_types.therapy_type: 
chemotherapy         NA's 
           1          442 

patient.drugs.drug.10.tx_on_clinical_trial: 
  no NA's 
   1  442 

patient.drugs.drug.10.year_of_form_completion: 
2014 NA's 
   1  442 

patient.drugs.drug.11.bcr_drug_barcode: 
tcga-sw-a7eb-d56034                NA's 
                  1                 442 

patient.drugs.drug.11.bcr_drug_uuid: 
9b3b6e87-eb4b-4ac5-aaef-f8062393c748                                 NA's 
                                   1                                  442 

patient.drugs.drug.11.day_of_form_completion: 
  25 NA's 
   1  442 

patient.drugs.drug.11.days_to_drug_therapy_end: 
 141 NA's 
   1  442 

patient.drugs.drug.11.days_to_drug_therapy_start: 
 136 NA's 
   1  442 

patient.drugs.drug.11.drug_name: 
doxorubicin        NA's 
          1         442 

patient.drugs.drug.11.measure_of_response: 
stable disease           NA's 
             1            442 

patient.drugs.drug.11.month_of_form_completion: 
   1 NA's 
   1  442 

patient.drugs.drug.11.therapy_ongoing: 
  no NA's 
   1  442 

patient.drugs.drug.11.therapy_types.therapy_type: 
chemotherapy         NA's 
           1          442 

patient.drugs.drug.11.tx_on_clinical_trial: 
  no NA's 
   1  442 

patient.drugs.drug.11.year_of_form_completion: 
2014 NA's 
   1  442 

patient.drugs.drug.12.bcr_drug_barcode: 
tcga-sw-a7eb-d56035                NA's 
                  1                 442 

patient.drugs.drug.12.bcr_drug_uuid: 
dae012fb-2f68-4f22-ac83-0c1beebaa327                                 NA's 
                                   1                                  442 

patient.drugs.drug.12.day_of_form_completion: 
  25 NA's 
   1  442 

patient.drugs.drug.12.days_to_drug_therapy_end: 
 141 NA's 
   1  442 

patient.drugs.drug.12.days_to_drug_therapy_start: 
 136 NA's 
   1  442 

patient.drugs.drug.12.drug_name: 
cisplatin      NA's 
        1       442 

patient.drugs.drug.12.measure_of_response: 
stable disease           NA's 
             1            442 

patient.drugs.drug.12.month_of_form_completion: 
   1 NA's 
   1  442 

patient.drugs.drug.12.therapy_ongoing: 
  no NA's 
   1  442 

patient.drugs.drug.12.therapy_types.therapy_type: 
chemotherapy         NA's 
           1          442 

patient.drugs.drug.12.tx_on_clinical_trial: 
  no NA's 
   1  442 

patient.drugs.drug.12.year_of_form_completion: 
2014 NA's 
   1  442 



patient.drugs.drug.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    10.0    15.0    16.1    25.0    31.0     327 

patient.drugs.drug.2.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   116.0   167.0   214.8   226.5  2070.0     344 

patient.drugs.drug.2.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0    43.5    63.0   127.1   108.5  1949.0     332 


patient.drugs.drug.2.measure_of_response: 
clinical progressive disease            complete response 
                          19                           54 
            partial response               stable disease 
                           2                            9 
                        NA's 
                         359 

patient.drugs.drug.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.198   9.000  12.000     327 

patient.drugs.drug.2.number_cycles: 
   2    3    5    6 NA's 
   1    2    3    1  436 

patient.drugs.drug.2.prescribed_dose: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  100.0   263.5   400.0   474.6   622.5  1050.0     436 

patient.drugs.drug.2.prescribed_dose_units: 
    mg mg/day  mg/m2   NA's 
     3      2      2    436 

patient.drugs.drug.2.regimen_indication: 
   adjuvant progression        NA's 
          6           1         436 

patient.drugs.drug.2.regimen_number: 
   1 NA's 
   4  439 

patient.drugs.drug.2.route_of_administrations.route_of_administration: 
intravenous (iv)             oral             NA's 
               6                1              436 

patient.drugs.drug.2.therapy_ongoing: 
  no  yes NA's 
 105   10  328 

patient.drugs.drug.2.therapy_types.therapy_type: 
     ancillary   chemotherapy other, specify           NA's 
            19             96              1            327 

patient.drugs.drug.2.therapy_types.therapy_type_notes: 
ancillary      NA's 
        1       442 

patient.drugs.drug.2.total_dose: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    782     975    1814    5110    4845   19700     437 

patient.drugs.drug.2.total_dose_units: 
   mg mg/m2  NA's 
    5     1   437 

patient.drugs.drug.2.tx_on_clinical_trial: 
  no  yes NA's 
 108    1  334 

patient.drugs.drug.2.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2013    2013    2014    2015     327 



patient.drugs.drug.3.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   15.00   16.29   25.00   31.00     354 

patient.drugs.drug.3.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   114.5   174.5   238.5   222.8  2070.0     363 

patient.drugs.drug.3.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   41.25   61.00  148.59  111.75 1949.00     357 


patient.drugs.drug.3.measure_of_response: 
clinical progressive disease            complete response 
                          14                           42 
            partial response               stable disease 
                           2                            8 
                        NA's 
                         377 

patient.drugs.drug.3.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.202  10.000  12.000     354 

patient.drugs.drug.3.number_cycles: 
   1    3    5 NA's 
   1    2    1  439 

patient.drugs.drug.3.prescribed_dose: 
  40   87   90  200 NA's 
   1    1    1    1  439 

patient.drugs.drug.3.prescribed_dose_units: 
    mg mg/day  mg/m2   NA's 
     1      2      1    439 

patient.drugs.drug.3.regimen_indication: 
   adjuvant progression        NA's 
          3           1         439 

patient.drugs.drug.3.regimen_number: 
   1 NA's 
   2  441 

patient.drugs.drug.3.route_of_administrations.route_of_administration: 
intravenous (iv)             NA's 
               4              439 

patient.drugs.drug.3.therapy_ongoing: 
  no  yes NA's 
  83    6  354 

patient.drugs.drug.3.therapy_types.therapy_type: 
   ancillary chemotherapy         NA's 
          11           78          354 

patient.drugs.drug.3.total_dose: 
  87  120  501 1800 NA's 
   1    1    1    1  439 

patient.drugs.drug.3.total_dose_units: 
   mg mg/m2  NA's 
    3     1   439 

patient.drugs.drug.3.tx_on_clinical_trial: 
  no  yes NA's 
  84    1  358 

patient.drugs.drug.3.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2013    2013    2014    2015     354 



patient.drugs.drug.4.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   12.00   17.00   16.81   24.25   30.00     411 

patient.drugs.drug.4.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   48.0   128.0   177.0   336.4   410.0  2070.0     416 

patient.drugs.drug.4.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   27.0    60.5   117.0   250.2   353.5  1949.0     412 


patient.drugs.drug.4.measure_of_response: 
clinical progressive disease            complete response 
                           8                            9 
            partial response               stable disease 
                           2                            3 
                        NA's 
                         421 

patient.drugs.drug.4.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.406  10.250  12.000     411 

patient.drugs.drug.4.number_cycles: 
   3    4 NA's 
   1    1  441 

patient.drugs.drug.4.prescribed_dose: 
 400  800 NA's 
   1    1  441 

patient.drugs.drug.4.prescribed_dose_units: 
   mg/day mg/m2/day      NA's 
        1         1       441 

patient.drugs.drug.4.regimen_indication: 
adjuvant     NA's 
       2      441 

patient.drugs.drug.4.regimen_number: 
   1 NA's 
   1  442 

patient.drugs.drug.4.route_of_administrations.route_of_administration: 
intravenous (iv)             NA's 
               2              441 

patient.drugs.drug.4.therapy_ongoing: 
  no  yes NA's 
  28    4  411 

patient.drugs.drug.4.therapy_types.therapy_type: 
   ancillary chemotherapy         NA's 
           6           26          411 

patient.drugs.drug.4.total_dose: 
 1200 14100  NA's 
    1     1   441 

patient.drugs.drug.4.total_dose_units: 
       mg mg/m2/day      NA's 
        1         1       441 

patient.drugs.drug.4.tx_on_clinical_trial: 
  no  yes NA's 
  29    1  413 

patient.drugs.drug.4.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2013    2013    2014    2015     411 



patient.drugs.drug.5.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   14.00   19.00   17.79   24.50   30.00     424 

patient.drugs.drug.5.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   48.0   129.5   281.5   420.9   565.2  2070.0     427 

patient.drugs.drug.5.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0    85.0   189.0   354.8   402.0  2070.0     426 


patient.drugs.drug.5.measure_of_response: 
clinical progressive disease            complete response 
                           7                            3 
            partial response               stable disease 
                           1                            3 
                        NA's 
                         429 

patient.drugs.drug.5.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   7.421  11.000  12.000     424 

patient.drugs.drug.5.number_cycles: 
   4 NA's 
   1  442 

patient.drugs.drug.5.prescribed_dose: 
  40 NA's 
   1  442 

patient.drugs.drug.5.prescribed_dose_units: 
mg/day   NA's 
     1    442 

patient.drugs.drug.5.regimen_indication: 
adjuvant     NA's 
       1      442 

patient.drugs.drug.5.route_of_administrations.route_of_administration: 
intravenous (iv)             NA's 
               1              442 

patient.drugs.drug.5.therapy_ongoing: 
  no  yes NA's 
  18    1  424 

patient.drugs.drug.5.therapy_types.therapy_type: 
                 ancillary               chemotherapy 
                         5                         13 
targeted molecular therapy                       NA's 
                         1                        424 

patient.drugs.drug.5.total_dose: 
 680 NA's 
   1  442 

patient.drugs.drug.5.total_dose_units: 
  mg NA's 
   1  442 

patient.drugs.drug.5.tx_on_clinical_trial: 
  no  yes NA's 
  17    1  425 

patient.drugs.drug.5.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2013    2013    2014    2015     424 



patient.drugs.drug.6.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   15.00   19.00   20.11   25.00   30.00     434 

patient.drugs.drug.6.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     70     149     351     331     410     611     434 

patient.drugs.drug.6.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   64.0    85.0   249.0   240.2   370.0   402.0     434 


patient.drugs.drug.6.measure_of_response: 
clinical progressive disease               stable disease 
                           5                            1 
                        NA's 
                         437 

patient.drugs.drug.6.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.556   9.000  12.000     434 

patient.drugs.drug.6.therapy_ongoing: 
  no NA's 
   9  434 

patient.drugs.drug.6.therapy_types.therapy_type: 
   ancillary chemotherapy         NA's 
           1            8          434 

patient.drugs.drug.6.tx_on_clinical_trial: 
  no NA's 
   9  434 

patient.drugs.drug.6.year_of_form_completion: 
2012 2013 2014 2015 NA's 
   2    2    4    1  434 

patient.drugs.drug.7.bcr_drug_barcode: 
tcga-br-6706-d48238 tcga-d7-6520-d64898 tcga-sw-a7eb-d56030                NA's 
                  1                   1                   1                 440 

patient.drugs.drug.7.bcr_drug_uuid: 
0d4a2166-a909-44ed-973e-b89436c81b2d ab5c6559-7f18-4f29-bbbb-3e8e18b8562a 
                                   1                                    1 
c9f32fda-f421-4527-902a-4abb77f4acdd                                 NA's 
                                   1                                  440 

patient.drugs.drug.7.day_of_form_completion: 
  10   15   25 NA's 
   1    1    1  440 

patient.drugs.drug.7.days_to_drug_therapy_end: 
 106  321  445 NA's 
   1    1    1  440 

patient.drugs.drug.7.days_to_drug_therapy_start: 
 101  249  441 NA's 
   1    1    1  440 

patient.drugs.drug.7.drug_name: 
     cisplatin   fluorouracil fluorouracilum           NA's 
             1              1              1            440 

patient.drugs.drug.7.measure_of_response: 
clinical progressive disease               stable disease 
                           1                            1 
                        NA's 
                         441 

patient.drugs.drug.7.month_of_form_completion: 
   1    9 NA's 
   1    2  440 

patient.drugs.drug.7.therapy_ongoing: 
  no NA's 
   3  440 

patient.drugs.drug.7.therapy_types.therapy_type: 
chemotherapy         NA's 
           3          440 

patient.drugs.drug.7.tx_on_clinical_trial: 
  no NA's 
   3  440 

patient.drugs.drug.7.year_of_form_completion: 
2013 2014 NA's 
   1    2  440 

patient.drugs.drug.8.bcr_drug_barcode: 
tcga-sw-a7eb-d56031                NA's 
                  1                 442 

patient.drugs.drug.8.bcr_drug_uuid: 
9c70d3a7-938a-48ba-a737-c21f70d4284e                                 NA's 
                                   1                                  442 

patient.drugs.drug.8.day_of_form_completion: 
  25 NA's 
   1  442 

patient.drugs.drug.8.days_to_drug_therapy_end: 
 106 NA's 
   1  442 

patient.drugs.drug.8.days_to_drug_therapy_start: 
 101 NA's 
   1  442 

patient.drugs.drug.8.drug_name: 
doxorubicin        NA's 
          1         442 

patient.drugs.drug.8.measure_of_response: 
stable disease           NA's 
             1            442 

patient.drugs.drug.8.month_of_form_completion: 
   1 NA's 
   1  442 

patient.drugs.drug.8.therapy_ongoing: 
  no NA's 
   1  442 

patient.drugs.drug.8.therapy_types.therapy_type: 
chemotherapy         NA's 
           1          442 

patient.drugs.drug.8.tx_on_clinical_trial: 
  no NA's 
   1  442 

patient.drugs.drug.8.year_of_form_completion: 
2014 NA's 
   1  442 

patient.drugs.drug.9.bcr_drug_barcode: 
tcga-sw-a7eb-d56032                NA's 
                  1                 442 

patient.drugs.drug.9.bcr_drug_uuid: 
5ab31e67-4dc4-4dd8-a7c3-0773f438d310                                 NA's 
                                   1                                  442 

patient.drugs.drug.9.day_of_form_completion: 
  25 NA's 
   1  442 

patient.drugs.drug.9.days_to_drug_therapy_end: 
 106 NA's 
   1  442 

patient.drugs.drug.9.days_to_drug_therapy_start: 
 101 NA's 
   1  442 

patient.drugs.drug.9.drug_name: 
cisplatin      NA's 
        1       442 

patient.drugs.drug.9.measure_of_response: 
stable disease           NA's 
             1            442 

patient.drugs.drug.9.month_of_form_completion: 
   1 NA's 
   1  442 

patient.drugs.drug.9.therapy_ongoing: 
  no NA's 
   1  442 

patient.drugs.drug.9.therapy_types.therapy_type: 
chemotherapy         NA's 
           1          442 

patient.drugs.drug.9.tx_on_clinical_trial: 
  no NA's 
   1  442 

patient.drugs.drug.9.year_of_form_completion: 
2014 NA's 
   1  442 



patient.drugs.drug.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.25   16.00   17.05   26.00   31.00     253 

patient.drugs.drug.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -56.0   126.0   186.0   209.1   244.0   854.0     280 

patient.drugs.drug.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0    41.0    69.5   107.0   121.2   847.0     259 


patient.drugs.drug.measure_of_response: 
clinical progressive disease            complete response 
                          36                           90 
            partial response               stable disease 
                           3                           10 
                        NA's 
                         304 

patient.drugs.drug.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.195   9.000  12.000     253 

patient.drugs.drug.number_cycles: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       3       5       4       5       6     430 


patient.drugs.drug.prescribed_dose_units: 
    mg mg/day  mg/m2   NA's 
     8      2      6    427 

patient.drugs.drug.regimen_indication: 
   adjuvant  palliative progression        NA's 
         15           1           2         425 

patient.drugs.drug.regimen_number: 
  01    1 plfe NA's 
   2    4    1  436 

patient.drugs.drug.route_of_administrations.route_of_administration: 
    intrahepatic intravenous (iv)             NA's 
               1               16              426 

patient.drugs.drug.therapy_ongoing: 
  no  yes NA's 
 170   19  254 

patient.drugs.drug.therapy_types.therapy_type: 
   ancillary chemotherapy         NA's 
           3          186          254 


patient.drugs.drug.total_dose_units: 
   mg mg/m2  NA's 
   11     3   429 

patient.drugs.drug.tx_on_clinical_trial: 
  no  yes NA's 
 171    1  271 

patient.drugs.drug.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2013    2013    2014    2015     253 

patient.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     5                    318                    120 

patient.family_history_of_stomach_cancer: 
  no  yes NA's 
 324   18  101 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy: 
  no  yes NA's 
   9    7  427 

patient.follow_ups.follow_up.2.additional_radiation_therapy: 
  no NA's 
  16  427 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure: 
  no NA's 
   6  437 

patient.follow_ups.follow_up.2.additional_surgery_metastatic_procedure: 
  no  yes NA's 
   8    1  434 



patient.follow_ups.follow_up.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     9.5    15.0    15.4    21.5    31.0     348 

patient.follow_ups.follow_up.2.days_to_additional_surgery_metastatic_procedure: 
 826 NA's 
   1  442 

patient.follow_ups.follow_up.2.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  192.0   508.5   618.0   609.1   780.5   874.0     424 

patient.follow_ups.follow_up.2.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   86.0   679.0   940.0   956.9  1100.0  2351.0     370 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   79.0   311.5   426.0   474.9   631.0   962.0     428 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason: 
    additional new tumor event scheduled follow-up submission 
                             1                             94 
                          NA's 
                           348 

patient.follow_ups.follow_up.2.followup_treatment_success: 
complete remission/response  partial remission/response 
                         59                           1 
        progressive disease              stable disease 
                         15                           4 
                       NA's 
                        364 

patient.follow_ups.follow_up.2.lost_follow_up: 
  no  yes NA's 
  83   10  350 

patient.follow_ups.follow_up.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.305  11.000  12.000     348 


patient.follow_ups.follow_up.2.new_neoplasm_event_types.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence       new primary tumor 
                      8                       6                       1 
                   NA's 
                    428 

patient.follow_ups.follow_up.2.new_neoplasm_event_types.new_neoplasm_event_type.2: 
distant metastasis               NA's 
                 1                442 

patient.follow_ups.follow_up.2.new_neoplasm_occurrence_anatomic_site_text: 
adrenal gland, bone      liver and lung                NA's 
                  1                   1                 441 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
  66   15  362 

patient.follow_ups.follow_up.2.patient_death_reason: 
other, non-malignant disease               stomach cancer 
                           6                           13 
                        NA's 
                         424 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
        60         19        364 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success: 
complete remission/response         progressive disease 
                         82                           3 
             stable disease                        NA's 
                          2                         356 

patient.follow_ups.follow_up.2.progression_determined_by_list.progression_determined_by: 
biopsy with histologic confirmation                  convincing imaging 
                                  3                                  10 
                               NA's 
                                430 

patient.follow_ups.follow_up.2.radiation_therapy: 
  no  yes NA's 
  70   20  353 

patient.follow_ups.follow_up.2.residual_disease_post_new_tumor_event_margin_status: 
  r0 NA's 
   1  442 

patient.follow_ups.follow_up.2.targeted_molecular_therapy: 
  no  yes NA's 
  43   45  355 

patient.follow_ups.follow_up.2.vital_status: 
alive  dead  NA's 
   70    20   353 

patient.follow_ups.follow_up.2.year_of_form_completion: 
2013 2014 2015 NA's 
  18   52   25  348 

patient.follow_ups.follow_up.3.additional_pharmaceutical_therapy: 
  no  yes NA's 
   2    3  438 

patient.follow_ups.follow_up.3.additional_radiation_therapy: 
  no NA's 
   5  438 

patient.follow_ups.follow_up.3.additional_surgery_locoregional_procedure: 
  no NA's 
   1  442 

patient.follow_ups.follow_up.3.additional_surgery_metastatic_procedure: 
  no NA's 
   4  439 



patient.follow_ups.follow_up.3.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    10.5    12.0    14.8    23.0    28.0     428 

patient.follow_ups.follow_up.3.days_to_death: 
 588 NA's 
   1  442 

patient.follow_ups.follow_up.3.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    415    1051    1169    1159    1314    2032     429 

patient.follow_ups.follow_up.3.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  383.0   574.0   777.0   793.2  1048.0  1184.0     438 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason: 
    additional new tumor event scheduled follow-up submission 
                             1                             14 
                          NA's 
                           428 

patient.follow_ups.follow_up.3.followup_treatment_success: 
complete remission/response         progressive disease 
                          6                           5 
             stable disease                        NA's 
                          1                         431 

patient.follow_ups.follow_up.3.lost_follow_up: 
  no NA's 
  15  428 

patient.follow_ups.follow_up.3.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     5.0     5.0     6.5    12.0     428 

patient.follow_ups.follow_up.3.new_neoplasm_event_occurrence_anatomic_site: 
                              lung non-regional / distant lymph nodes 
                                 2                                  1 
                    other, specify                               NA's 
                                 1                                439 

patient.follow_ups.follow_up.3.new_neoplasm_event_types.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence                    NA's 
                      4                       1                     438 

patient.follow_ups.follow_up.3.new_neoplasm_occurrence_anatomic_site_text: 
adrenal    NA's 
      1     442 

patient.follow_ups.follow_up.3.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
   7    5  431 

patient.follow_ups.follow_up.3.patient_death_reason: 
stomach cancer           NA's 
             1            442 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
         6          6        431 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success: 
complete remission/response         progressive disease 
                         13                           1 
             stable disease                        NA's 
                          1                         428 

patient.follow_ups.follow_up.3.progression_determined_by_list.progression_determined_by: 
convincing imaging               NA's 
                 3                440 

patient.follow_ups.follow_up.3.radiation_therapy: 
  no  yes NA's 
  11    4  428 

patient.follow_ups.follow_up.3.targeted_molecular_therapy: 
  no  yes NA's 
   8    7  428 

patient.follow_ups.follow_up.3.vital_status: 
alive  dead  NA's 
   14     1   428 

patient.follow_ups.follow_up.3.year_of_form_completion: 
2013 2014 2015 NA's 
   1    4   10  428 

patient.follow_ups.follow_up.4.additional_pharmaceutical_therapy: 
  no NA's 
   1  442 

patient.follow_ups.follow_up.4.additional_radiation_therapy: 
  no NA's 
   1  442 

patient.follow_ups.follow_up.4.additional_surgery_metastatic_procedure: 
  no NA's 
   1  442 

patient.follow_ups.follow_up.4.bcr_followup_barcode: 
tcga-br-6458-f42622 tcga-vq-a91e-f70289                NA's 
                  1                   1                 441 

patient.follow_ups.follow_up.4.bcr_followup_uuid: 
0d3cf860-216c-4e55-936a-759c29894a03 36cc9c86-8e36-4e8f-a5bd-b03761034cc6 
                                   1                                    1 
                                NA's 
                                 441 

patient.follow_ups.follow_up.4.day_of_form_completion: 
   3   25 NA's 
   1    1  441 

patient.follow_ups.follow_up.4.days_to_death: 
 588 NA's 
   1  442 

patient.follow_ups.follow_up.4.days_to_last_followup: 
 559 NA's 
   1  442 

patient.follow_ups.follow_up.4.days_to_new_tumor_event_after_initial_treatment: 
 574 NA's 
   1  442 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason: 
    additional new tumor event scheduled follow-up submission 
                             1                              1 
                          NA's 
                           441 

patient.follow_ups.follow_up.4.followup_treatment_success: 
progressive disease                NA's 
                  1                 442 

patient.follow_ups.follow_up.4.lost_follow_up: 
  no NA's 
   2  441 

patient.follow_ups.follow_up.4.month_of_form_completion: 
   2    4 NA's 
   1    1  441 

patient.follow_ups.follow_up.4.new_neoplasm_event_occurrence_anatomic_site: 
other, specify           NA's 
             1            442 

patient.follow_ups.follow_up.4.new_neoplasm_event_types.new_neoplasm_event_type: 
distant metastasis               NA's 
                 1                442 

patient.follow_ups.follow_up.4.new_neoplasm_occurrence_anatomic_site_text: 
lung pleura        NA's 
          1         442 

patient.follow_ups.follow_up.4.new_tumor_event_after_initial_treatment: 
 yes NA's 
   1  442 

patient.follow_ups.follow_up.4.patient_death_reason: 
stomach cancer           NA's 
             1            442 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status: 
with tumor       NA's 
         1        442 

patient.follow_ups.follow_up.4.primary_therapy_outcome_success: 
complete remission/response                        NA's 
                          2                         441 

patient.follow_ups.follow_up.4.progression_determined_by_list.progression_determined_by: 
convincing imaging               NA's 
                 1                442 

patient.follow_ups.follow_up.4.radiation_therapy: 
  no  yes NA's 
   1    1  441 

patient.follow_ups.follow_up.4.targeted_molecular_therapy: 
 yes NA's 
   2  441 

patient.follow_ups.follow_up.4.vital_status: 
alive  dead  NA's 
    1     1   441 

patient.follow_ups.follow_up.4.year_of_form_completion: 
2013 2015 NA's 
   1    1  441 

patient.follow_ups.follow_up.5.bcr_followup_barcode: 
tcga-vq-a91e-f70863                NA's 
                  1                 442 

patient.follow_ups.follow_up.5.bcr_followup_uuid: 
398c15dd-b3a4-4771-93a4-6dfcb1100a9b                                 NA's 
                                   1                                  442 

patient.follow_ups.follow_up.5.day_of_form_completion: 
  10 NA's 
   1  442 

patient.follow_ups.follow_up.5.days_to_last_followup: 
 559 NA's 
   1  442 

patient.follow_ups.follow_up.5.followup_case_report_form_submission_reason: 
scheduled follow-up submission                           NA's 
                             1                            442 

patient.follow_ups.follow_up.5.lost_follow_up: 
  no NA's 
   1  442 

patient.follow_ups.follow_up.5.month_of_form_completion: 
   3 NA's 
   1  442 

patient.follow_ups.follow_up.5.primary_therapy_outcome_success: 
complete remission/response                        NA's 
                          1                         442 

patient.follow_ups.follow_up.5.radiation_therapy: 
 yes NA's 
   1  442 

patient.follow_ups.follow_up.5.targeted_molecular_therapy: 
 yes NA's 
   1  442 

patient.follow_ups.follow_up.5.vital_status: 
alive  NA's 
    1   442 

patient.follow_ups.follow_up.5.year_of_form_completion: 
2015 NA's 
   1  442 

patient.follow_ups.follow_up.6.bcr_followup_barcode: 
tcga-vq-a91e-f72381                NA's 
                  1                 442 

patient.follow_ups.follow_up.6.bcr_followup_uuid: 
95272104-dc6e-438e-be78-be5ecc30cc2c                                 NA's 
                                   1                                  442 

patient.follow_ups.follow_up.6.day_of_form_completion: 
  11 NA's 
   1  442 

patient.follow_ups.follow_up.6.days_to_last_followup: 
 664 NA's 
   1  442 

patient.follow_ups.follow_up.6.followup_case_report_form_submission_reason: 
scheduled follow-up submission                           NA's 
                             1                            442 

patient.follow_ups.follow_up.6.followup_treatment_success: 
complete remission/response                        NA's 
                          1                         442 

patient.follow_ups.follow_up.6.lost_follow_up: 
  no NA's 
   1  442 

patient.follow_ups.follow_up.6.month_of_form_completion: 
   6 NA's 
   1  442 

patient.follow_ups.follow_up.6.new_tumor_event_after_initial_treatment: 
  no NA's 
   1  442 

patient.follow_ups.follow_up.6.person_neoplasm_cancer_status: 
tumor free       NA's 
         1        442 

patient.follow_ups.follow_up.6.primary_therapy_outcome_success: 
complete remission/response                        NA's 
                          1                         442 

patient.follow_ups.follow_up.6.radiation_therapy: 
 yes NA's 
   1  442 

patient.follow_ups.follow_up.6.targeted_molecular_therapy: 
 yes NA's 
   1  442 

patient.follow_ups.follow_up.6.vital_status: 
alive  NA's 
    1   442 

patient.follow_ups.follow_up.6.year_of_form_completion: 
2015 NA's 
   1  442 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy: 
  no  yes NA's 
  44   23  376 

patient.follow_ups.follow_up.additional_radiation_therapy: 
  no  yes NA's 
  58    7  378 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure: 
  no  yes NA's 
  29    1  413 

patient.follow_ups.follow_up.additional_surgery_metastatic_procedure: 
  no  yes NA's 
  32    2  409 



patient.follow_ups.follow_up.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   14.00   16.11   24.00   31.00      86 

patient.follow_ups.follow_up.days_to_additional_surgery_locoregional_procedure: 
 143 NA's 
   1  442 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure: 
 419  420 NA's 
   1    1  441 

patient.follow_ups.follow_up.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   201.0   300.0   407.2   457.0  2197.0     354 

patient.follow_ups.follow_up.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   363.0   450.0   596.3   644.0  3720.0     192 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   46.0   159.0   307.0   347.1   410.5  1949.0     372 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason: 
    additional new tumor event scheduled follow-up submission 
                            10                            339 
                          NA's 
                            94 

patient.follow_ups.follow_up.followup_treatment_success: 
complete remission/response  partial remission/response 
                        183                           5 
        progressive disease              stable disease 
                         70                           9 
                       NA's 
                        176 

patient.follow_ups.follow_up.lost_follow_up: 
  no  yes NA's 
 246   55  142 

patient.follow_ups.follow_up.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   6.000   6.188  10.000  12.000      86 


patient.follow_ups.follow_up.new_neoplasm_event_types.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence       new primary tumor 
                     42                      19                       1 
                   NA's 
                    381 

patient.follow_ups.follow_up.new_neoplasm_event_types.new_neoplasm_event_type.2: 
distant metastasis               NA's 
                 3                440 


patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 258   72  113 

patient.follow_ups.follow_up.patient_death_reason: 
            other malignancy other, non-malignant disease 
                           2                           17 
              stomach cancer                         NA's 
                          55                          369 

patient.follow_ups.follow_up.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       196         85        162 

patient.follow_ups.follow_up.primary_therapy_outcome_success: 
complete remission/response  partial remission/response 
                        241                           6 
        progressive disease              stable disease 
                         40                          27 
                       NA's 
                        129 

patient.follow_ups.follow_up.progression_determined_by_list.progression_determined_by: 
biopsy with histologic confirmation                  convincing imaging 
                                 11                                  33 
                               NA's 
                                399 

patient.follow_ups.follow_up.progression_determined_by_list.progression_determined_by.2: 
    convincing imaging positive biomarker (s)                   NA's 
                     2                      1                    440 

patient.follow_ups.follow_up.progression_determined_by_list.progression_determined_by.3: 
positive biomarker (s)                   NA's 
                     1                    442 

patient.follow_ups.follow_up.radiation_therapy: 
  no  yes NA's 
 278   57  108 

patient.follow_ups.follow_up.targeted_molecular_therapy: 
  no  yes NA's 
 184  148  111 

patient.follow_ups.follow_up.vital_status: 
alive  dead  NA's 
  253    92    98 

patient.follow_ups.follow_up.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2013    2013    2014    2015      86 

patient.gender: 
female   male 
   158    285 

patient.h_pylori_infection: 
  no  yes NA's 
 168   20  255 


patient.history_of_neoadjuvant_treatment: 
 no 
443 




patient.informed_consent_verified: 
yes 
443 

patient.lymph_node_examined_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   16.00   22.33   29.00  105.00      51 

patient.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.494   7.000  12.000       2 

patient.neoplasm_histologic_grade: 
 g1  g2  g3  gx 
 12 159 263   9 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no  yes NA's 
  25   20  398 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no  yes NA's 
  38    6  399 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   91.0   168.5   277.0   385.8   472.0  1676.0     396 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.additional_surgery_locoregional_procedure: 
  no  yes NA's 
  20    4  419 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.days_to_additional_surgery_locoregional_procedure: 
 335  475 1029 1326 NA's 
   1    1    1    1  439 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.residual_disease_post_new_tumor_event_margin_status: 
  r0   r2 NA's 
   1    2  440 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.additional_surgery_metastatic_procedure: 
  no  yes NA's 
  23    3  417 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.days_to_additional_surgery_metastatic_procedure: 
 291  420  647  913 NA's 
   1    1    1    1  439 


patient.new_tumor_events.new_tumor_event.metastatic_procedure.residual_disease_post_new_tumor_event_margin_status: 
  r0 NA's 
   2  441 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence       new primary tumor 
                     26                      21                       1 
                   NA's 
                    395 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_types.new_neoplasm_event_type.2: 
distant metastasis  new primary tumor               NA's 
                 2                  1                440 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by: 
biopsy with histologic confirmation                  convincing imaging 
                                 10                                  37 
                               NA's 
                                396 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by.2: 
    convincing imaging positive biomarker (s)                   NA's 
                     2                      1                    440 

patient.new_tumor_events.new_tumor_event.progression_determined_by_list.progression_determined_by.3: 
positive biomarker (s)                   NA's 
                     1                    442 

patient.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 181   48  214 

patient.number_of_lymphnodes_positive_by_he: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   5.635   8.000  57.000      51 

patient.number_of_relatives_with_stomach_cancer: 
   0    1    2 NA's 
  58   16    2  367 

patient.patient_death_reason: 
other, non-malignant disease               stomach cancer 
                           7                           46 
                        NA's 
                         390 


patient.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       291         92         60 

patient.primary_lymph_node_presentation_assessment: 
  no  yes NA's 
  26  396   21 

patient.primary_therapy_outcome_success: 
complete remission/response  partial remission/response 
                        145                           2 
        progressive disease              stable disease 
                         43                          10 
                       NA's 
                        243 

patient.prior_dx: 
                              no                              yes 
                             429                                8 
yes, history of prior malignancy 
                               6 

patient.race: 
                                    asian 
                                       89 
                black or african american 
                                       13 
native hawaiian or other pacific islander 
                                        1 
                                    white 
                                      278 
                                     NA's 
                                       62 

patient.radiation_therapy: 
  no  yes NA's 
 166   51  226 

patient.radiations.radiation.2.anatomic_treatment_site: 
primary tumor field       regional site                NA's 
                  3                   1                 439 



patient.radiations.radiation.2.day_of_form_completion: 
   8   14   17   19 NA's 
   1    1    1    2  438 

patient.radiations.radiation.2.days_to_radiation_therapy_end: 
 141  147  215 NA's 
   1    1    1  440 

patient.radiations.radiation.2.days_to_radiation_therapy_start: 
 103  113  179 NA's 
   1    1    1  440 

patient.radiations.radiation.2.measure_of_response: 
complete response              NA's 
                1               442 

patient.radiations.radiation.2.month_of_form_completion: 
   3    5   11 NA's 
   2    1    2  438 

patient.radiations.radiation.2.numfractions: 
  25   28 NA's 
   3    1  439 

patient.radiations.radiation.2.radiation_dosage: 
4500 NA's 
   3  440 

patient.radiations.radiation.2.radiation_treatment_ongoing: 
  no NA's 
   4  439 

patient.radiations.radiation.2.radiation_type: 
     external external beam          NA's 
            3             1           439 

patient.radiations.radiation.2.regimen_indication: 
adjuvant     NA's 
       1      442 

patient.radiations.radiation.2.units: 
 cgy NA's 
   3  440 

patient.radiations.radiation.2.year_of_form_completion: 
2011 2013 2014 2015 NA's 
   1    1    1    2  438 

patient.radiations.radiation.anatomic_treatment_site: 
 distant recurrence        distant site primary tumor field       regional site 
                  2                   4                  54                  15 
               NA's 
                368 



patient.radiations.radiation.course_number: 
   1 NA's 
   3  440 

patient.radiations.radiation.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   14.00   16.27   24.00   31.00     366 

patient.radiations.radiation.days_to_radiation_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   53.0   115.0   156.0   197.8   222.0   756.0     370 

patient.radiations.radiation.days_to_radiation_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0    91.5   122.5   163.0   180.0   737.0     371 

patient.radiations.radiation.measure_of_response: 
               complete response                 partial response 
                              39                                2 
radiographic progressive disease                   stable disease 
                              12                                3 
                            NA's 
                             387 

patient.radiations.radiation.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.117   9.000  12.000     366 

patient.radiations.radiation.numfractions: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   25.00   25.00   22.67   25.00   30.00     370 

patient.radiations.radiation.radiation_dosage: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     12      45    4320    2489    4500    6000     372 

patient.radiations.radiation.radiation_treatment_ongoing: 
  no NA's 
  77  366 

patient.radiations.radiation.radiation_type: 
     external external beam      systemic          NA's 
           67             8             1           367 

patient.radiations.radiation.regimen_indication: 
   adjuvant  palliative progression        NA's 
          6           1           1         435 

patient.radiations.radiation.units: 
 cgy   gy NA's 
  40   30  373 

patient.radiations.radiation.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2014    2014    2014    2015     366 

patient.reflux_history: 
  no  yes NA's 
 212   46  185 

patient.residual_tumor: 
  r0   r1   r2   rx NA's 
 350   18   19   25   31 


patient.stage_event.system_version: 
4th 5th 6th 7th 
  1  22 129 291 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m: 
 m0  m1  mx 
391  30  22 




patient.targeted_molecular_therapy: 
  no  yes NA's 
 117  102  224 

patient.tissue_prospective_collection_indicator: 
 no yes 
212 231 

patient.tissue_retrospective_collection_indicator: 
  no  yes NA's 
 232  210    1 


patient.tumor_tissue_site: 
stomach 
    443 

patient.vital_status: 
alive  dead 
  356    87 

patient.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2014    2015       2 

patient.year_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1996    2009    2011    2010    2012    2013       1 


patient.microsatellite_instability_test_results.microsatellite_instability_test_result.mononucleotide_and_dinucleotide_marker_panel_analysis_status: 
msi-h msi-l   mss 
   85    63   295 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason: 
 case matched expression control case matched methylation control 
                              41                               38 
             germline comparison                             NA's 
                              15                              349 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2: 
case matched methylation control              germline comparison 
                              15                               32 
                            NA's 
                             396 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3: 
germline comparison                NA's 
                  5                 438 

patient.bcr_canonical_check.bcr_patient_canonical_status: 
       canonical canonical - plus 
             349               94 

patient.biospecimen_cqcf.consent_or_death_status: 
consented  deceased    waiver 
      428        14         1 


patient.biospecimen_cqcf.days_to_pathology_review: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -4.0    65.0   163.0   565.7   720.0  5421.0       6 

patient.biospecimen_cqcf.days_to_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-369.00    0.00    2.00   16.51   27.00  425.00       6 

patient.biospecimen_cqcf.digital_image_submitted: 
  no  yes NA's 
 270   75   98 

patient.biospecimen_cqcf.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     3                     63                    377 



patient.biospecimen_cqcf.history_of_neoadjuvant_treatment: 
 no 
443 

patient.biospecimen_cqcf.method_of_sample_procurement: 
     endoscopic biopsy other (please specify) other method (specify) 
                     8                      1                     43 
    surgical resection 
                   391 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -6.00    0.00    0.00   19.54   12.00  552.00     320 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement: 
            blood draw other method (specify)     surgical resection 
                   107                      3                     13 
                  NA's 
                   320 


patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
           18           425 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number: 
   1    2    3 NA's 
  69   52    2  320 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid: 
22e3537f-1528-4519-af86-edab8d09efef 53a95d72-9090-445d-a0c6-68cd93a6d2a0 
                                   1                                    1 
5c1dd63d-253f-4c93-b630-d82af9ae87cb 74df96b9-4fd0-40e2-9cf1-e09d45ab97bd 
                                   1                                    1 
                                NA's 
                                 439 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement: 
   0   31 NA's 
   2    1  440 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement: 
other method (specify)                   NA's 
                     3                    440 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number: 
   2    3 NA's 
   1    3  439 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -369.0     0.0     0.0    20.5    22.0  1124.0       6 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0600   0.1610   0.2730   6.3151   0.5965 179.0000      413 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   15.00   15.00   17.07   15.00   62.70     413 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  16.20   25.41   60.00   93.75  110.75  500.00     413 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement: 
            blood draw other method (specify)     surgical resection 
                   318                     17                    108 


patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
           91           352 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number: 
  1   2 
439   4 

patient.biospecimen_cqcf.other_method_of_sample_procurement: 
                 bag billroth operation 1          gastrectomy 
                   1                    1                   39 
   partial resection                 NA's 
                   3                  399 

patient.biospecimen_cqcf.other_vessel_used: 
                    bag biospecimen storage bag                    NA's 
                      8                       2                     433 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching: 
  no  yes NA's 
  36  365   42 

patient.biospecimen_cqcf.path_confirm_report_attached: 
 no yes 
 22 421 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics: 
 yes NA's 
 401   42 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics: 
  no  yes NA's 
   1  400   42 

patient.biospecimen_cqcf.race: 
                                    asian 
                                       11 
                black or african american 
                                        9 
native hawaiian or other pacific islander 
                                        1 
                                    white 
                                      115 
                                     NA's 
                                      307 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching: 
                                    macrodissection performed at tss to select for a region containing an acceptable tcga diagnosis 
                                                                                                                                  5 
  pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                 28 
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  6 
                                                                                                                               NA's 
                                                                                                                                404 

patient.biospecimen_cqcf.sample_prescreened: 
  no  yes NA's 
   1  436    6 

patient.biospecimen_cqcf.site_of_disease: 
  other stomach 
      4     439 


patient.biospecimen_cqcf.site_of_disease_text: 
   gastric stump         incisura lesser curvature             NA's 
               2                1                1              439 

patient.biospecimen_cqcf.submitted_for_lce: 
  no  yes NA's 
 190   12  241 

patient.biospecimen_cqcf.top_slide_submitted: 
  no  yes NA's 
 113  232   98 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent: 
   0    5   10 NA's 
   4    3    1  435 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent: 
  55   70   75   80 NA's 
   1    1    1    5  435 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight: 
   5   20 NA's 
   7    1  435 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number: 
   2 NA's 
   8  435 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   4.587   5.000  30.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  20.00   70.00   75.00   75.76   90.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    5.0   125.5   300.0   558.3   908.5  2775.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number: 
  1 
443 

patient.biospecimen_cqcf.vessel_used: 
biospecimen storage bag                cassette                cryomold 
                     37                     135                      13 
               cryovial  other (please specify) 
                    248                      10 

patient.days_to_index: 
   0 NA's 
 437    6 



patient.samples.sample.2.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   144.2   465.0   801.6  1069.5  4752.0     239 

patient.samples.sample.2.initial_weight: 
  60   70  160  280 NA's 
   1    1    1    1  439 

patient.samples.sample.2.intermediate_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   0.800   1.200   1.251   1.700   2.300     400 

patient.samples.sample.2.is_ffpe: 
 no 
443 

patient.samples.sample.2.longest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.700   1.050   1.700   1.837   2.500   3.400     400 

patient.samples.sample.2.oct_embedded: 
false  NA's 
  205   238 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio: 
 1.7  1.8 NA's 
   2    8  433 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100503319 100503833 100503854 100505038 100507740 100507813       395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
  10 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
 0.5 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
  16   26 NA's 
  33   15  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   7    8 NA's 
  15   33  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.750   7.000   6.542   9.000  11.000     395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
1149 1177 NA's 
  15   33  395 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
  40 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  22 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
  80 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  48  395 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.136e+08 1.323e+08 1.940e+08 1.641e+08 1.054e+09       121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   8   13 NA's 
 312   10  121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
 0.1 0.15 0.16  0.5 NA's 
   1    6    3  312  121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   16.00   18.23   26.00   30.00     121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 322  121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.093   7.000  10.000     121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   6.615   9.000  12.000     121 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   40.00   40.00   38.86   40.00   40.00     121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  22   23 NA's 
 145  177  121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  20 33.3   80 NA's 
   8    2  312  121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2014     121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    88.0   175.0   136.2   188.0   249.8     298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna                 NA's 
                 312                   10                  121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    w NA's 
  10  312  121 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration: 
 0.1 0.15 0.16  0.5 NA's 
   1    6    3  135  298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
 312  131 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
  no  yes NA's 
 135  177  131 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             322 
                                            NA's 
                                             121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
 322  121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                   repli-g                 repli-g x 
                       10                       283                        29 
                     NA's 
                      121 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.120   1.245   1.350   1.360   1.438   1.710     433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.175   7.950   7.810   8.300   8.700     433 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method: 
picogreen   uv spec      NA's 
       66        79       298 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence: 
   1 NA's 
   8  435 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100502454 100502493 100502516 100505128 100510912 100510981       395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id: 
  10 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration: 
 0.5 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment: 
  16   26 NA's 
  33   15  395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment: 
   7    8 NA's 
  15   33  395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.750   7.000   6.542   9.000  11.000     395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id: 
1150 1178 NA's 
  15   33  395 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity: 
  40 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center: 
  22 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume: 
  80 NA's 
  48  395 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  48  395 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.005e+08 1.005e+08 1.345e+08 1.005e+08 1.054e+09       385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8 NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
  11   16   26   29 NA's 
   8   33   15    2  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   4    6    7    8 NA's 
   8    2   15   33  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   6.586   8.000  11.000     385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id: 
1146 1176 1663 1859 NA's 
  15   33    8    2  385 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  22 NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2010 2011 NA's 
  48   10  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   55.00   63.00   78.25  195.00     385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type: 
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                    10                     48                    385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id: 
   w    x NA's 
  10   48  385 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration: 
 0.5 NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
  no NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              58 
                                            NA's 
                                             385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
  repli-g repli-g x      NA's 
       10        48       385 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method: 
picogreen   uv spec      NA's 
        2        56       385 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio: 
 1.8  1.9 NA's 
   4  114  325 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
9.889e+07 1.323e+08 1.604e+08 4.073e+08 1.062e+09 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  1   2   8  10 
 99   2 327  15 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05000 0.05000 0.08000 0.08858 0.13500 0.17000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   16.00   19.00   19.32   28.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    5.00    7.00    7.05    9.00   12.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   6.000   6.747   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.130   3.000   2.507   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 22  23 
238 205 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
6.67  6.7 13.3   40   60 
  66   48    2  205  122 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2010 2011 2012 2013 2014 
  48   87  173   35  100 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 1.005e+08 1.070e+08 2.253e+08 1.498e+08 1.054e+09       309 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   1    2    5    8 NA's 
   4   15   10  105  309 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.05000 0.07000 0.09201 0.15000 0.16000     309 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   10.00   16.00   16.69   21.00   28.00     309 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
 134  309 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   6.000   5.336   7.000   7.000     309 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   6.000   6.425   8.000  12.000     309 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.525   2.000   2.188   3.000   4.270     309 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  22   23 NA's 
 116   18  309 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   26.70   32.22   60.00   60.00     309 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2010 2011 2014 NA's 
  48   68   18  309 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 98884987 100513164 100518242 114833669 107039236 167811806       361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   5    8    9   10 NA's 
  30   27    2   23  361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1300  0.1500  0.1328  0.1500  0.1700     361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   16.00   21.00   20.28   28.00   28.00     361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
  82  361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   5.256   7.000   7.000     361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   6.463   8.000  12.000     361 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.070   1.400   2.166   3.740   4.540     361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  22   23 NA's 
  65   17  361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70    6.70   13.40   16.78   26.70   60.00     361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2010 2011 2014 2015 NA's 
  48   17   15    2  361 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 98887545  99295325 153880914 133789522 164052746 167811819       385 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.750   8.000   8.103   9.750  26.000     385 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1300  0.1143  0.1500  0.1600     385 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   10.00   18.50   17.69   21.00   30.00     385 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
  58  385 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   7.000   4.845   7.000   9.000     385 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   7.000   7.207   8.750  12.000     385 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.070   1.093   1.070   1.600     385 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  22   23 NA's 
  25   33  385 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
 6.7   10 13.4   20 NA's 
  25    5   25    3  385 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2010 2013 2014 2015 NA's 
  25    8   24    1  385 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132267447 164052680 164052704 162240288 164052728 167811825       410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id: 
   8    9 NA's 
  32    1  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration: 
0.07 0.08 NA's 
  11   22  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment: 
  10   21   30 NA's 
  30    1    2  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe: 
  no NA's 
  33  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment: 
   1    2    4 NA's 
   1   30    2  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.697   6.000  11.000     410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id: 
a290 a36q a46a NA's 
   2   30    1  410 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity: 
0.94 1.07  1.4  1.6 NA's 
  10   21    1    1  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center: 
  23 NA's 
  33  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume: 
13.4   20 NA's 
  31    2  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment: 
2013 2014 2015 NA's 
   2   30    1  410 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode: 
tcga-br-4255-11a-01d-a36q-08 tcga-br-4280-11a-01d-a36q-08 
                           1                            1 
                        NA's 
                         441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid: 
059acfa0-2ec6-48a4-ae11-b57da231c6a9 07af57b2-4277-4314-9470-6f347320f0f6 
                                   1                                    1 
                                NA's 
                                 441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom: 
164052673 164052747      NA's 
        1         1       441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id: 
   8 NA's 
   2  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration: 
0.07 0.08 NA's 
   1    1  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment: 
  10 NA's 
   2  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe: 
  no NA's 
   2  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment: 
   2 NA's 
   2  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column: 
   2    4 NA's 
   1    1  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id: 
a36q NA's 
   2  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row: 
   b    h NA's 
   1    1  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity: 
0.94 1.07 NA's 
   1    1  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center: 
  23 NA's 
   2  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume: 
13.4 NA's 
   2  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment: 
2014 NA's 
   2  441 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
9.889e+07 1.323e+08 1.604e+08 4.073e+08 1.062e+09 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  1   2   8 
329  99  15 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.13 0.14 0.15 0.16 0.17 
  21   94  163  128   37 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    6.0    16.0    19.0    19.4    28.0    30.0 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   7.059   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.765   9.500  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.000   1.070   1.237   1.300   2.260 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 22  23 
238 205 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.67  6.7   10 13.3 
 122  207   31   83 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2010 2011 2012 2013 2014 
  50   85  173   35  100 

patient.samples.sample.2.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   20.34   66.00  132.24  201.00 1646.64 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type: 
dna 
443 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id: 
  d 
443 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration: 
0.13 0.14 0.15 0.16 0.17 
  21   94  163  128   37 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
443 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 no yes 
238 205 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             443 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
            adna preparation type     chemical lysis dna extraction 
                              221                               192 
pre-extracted dna received by tss 
                               30 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen   uv spec 
      325       118 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence: 
   1 NA's 
   8  435 



patient.samples.sample.2.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   16.00   16.41   23.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe: 
 no 
443 

patient.samples.sample.2.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.424  10.000  12.000 

patient.samples.sample.2.portions.portion.portion_number: 
  1   2  11  21 
433   2   4   4 

patient.samples.sample.2.portions.portion.portion_sequence: 
   1    2 NA's 
 130    4  309 




patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe: 
  no NA's 
  25  418 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location: 
 top NA's 
  25  418 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
  47  396 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   2   10   25 NA's 
   2    1    1  439 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration: 
   0    1    3 NA's 
   1    2    1  439 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis: 
   0 NA's 
   4  439 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0 NA's 
   4  439 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells: 
  60  100 NA's 
   1    3  439 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells: 
   0   40 NA's 
   3    1  439 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells: 
   0 NA's 
   4  439 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei: 
   0 NA's 
   4  439 

patient.samples.sample.2.portions.portion.slides.slide.section_location: 
bottom    top   NA's 
    25     22    396 

patient.samples.sample.2.portions.portion.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   2.000   9.548   2.000  73.000     222 

patient.samples.sample.2.portions.portion.year_of_creation: 
2010 2011 2012 2013 2014 
  44   89  175   81   54 

patient.samples.sample.2.sample_type: 
blood derived normal  solid tissue normal 
                 396                   47 

patient.samples.sample.2.sample_type_id: 
 10  11 
396  47 

patient.samples.sample.2.shortest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.4000  0.4000  0.4535  0.5000  0.7000     400 

patient.samples.sample.2.vial_number: 
  a   b   e 
437   5   1 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
germline comparison                NA's 
                  1                 442 



patient.samples.sample.3.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   96.0   152.0   189.0   278.1   413.0   547.0     430 

patient.samples.sample.3.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   70.0   110.0   140.0   225.4   300.0   710.0     430 

patient.samples.sample.3.intermediate_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   0.600   1.000   1.002   1.300   2.200     402 

patient.samples.sample.3.is_ffpe: 
  no NA's 
  54  389 

patient.samples.sample.3.longest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.000   1.500   1.695   2.300   3.000     402 

patient.samples.sample.3.oct_embedded: 
false  true  NA's 
   10     3   430 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   1.770   1.780   1.784   1.800   1.830     428 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched expression control                            NA's 
                              1                             442 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
  13 NA's 
  10  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.15 0.16 0.17 0.18 NA's 
   1    4    4    1  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
  12   19   30 NA's 
   4    5    1  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  10  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   4   11   12 NA's 
   1    4    5  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   1    5    6    7 NA's 
   1    4    4    1  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
a24l a252 a39f NA's 
   4    5    1  433 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   0 NA's 
  10  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  23 NA's 
  10  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
   0 NA's 
  10  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2012 2014 NA's 
   9    1  433 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode: 
tcga-hu-a4g9-11a-11r-a360-13 tcga-hu-a4gh-11a-11r-a360-13 
                           1                            1 
tcga-hu-a4gx-11a-11r-a360-13 tcga-hu-a4gy-11a-11r-a360-13 
                           1                            1 
                        NA's 
                         439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid: 
336f8831-2116-4b52-9839-9c7623a93fb1 58c2eeac-b6fd-4030-84a8-8616d34d6341 
                                   1                                    1 
ec4c67ad-24af-4b3b-9303-6253716287a5 f65a70ff-2211-4183-a4be-34a6de42b661 
                                   1                                    1 
                                NA's 
                                 439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id: 
  13 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration: 
0.16 0.17 NA's 
   1    3  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment: 
  29 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment: 
   1 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column: 
   1    7    8 NA's 
   1    2    1  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id: 
a360 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row: 
   d    f    g    h NA's 
   1    1    1    1  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity: 
   0 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center: 
  23 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume: 
   0 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment: 
2014 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode: 
tcga-hu-a4g9-11a-11r-a36d-31 tcga-hu-a4gh-11a-11r-a36d-31 
                           1                            1 
tcga-hu-a4gx-11a-11r-a36d-31 tcga-hu-a4gy-11a-11r-a36d-31 
                           1                            1 
                        NA's 
                         439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid: 
0b95f563-7c9c-4c77-8f45-266c754d2f10 3b981e83-f0b5-449a-b0d7-2da0cc5b1f35 
                                   1                                    1 
a3c90fa9-6541-45cb-81ed-056797aceeb1 d26dcb61-dd83-487d-b67f-665ec2631f18 
                                   1                                    1 
                                NA's 
                                 439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom: 
164082144 164082161 164082175 164082198      NA's 
        1         1         1         1       439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id: 
  31 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration: 
0.16 0.17 NA's 
   1    3  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment: 
  29 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment: 
   1 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column: 
   7    8    9 NA's 
   1    2    1  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id: 
a36d NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row: 
   a    d    e    h NA's 
   1    1    1    1  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity: 
2.08 3.96 NA's 
   1    3  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center: 
  23 NA's 
   4  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume: 
  13 23.3 NA's 
   1    3  439 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment: 
2014 NA's 
   4  439 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched expression control                            NA's 
                              1                             442 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.244e+08 1.323e+08 5.012e+08 1.062e+09 1.076e+09       398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   8   13   31 NA's 
  16   19   10  398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.2764  0.5000  0.5000     398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   18.00   15.49   19.00   30.00     398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  45  398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   7.000   7.978  12.000  12.000     398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.444   7.000  12.000     398 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.33    5.00   17.02   40.00   40.00     398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  22   23 NA's 
  30   15  398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  20 33.3   80 NA's 
  10   19   16  398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2011 2012 2013 2014 NA's 
  21   18    5    1  398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.01   36.93  177.60  163.03  196.65  694.20     403 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna                 NA's 
                  16                   29                  398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    w NA's 
  29   16  398 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.2485  0.5000  0.5000     403 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
  16  427 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
  no  yes NA's 
  11    5  427 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              45 
                                            NA's 
                                             398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
  45  398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                   repli-g                 repli-g x 
                       29                        12                         4 
                     NA's 
                      398 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.06    1.34    1.46    1.45    1.60    1.89     414 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.100   7.800   8.300   8.297   8.800   9.600     414 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method: 
picogreen   uv spec      NA's 
       10        30       403 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2 NA's 
  10    1  432 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.210e+08 1.283e+08 2.011e+08 1.456e+08 1.054e+09       417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8 NA's 
  26  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
  26  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   25.00   29.00   23.77   30.00   30.00     417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  26  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   3.115   4.500   8.000     417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   5.000   7.346  12.000  12.000     417 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
  26  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  22   23 NA's 
  15   11  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
  26  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2011 2012 2013 2014 NA's 
   5   10   10    1  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  158.0   171.5   176.0   176.7   183.5   202.0     428 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  26                  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
  26  417 



patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration: 
 0.5 NA's 
  15  428 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
  26  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
  no  yes NA's 
  15   11  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              26 
                                            NA's 
                                             417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
  26  417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
  repli-g repli-g x      NA's 
       25         1       417 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method: 
picogreen      NA's 
       15       428 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2 NA's 
   9    1  433 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio: 
 1.8 1.83  1.9 1.98 NA's 
   1    1   12    1  428 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched expression control                            NA's 
                              2                             441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.323e+08 1.054e+09 6.591e+08 1.076e+09 1.076e+09       390 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
   1    8   13   31 NA's 
  13   39    1    1  389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0839  0.1400  0.1700     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    7.75   17.00   14.30   19.00   30.00     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  54  389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   7.000   7.000   8.241  12.000  12.000     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.250   9.000   7.222   9.750  12.000     389 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.548   3.000   2.494   3.000   3.600     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  22   23 NA's 
  41   13  389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   10.00   60.00   41.24   60.00   60.00     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2011 2012 2013 2014 NA's 
  21   30    1    2  389 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.070e+08 1.054e+09 6.169e+08 1.054e+09 1.054e+09       430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   8 NA's 
  13  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
0.05 NA's 
  13  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   6   13   16 NA's 
   5    7    1  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
  13  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   3    6    7 NA's 
   1    5    7  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   3    4    8 NA's 
   7    5    1  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id: 
1600 1800 1882 NA's 
   1    5    7  430 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   2    3 NA's 
   1   12  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  22 NA's 
  13  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
  40   60 NA's 
   1   12  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2011 NA's 
  13  430 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.070e+08 1.054e+09 6.169e+08 1.054e+09 1.054e+09       430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   5 NA's 
  13  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
0.14 0.15 0.16 0.17 NA's 
   5    6    1    1  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
   6   13   16 NA's 
   5    7    1  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
  13  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   3    6    7 NA's 
   1    5    7  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   3    4    6 NA's 
   7    5    1  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id: 
1601 1801 1883 NA's 
   1    5    7  430 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
1.86    2 2.13 4.54 NA's 
   5    6    1    1  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  22 NA's 
  13  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
13.3 26.7 NA's 
  12    1  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2011 NA's 
  13  430 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode: 
tcga-br-6452-11a-01d-2365-05 tcga-br-6456-11a-01d-a46a-09 
                           1                            1 
                        NA's 
                         441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid: 
56a9b06f-05f5-4895-8af8-3c2feb5976d5 67f41aca-706d-4b95-90b9-36d7559b6bd3 
                                   1                                    1 
                                NA's 
                                 441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
 167811788 1076438132       NA's 
         1          1        441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
   5    9 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
0.08 0.15 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
  21   29 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
   2  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
   1    8 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
   2    4 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id: 
2365 a46a NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row: 
   a    e NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
1.07    6 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  22   23 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
13.4   40 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2012 2015 NA's 
   1    1  441 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched expression control                            NA's 
                              2                             441 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.070e+08 1.323e+08 1.054e+09 6.594e+08 1.076e+09 1.076e+09       390 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   1    2   13   31 NA's 
  39   13    1    1  389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1485  0.1575  0.1700     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    7.75   17.00   14.30   19.00   30.00     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  54  389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   7.000   7.000   8.241  12.000  12.000     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   9.000   7.204   9.750  12.000     389 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.940   1.000   1.189   1.335   3.400     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  22   23 NA's 
  41   13  389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   6.670   6.670   7.968   9.175  20.000     389 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2011 2012 2013 2014 NA's 
  21   30    1    2  389 

patient.samples.sample.3.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.24   32.91  186.35  198.33  306.25  800.70     389 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type: 
 dna  rna NA's 
  52    2  389 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id: 
   d    r NA's 
  52    2  389 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1485  0.1575  0.1700     389 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  52  391 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
  no  yes NA's 
  41   11  391 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              54 
                                            NA's 
                                             389 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  54  389 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
    adna preparation type mirvana (allprep dna) rna                      NA's 
                       52                         2                       389 

patient.samples.sample.3.portions.portion.analytes.analyte.rna.ratio_28s_18s: 
1.08 1.58 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.rna.rinvalue: 
   7  8.8 NA's 
   1    1  441 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen   uv spec      NA's 
       39        15       389 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence: 
   1    2 NA's 
  12    1  430 



patient.samples.sample.3.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   14.50   15.69   24.00   31.00     389 

patient.samples.sample.3.portions.portion.is_ffpe: 
  no NA's 
  54  389 

patient.samples.sample.3.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   7.056  10.750  12.000     389 

patient.samples.sample.3.portions.portion.portion_number: 
   1   11   12   21 NA's 
  41   10    1    2  389 

patient.samples.sample.3.portions.portion.portion_sequence: 
   1    2 NA's 
  10    1  432 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_barcode: 
tcga-br-6710-11a-01-ts1                    NA's 
                      1                     442 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_uuid: 
077944a5-ff87-4652-a929-dc4178d4f044                                 NA's 
                                   1                                  442 

patient.samples.sample.3.portions.portion.slides.slide.2.image_file_name: 
tcga-br-6710-11a-01-ts1.077944a5-ff87-4652-a929-dc4178d4f044.svs 
                                                               1 
                                                            NA's 
                                                             442 

patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe: 
  no NA's 
   1  442 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location: 
 top NA's 
   1  442 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
  54  389 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    3.50   11.68   18.75   50.00     421 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration: 
   0    1    2   20 NA's 
  15    4    1    1  422 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis: 
   0    1 NA's 
  12    1  430 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.191   1.000  20.000     422 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   15.00  100.00   66.77  100.00  100.00     430 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   33.15   85.00  100.00     430 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells: 
   0 NA's 
  13  430 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei: 
   0 NA's 
  13  430 

patient.samples.sample.3.portions.portion.slides.slide.section_location: 
bottom    top   NA's 
     1     53    389 

patient.samples.sample.3.portions.portion.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   30.00   59.00   50.73   68.00   77.00     394 

patient.samples.sample.3.portions.portion.year_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2014     389 

patient.samples.sample.3.sample_type: 
solid tissue normal                NA's 
                 54                 389 

patient.samples.sample.3.sample_type_id: 
  11 NA's 
  54  389 

patient.samples.sample.3.shortest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.4000  0.5000  0.5049  0.6000  1.0000     402 

patient.samples.sample.3.vial_number: 
   a NA's 
  54  389 



patient.samples.sample.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   144.2   465.0   797.5  1046.5  4752.0     239 



patient.samples.sample.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    90.0   160.0   299.1   320.0  2420.0     238 

patient.samples.sample.intermediate_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.200   0.700   1.000   1.096   1.500   2.500     211 

patient.samples.sample.is_ffpe: 
 no 
443 

patient.samples.sample.longest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.30    1.10    1.70    1.75    2.40    3.50     211 

patient.samples.sample.oct_embedded: 
false  true  NA's 
  128    77   238 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   1.800   1.820   1.824   1.870   2.020     114 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
160339596 160340725 160348968 160348718 160356924 160356949       423 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
  13   31 NA's 
 188   20  235 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1600  0.1700  0.1671  0.1700  0.1800     235 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   19.00   25.00   23.75   28.00   30.00     235 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
 208  235 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   7.673  12.000  12.000     235 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.524   5.000   8.000     235 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3269  0.0000  3.6000     235 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  23 NA's 
 208  235 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
   0   20 NA's 
 188   20  235 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2012 2013 2014 NA's 
  70   45   93  235 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom: 
164079928 164082150 164082232      NA's 
        1         1         1       440 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id: 
  13   31 NA's 
   7    3  433 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1525  0.1650  0.1640  0.1775  0.1800     433 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment: 
  29 NA's 
  10  433 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
  10  433 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment: 
   1 NA's 
  10  433 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column: 
   1    2    8 NA's 
   4    3    3  433 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id: 
a360 a36d NA's 
   7    3  433 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity: 
   0 3.26 3.96 NA's 
   7    1    2  433 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center: 
  23 NA's 
  10  433 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume: 
   0 23.3 NA's 
   7    3  433 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment: 
2014 NA's 
  10  433 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
164082167 164082180 164082214 164082205 164082228 164082239       436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id: 
  31 NA's 
   7  436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration: 
0.15 0.16 0.18 NA's 
   2    2    3  436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment: 
  29 NA's 
   7  436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
   7  436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment: 
   1 NA's 
   7  436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column: 
   7    8 NA's 
   4    3  436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id: 
a36d NA's 
   7  436 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity: 
 3.5 3.73 4.19 NA's 
   2    2    3  436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center: 
  23 NA's 
   7  436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume: 
23.3 NA's 
   7  436 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment: 
2014 NA's 
   7  436 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 1.244e+08 1.641e+08 4.189e+08 1.062e+09 1.076e+09        20 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
 13  31 
258 185 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1581  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   16.00   19.00   19.81   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   7.298   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.237   5.000   8.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   3.200   3.600   4.172   5.000   7.940 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
 22  23 
238 205 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
   0   20 33.3 41.6 46.7 
  20  214  161    1   47 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2010 2011 2012 2013 2014 
  48   87  173   45   90 

patient.samples.sample.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.37   44.98   76.10  142.33  219.20  765.70 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type: 
rna 
443 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id: 
  r 
443 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1581  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             443 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna 
                      443 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.450   1.620   1.611   1.800   2.500 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.500   8.300   8.297   9.000  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec 
    443 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2    3 NA's 
 323   51    4   65 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100503323 100503827 100503852 100507388 100515324 100515380       395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id: 
  10 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration: 
 0.5 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment: 
  16   26 NA's 
  33   15  395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment: 
   7    8 NA's 
  15   33  395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.458   4.250  11.000     395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id: 
1147 1177 NA's 
  15   33  395 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity: 
  40 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center: 
  22 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume: 
  80 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  48  395 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.136e+08 1.323e+08 1.999e+08 1.641e+08 1.054e+09       121 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8 NA's 
 322  121 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
 322  121 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   21.00   18.25   27.50   30.00     121 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 322  121 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.118   7.000  10.000     121 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   5.003   7.000  12.000     121 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
 322  121 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  22   23 NA's 
 145  177  121 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
 322  121 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2013    2014     121 

patient.samples.sample.portions.portion.analytes.analyte.3.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    95.0   165.0   136.5   181.0   208.0     298 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                 322                  121 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
 322  121 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration: 
 0.5 NA's 
 145  298 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
 322  121 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
  no  yes NA's 
 145  177  121 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             322 
                                            NA's 
                                             121 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
 322  121 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
  repli-g repli-g x      NA's 
      293        29       121 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method: 
picogreen   uv spec      NA's 
       68        77       298 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2    3 NA's 
 216   40    2  185 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100502459 100502485 100502510 100504354 100508421 100508485       395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id: 
  10 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration: 
 0.5 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment: 
  16   26 NA's 
  33   15  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment: 
   7    8 NA's 
  15   33  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.458   4.250  11.000     395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id: 
1148 1178 NA's 
  15   33  395 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity: 
  40 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center: 
  22 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume: 
  80 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  48  395 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100500563 100502195 100502222 100507180 100518396 100518468       395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id: 
   8 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration: 
 0.5 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment: 
  16   26 NA's 
  33   15  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment: 
   7    8 NA's 
  15   33  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.458   4.250  11.000     395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id: 
1144 1176 NA's 
  15   33  395 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity: 
  40 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center: 
  22 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume: 
  80 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment: 
2010 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   57.50   42.58   65.25   98.00     395 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type: 
repli-g x (qiagen) dna                   NA's 
                    48                    395 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id: 
   x NA's 
  48  395 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration: 
 0.5 NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match: 
 yes NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful: 
  no NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              48 
                                            NA's 
                                             395 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe: 
  no NA's 
  48  395 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type: 
repli-g x      NA's 
       48       395 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method: 
uv spec    NA's 
     48     395 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   1.900   1.900   1.901   1.900   2.000     326 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
9.889e+07 1.323e+08 1.604e+08 4.072e+08 1.062e+09 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  1   2   8  10 
363  65   2  13 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
0.13 0.14 0.15 0.16 0.17 
  11   92  193  102   45 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   16.00   19.00   19.32   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    5.00    7.00    7.05    9.00   12.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    2.00    5.00    4.91    7.00   12.00 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.000   1.010   1.146   1.070   2.260 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 22  23 
238 205 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
6.67  6.7   10 13.3 
 126  250   18   49 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2010 2011 2012 2013 2014 
  48   87  173   35  100 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
9.889e+07 1.322e+08 1.604e+08 4.072e+08 1.062e+09 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
  1   2   5   8  10 
 65  13  83 280   2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05000 0.05000 0.08000 0.09625 0.15000 0.17000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   16.00   19.00   19.32   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    5.00    7.00    7.05    9.00   12.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   4.914   7.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   2.080   3.000   2.636   3.200   4.270 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
 22  23 
238 205 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   13.00   40.00   35.61   60.00   60.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2010 2011 2012 2013 2014 
  48   87  173   35  100 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
9.889e+07 1.317e+08 1.508e+08 3.454e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
  2   5   8 
  2 294 147 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0500  0.0800  0.1500  0.1286  0.1500  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   16.00   19.00   19.81   28.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   7.298   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.014   7.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.300   1.950   2.000   2.263   2.260   4.270 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
 22  23 
238 205 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  10.00   13.00   13.30   22.19   40.00   60.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2010 2011 2012 2013 2014 
  48   87  173   45   90 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 1.005e+08 1.070e+08 2.269e+08 1.641e+08 1.054e+09       309 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   5.000   6.566   8.500  23.000     307 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1400  0.1361  0.1500  0.1700     309 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   10.00   16.00   16.46   21.00   28.00     309 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
 134  309 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
   2    3    6    7 NA's 
  22   25   23   64  309 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       1       2       3       3      11     309 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.305   1.903   2.000   4.540     309 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  22   23 NA's 
 112   24  307 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70    6.70   13.30   13.78   13.40   26.70     309 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2010 2011 2014 NA's 
  48   64   22  309 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 1.641e+08 1.641e+08 2.062e+08 1.641e+08 1.076e+09       365 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00    8.00   10.05    8.00   26.00     358 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0700  0.0800  0.0878  0.0800  0.1600     365 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   10.00   10.00   13.32   10.00   30.00     365 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe: 
  no NA's 
  78  365 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   3.013   2.000   9.000     365 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.641   6.750  12.000     365 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   0.940   1.070   1.335   1.070   6.400     365 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center: 
  22   23 NA's 
   7   78  358 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.40   13.40   14.63   13.40   40.00     365 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2014    2014    2014    2014    2015     365 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132268908 164052694 164052735 161607864 164052764 164053278       430 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id: 
   8   23 NA's 
  13   33  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration: 
0.07 0.08 NA's 
   7    6  430 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment: 
  10   30 NA's 
  12    1  430 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe: 
  no NA's 
  13  430 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment: 
   2    4 NA's 
  12    1  430 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       2       7       6       9      10     430 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id: 
a290 a36q a36r yyyy NA's 
   1   11    1   33  397 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity: 
0.94 1.07  1.4 NA's 
   6    6    1  430 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center: 
  23 NA's 
  46  397 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume: 
13.4   20 NA's 
  12    1  430 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment: 
2013 2014 NA's 
   1   12  430 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom: 
164052709 167811830      NA's 
        1         1       441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id: 
   8    9   23 NA's 
   1    1    9  432 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration: 
0.07 0.08 NA's 
   1    1  441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment: 
  10   21 NA's 
   1    1  441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe: 
  no NA's 
   2  441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment: 
   1    2 NA's 
   1    1  441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column: 
   2   11 NA's 
   1    1  441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id: 
a36q a46a yyyy NA's 
   1    1    9  432 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row: 
   e    g NA's 
   1    1  441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity: 
0.94 1.07 NA's 
   1    1  441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center: 
  23 NA's 
  11  432 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume: 
13.4 NA's 
   2  441 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment: 
2014 2015 NA's 
   1    1  441 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 98886760  99697442 100507547 100606347 100507596 107032242       392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  1   2   8  23 
  2  36  13 392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.13 0.14 0.15 0.16 NA's 
   5   20   22    4  392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
  16   21   23   28 NA's 
   4   13    2   32  392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  51  392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   3    7    8 NA's 
   4   45    2  392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    3.02    4.00    6.00     392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
1126 1154 1181 1598 yyyy 
  13   32    2    4  392 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.550   1.860   1.768   2.000   2.130     392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 22  23 
 51 392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
 6.7   10 13.3 NA's 
   2   13   36  392 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2010 2011 NA's 
  47    4  392 

patient.samples.sample.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   30.38   62.05  137.21  238.35  583.70 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type: 
dna 
443 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id: 
  d 
443 



patient.samples.sample.portions.portion.analytes.analyte.concentration: 
0.13 0.14 0.15 0.16 0.17 
  11   92  193  102   45 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
443 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 no yes 
238 205 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             443 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
adna preparation type 
                  443 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen   uv spec 
      326       117 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence: 
   1    2    3 NA's 
 323   51    4   65 



patient.samples.sample.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    9.00   14.00   14.89   21.00   31.00 

patient.samples.sample.portions.portion.is_ffpe: 
 no 
443 

patient.samples.sample.portions.portion.lce: 
 lce NA's 
   1  442 

patient.samples.sample.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.546  10.000  12.000 

patient.samples.sample.portions.portion.portion_number: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   11.00   11.48   11.00   91.00 

patient.samples.sample.portions.portion.portion_sequence: 
   1    2    3    9 NA's 
 108   25    7    1  302 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe: 
  no NA's 
 201  242 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00   10.00   10.82   15.00   20.00     392 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.178   2.000  10.000     398 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    2.98    5.00   20.00     242 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   5.000   5.553   9.000  35.000     396 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   4.849   5.000  57.000     244 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   14.73   20.00   70.00     245 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   70.00   80.00   77.58   88.00  100.00     244 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0      70      75      74      80      90     242 

patient.samples.sample.portions.portion.slides.slide.2.section_location: 
bottom    top   NA's 
     3    198    242 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe: 
 no 
443 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    5.00   10.91   15.00  100.00     175 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.726   2.000  20.000     184 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.604   4.750  20.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.500   4.084   5.000 100.000     181 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   5.715   8.000  95.000       8 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   15.00   16.47   24.50   84.00       8 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   65.00   80.00   75.18   90.00  100.00       8 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   65.00   75.00   72.15   80.00  100.00       1 

patient.samples.sample.portions.portion.slides.slide.section_location: 
bottom    top 
   198    245 

patient.samples.sample.portions.portion.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    30.0    30.0    31.8    35.0   107.0       6 

patient.samples.sample.portions.portion.year_of_creation: 
2010 2011 2012 2013 2014 
  50   87  171   89   46 


patient.samples.sample.portions.shipment_portion.center_id: 
  20 NA's 
 357   86 

patient.samples.sample.portions.shipment_portion.is_ffpe: 
  no NA's 
 357   86 


patient.samples.sample.portions.shipment_portion.portion_number: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   20.64   21.00   81.00      86 

patient.samples.sample.portions.shipment_portion.portion_sequence: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    2.00    1.97    2.00    8.00     138 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment: 
   1   24 NA's 
 264   93   86 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment: 
   4    6 NA's 
 264   93   86 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment: 
2013 2014 NA's 
 264   93   86 

patient.samples.sample.sample_type: 
primary tumor 
          443 

patient.samples.sample.sample_type_id: 
  1 
443 

patient.samples.sample.shortest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.4000  0.5000  0.4509  0.5000  1.0000     211 

patient.samples.sample.vial_number: 
  a   b 
441   2 

Lauren.Class: 
   Diffuse Intestinal      Mixed       NA's 
        69        196         19        159 

Intestinal.Type.Subclass: 
 Mucinous Papillary   Tubular      NA's 
       18        22       140       263 

Signet.Ring: 
   0    1 NA's 
  47   22  374 




Pathologic.M: 
  M0   M1   MX NA's 
 273   20    2  148 


Anatomic.Region: 
     ANTRUM FUNDUS_BODY  GEJ_CARDIA        NA's 
        114         116          57         156 

Residual.Tumor: 
  R0   R1   R2   RX NA's 
 232    9   10   25  167 

Age.at.Initial.Diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  34.50   58.65   67.66   66.43   74.05   90.00     151 


Gender: 
FEMALE   MALE   NA's 
   113    182    148 

Race: 
                    ASIAN BLACK_OR_AFRICAN_AMERICAN                     WHITE 
                       77                         4                       172 
                     NA's 
                      190 

Vital.Status: 
DECEASED   LIVING     NA's 
      57      238      148 

Days.to.Last.Follow.up: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0   143.0   262.6   411.0  3196.0     170 

Days.to.Death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   128.0   279.0   399.5   448.0  2197.0     386 

Recurrence: 
  NO  YES NA's 
 170   31  242 

Days.to.Recurrence: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   46.0   181.5   348.0   387.8   411.5  1949.0     412 

Molecular.Subtype: 
 CIN  EBV   GS  MSI NA's 
 147   26   58   64  148 

Total.Mutation.Rate: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.033   3.250   5.638  16.844  11.529 282.946     154 

Hypermutated: 
   0    1 NA's 
 226   63  154 

MLH1.Epigenetically.Silenced: 
FALSE  TRUE  NA's 
  183    37   223 

CDKN2A.Epigenetically.Silenced: 
FALSE  TRUE  NA's 
  153    67   223 

MSI.status: 
MSI-H MSI-L   MSS  NA's 
   64    44   187   148 

EBV.positive: 
   0    1 NA's 
 269   26  148 

TP53.mutation: 
   0    1 NA's 
 152  137  154 

PIK3CA.mutation: 
   0    1 NA's 
 232   57  154 

KRAS.mutation: 
   0    1 NA's 
 261   28  154 

ARID1A.mutation: 
   0    1 NA's 
 199   90  154 

RHOA.mutation: 
   0    1 NA's 
 273   16  154 

ARHGAP26.ARHGAP6.CLDN18.Rearrangement: 
   0    1 NA's 
 271   13  159 

MET.Skipped.Exon.2: 
   0    1 NA's 
 190   82  171 

MET.Skipped.Exons.18.and.19: 
   0    1 NA's 
 250   22  171 

ABSOLUTE.Ploidy: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.220   2.000   2.120   2.627   3.047   7.770     165 

ABSOLUTE.Purity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1700  0.3700  0.5400  0.5631  0.7300  1.0000     165 

Estimated.Leukocyte.Percentage: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0120  0.1180  0.1990  0.2131  0.2950  0.5690     150 

Percent.Tumor.Nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   70.00   75.00   73.15   80.00  100.00     148 

Percent.Tumor.Cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   65.00   78.00   74.01   88.00  100.00     148 

Percent.Lymphocyte.Infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   3.759   5.000 100.000     148 

Gene.Expression.Cluster: 
  C1   C2   C3   C4 NA's 
  49   59   98   48  189 


Copy.Number.Cluster: 
High  Low NA's 
 159  134  150 

Methylation.Cluster: 
  C1   C2   C3   C4 NA's 
  28   77   73  117  148 

CIMP.Category: 
    GASTRIC-CIMP GASTRIC-EBV-CIMP            OTHER             NA's 
              77               28              190              148 

}}
\keyword{datasets}
