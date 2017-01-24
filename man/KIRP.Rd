\name{ KIRP }
\alias{ KIRP }
\docType{data}
\title{ KIRP }

\details{
\preformatted{
> KIRP
A MultiAssayExperiment object of 8 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 8: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 14 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 323 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 326 columns 
 [4] Methylation: SummarizedExperiment with 485577 rows and 321 columns 
 [5] mRNAArray: ExpressionSet with 17814 rows and 16 columns 
 [6] RPPAArray: ExpressionSet with 195 rows and 216 columns 
 [7] gistica: SummarizedExperiment with 24776 rows and 288 columns 
 [8] gistict: SummarizedExperiment with 24776 rows and 288 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( KIRP )
CharacterList of length 8
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( KIRP )
CharacterList of length 8
[["RNASeqGene"]] TCGA-AL-3466-01A-02R-1351-07 ... TCGA-B9-4617-01A-01R-1193-07
[["RNASeq2GeneNorm"]] TCGA-2K-A9WE-01A-11R-A38C-07 ...
[["miRNASeqGene"]] TCGA-2K-A9WE-01A-11R-A38N-13 ...
[["Methylation"]] TCGA-2K-A9WE-01A-11D-A383-05 ... TCGA-Y8-A8S1-01A-11D-A36Y-05
[["mRNAArray"]] TCGA-AL-3466-01A-01R-1193-07 ... TCGA-B9-4617-01A-01R-1193-07
[["RPPAArray"]] TCGA-2K-A9WE-01A-21-A39N-20 ... TCGA-Y8-A8S1-01A-21-A39P-20
[["gistica"]] TCGA-2K-A9WE-01A-11D-A381-01 ... TCGA-Y8-A8S1-01A-11D-A36W-01
[["gistict"]] TCGA-2K-A9WE-01A-11D-A381-01 ... TCGA-Y8-A8S1-01A-11D-A36W-01

Sizes of each ExperimentList element:

            assay size.Mb
1      RNASeqGene  1.1 Mb
2 RNASeq2GeneNorm  1.2 Mb
3    miRNASeqGene  0.1 Mb
4     Methylation 67.4 Mb
5       mRNAArray    1 Mb
6       RPPAArray  0.1 Mb
7         gistica  4.3 Mb
8         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   247 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  44.00   44.00    1.76    1.35    3.60 


--------------------------- 
Available sample meta-data: 
--------------------------- 


years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  28.00   53.25   61.50   61.52   71.00   88.00       5 

vital_status: 
  0   1 
247  44 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  122.0   341.2   641.0   989.8  1498.5  2941.0     247 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     406     766    1042    1500    5925      33 

tumor_tissue_site: 
kidney 
   291 

pathologic_stage: 
  stage i  stage ii stage iii  stage iv      NA's 
      173        23        52        15        28 


pathology_N_stage: 
  n0   n1   n2   nx NA's 
  50   24    4  212    1 

pathology_M_stage: 
  m0   m1   mx NA's 
  95    9  172   15 

gender: 
female   male 
    77    214 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1996    2008    2011    2010    2012    2013      25 

days_to_last_known_alive: 
  34 NA's 
   1  290 

radiation_therapy: 
  no  yes NA's 
 209    1   81 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00   90.00   87.66  100.00  100.00     214 

histological_type: 
kidney papillary renal cell carcinoma 
                                  291 

number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   15.00   24.50   31.73   41.25  185.00     215 

year_of_tobacco_smoking_onset: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1941    1961    1970    1972    1984    2010     235 

race: 
american indian or alaska native                            asian 
                               2                                6 
       black or african american                            white 
                              61                              207 
                            NA's 
                              15 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    12                    243                     36 

admin.bcr: 
nationwide children's hospital 
                           291 

admin.day_of_dcc_upload: 
  1 
291 

admin.disease_code: 
kirp 
 291 


admin.month_of_dcc_upload: 
 10 
291 

admin.patient_withdrawal.withdrawn: 
false 
  291 

admin.project_code: 
tcga 
 291 

admin.year_of_dcc_upload: 
2015 
 291 

patient.age_at_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  28.00   53.75   61.00   61.48   71.00   88.00       3 


patient.clinical_cqcf.anatomic_neoplasm_subdivision: 
 left right  NA's 
  153   120    18 

patient.clinical_cqcf.consent_or_death_status: 
consented  deceased 
      282         9 

patient.clinical_cqcf.country: 
       canada       germany united states       vietnam          NA's 
            9            17           216             6            43 

patient.clinical_cqcf.days_to_consent: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2296.00   -12.00     0.00    -4.84     4.00  5559.00       34 

patient.clinical_cqcf.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    331     343    1536    1479    2609    2941     282 

patient.clinical_cqcf.histological_type: 
kidney papillary renal cell carcinoma 
                                  291 

patient.clinical_cqcf.history_of_neoadjuvant_treatment: 
 no 
291 

patient.clinical_cqcf.history_of_prior_malignancy: 
                                                     no 
                                                    235 
                       yes, history of prior malignancy 
                                                     38 
yes, history of synchronous and or bilateral malignancy 
                                                     18 

patient.clinical_cqcf.normal_tissue_anatomic_site: 
       kidney kidney; mixed         other          NA's 
           83            11             7           190 

patient.clinical_cqcf.normal_tissue_proximity: 
distal (>2cm)          NA's 
           92           199 

patient.clinical_cqcf.other_anatomic_site_normal_tissue: 
     blood     cortex peripheral       NA's 
         7          1          1        282 

patient.clinical_cqcf.tumor_type: 
primary 
    291 

patient.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   16.00   15.71   23.00   31.00 

patient.days_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32142  -26082  -22640  -22634  -19676  -10323       5 

patient.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  122.0   341.2   623.5   973.3  1478.0  2941.0     259 

patient.days_to_initial_pathologic_diagnosis: 
   0 NA's 
 266   25 

patient.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   79.75  335.50  681.14  937.25 5559.00      35 

patient.days_to_last_known_alive: 
  34 NA's 
   1  290 



patient.drugs.drug.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.50   16.00   16.42   26.25   28.00     279 

patient.drugs.drug.2.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  205.0   308.0   436.0   517.0   665.5  1058.0     280 

patient.drugs.drug.2.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   86.0   257.0   406.0   425.8   531.5   829.0     279 


patient.drugs.drug.2.measure_of_response: 
clinical progressive disease            complete response 
                           2                            1 
            partial response                         NA's 
                           1                          287 

patient.drugs.drug.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.833   9.000  10.000     279 

patient.drugs.drug.2.number_cycles: 
   1 NA's 
   2  289 

patient.drugs.drug.2.prescribed_dose: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   31.25  225.00  466.67  700.00 1500.00     285 

patient.drugs.drug.2.prescribed_dose_units: 
  mg NA's 
   5  286 

patient.drugs.drug.2.regimen_indication: 
               adjuvant other: specify in notes             progression 
                      2                       2                       2 
                   NA's 
                    285 

patient.drugs.drug.2.regimen_indication_notes: 
metastasis       NA's 
         1        290 

patient.drugs.drug.2.regimen_number: 
   1    2    3 NA's 
   3    2    1  285 

patient.drugs.drug.2.route_of_administrations.route_of_administration: 
intramuscular (im)   intravenous (iv)               oral               NA's 
                 1                  1                  4                285 

patient.drugs.drug.2.therapy_ongoing: 
  no  yes NA's 
  11    1  279 

patient.drugs.drug.2.therapy_types.therapy_type: 
              chemotherapy             other, specify 
                         6                          1 
targeted molecular therapy                       NA's 
                         4                        280 

patient.drugs.drug.2.therapy_types.therapy_type_notes: 
vaccine    NA's 
      1     290 

patient.drugs.drug.2.tx_on_clinical_trial: 
  no NA's 
   6  285 

patient.drugs.drug.2.year_of_form_completion: 
2011 2012 2014 NA's 
   5    2    5  279 



patient.drugs.drug.3.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   24.00   19.14   26.50   28.00     284 

patient.drugs.drug.3.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  241.0   365.5   493.0   889.9  1183.0  2398.0     284 

patient.drugs.drug.3.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  175.0   253.0   472.0   693.3   777.0  2146.0     284 


patient.drugs.drug.3.measure_of_response: 
clinical progressive disease            complete response 
                           1                            1 
                        NA's 
                         289 

patient.drugs.drug.3.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   6.143   9.000  10.000     284 

patient.drugs.drug.3.number_cycles: 
   1 NA's 
   1  290 

patient.drugs.drug.3.prescribed_dose: 
  15   25   50  400 NA's 
   1    1    1    1  287 

patient.drugs.drug.3.prescribed_dose_units: 
  mg NA's 
   4  287 

patient.drugs.drug.3.regimen_indication: 
other: specify in notes             progression                    NA's 
                      1                       3                     287 

patient.drugs.drug.3.regimen_indication_notes: 
metastasis       NA's 
         1        290 

patient.drugs.drug.3.regimen_number: 
   2    4 NA's 
   3    1  287 

patient.drugs.drug.3.route_of_administrations.route_of_administration: 
intravenous (iv)             oral             NA's 
               2                2              287 

patient.drugs.drug.3.therapy_ongoing: 
  no NA's 
   7  284 

patient.drugs.drug.3.therapy_types.therapy_type: 
              chemotherapy targeted molecular therapy 
                         5                          2 
                      NA's 
                       284 

patient.drugs.drug.3.tx_on_clinical_trial: 
  no NA's 
   3  288 

patient.drugs.drug.3.year_of_form_completion: 
2011 2014 NA's 
   4    3  284 

patient.drugs.drug.4.bcr_drug_barcode: 
tcga-bq-5879-d11687 tcga-bq-7048-d16995 tcga-ia-a83w-d56323                NA's 
                  1                   1                   1                 288 

patient.drugs.drug.4.bcr_drug_uuid: 
1b60764b-02aa-4b51-95dd-abf32830fd63 92e5e438-bb9c-4cdf-9daa-4ffd2a406599 
                                   1                                    1 
d3be17c8-b8e5-4020-8a86-95080e6e2c0d                                 NA's 
                                   1                                  288 

patient.drugs.drug.4.day_of_form_completion: 
   3   13   29 NA's 
   1    1    1  288 

patient.drugs.drug.4.days_to_drug_therapy_end: 
 601 2042 NA's 
   1    1  289 

patient.drugs.drug.4.days_to_drug_therapy_start: 
 496  612 1880 NA's 
   1    1    1  288 

patient.drugs.drug.4.drug_name: 
     cytoxan    sunitinib temsirolimus         NA's 
           1            1            1          288 

patient.drugs.drug.4.measure_of_response: 
complete response              NA's 
                1               290 

patient.drugs.drug.4.month_of_form_completion: 
   3    5    9 NA's 
   1    1    1  288 

patient.drugs.drug.4.number_cycles: 
   4 NA's 
   1  290 

patient.drugs.drug.4.prescribed_dose: 
  25   50 NA's 
   1    1  289 

patient.drugs.drug.4.prescribed_dose_units: 
  mg NA's 
   2  289 

patient.drugs.drug.4.regimen_indication: 
progression        NA's 
          2         289 

patient.drugs.drug.4.regimen_number: 
   3    4 NA's 
   1    1  289 

patient.drugs.drug.4.route_of_administrations.route_of_administration: 
intravenous (iv)             oral             NA's 
               1                1              289 

patient.drugs.drug.4.therapy_ongoing: 
  no NA's 
   3  288 

patient.drugs.drug.4.therapy_types.therapy_type: 
              chemotherapy targeted molecular therapy 
                         2                          1 
                      NA's 
                       288 

patient.drugs.drug.4.tx_on_clinical_trial: 
  no NA's 
   1  290 

patient.drugs.drug.4.year_of_form_completion: 
2011 2014 NA's 
   2    1  288 

patient.drugs.drug.5.bcr_drug_barcode: 
tcga-bq-5879-d11686                NA's 
                  1                 290 

patient.drugs.drug.5.bcr_drug_uuid: 
09cfe30c-4580-4e9e-8d61-22a3ad817204                                 NA's 
                                   1                                  290 

patient.drugs.drug.5.day_of_form_completion: 
  13 NA's 
   1  290 

patient.drugs.drug.5.days_to_drug_therapy_end: 
 591 NA's 
   1  290 

patient.drugs.drug.5.days_to_drug_therapy_start: 
 508 NA's 
   1  290 

patient.drugs.drug.5.drug_name: 
sorafenib      NA's 
        1       290 

patient.drugs.drug.5.month_of_form_completion: 
   5 NA's 
   1  290 

patient.drugs.drug.5.prescribed_dose: 
 400 NA's 
   1  290 

patient.drugs.drug.5.prescribed_dose_units: 
  mg NA's 
   1  290 

patient.drugs.drug.5.regimen_indication: 
other: specify in notes                    NA's 
                      1                     290 

patient.drugs.drug.5.regimen_indication_notes: 
previous trial closed, prescribed new medical treatment 
                                                      1 
                                                   NA's 
                                                    290 

patient.drugs.drug.5.regimen_number: 
   3 NA's 
   1  290 

patient.drugs.drug.5.route_of_administrations.route_of_administration: 
oral NA's 
   1  290 

patient.drugs.drug.5.therapy_ongoing: 
  no NA's 
   1  290 

patient.drugs.drug.5.therapy_types.therapy_type: 
targeted molecular therapy                       NA's 
                         1                        290 

patient.drugs.drug.5.year_of_form_completion: 
2011 NA's 
   1  290 

patient.drugs.drug.6.bcr_drug_barcode: 
tcga-bq-5879-d11683                NA's 
                  1                 290 

patient.drugs.drug.6.bcr_drug_uuid: 
75b02040-4cac-4c55-8cac-9624d0396034                                 NA's 
                                   1                                  290 

patient.drugs.drug.6.day_of_form_completion: 
  13 NA's 
   1  290 

patient.drugs.drug.6.days_to_drug_therapy_end: 
 430 NA's 
   1  290 

patient.drugs.drug.6.days_to_drug_therapy_start: 
 192 NA's 
   1  290 

patient.drugs.drug.6.drug_name: 
sunitinib      NA's 
        1       290 

patient.drugs.drug.6.month_of_form_completion: 
   5 NA's 
   1  290 

patient.drugs.drug.6.number_cycles: 
   6 NA's 
   1  290 

patient.drugs.drug.6.prescribed_dose: 
  25 NA's 
   1  290 

patient.drugs.drug.6.prescribed_dose_units: 
  mg NA's 
   1  290 

patient.drugs.drug.6.regimen_number: 
   1 NA's 
   1  290 

patient.drugs.drug.6.route_of_administrations.route_of_administration: 
oral NA's 
   1  290 

patient.drugs.drug.6.therapy_ongoing: 
  no NA's 
   1  290 

patient.drugs.drug.6.therapy_types.therapy_type: 
targeted molecular therapy                       NA's 
                         1                        290 

patient.drugs.drug.6.year_of_form_completion: 
2011 NA's 
   1  290 



patient.drugs.drug.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   14.00   16.13   25.00   30.00     268 

patient.drugs.drug.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   74.0   176.0   295.0   412.9   460.5  2146.0     272 

patient.drugs.drug.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   33.0    61.5    96.0   324.9   389.5  2042.0     268 


patient.drugs.drug.measure_of_response: 
clinical progressive disease            complete response 
                           5                            1 
            partial response               stable disease 
                           2                            2 
                        NA's 
                         281 

patient.drugs.drug.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   5.000   6.043   8.500  11.000     268 

patient.drugs.drug.number_cycles: 
   2    4    6    7 NA's 
   2    2    1    1  285 


patient.drugs.drug.prescribed_dose_units: 
m[iu]    mg mg/kg mg/m2  NA's 
    1     7     1     1   281 

patient.drugs.drug.regimen_indication: 
               adjuvant other: specify in notes             progression 
                      3                       6                       2 
                   NA's 
                    280 

patient.drugs.drug.regimen_indication_notes: 
metastasis       NA's 
         2        289 

patient.drugs.drug.regimen_number: 
   1    2    3 NA's 
   8    2    1  280 

patient.drugs.drug.route_of_administrations.route_of_administration: 
 intravenous (iv)              oral subcutaneous (sc)              NA's 
                5                 4                 2               280 

patient.drugs.drug.therapy_ongoing: 
  no  yes NA's 
  20    2  269 

patient.drugs.drug.therapy_types.therapy_type: 
              chemotherapy              immunotherapy 
                        14                          2 
targeted molecular therapy                       NA's 
                         6                        269 

patient.drugs.drug.total_dose: 
54 million units              700             NA's 
               1                1              289 

patient.drugs.drug.total_dose_units: 
  mg NA's 
   1  290 

patient.drugs.drug.tx_on_clinical_trial: 
  no  yes NA's 
  10    2  279 

patient.drugs.drug.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2014    2014     268 

patient.eastern_cancer_oncology_group: 
   0    1    2    3 NA's 
  65   17    3    3  203 

patient.erythrocyte_sedimentation_rate_result: 
elevated      low   normal     NA's 
       3        2       12      274 

patient.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    12                    243                     36 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy: 
  no  yes NA's 
   7    6  278 

patient.follow_ups.follow_up.2.additional_radiation_therapy: 
  no  yes NA's 
   9    2  280 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure: 
  no NA's 
   5  286 

patient.follow_ups.follow_up.2.additional_surgery_metastatic_procedure: 
  no  yes NA's 
   6    1  284 



patient.follow_ups.follow_up.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   15.00   16.44   25.25   30.00     243 

patient.follow_ups.follow_up.2.days_to_additional_surgery_metastatic_procedure: 
 376 NA's 
   1  290 

patient.follow_ups.follow_up.2.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  241.0   324.8   610.5   825.4   897.8  2331.0     283 

patient.follow_ups.follow_up.2.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   694.2  1093.0  1206.8  1490.5  2839.0     251 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   229.0   505.0   705.1   812.0  2399.0     282 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group: 
   0    4 NA's 
  12    1  278 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason: 
    additional new tumor event scheduled follow-up submission 
                             3                             42 
                          NA's 
                           246 

patient.follow_ups.follow_up.2.followup_treatment_success: 
complete remission/response         progressive disease 
                         32                           4 
                       NA's 
                        255 

patient.follow_ups.follow_up.2.karnofsky_performance_score: 
  20   90  100 NA's 
   1    2   10  278 

patient.follow_ups.follow_up.2.lost_follow_up: 
  no  yes NA's 
  36    3  252 

patient.follow_ups.follow_up.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.479  10.000  12.000     243 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
  32    4  255 

patient.follow_ups.follow_up.2.performance_status_scale_timing: 
at recurrence/progression of disease                                 NA's 
                                   1                                  290 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
        35         10        246 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success: 
complete remission/response         progressive disease 
                         31                           2 
                       NA's 
                        258 

patient.follow_ups.follow_up.2.radiation_therapy: 
  no NA's 
  34  257 

patient.follow_ups.follow_up.2.targeted_molecular_therapy: 
  no  yes NA's 
  33    1  257 

patient.follow_ups.follow_up.2.vital_status: 
alive  dead  NA's 
   40     8   243 

patient.follow_ups.follow_up.2.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2014    2014    2014    2015     243 

patient.follow_ups.follow_up.3.additional_pharmaceutical_therapy: 
 yes NA's 
   2  289 

patient.follow_ups.follow_up.3.additional_radiation_therapy: 
  no NA's 
   2  289 

patient.follow_ups.follow_up.3.additional_surgery_locoregional_procedure: 
  no NA's 
   1  290 

patient.follow_ups.follow_up.3.additional_surgery_metastatic_procedure: 
  no NA's 
   2  289 



patient.follow_ups.follow_up.3.day_of_form_completion: 
  13   14   16   26 NA's 
   1    1    1    2  286 

patient.follow_ups.follow_up.3.days_to_death: 
 697 NA's 
   1  290 

patient.follow_ups.follow_up.3.days_to_last_followup: 
  14 1338 1615 2839 NA's 
   1    1    1    1  287 

patient.follow_ups.follow_up.3.days_to_new_tumor_event_after_initial_treatment: 
 591 1858 NA's 
   1    1  289 

patient.follow_ups.follow_up.3.eastern_cancer_oncology_group: 
   0 NA's 
   1  290 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason: 
    additional new tumor event scheduled follow-up submission 
                             1                              3 
                          NA's 
                           287 

patient.follow_ups.follow_up.3.followup_treatment_success: 
complete remission/response                        NA's 
                          3                         288 

patient.follow_ups.follow_up.3.karnofsky_performance_score: 
 100 NA's 
   1  290 

patient.follow_ups.follow_up.3.lost_follow_up: 
  no  yes NA's 
   2    1  288 

patient.follow_ups.follow_up.3.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     5.0     7.0     6.8     9.0    12.0     286 

patient.follow_ups.follow_up.3.new_tumor_event_after_initial_treatment: 
  no NA's 
   2  289 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
         2          2        287 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success: 
complete remission/response                        NA's 
                          1                         290 

patient.follow_ups.follow_up.3.radiation_therapy: 
  no NA's 
   1  290 

patient.follow_ups.follow_up.3.targeted_molecular_therapy: 
  no NA's 
   1  290 

patient.follow_ups.follow_up.3.vital_status: 
alive  dead  NA's 
    4     1   286 

patient.follow_ups.follow_up.3.year_of_form_completion: 
2011 2012 2014 NA's 
   2    1    2  286 

patient.follow_ups.follow_up.4.additional_pharmaceutical_therapy: 
 yes NA's 
   1  290 

patient.follow_ups.follow_up.4.additional_radiation_therapy: 
 yes NA's 
   1  290 

patient.follow_ups.follow_up.4.additional_surgery_metastatic_procedure: 
  no NA's 
   1  290 

patient.follow_ups.follow_up.4.bcr_followup_barcode: 
tcga-bq-7048-f16726                NA's 
                  1                 290 

patient.follow_ups.follow_up.4.bcr_followup_uuid: 
df9f17cf-c3ca-470c-83a0-5b1a1bdcc7bc                                 NA's 
                                   1                                  290 

patient.follow_ups.follow_up.4.day_of_form_completion: 
  26 NA's 
   1  290 

patient.follow_ups.follow_up.4.days_to_last_followup: 
2839 NA's 
   1  290 

patient.follow_ups.follow_up.4.days_to_new_tumor_event_after_initial_treatment: 
2142 NA's 
   1  290 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason: 
additional new tumor event                       NA's 
                         1                        290 

patient.follow_ups.follow_up.4.month_of_form_completion: 
   9 NA's 
   1  290 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status: 
with tumor       NA's 
         1        290 

patient.follow_ups.follow_up.4.vital_status: 
alive  NA's 
    1   290 

patient.follow_ups.follow_up.4.year_of_form_completion: 
2011 NA's 
   1  290 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy: 
  no  yes NA's 
  33    9  249 

patient.follow_ups.follow_up.additional_radiation_therapy: 
  no  yes NA's 
  39    4  248 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure: 
  no  yes NA's 
  26    7  258 

patient.follow_ups.follow_up.additional_surgery_metastatic_procedure: 
  no  yes NA's 
  27    5  259 



patient.follow_ups.follow_up.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.25   17.00   18.39   26.00   31.00      29 

patient.follow_ups.follow_up.days_to_additional_surgery_locoregional_procedure: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   65.0   223.0   343.0   379.9   538.5   728.0     284 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  137.0   147.0   412.0   792.8   974.0  2294.0     286 

patient.follow_ups.follow_up.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  122.0   324.0   677.5   828.5  1326.5  2624.0     267 

patient.follow_ups.follow_up.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   403.5   708.0   998.4  1254.0  5925.0      60 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -35.0   212.0   360.0   675.0   743.8  3157.0     255 

patient.follow_ups.follow_up.eastern_cancer_oncology_group: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4375  1.0000  4.0000     227 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason: 
    additional new tumor event scheduled follow-up submission 
                             5                            210 
                          NA's 
                            76 

patient.follow_ups.follow_up.followup_treatment_success: 
complete remission/response         progressive disease 
                        127                          10 
             stable disease                        NA's 
                          6                         148 

patient.follow_ups.follow_up.karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00  100.00   88.79  100.00  100.00     233 

patient.follow_ups.follow_up.lost_follow_up: 
  no  yes NA's 
 156   17  118 

patient.follow_ups.follow_up.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.511  10.000  12.000      29 

patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 137   21  133 

patient.follow_ups.follow_up.performance_status_scale_timing: 
at recurrence/progression of disease                                other 
                                   1                                   16 
               post adjuvant therapy               post secondary therapy 
                                  21                                    2 
                                NA's 
                                 251 

patient.follow_ups.follow_up.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       204         35         52 

patient.follow_ups.follow_up.primary_therapy_outcome_success: 
complete remission/response         progressive disease 
                        139                           2 
             stable disease                        NA's 
                          4                         146 

patient.follow_ups.follow_up.radiation_therapy: 
  no NA's 
 157  134 

patient.follow_ups.follow_up.targeted_molecular_therapy: 
  no  yes NA's 
 152    3  136 

patient.follow_ups.follow_up.vital_status: 
alive  dead  NA's 
  234    24    33 

patient.follow_ups.follow_up.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2014    2013    2014    2015      29 

patient.gender: 
female   male 
    77    214 

patient.height: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0   166.0   173.0   171.8   180.0   193.0      78 

patient.hemoglobin_result: 
elevated      low   normal     NA's 
       1       97      112       81 

patient.histological_type: 
kidney papillary renal cell carcinoma 
                                  291 

patient.history_of_neoadjuvant_treatment: 
 no 
291 

patient.icd_10: 
c64.1 
  291 

patient.icd_o_3_histology: 
8260/3 
   291 

patient.icd_o_3_site: 
c64.9 
  291 

patient.informed_consent_verified: 
yes 
291 

patient.karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00  100.00   91.94  100.00  100.00     229 

patient.lactate_dehydrogenase_result: 
elevated      low   normal     NA's 
       8        1       34      248 

patient.laterality: 
bilateral      left     right      NA's 
        2       160       128         1 

patient.lymph_node_examined_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     2.0     7.0    10.1    15.0    34.0     239 

patient.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   7.000   6.646  10.000  12.000 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no  yes NA's 
   8    7  276 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no  yes NA's 
  13    1  277 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0   124.2   415.0   847.3  1253.5  3228.0     273 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.additional_surgery_locoregional_procedure: 
  no  yes NA's 
   9    7  275 

patient.new_tumor_events.new_tumor_event.locoregional_procedure.days_to_additional_surgery_locoregional_procedure: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  284.0   333.0   728.0   972.6  1491.0  2148.0     284 

patient.new_tumor_events.new_tumor_event.metastatic_procedure.additional_surgery_metastatic_procedure: 
  no NA's 
   8  283 

patient.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 140   18  133 

patient.number_of_lymphnodes_positive: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.889   3.000  30.000     237 

patient.number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   15.00   24.50   31.73   41.25  185.00     215 


patient.performance_status_scale_timing: 
                 other  post adjuvant therapy post secondary therapy 
                    52                      2                      1 
         pre-operative                   NA's 
                    25                    211 

patient.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       220         39         32 

patient.platelet_qualitative_result: 
elevated      low   normal     NA's 
       7       16      183       85 

patient.primary_lymph_node_presentation_assessment: 
  no  yes NA's 
 232   55    4 

patient.primary_therapy_outcome_success: 
complete remission/response  partial remission/response 
                        124                           2 
        progressive disease              stable disease 
                          3                           5 
                       NA's 
                        157 

patient.prior_dx: 
                                                     no 
                                                    235 
                                                    yes 
                                                     23 
                       yes, history of prior malignancy 
                                                     23 
yes, history of synchronous and or bilateral malignancy 
                                                     10 

patient.race: 
american indian or alaska native                            asian 
                               2                                6 
       black or african american                            white 
                              61                              207 
                            NA's 
                              15 

patient.radiation_therapy: 
  no  yes NA's 
 159    1  131 

patient.radiations.radiation.2.anatomic_treatment_site: 
distant site         NA's 
           3          288 

patient.radiations.radiation.2.bcr_radiation_barcode: 
tcga-2z-a9j7-r66719 tcga-bq-5877-r11671 tcga-f9-a4jj-r46416                NA's 
                  1                   1                   1                 288 

patient.radiations.radiation.2.bcr_radiation_uuid: 
7039da56-a4c6-4645-9765-986fbfbd1808 f04498b9-c312-4d09-a99a-b2980f3f82fd 
                                   1                                    1 
fa83f0cf-0902-404a-8a14-29592487b83f                                 NA's 
                                   1                                  288 

patient.radiations.radiation.2.course_number: 
   2 NA's 
   1  290 

patient.radiations.radiation.2.day_of_form_completion: 
   7   13   22 NA's 
   1    1    1  288 

patient.radiations.radiation.2.days_to_radiation_therapy_end: 
 105  134  211 NA's 
   1    1    1  288 

patient.radiations.radiation.2.days_to_radiation_therapy_start: 
 103  112  196 NA's 
   1    1    1  288 

patient.radiations.radiation.2.month_of_form_completion: 
   5    8   10 NA's 
   1    1    1  288 

patient.radiations.radiation.2.numfractions: 
   5   10   14 NA's 
   1    1    1  288 

patient.radiations.radiation.2.radiation_dosage: 
2000 3000 3500 NA's 
   1    1    1  288 

patient.radiations.radiation.2.radiation_treatment_ongoing: 
  no NA's 
   3  288 

patient.radiations.radiation.2.radiation_type: 
               external other: specify in notes                    NA's 
                      2                       1                     288 

patient.radiations.radiation.2.radiation_type_notes: 
palliative       NA's 
         1        290 

patient.radiations.radiation.2.regimen_indication: 
progression        NA's 
          1         290 

patient.radiations.radiation.2.units: 
 cgy NA's 
   3  288 

patient.radiations.radiation.2.year_of_form_completion: 
2011 2013 2014 NA's 
   1    1    1  288 

patient.radiations.radiation.3.anatomic_treatment_site: 
distant site         NA's 
           3          288 

patient.radiations.radiation.3.bcr_radiation_barcode: 
tcga-2z-a9j7-r66721 tcga-bq-5877-r11673 tcga-f9-a4jj-r46417                NA's 
                  1                   1                   1                 288 

patient.radiations.radiation.3.bcr_radiation_uuid: 
4b6955e9-c072-45ba-a535-b0623841f940 910da378-c792-4fc2-9b4e-5190e17ca13a 
                                   1                                    1 
c2380807-915f-4c49-9cf8-a6543faa57f0                                 NA's 
                                   1                                  288 

patient.radiations.radiation.3.course_number: 
   2 NA's 
   1  290 

patient.radiations.radiation.3.day_of_form_completion: 
   7   13   22 NA's 
   1    1    1  288 

patient.radiations.radiation.3.days_to_radiation_therapy_end: 
 115  133  211 NA's 
   1    1    1  288 

patient.radiations.radiation.3.days_to_radiation_therapy_start: 
 103  133  196 NA's 
   1    1    1  288 

patient.radiations.radiation.3.month_of_form_completion: 
   5    8   10 NA's 
   1    1    1  288 

patient.radiations.radiation.3.numfractions: 
   1    5   10 NA's 
   1    1    1  288 

patient.radiations.radiation.3.radiation_dosage: 
 800 3000 NA's 
   1    2  288 

patient.radiations.radiation.3.radiation_treatment_ongoing: 
  no NA's 
   3  288 

patient.radiations.radiation.3.radiation_type: 
               external other: specify in notes                    NA's 
                      2                       1                     288 

patient.radiations.radiation.3.radiation_type_notes: 
palliative       NA's 
         1        290 

patient.radiations.radiation.3.regimen_indication: 
progression        NA's 
          1         290 

patient.radiations.radiation.3.units: 
 cgy NA's 
   3  288 

patient.radiations.radiation.3.year_of_form_completion: 
2011 2013 2014 NA's 
   1    1    1  288 

patient.radiations.radiation.anatomic_treatment_site: 
distant recurrence       distant site               NA's 
                 2                  7                282 



patient.radiations.radiation.course_number: 
   1 NA's 
   5  286 

patient.radiations.radiation.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   13.00   13.00   16.33   22.00   28.00     282 

patient.radiations.radiation.days_to_radiation_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   35.0   130.0   169.0   437.2   222.0  2497.0     282 

patient.radiations.radiation.days_to_radiation_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   23.0   116.0   168.0   428.3   209.0  2497.0     282 

patient.radiations.radiation.measure_of_response: 
radiographic progressive disease                             NA's 
                               1                              290 

patient.radiations.radiation.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   5.000   6.556   9.000  11.000     282 

patient.radiations.radiation.numfractions: 
   1    5    6   10 NA's 
   1    3    1    4  282 

patient.radiations.radiation.radiation_dosage: 
 600 1800 2000 3000 NA's 
   1    1    2    5  282 

patient.radiations.radiation.radiation_treatment_ongoing: 
  no NA's 
   9  282 

patient.radiations.radiation.radiation_type: 
               external           external beam other: specify in notes 
                      3                       3                       3 
                   NA's 
                    282 

patient.radiations.radiation.radiation_type_notes: 
palliative       NA's 
         3        288 

patient.radiations.radiation.regimen_indication: 
 palliative progression  recurrence        NA's 
          2           3           1         285 

patient.radiations.radiation.units: 
 cgy NA's 
   9  282 

patient.radiations.radiation.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2013    2014     282 

patient.serum_calcium_result: 
elevated      low   normal     NA's 
       6       41      134      110 

patient.stage_event.clinical_stage: 
  stage i  stage ii stage iii  stage iv      NA's 
      141        21        29        11        89 

patient.stage_event.pathologic_stage: 
  stage i  stage ii stage iii  stage iv      NA's 
      173        23        52        15        28 

patient.stage_event.system_version: 
 3rd  5th  6th  7th NA's 
   1    6   79  176   29 

patient.stage_event.tnm_categories.clinical_categories.clinical_m: 
  m0   m1   mx NA's 
 199   10   61   21 

patient.stage_event.tnm_categories.clinical_categories.clinical_n: 
  n0   n1   n2   nx NA's 
 133   19    2   51   86 


patient.stage_event.tnm_categories.pathologic_categories.pathologic_m: 
  m0   m1   mx NA's 
  95    9  172   15 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n: 
  n0   n1   n2   nx NA's 
  50   24    4  212    1 


patient.stopped_smoking_year: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1968    1980    1994    1993    2006    2013     227 

patient.targeted_molecular_therapy: 
  no  yes NA's 
 153    6  132 

patient.tissue_prospective_collection_indicator: 
  no  yes NA's 
 186   99    6 

patient.tissue_retrospective_collection_indicator: 
  no  yes NA's 
  98  186    7 


patient.tobacco_smoking_history: 
    current reformed smoker for < or = 15 years 
                                             85 
current reformed smoker, duration not specified 
                                             11 
                                 current smoker 
                                             36 
                            lifelong non-smoker 
                                            116 
                                           NA's 
                                             43 

patient.tumor_tissue_site: 
kidney 
   291 

patient.tumor_type: 
type 1 type 2   NA's 
    77     86    128 

patient.vital_status: 
alive  dead 
  259    32 

patient.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  35.00   73.00   86.05   87.19  101.12  178.00      71 

patient.white_cell_count_result: 
elevated      low   normal     NA's 
      44       11      152       84 

patient.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2010    2011    2013    2013    2014    2015 

patient.year_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1996    2008    2011    2010    2012    2013      22 

patient.year_of_tobacco_smoking_onset: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1941    1961    1970    1972    1984    2010     235 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason: 
              case matched expression control 
                                           29 
             case matched methylation control 
                                           26 
                          germline comparison 
                                           17 
primary tumor and additional primary tumor(s) 
                                            1 
                                         NA's 
                                          218 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2: 
case matched methylation control              germline comparison 
                              25                               10 
                            NA's 
                             256 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3: 
germline comparison                NA's 
                  4                 287 

patient.bcr_canonical_check.bcr_patient_canonical_status: 
       canonical canonical - plus 
             218               73 

patient.biospecimen_cqcf.consent_or_death_status: 
consented  deceased 
      282         9 

patient.biospecimen_cqcf.country: 
       canada       germany united states       vietnam          NA's 
            9            17           216             6            43 

patient.biospecimen_cqcf.days_to_pathology_review: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    57.0   181.0   733.8  1100.0  5602.0      25 

patient.biospecimen_cqcf.days_to_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -23.00    0.00    0.00   42.67    2.50 5559.00      25 

patient.biospecimen_cqcf.digital_image_submitted: 
 no yes 
253  38 

patient.biospecimen_cqcf.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     9                    141                    141 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site: 
kidney 
   291 

patient.biospecimen_cqcf.histological_type: 
kidney papillary renal cell carcinoma 
                                  291 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment: 
 no 
291 

patient.biospecimen_cqcf.laterality: 
 left right 
  163   128 

patient.biospecimen_cqcf.maximum_tumor_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.200   5.138   6.500  22.000       2 



patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -51.0     0.0     0.0   162.2    25.0  5559.0     254 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc: 
0.123 0.243  NA's 
    1     1   289 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm: 
   cdrop nanodrop     NA's 
       1        1      289 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty: 
  15 NA's 
   2  289 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol: 
61.73 122.4  NA's 
    1     1   289 


patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site: 
kidney   NA's 
    30    261 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
           28           263 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number: 
   1    2 NA's 
  33    8  250 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2296.00     0.00     0.00    72.57    16.00  5559.00       26 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0442   0.2000   0.3145  67.7602 116.5550 550.0000      205 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   15.00   20.00   23.73   29.75   87.00     205 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   68.69  108.62  126.36  157.50  500.00     205 


patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site: 
       kidney kidney; mixed         other          NA's 
           53            10             8           220 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
           63           228 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number: 
  1   2 
281  10 


patient.biospecimen_cqcf.other_vessel_used: 
           50ml conical                     bag biospecimen storage bag 
                      2                       2                       3 
                   NA's 
                    284 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching: 
 no yes 
  1 290 

patient.biospecimen_cqcf.path_confirm_report_attached: 
yes 
291 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics: 
yes 
291 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics: 
yes 
291 

patient.biospecimen_cqcf.race: 
                    asian black or african american                     white 
                        4                        27                       124 
                     NA's 
                      136 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching: 
pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                1 
                                                                                                                             NA's 
                                                                                                                              290 

patient.biospecimen_cqcf.sample_prescreened: 
yes 
291 

patient.biospecimen_cqcf.site_of_disease: 
kidney 
   291 

patient.biospecimen_cqcf.submitted_for_lce: 
  no  yes NA's 
 177   13  101 

patient.biospecimen_cqcf.top_slide_submitted: 
 no yes 
 37 254 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid: 
c27551a9-e0bd-4d67-803b-1074c12a8def eeb76126-f7f3-4b13-8c06-165b0c5bc41a 
                                   1                                    1 
                                NA's 
                                 289 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent: 
   0   10 NA's 
   1    1  289 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent: 
  80   95 NA's 
   1    1  289 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight: 
  40  120 NA's 
   1    1  289 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number: 
   1    2 NA's 
   1    1  289 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   4.278   5.000  50.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00   80.00   85.00   83.32   90.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   135.0   200.0   308.9   340.0  3600.0       2 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number: 
  1 
291 

patient.biospecimen_cqcf.vessel_used: 
biospecimen storage bag                cassette                cryomold 
                     39                      11                      74 
               cryovial  other (please specify) 
                    160                       7 

patient.days_to_index: 
   0 NA's 
 266   25 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
primary tumor and additional primary tumor(s) 
                                            1 
                                         NA's 
                                          290 



patient.samples.sample.2.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0   134.8   426.0   851.4  1214.2  4013.0     139 

patient.samples.sample.2.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0    90.0   840.0   816.5  1610.0  1940.0     274 

patient.samples.sample.2.intermediate_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.6000  0.8000  0.8512  1.0000  1.7000     250 

patient.samples.sample.2.is_ffpe: 
 no 
291 

patient.samples.sample.2.longest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   1.000   1.300   1.339   1.700   2.300     250 

patient.samples.sample.2.oct_embedded: 
false  true  NA's 
  159    15   117 

patient.samples.sample.2.pathology_report_file_name: 
tcga-uz-a9ps.bae8c63f-81b6-431a-9431-23acac11bad0.pdf 
                                                    1 
                                                 NA's 
                                                  290 

patient.samples.sample.2.pathology_report_uuid: 
bae8c63f-81b6-431a-9431-23acac11bad0                                 NA's 
                                   1                                  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.700   1.800   1.800   1.801   1.800   1.890     267 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.005e+08 1.050e+09 6.071e+08 1.053e+09 1.054e+09       253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
   7   10   13 NA's 
  21   13    4  253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.2716  0.5000  0.5000     253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
   9   17   18   20 NA's 
  10   13    2   13  253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  38  253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   6.474   9.000   9.000     253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   5.000   5.342   7.500   9.000     253 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.860   2.033   4.010  15.677  40.000  40.000     253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  22   23 NA's 
  34    4  253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
13.3   20 26.7   80 NA's 
  11    4   10   13  253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014     253 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode: 
tcga-a4-a5y1-11a-11r-a466-07                         NA's 
                           1                          290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid: 
7dfa61df-2100-4d1c-a9a7-7f24ccb54755                                 NA's 
                                   1                                  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom: 
167812972      NA's 
        1       290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id: 
   7 NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration: 
0.16 NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment: 
   9 NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment: 
  12 NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column: 
   5 NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id: 
a466 NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row: 
   b NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity: 
1.06 NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center: 
  23 NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume: 
 6.6 NA's 
   1  290 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment: 
2014 NA's 
   1  290 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.210e+08 1.507e+08 2.633e+08 1.678e+08 1.054e+09        51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   7    8   10   13 NA's 
   4   79  136   21   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.5000  0.5000  0.4638  0.5000  0.5000      51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    8.00   12.00   13.34   19.00   28.00      51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 240   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.958   7.000  10.000      51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.750   5.000   5.658   7.000  11.000      51 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.08   40.00   40.00   36.13   40.00   40.00      51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  22   23 NA's 
 100  140   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  13   20   80 NA's 
   4   21  215   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2013    2014    2014      51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.2    92.5   166.0   143.3   184.8   212.2     187 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna                 NA's 
                 215                   25                   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    w NA's 
  25  215   51 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.5000  0.5000  0.4165  0.5000  0.5000     187 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
 215   76 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
  no  yes NA's 
  79  136   76 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             240 
                                            NA's 
                                              51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
 240   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                   repli-g                 repli-g x 
                       25                       201                        14 
                     NA's 
                       51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.05    1.29    1.34    1.38    1.41    1.76     266 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.700   7.900   8.012   8.500   9.300     266 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method: 
picogreen   uv spec      NA's 
       49        55       187 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2 NA's 
  11    2  278 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100512923 100512946 100512954 100512958 100512978 100512995       278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id: 
  10 NA's 
  13  278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration: 
 0.5 NA's 
  13  278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment: 
  20 NA's 
  13  278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  13  278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment: 
   9 NA's 
  13  278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column: 
   3    4    5 NA's 
   4    4    5  278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id: 
1249 NA's 
  13  278 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity: 
  40 NA's 
  13  278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center: 
  22 NA's 
  13  278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume: 
  80 NA's 
  13  278 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  13  278 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.005e+08 1.087e+08 3.650e+08 1.054e+09 1.054e+09       254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8   10 NA's 
  34    3  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
  37  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   13.00   19.00   16.81   20.00   20.00     254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  37  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   2    4    6    9 NA's 
   1   10    3   23  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   6.000   6.541   9.000  11.000     254 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
  37  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  22   23 NA's 
  34    3  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
  37  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2010 2011 2012 2013 NA's 
  13   20    1    3  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   59.0    92.0   157.0   133.9   164.8   201.0     257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type: 
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                    24                     13                    254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id: 
   w    x NA's 
  24   13  254 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration: 
 0.5 NA's 
  34  257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
  37  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
  no  yes NA's 
  34    3  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              37 
                                            NA's 
                                             254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
  37  254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
  repli-g repli-g x      NA's 
       24        13       254 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method: 
picogreen   uv spec      NA's 
       11        23       257 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2 NA's 
   2    1  288 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   1.900   1.900   1.872   1.900   2.100     233 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.005e+08 1.244e+08 1.603e+08 2.741e+08 1.742e+08 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  1   8  10 
103  14 174 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0500  0.0750  0.0800  0.1018  0.1450  0.1700 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   17.00   18.00   19.16   25.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.595   8.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   5.000   5.306   7.000  10.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.010   2.800   2.356   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 22  23 
117 174 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
6.67  6.7   40   60 
  90   13  174   14 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2010 2011 2012 2013 2014 
  16   59   52   71   93 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100511034 107031836 113625334 118568595 121000884 175154840       164 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   5    8   10   26 NA's 
   3  101    6   17  164 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0763  0.0800  0.1700     164 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   17.00   13.84   18.00   25.00     164 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
 127  164 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.417   8.000  10.000     164 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.638   6.000  10.000     164 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.160   1.700   2.000   2.348   3.000   4.010     164 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  22   23 NA's 
 116   11  164 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   14.00   40.00   41.09   60.00   60.00     164 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2012    2015     164 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.070e+08 1.678e+08 2.915e+08 1.751e+08 1.054e+09       179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   5    8   10 NA's 
  39    2   71  179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0800  0.0900  0.1109  0.1500  0.1700     179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   7.598  17.000  20.000     179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
 112  179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       8      10       8      10      10     179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.741   8.250  12.000     179 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.180   1.120   1.120   1.756   2.000   4.270     179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  22   23 NA's 
  55   57  179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.30   14.00   15.78   14.00   40.00     179 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2010 2011 2012 2014 NA's 
  13   40    2   57  179 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100513673 167810407 167811486 161631886 175141792 175154870       240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
  10 NA's 
  51  240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
0.07 0.08 0.09 0.15 NA's 
  10   38    1    2  240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
   1   20   21 NA's 
  39    2   10  240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
  51  240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
   8    9   10 NA's 
  10    2   39  240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.902   7.000  12.000     240 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.140   1.070   1.120   1.065   1.120   1.210     240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  22   23 NA's 
   2   49  240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
   2  6.7 13.4   14 NA's 
   1    2   10   38  240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2010 2013 2014 NA's 
   2   10   39  240 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167811542 175141731 175141742 173675702 175141744 175141746       281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id: 
  10 NA's 
  10  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration: 
0.07 0.08 NA's 
   1    9  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment: 
   1 NA's 
  10  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment: 
  10 NA's 
  10  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     7.0     8.5     8.1    10.0    12.0     281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id: 
a45y a45z NA's 
   2    8  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row: 
   a    g    h NA's 
   2    7    1  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity: 
0.98 1.12 NA's 
   1    9  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center: 
  23 NA's 
  10  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume: 
  14 NA's 
  10  281 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment: 
2014 NA's 
  10  281 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.005e+08 1.244e+08 1.603e+08 2.739e+08 1.742e+08 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  1   2  26 
188  75  28 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.13 0.14 0.15 0.16 0.17 
   9   58  102   79   43 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00    9.00   18.00   17.67   25.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   5.141   8.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   5.000   5.237   7.000  10.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.930   1.010   1.070   1.275   1.500   2.260 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 22  23 
117 174 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.67  6.7   10 13.3 
  14  174   60   43 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2010 2011 2012 2013 2014 
  16   59   52   55  109 

patient.samples.sample.2.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   19.46   33.24   84.08  102.35 1110.00 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type: 
dna 
291 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id: 
  d 
291 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration: 
0.13 0.14 0.15 0.16 0.17 
   4   57  107   80   43 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
291 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 no yes 
109 182 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             291 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
            adna preparation type     chemical lysis dna extraction 
                              150                                45 
pre-extracted dna received by tss 
                               96 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen   uv spec 
      233        58 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence: 
   1    2 NA's 
  15    2  274 



patient.samples.sample.2.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    6.00   14.00   15.91   27.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe: 
 no 
291 

patient.samples.sample.2.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   5.155   8.000  12.000 

patient.samples.sample.2.portions.portion.portion_number: 
  1   2  11  12  22 
273   1  15   1   1 

patient.samples.sample.2.portions.portion.portion_sequence: 
   1    2 NA's 
  94    1  196 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_barcode: 
tcga-b3-3926-11a-01-ts1 tcga-b9-4113-11a-01-ts1                    NA's 
                      1                       1                     289 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_uuid: 
070052be-73bd-4b5b-a2ee-7f2aff93f202 2b81f0a5-dacc-4454-904a-681893fb3d6d 
                                   1                                    1 
                                NA's 
                                 289 

patient.samples.sample.2.portions.portion.slides.slide.2.image_file_name: 
tcga-b3-3926-11a-01-ts1.2b81f0a5-dacc-4454-904a-681893fb3d6d.svs 
                                                               1 
tcga-b9-4113-11a-01-ts1.070052be-73bd-4b5b-a2ee-7f2aff93f202.svs 
                                                               1 
                                                            NA's 
                                                             289 

patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe: 
  no NA's 
   2  289 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location: 
 top NA's 
   2  289 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
  58  233 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.588   3.000  15.000     274 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration: 
   0    1    2 NA's 
   8    4    5  274 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis: 
   0 NA's 
  17  274 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0    1    2 NA's 
  13    2    2  274 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   60.00   80.00   75.59  100.00  100.00     274 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   10.00   19.12   40.00   50.00     274 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells: 
   0   90 NA's 
  16    1  274 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei: 
   0   95 NA's 
  16    1  274 

patient.samples.sample.2.portions.portion.slides.slide.section_location: 
bottom    top   NA's 
     2     56    233 

patient.samples.sample.2.portions.portion.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    2.00   17.87   30.00  123.00     141 

patient.samples.sample.2.portions.portion.year_of_creation: 
2010 2011 2012 2013 2014 
  20   75   40   79   77 

patient.samples.sample.2.portions.shipment_portion.bcr_shipment_portion_uuid: 
46494a68-2269-4603-8d93-0b6b7d41ebe8                                 NA's 
                                   1                                  290 

patient.samples.sample.2.portions.shipment_portion.center_id: 
  20 NA's 
   1  290 

patient.samples.sample.2.portions.shipment_portion.is_ffpe: 
  no NA's 
   1  290 

patient.samples.sample.2.portions.shipment_portion.plate_id: 
a39o NA's 
   1  290 

patient.samples.sample.2.portions.shipment_portion.portion_number: 
  21 NA's 
   1  290 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode: 
tcga-uz-a9ps-05a-21-a39o-20                        NA's 
                          1                         290 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment: 
   7 NA's 
   1  290 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment: 
   5 NA's 
   1  290 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment: 
2014 NA's 
   1  290 

patient.samples.sample.2.sample_type: 
additional - new primary     blood derived normal      solid tissue normal 
                       1                      233                       57 

patient.samples.sample.2.sample_type_id: 
  5  10  11 
  1 233  57 

patient.samples.sample.2.shortest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.2000  0.3000  0.3171  0.4000  0.8000     250 

patient.samples.sample.2.vial_number: 
  a   b   c 
288   2   1 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
germline comparison                NA's 
                 11                 280 



patient.samples.sample.3.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  28.00   66.25   77.00  152.62  152.00  588.00     275 

patient.samples.sample.3.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0    95.0   135.0   171.9   225.0   410.0     275 

patient.samples.sample.3.intermediate_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.40    0.50    0.50    0.62    0.70    1.00     276 

patient.samples.sample.3.is_ffpe: 
  no NA's 
  32  259 

patient.samples.sample.3.longest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   1.000   1.300   1.273   1.450   2.300     276 

patient.samples.sample.3.oct_embedded: 
false  true  NA's 
   14     3   274 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.700   1.800   1.820   1.833   1.900   1.920     282 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.244e+08 1.457e+08 4.159e+08 1.054e+09 1.054e+09       278 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
   7   10   13 NA's 
   7    3    3  278 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.2354  0.1700  0.5000     278 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   17.00   20.00   20.15   26.00   27.00     278 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  13  278 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   5.000   5.308   8.000   9.000     278 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   4.077   6.000   9.000     278 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.86    2.00    2.21   11.07    3.40   40.00     278 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  22   23 NA's 
   8    5  278 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
  13 13.3   20   80 NA's 
   2    5    3    3  278 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2013    2014     278 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.323e+08 1.457e+08 2.895e+08 1.678e+08 1.054e+09       260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   7    8   10   13 NA's 
   3    5   16    7  260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.140   0.165   0.500   0.389   0.500   0.500     260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0     8.0    16.0    16.0    22.5    27.0     260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  31  260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   5.000   5.742   8.000  10.000     260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.968   9.000  12.000     260 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.08    3.10   40.00   27.99   40.00   40.00     260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  22   23 NA's 
  10   21  260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  13   20   80 NA's 
   3    7   21  260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2013    2013    2014    2014     260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.34   20.30   79.00   79.63  105.20  187.00     276 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna                 NA's 
                  21                   10                  260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    w NA's 
  10   21  260 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.2707  0.5000  0.5000     276 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
  21  270 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
  no  yes NA's 
   5   16  270 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              31 
                                            NA's 
                                             260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
  31  260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                   repli-g                 repli-g x 
                       10                        16                         5 
                     NA's 
                      260 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.200   1.330   1.400   1.405   1.518   1.540     281 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    7.20    7.55    7.55    7.80    8.30     281 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method: 
picogreen   uv spec      NA's 
        2        13       276 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2 NA's 
  15    1  275 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode: 
tcga-b3-3925-11a-01x-1249-10 tcga-b3-4103-11a-01x-1249-10 
                           1                            1 
tcga-b3-4104-11a-01x-1249-10                         NA's 
                           1                          288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid: 
1af90e1e-c7e9-4049-8738-4407a3ae7520 eabffef5-99c5-4d72-b336-4a3e2c22c5fb 
                                   1                                    1 
f39680e6-08be-48e8-8ef7-91599e6b5247                                 NA's 
                                   1                                  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom: 
100512930 100512947 100512971      NA's 
        1         1         1       288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id: 
  10 NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration: 
 0.5 NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment: 
  20 NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment: 
   9 NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column: 
   4 NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id: 
1249 NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row: 
   c    e    f NA's 
   1    1    1  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity: 
  40 NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center: 
  22 NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume: 
  80 NA's 
   3  288 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
   3  288 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.184e+08 1.507e+08 3.646e+08 3.893e+08 1.054e+09       279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8   10 NA's 
   7    5  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
  12  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   12.00   19.00   16.42   20.00   28.00     279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  12  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   3    6    7    9 NA's 
   1    3    2    6  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    4.00    6.00    7.25    9.50   12.00     279 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
  12  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  22   23 NA's 
   7    5  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
  12  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2014     279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   77.0   100.0   162.0   138.7   170.5   191.0     284 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type: 
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                     9                      3                    279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id: 
   w    x NA's 
   9    3  279 



patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration: 
 0.5 NA's 
   7  284 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
  12  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
  no  yes NA's 
   7    5  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              12 
                                            NA's 
                                             279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
  12  279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
  repli-g repli-g x      NA's 
        9         3       279 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method: 
picogreen   uv spec      NA's 
        4         3       284 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence: 
   1 NA's 
   5  286 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio: 
 1.9    2 NA's 
   7    1  283 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.236e+08 1.507e+08 3.137e+08 1.641e+08 1.076e+09       259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
   1    5    8   10 NA's 
  10    1    5   16  259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.07000 0.08000 0.09812 0.14000 0.17000     259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   17.00   24.00   20.47   26.00   30.00     259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  32  259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   7.000   5.875   8.250  12.000     259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     6.0     8.0     7.5    10.0    12.0     259 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.070   2.800   2.407   3.200   3.600     259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  22   23 NA's 
  15   17  259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   40.00   31.87   40.00   60.00     259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2012    2012    2013    2014     259 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.136e+08 1.210e+08 1.807e+08 1.317e+08 1.050e+09       276 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   5    8   10   26 NA's 
   4    6    1    4  276 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1400  0.1033  0.1400  0.1600     276 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   10.00   17.00   16.07   18.50   25.00     276 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
  15  276 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   8.000   7.333   9.000  12.000     276 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   5.267  10.000  12.000     276 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   2.250   3.000   2.861   3.470   4.270     276 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  22   23 NA's 
  14    1  276 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
  10 26.7   40   60 NA's 
   4    4    1    6  276 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2010 2011 2012 NA's 
   3    5    7  276 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.005e+08 1.050e+09 6.320e+08 1.054e+09 1.054e+09       282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   5    8 NA's 
   5    4  282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
0.05 0.13 0.14 0.16 NA's 
   1    3    1    4  282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
  14   17   18   20 NA's 
   1    4    1    3  282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
   9  282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   1    2    8    9 NA's 
   1    1    4    3  282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   3    4    5   10 NA's 
   1    3    1    4  282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id: 
1252 1553 1963 2137 NA's 
   3    1    4    1  282 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.600   1.730   1.783   2.000   2.130     282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  22 NA's 
   9  282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
  10 13.3   40 NA's 
   3    5    1  282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2010 2011 2012 NA's 
   3    5    1  282 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode: 
tcga-b3-3925-11a-01d-1253-10 tcga-b3-4103-11a-01d-1253-10 
                           1                            1 
tcga-b3-4104-11a-01d-1253-10 tcga-b9-4115-11a-01d-1554-10 
                           1                            1 
                        NA's 
                         287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid: 
7c91f661-d4b2-439e-89d0-2f1987429d9b a540da82-d1a3-485a-9137-b513e2d8a766 
                                   1                                    1 
e10322c1-fe04-4645-bf35-def600c39fba ef4d3bff-f62f-4cf0-a538-f6ae615f22e1 
                                   1                                    1 
                                NA's 
                                 287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
 100513692  100513746  100513763 1049985704       NA's 
         1          1          1          1        287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
  10 NA's 
   4  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
0.05 0.14 0.16 NA's 
   1    1    2  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
  14   20 NA's 
   1    3  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
   4  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
   2    9 NA's 
   1    3  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
   3    4   10 NA's 
   1    2    1  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id: 
1253 1554 NA's 
   3    1  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row: 
   a    b    g NA's 
   1    1    2  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
0.94 1.07    2 NA's 
   1    2    1  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  22 NA's 
   4  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
 6.7   40 NA's 
   3    1  287 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2010 2011 NA's 
   3    1  287 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.236e+08 1.507e+08 3.137e+08 1.641e+08 1.076e+09       259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   1    2   26 NA's 
  22    8    2  259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1475  0.1500  0.1516  0.1600  0.1700     259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   16.25   24.00   20.12   26.00   30.00     259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  32  259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   7.000   5.938   8.250  12.000     259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     6.0     8.0     7.5    10.0    12.0     259 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.010   1.070   1.228   1.325   2.130     259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  22   23 NA's 
  15   17  259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.67  6.7   10 13.3 NA's 
   5   17    6    4  259 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2012    2012    2013    2014     259 

patient.samples.sample.3.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.62   23.11   33.67  128.29  214.95  531.94     259 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type: 
 dna NA's 
  32  259 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id: 
   d NA's 
  32  259 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1509  0.1600  0.1700     259 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  32  259 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
  no  yes NA's 
  14   18  259 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              32 
                                            NA's 
                                             259 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  32  259 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
adna preparation type                  NA's 
                   32                   259 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen   uv spec      NA's 
       24         8       259 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence: 
   1    2 NA's 
  15    1  275 



patient.samples.sample.3.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   11.75   15.00   17.28   26.00   31.00     259 

patient.samples.sample.3.portions.portion.is_ffpe: 
  no NA's 
  32  259 

patient.samples.sample.3.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   7.031  11.000  12.000     259 

patient.samples.sample.3.portions.portion.portion_number: 
   1   11   21 NA's 
  17   14    1  259 

patient.samples.sample.3.portions.portion.portion_sequence: 
   1    2 NA's 
  12    1  278 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_barcode: 
tcga-b3-3925-11a-01-ts1 tcga-b3-4103-11a-01-ts1 tcga-b3-4104-11a-01-ts1 
                      1                       1                       1 
                   NA's 
                    288 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_uuid: 
59c23c72-802f-4ad2-b35b-9075e3eeb2b3 9a56225a-cad5-46ad-aced-2c2e5756bfb7 
                                   1                                    1 
d2361968-53be-4e6a-89fb-c9a9a318b4c6                                 NA's 
                                   1                                  288 

patient.samples.sample.3.portions.portion.slides.slide.2.image_file_name: 
tcga-b3-3925-11a-01-ts1.59c23c72-802f-4ad2-b35b-9075e3eeb2b3.svs 
                                                               1 
tcga-b3-4103-11a-01-ts1.9a56225a-cad5-46ad-aced-2c2e5756bfb7.svs 
                                                               1 
tcga-b3-4104-11a-01-ts1.d2361968-53be-4e6a-89fb-c9a9a318b4c6.svs 
                                                               1 
                                                            NA's 
                                                             288 

patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe: 
  no NA's 
   3  288 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location: 
 top NA's 
   3  288 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
  31  260 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.500   2.000   7.684   4.000  90.000     272 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.263   1.000  10.000     272 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis: 
   0 NA's 
  16  275 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0    1 NA's 
  15    4  272 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  35.00   65.00   67.50   77.19  100.00  100.00     275 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   32.50   22.81   35.00   65.00     275 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells: 
   0 NA's 
  16  275 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei: 
   0 NA's 
  16  275 

patient.samples.sample.3.portions.portion.slides.slide.section_location: 
bottom    top   NA's 
     3     28    260 

patient.samples.sample.3.portions.portion.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   30.00   30.00   35.87   44.50   78.00     260 

patient.samples.sample.3.portions.portion.year_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2012    2012    2013    2014     259 

patient.samples.sample.3.sample_type: 
blood derived normal  solid tissue normal                 NA's 
                   1                   31                  259 

patient.samples.sample.3.sample_type_id: 
  10   11 NA's 
   1   31  259 

patient.samples.sample.3.shortest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.3000  0.4000  0.3933  0.5000  0.6000     276 

patient.samples.sample.3.vial_number: 
   a NA's 
  32  259 



patient.samples.sample.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0   134.8   419.5   849.9  1214.2  4013.0     139 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.10: 
1788b226-4334-4c00-afb7-02f64095c3ac c3e7fc54-10cf-478c-a1ba-cda00691fa82 
                                   1                                    1 
f49f9e52-bde3-4f0c-b4bd-eb6e25243736 f71d175f-5d9e-4bd2-a185-e0c1ca1e333e 
                                   1                                    1 
                                NA's 
                                 287 









patient.samples.sample.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    80.0   165.0   327.5   367.5  3450.0     117 

patient.samples.sample.intermediate_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.6000  0.8000  0.8376  1.0000  2.0000     174 

patient.samples.sample.is_ffpe: 
 no 
291 

patient.samples.sample.longest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   0.900   1.100   1.292   1.700   3.400     174 

patient.samples.sample.oct_embedded: 
false  true  NA's 
   79    95   117 



patient.samples.sample.portions.portion.2.analytes.analyte.2.a260_a280_ratio: 
 1.8 NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103928775 103928799 103928822 103928820 103928840 103928870       281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.center_id: 
   7 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
  18 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
  10 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   1    2 NA's 
   8    2  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
1351 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.quantity: 
4.01 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.volume: 
26.7 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103949703 103949733 103949762 103949758 103949776 103949798       281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.center_id: 
  13 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.concentration: 
0.15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
  18 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
  10 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_column: 
   1    2 NA's 
   8    2  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_id: 
1350 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.quantity: 
   3 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.volume: 
  20 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2010 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  156.3   194.3   240.3   243.8   274.1   403.3     281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type: 
 rna NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type_id: 
   r NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.2.concentration: 
0.14 0.15 0.16 NA's 
   3    2    5  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              10 
                                            NA's 
                                             281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                      NA's 
                       10                       281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.450   1.650   1.620   1.775   2.000     281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    7.40    8.05    7.95    8.25    9.50     281 

patient.samples.sample.portions.portion.2.analytes.analyte.2.spectrophotometer_method: 
uv spec    NA's 
     10     281 



patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103929086 103929110 103929122 103929120 103929135 103929157       281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.center_id: 
   8 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.concentration: 
 0.5 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment: 
  15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment: 
  11 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.plate_column: 
   1    2 NA's 
   6    4  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.plate_id: 
1400 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.quantity: 
  40 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.volume: 
  80 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08      281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.center_id: 
  10 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
  15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
  11 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_column: 
   1    2 NA's 
   6    4  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_id: 
1398 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2010 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  51.00   78.25   86.00   81.10   90.25  100.00     281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  10                  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type_id: 
   w NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.3.concentration: 
 0.5 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.pcr_amplification_successful: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              10 
                                            NA's 
                                             281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
     10     281 

patient.samples.sample.portions.portion.2.analytes.analyte.3.spectrophotometer_method: 
uv spec    NA's 
     10     281 



patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103927934 103927958 103927970 103927968 103927983 103928005       281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.center_id: 
   8 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.concentration: 
 0.5 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment: 
  15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment: 
  11 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_column: 
   1    2 NA's 
   6    4  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_id: 
1401 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.quantity: 
  40 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.volume: 
  80 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103927713 103928901 103928920 103928803 103928942 103928944       281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.center_id: 
  10 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.concentration: 
 0.5 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.day_of_shipment: 
  15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.month_of_shipment: 
  11 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_column: 
   1    2 NA's 
   6    4  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_id: 
1399 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.quantity: 
  40 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.volume: 
  80 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.year_of_shipment: 
2010 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   77.0    94.0    96.5    98.1   102.8   122.0     281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type: 
repli-g x (qiagen) dna                   NA's 
                    10                    281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type_id: 
   x NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.4.concentration: 
 0.5 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.normal_tumor_genotype_match: 
 yes NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.pcr_amplification_successful: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              10 
                                            NA's 
                                             281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.protocols.protocol.experimental_protocol_type: 
repli-g x      NA's 
       10       281 

patient.samples.sample.portions.portion.2.analytes.analyte.4.spectrophotometer_method: 
uv spec    NA's 
     10     281 

patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio: 
 1.8  1.9 NA's 
   1    9  281 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103928391 103928421 103928450 103928446 103928464 103928486       281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id: 
   1 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration: 
0.15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
  18 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
  10 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column: 
   1    2 NA's 
   8    2  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id: 
1348 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity: 
   1 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume: 
6.67 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103928295 103928325 103928354 103928350 103928368 103928390       281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id: 
   5 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration: 
0.15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
  18 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
  10 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column: 
   1    2 NA's 
   8    2  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id: 
1349 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity: 
4.01 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume: 
26.7 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2010 NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103928679 103928709 103928738 103928734 103928752 103928774       281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id: 
  10 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration: 
0.05 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
  15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
  11 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column: 
   1    2 NA's 
   8    2  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id: 
1402 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity: 
   2 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume: 
  40 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2010 NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08 1.04e+08      281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.center_id: 
   8 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.concentration: 
0.05 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
  15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
  11 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_column: 
   1    2 NA's 
   8    2  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_id: 
1403 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.quantity: 
   2 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.volume: 
  40 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2010 NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
103949511 103949541 103949570 103949566 103949584 103949606       281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id: 
   2 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration: 
0.15 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment: 
  18 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment: 
  10 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column: 
   1    2 NA's 
   8    2  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id: 
1347 NA's 
  10  281 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity: 
   2 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center: 
  22 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume: 
13.3 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2010 NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  127.1   152.1   181.1   190.5   211.6   305.1     281 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type: 
 dna NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id: 
   d NA's 
  10  281 



patient.samples.sample.portions.portion.2.analytes.analyte.concentration: 
0.14 0.15 0.16 NA's 
   1    7    2  281 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              10 
                                            NA's 
                                             281 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type: 
adna preparation type                  NA's 
                   10                   281 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method: 
uv spec    NA's 
     10     281 



patient.samples.sample.portions.portion.2.day_of_creation: 
   3   30 NA's 
   1    9  281 

patient.samples.sample.portions.portion.2.is_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.month_of_creation: 
   5   11 NA's 
   1    9  281 

patient.samples.sample.portions.portion.2.portion_number: 
   2 NA's 
  10  281 

patient.samples.sample.portions.portion.2.portion_sequence: 
   2 NA's 
   9  282 




patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_necrosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     2.5     3.8     5.0    15.0     281 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_normal_cells: 
   0    3 NA's 
   9    1  281 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     3.5     7.5     8.7    10.0    20.0     281 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  77.00   81.25   88.00   87.70   93.75   98.00     281 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_nuclei: 
  75   80   85   90 NA's 
   1    4    4    1  281 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location: 
bottom   NA's 
    10    281 




patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.5     3.0     3.4     4.5    10.0     281 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells: 
   0    5 NA's 
   9    1  281 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     5.0     9.0     9.3    10.0    20.0     281 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  78.00   81.25   86.50   86.50   90.75   98.00     281 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei: 
  75   80   85   90 NA's 
   1    5    3    1  281 

patient.samples.sample.portions.portion.2.slides.slide.section_location: 
 top NA's 
  10  281 

patient.samples.sample.portions.portion.2.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    20.0    38.0    33.6    40.0    70.0     281 

patient.samples.sample.portions.portion.2.year_of_creation: 
2010 2012 NA's 
   1    9  281 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.630   1.800   1.860   1.859   1.900   2.120      41 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.457e+08 1.641e+08 3.441e+08 1.678e+08 1.054e+09        15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
   7   13 NA's 
 131  145   15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1628  0.1700  0.1800      15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   17.00   18.00   19.04   25.25   30.00      15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
 276   15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    6.00    5.42    8.00   12.00      15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    3.75    5.00   11.00      15 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   2.130   3.200   2.961   3.600   4.540      15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  22   23 NA's 
 102  174   15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
  13 13.3   20 26.7 NA's 
  29   74  145   28   15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2013    2013    2014    2014      15 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
150752720 150757358 150757375 150756915 150757378 150757434       281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id: 
   7 NA's 
  10  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration: 
0.14 0.15 0.16 0.17 NA's 
   3    4    2    1  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment: 
   2 NA's 
  10  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
  10  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment: 
  10 NA's 
  10  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.25    4.00    3.90    5.75    7.00     281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id: 
a32z NA's 
  10  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row: 
   a    b    h NA's 
   5    4    1  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity: 
1.82 1.95 2.08 2.21 NA's 
   3    4    2    1  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center: 
  23 NA's 
  10  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume: 
  13 NA's 
  10  281 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment: 
2013 NA's 
  10  281 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.005e+08 1.290e+08 1.604e+08 2.897e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
  7  13 
160 131 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1619  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   17.00   18.00   19.35   25.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.553   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.911   5.000  11.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.820   2.210   2.600   2.697   3.100   4.270 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
 22  23 
117 174 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  13   20 26.7 
 145  131   15 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2010 2011 2012 2013 2014 
  16   59   52   71   93 

patient.samples.sample.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.92   39.83   56.18  106.67  149.40  520.20 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type: 
rna 
291 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id: 
  r 
291 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1619  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             291 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
   allprep rna extraction mirvana (allprep dna) rna 
                       15                       276 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.400   1.270   1.580   1.825   1.740  96.430 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.900   8.600   8.502   9.050  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec 
    291 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2 NA's 
 254   14   23 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode: 
tcga-b9-4617-01a-01w-1248-10                         NA's 
                           1                          290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid: 
0a17febf-8e13-421c-886c-b88ca1102bf2                                 NA's 
                                   1                                  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom: 
100511244      NA's 
        1       290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id: 
  10 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration: 
 0.5 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment: 
  20 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment: 
   9 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column: 
   3 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id: 
1248 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row: 
   c NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity: 
  40 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center: 
  22 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume: 
  80 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
   1  290 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.210e+08 1.507e+08 2.624e+08 1.678e+08 1.054e+09        49 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8   10   13 NA's 
  88  139   15   49 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.1  0.5 NA's 
  15  227   49 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    8.00   12.00   13.85   19.00   28.00      49 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 242   49 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   7.000   6.045   8.000  10.000      49 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.822   4.000   8.000      49 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
   3   40 NA's 
  15  227   49 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  22   23 NA's 
 103  139   49 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  30   80 NA's 
  15  227   49 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2013    2012    2014    2014      49 

patient.samples.sample.portions.portion.analytes.analyte.3.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0   162.0   173.0   177.8   182.5   589.0     188 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna            total rna                 NA's 
                 227                   15                   49 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id: 
   t    w NA's 
  15  227   49 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration: 
 0.1  0.5 NA's 
  15   88  188 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
 227   64 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
  no  yes NA's 
  88  139   64 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             242 
                                            NA's 
                                              49 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
 242   49 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
  repli-g repli-g x total rna      NA's 
      213        14        15        49 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method: 
picogreen   uv spec      NA's 
       60        43       188 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2 NA's 
 209   10   72 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100511197 100511216 100511234 100511339 100511248 100512972       275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id: 
  10 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration: 
 0.5 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment: 
  20 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment: 
   9 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column: 
   1    2    3 NA's 
   6    7    3  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id: 
1248 1249 NA's 
  15    1  275 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity: 
  40 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center: 
  22 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume: 
  80 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  16  275 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100512178 100512638 100512662 100512616 100512687 100513260       275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id: 
   8 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration: 
 0.5 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment: 
  20 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment: 
   9 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column: 
   1    2    3 NA's 
   6    7    3  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id: 
1246 1247 NA's 
  15    1  275 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity: 
  40 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center: 
  22 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume: 
  80 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment: 
2010 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  52.00   76.25   80.50   79.12   87.00  100.00     275 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type: 
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                    15                      1                    275 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id: 
   w    x NA's 
  15    1  275 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration: 
 0.5 NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match: 
 yes NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful: 
  no NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              16 
                                            NA's 
                                             275 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe: 
  no NA's 
  16  275 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type: 
  repli-g repli-g x      NA's 
       15         1       275 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method: 
uv spec    NA's 
     16     275 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100512367 100512928 100512951 100512916 100512974 100512997       276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id: 
  10 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration: 
 0.5 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment: 
  20 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment: 
   9 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column: 
   1    2    3 NA's 
   6    7    2  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id: 
1249 NA's 
  15  276 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity: 
  40 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center: 
  22 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume: 
  80 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment: 
2010 NA's 
  15  276 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100513213 100513226 100513240 100513264 100513264 100513556       276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id: 
   8 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration: 
 0.5 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment: 
  20 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment: 
   9 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column: 
   1    2    3 NA's 
   6    7    2  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_id: 
1247 NA's 
  15  276 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity: 
  40 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center: 
  22 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.volume: 
  80 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment: 
2010 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   63.0    74.0    80.0    81.6    89.5   106.0     276 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type: 
repli-g x (qiagen) dna                   NA's 
                    15                    276 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type_id: 
   x NA's 
  15  276 



patient.samples.sample.portions.portion.analytes.analyte.5.concentration: 
 0.5 NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match: 
 yes NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful: 
  no NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              15 
                                            NA's 
                                             276 

patient.samples.sample.portions.portion.analytes.analyte.5.is_derived_from_ffpe: 
  no NA's 
  15  276 

patient.samples.sample.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type: 
repli-g x      NA's 
       15       276 

patient.samples.sample.portions.portion.analytes.analyte.5.spectrophotometer_method: 
uv spec    NA's 
     15     276 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio: 
 1.9    2 NA's 
  52    5  234 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_barcode: 
tcga-b9-4115-01a-01d-1554-10                         NA's 
                           1                          290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_uuid: 
0ed63d84-d3fe-4289-9255-35f4a03b703b                                 NA's 
                                   1                                  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom: 
1049985699       NA's 
         1        290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id: 
  10 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration: 
0.05 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment: 
  14 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe: 
  no NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment: 
   2 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column: 
   5 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_id: 
1554 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_row: 
   g NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity: 
   2 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center: 
  22 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.volume: 
  40 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment: 
2011 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_barcode: 
tcga-b9-4115-01a-01d-a45y-10                         NA's 
                           1                          290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_uuid: 
1b7136cd-5298-48c7-ba4e-081f24b1702e                                 NA's 
                                   1                                  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.biospecimen_barcode_bottom: 
167811548      NA's 
        1       290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.center_id: 
  10 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.concentration: 
0.08 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.day_of_shipment: 
   1 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.is_derived_from_ffpe: 
  no NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.month_of_shipment: 
  10 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_column: 
   4 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_id: 
a45y NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_row: 
   a NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.quantity: 
1.12 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.source_center: 
  23 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.volume: 
  14 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.year_of_shipment: 
2014 NA's 
   1  290 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.005e+08 1.244e+08 1.603e+08 2.740e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  1   5   8  10 
103  10  14 164 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0500  0.0800  0.0800  0.1049  0.1500  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   17.00   18.00   19.35   25.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.553   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   3.000   3.137   5.000   8.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.800   1.010   2.800   2.333   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 22  23 
117 174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
6.67  6.7   13   40   60 
  87   16   10  164   14 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2010 2011 2012 2013 2014 
  16   59   52   71   93 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
100514754 113625018 145676835 141081521 164082938 174163487 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
  5   8  10 
194  87  10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0500  0.0500  0.1500  0.1212  0.1600  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   17.00   18.00   19.35   25.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   5.553   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   3.000   3.175   5.000   8.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.820   1.950   2.080   2.374   3.000   4.270 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
 22  23 
117 174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
  13 13.3 26.7   40   60 
 164   14   16   37   60 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2010 2011 2012 2013 2014 
  16   59   52   71   93 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.070e+08 1.244e+08 3.480e+08 1.752e+08 1.054e+09       159 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   5.000   8.318   8.000  26.000     159 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1400  0.1500  0.1452  0.1525  0.1700     159 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   17.00   13.74   18.00   21.00     159 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
 132  159 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   5.735   9.000  10.000     159 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.735   4.000  12.000     159 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.500   2.000   2.228   2.130   4.540     159 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  22   23 NA's 
 116   16  159 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   10.00   13.30   15.31   14.00   40.00     159 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2012    2014     159 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100513522 167811480 167811829 161744443 175141788 175154846       180 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
  10 NA's 
 111  180 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0800  0.0882  0.0800  0.1600     180 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
   1   20   21 NA's 
  94   16    1  180 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
 111  180 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
   1    9   10 NA's 
   1   16   94  180 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.387   9.000  12.000     180 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.490   1.010   1.120   1.074   1.120   1.260     180 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  22   23 NA's 
  16   95  180 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
 6.7    7 13.4   14 NA's 
  16    1    1   93  180 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2010 2014 2015 NA's 
  16   94    1  180 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.678e+08 1.678e+08 2.215e+08 1.678e+08 1.050e+09       278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id: 
   2    8   10 NA's 
   1    3    9  278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration: 
0.05 0.07 0.08 0.16 NA's 
   3    3    6    1  278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment: 
   1    7   14 NA's 
   9    3    1  278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe: 
  no NA's 
  13  278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment: 
   2   10 NA's 
   4    9  278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   5.154   7.000  12.000     278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id: 
1458 1547 a45y a460 NA's 
   3    1    8    1  278 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity: 
0.98 1.12    2 2.13 NA's 
   3    6    3    1  278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center: 
  22   23 NA's 
   4    9  278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume: 
13.3   14   40 NA's 
   1    9    3  278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment: 
2011 2014 NA's 
   4    9  278 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode: 
tcga-b3-3925-01a-01d-1459-10 tcga-b3-4103-01a-01d-1459-10 
                           1                            1 
tcga-b3-4104-01a-01d-1459-10 tcga-b9-4115-01a-01d-1549-01 
                           1                            1 
                        NA's 
                         287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid: 
3cdd68e9-1c52-4b9b-9dde-3e2191e0bb0b 6fbfdf8a-4f1f-4228-ab8d-a87461edf379 
                                   1                                    1 
864c5da1-5a02-4be9-ab42-34613136b536 e8db52a6-f970-4fc9-bdb5-63022dedf32f 
                                   1                                    1 
                                NA's 
                                 287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom: 
 103953168  103953221  103953245 1049930110       NA's 
         1          1          1          1        287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id: 
   1   10 NA's 
   1    3  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration: 
0.05 0.16 NA's 
   3    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment: 
   7   14 NA's 
   3    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe: 
  no NA's 
   4  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment: 
   2 NA's 
   4  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column: 
   4    7 NA's 
   3    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id: 
1459 1549 NA's 
   3    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row: 
   c    d    e    f NA's 
   1    1    1    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity: 
1.07    2 NA's 
   1    3  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center: 
  22 NA's 
   4  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume: 
6.67   40 NA's 
   1    3  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment: 
2011 NA's 
   4  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode: 
tcga-b3-3925-01a-01d-a45y-10 tcga-b3-4103-01a-01d-a45z-10 
                           1                            1 
tcga-b3-4104-01a-01d-a45y-10 tcga-b9-4115-01a-01d-1551-05 
                           1                            1 
                        NA's 
                         287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid: 
227bcea4-72fc-43d8-a896-bfb565d68a67 871dd304-af83-4199-a383-af2159d56694 
                                   1                                    1 
abd3e1f8-cd6e-4f6c-b45f-844943964524 e6c72b2c-0944-4285-95a4-86293412f71d 
                                   1                                    1 
                                NA's 
                                 287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom: 
 167811544  167811551  175141798 1049930014       NA's 
         1          1          1          1        287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id: 
   5   10 NA's 
   1    3  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration: 
0.07 0.08 0.16 NA's 
   1    2    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment: 
   1   14 NA's 
   3    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe: 
  no NA's 
   4  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment: 
   2   10 NA's 
   1    3  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column: 
   1    2    7    8 NA's 
   1    1    1    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id: 
1551 a45y a45z NA's 
   1    2    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row: 
   a    c    f NA's 
   2    1    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity: 
0.98 1.12 4.27 NA's 
   1    2    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center: 
  22   23 NA's 
   1    3  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume: 
  14 26.7 NA's 
   3    1  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment: 
2011 2014 NA's 
   1    3  287 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode: 
tcga-b9-4115-01a-01d-1553-08                         NA's 
                           1                          290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid: 
771e1379-958f-4f17-9e42-ad2c1afaa355                                 NA's 
                                   1                                  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom: 
1049985554       NA's 
         1        290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id: 
   8 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration: 
0.05 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment: 
  14 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe: 
  no NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment: 
   2 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column: 
   3 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id: 
1553 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row: 
   c NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity: 
   2 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center: 
  22 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume: 
  40 NA's 
   1  290 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment: 
2011 NA's 
   1  290 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.005e+08 1.244e+08 1.603e+08 2.740e+08 1.742e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  1   2  26 
188  75  28 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.13 0.14 0.15 0.16 0.17 
   9   40  112   91   39 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   10.00   18.00   17.86   25.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0     3.0     4.0     5.1     8.0    12.0 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   3.000   3.137   5.000   8.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.93    1.01    1.07    1.28    1.50    2.26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 22  23 
117 174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.67  6.7   10 13.3 
  14  174   60   43 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2010 2011 2012 2013 2014 
  16   59   52   55  109 

patient.samples.sample.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   26.05   49.49  137.66  183.20  993.00 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type: 
dna 
291 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id: 
  d 
291 



patient.samples.sample.portions.portion.analytes.analyte.concentration: 
0.13 0.14 0.15 0.16 0.17 
   8   40  112   92   39 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
291 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 no yes 
112 179 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             291 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
adna preparation type 
                  291 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen   uv spec 
      234        57 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence: 
   1    2 NA's 
 254   14   23 



patient.samples.sample.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   14.00   16.57   26.00   31.00 

patient.samples.sample.portions.portion.is_ffpe: 
 no 
291 

patient.samples.sample.portions.portion.lce: 
 lce NA's 
   4  287 

patient.samples.sample.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.619   9.000  12.000 

patient.samples.sample.portions.portion.portion_number: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   11.00   10.78   11.00   31.00 

patient.samples.sample.portions.portion.portion_sequence: 
   1    2    3 NA's 
  94    2    1  194 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe: 
  no NA's 
  84  207 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   2.263   2.500  10.000     272 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.500   5.000   8.158  15.000  30.000     272 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.976   1.000  20.000     207 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration: 
   0    5 NA's 
  17    1  273 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells: 
   0   10   20 NA's 
  76    1    2  212 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   5.000   5.000   9.488  12.750  40.000     211 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   81.00   90.00   88.04   95.00  100.00     212 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   80.00   80.00   81.82   85.00   95.00     207 

patient.samples.sample.portions.portion.slides.slide.2.section_location: 
bottom    top   NA's 
     1     83    207 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe: 
 no 
291 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.419   3.000  50.000      81 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   6.814   5.000 100.000      81 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.772   1.000  20.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4354  0.0000 30.0000      82 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4841  0.0000 60.0000       8 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    6.00   10.64   20.00   60.00       8 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  27.00   80.00   90.00   87.09  100.00  100.00       9 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   75.00   80.00   80.61   90.00  100.00       2 

patient.samples.sample.portions.portion.slides.slide.section_location: 
bottom    top 
    83    208 

patient.samples.sample.portions.portion.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.00   30.00   35.99   34.00  164.00       2 

patient.samples.sample.portions.portion.year_of_creation: 
2010 2011 2012 2013 2014 
  21   77   39   85   69 


patient.samples.sample.portions.shipment_portion.center_id: 
  20 NA's 
 215   76 

patient.samples.sample.portions.shipment_portion.is_ffpe: 
  no NA's 
 215   76 

patient.samples.sample.portions.shipment_portion.plate_id: 
a39n a39o a39p a43r NA's 
  80   80   47    8   76 

patient.samples.sample.portions.shipment_portion.portion_number: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   20.83   21.00   41.00      76 

patient.samples.sample.portions.shipment_portion.portion_sequence: 
   1    2 NA's 
   3  149  139 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment: 
   7    9 NA's 
 207    8   76 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment: 
   5    7 NA's 
 207    8   76 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment: 
2014 NA's 
 215   76 

patient.samples.sample.sample_type: 
primary tumor 
          291 

patient.samples.sample.sample_type_id: 
  1 
291 

patient.samples.sample.shortest_dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.3000  0.3000  0.3803  0.5000  0.9000     174 

patient.samples.sample.vial_number: 
  a   b 
290   1 



Exome.data.: 
  no  yes NA's 
   4  157  130 

CDE_ID.3104937: 
[Not Available]       [Unknown]          Type 1          Type 2            NA's 
             52              20              43              46             130 

tumor_type..KIRP.path..USE.THIS.: 
      Type 1 Papillary RCC       Type 2 Papillary RCC 
                        75                         60 
Unclassified Papillary RCC                       NA's 
                        26                        130 


CDE_ID.3203106: 
  N0   N1   N2   NX NA's 
  28   17    4  112  130 


Status: 
Alive  Dead  NA's 
  143    18   130 

time_of_follow.up: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   220.5   554.0   786.5   979.5  5925.0     132 

CDE_ID.2006657: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  28.00   52.00   60.00   59.86   69.00   85.00     132 

Largest.Dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.200   5.348   6.500  22.000     130 

Middle.Dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.700   3.900   4.602   6.000  17.000     162 

Smallest.Dimension: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   2.000   3.150   3.825   5.000  12.000     165 



Clinical.Spread..Distant.Metastases..M.: 
[Not submitted]              M0              M1              MX            NA's 
              6             116               5              22             142 






COCA..k.4..Katie.Hoadley.: 
        C1        C2a        C2b C2c - CIMP       NA's 
        95         35         22          9        130 

mutation.rate: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0e+00   0e+00   0e+00   0e+00   0e+00   1e-05     136 

X3p.copy: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.7902 -0.0050  0.0030  0.1245  0.2200  1.0019     130 

SETD2: 
   0    1 NA's 
 146   11  134 

PBRM1: 
   0    1 NA's 
 151    6  134 

BAP1: 
   0    1 NA's 
 152    5  134 

SETD2.BAP1.PBRM1: 
   0    1    2 NA's 
 146    9    6  130 

TFE3.fusion: 
   0    1 NA's 
 155    6  130 

CTSK: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.914   7.177   8.117   8.365   9.248  17.440     130 

BIRC7: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   3.013   4.652   4.861   6.679  12.674     130 

DIAPH1: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.90   11.76   12.09   12.07   12.39   13.14     130 

HIF1A: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  8.159  11.481  12.186  12.091  12.750  14.925     130 

CDKN2Ameth: 
   0    1 NA's 
 138   10  143 

CDKN2A.expr..RPKM.: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   1.274   23.608   54.486   95.591  102.888 1248.079      130 

CDKN2A.exon.1.expr..p16.: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.00000 0.06346 0.18544 0.38444 0.49198 4.54478     130 

CDKN2A.copy..log2.normal.: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1.2930 -0.0140 -0.0020 -0.0945  0.0020  0.1040     130 

CDKN2A.mutation: 
mutant_nonsil            wt          NA's 
            3           154           134 

DNA.copy.cluster..Murray.: 
                    CN_1:Genome Unstable, p16 deleted 
                                                   24 
                 CN_2:Arm level Alterations,chr7 gain 
                                                   97 
CN_3:Genome Stable, sporadic SCNAs, few p16 deletions 
                                                   40 
                                                 NA's 
                                                  130 

Meth.Cluster..Laird.group.: 
              CIMP DNA_meth_cluster:1 DNA_meth_cluster:2               NA's 
                 9                 30                122                130 

miRNA.clusters..4.group.NMF..Robertson.group.: 
miRNA cluster:1 miRNA cluster:2 miRNA cluster:3 miRNA cluster:4            NA's 
             87              16              28              30             130 

mRNA.clusters..3.group.NMF..Rathmell.group.: 
mRNA.Cluster 1 mRNA.Cluster 2 mRNA.Cluster 3           NA's 
            77             61             23            130 

RPPA.clusters..Rehan.Akbani.: 
RPPA cluster 1 RPPA cluster 2 RPPA cluster 3           NA's 
            20             41             64            166 


Nrf2.Keap1.mRNA.signature.score..Creighton.: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2.31043 -0.53529 -0.01245  0.04256  0.66684  2.45752      130 

NQO1.expr..RPKM.: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   76.35   448.74  1116.90  4541.67  3803.08 58630.41      130 


FH.expr..RPKM.: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  149.1  1269.4  1665.5  1744.2  2214.4  5107.5     130 


MET.mutation..germline.: 
     --- p.H1112R     NA's 
     158        3      130 

MET.copy..log2.normal.: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.5490  0.0000  0.4720  0.5623  0.8470  3.0820     130 

MET.expr..RPKM.: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1897    8112   11914   13949   16684   52388     130 

MET.splicing.event..Brad.Murray..Angela.Brooks.: 
 ---  yes NA's 
 153    8  130 

ARID1A: 
   0    1 NA's 
 153    8  130 

ARID2: 
   0    1 NA's 
 157    4  130 

BAP1.1: 
   0    1 NA's 
 156    5  130 

CUL3: 
   0    1 NA's 
 156    5  130 

DNMT3A: 
   0    1 NA's 
 157    4  130 

EP300: 
   0    1 NA's 
 155    6  130 

FAT1: 
   0    1 NA's 
 150   11  130 

KDM6A: 
   0    1 NA's 
 155    6  130 

KRAS: 
   0    1 NA's 
 159    2  130 

MET: 
   0    1 NA's 
 146   15  130 

MLL2: 
   0    1 NA's 
 145   16  130 

MLL3: 
   0    1 NA's 
 149   12  130 

MLL4: 
   0    1 NA's 
 159    2  130 

NF2: 
   0    1 NA's 
 151   10  130 

PBRM1.1: 
   0    1 NA's 
 156    5  130 

PTEN: 
   0    1 NA's 
 156    5  130 

SETD2.1: 
   0    1 NA's 
 151   10  130 

SMARCB1: 
   0    1 NA's 
 157    4  130 

TP53: 
   0    1 NA's 
 158    3  130 

NF2.1: 
   0    1 NA's 
 152    9  130 

c.Met_pY1235.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.37414 -0.12776 -0.04864 -0.03687  0.04130  0.34164      166 

mTOR_pS2448.R.C: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1.2926 -0.2203 -0.0279 -0.0386  0.1847  0.6727     166 

YAP.R.E: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.41741 -0.07253  0.00991  0.04345  0.14054  0.80517      166 

YAP_pS127.R.E: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.44148 -0.39185 -0.07787 -0.01682  0.39229  1.83260      166 

ATM.R.E: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.50445 -0.51441  0.00000 -0.02891  0.46486  1.40858      166 

B.Raf.M.C: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.36858 -0.24933  0.06809  0.02997  0.26456  1.04354      166 

Bap1c.4.M.E: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.96306 -0.16900  0.02212 -0.03777  0.16480  0.61440      166 

beta.Catenin.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2.71427 -0.52197 -0.08612 -0.17143  0.27388  0.92143      166 

c.Kit.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.62881 -0.12127  0.05203  0.11186  0.21609  2.79296      166 

c.Met_pY1235.R.V.1: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.37414 -0.12776 -0.04864 -0.03687  0.04130  0.34164      166 

Cyclin_D1.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.96995 -0.39788 -0.08557 -0.05606  0.27017  1.30613      166 

E.Cadherin.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.95409 -0.52456  0.02451  0.03804  0.55440  2.29837      166 

EGFR.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.68005 -0.21704 -0.10422 -0.06477  0.06893  0.69535      166 

EGFR_pY1068.R.C: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.92918 -0.18918  0.04147  0.13124  0.38052  1.44143      166 

EGFR_pY1173.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.28931 -0.09972 -0.00348  0.00522  0.07683  0.59254      166 

GATA3.M.V: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.6113 -0.1008  0.1056  0.4879  0.9995  3.7825     166 

HER2.M.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.35184 -0.47322 -0.09677 -0.02397  0.50763  1.47757      166 

HER2_pY1248.R.C: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.50514 -0.12247  0.01913  0.05135  0.19168  0.85055      166 

HER3.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.25925 -0.50687  0.03501  0.01683  0.44931  1.88497      166 

HER3_pY1289.R.C: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.23571 -0.09465 -0.02651 -0.00666  0.07302  0.37198      166 

LKB1.M.E: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.47430 -0.11421  0.03884  0.03012  0.17544  0.88458      166 

MEK1.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.33866 -0.25511  0.10881  0.08382  0.47825  1.43108      166 

MEK1_pS217_S221.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.36769 -0.12031 -0.05230 -0.01964  0.08398  0.52760      166 

N.Ras.M.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.44738 -0.09303  0.10029  0.10538  0.24749  0.84676      166 

Notch1.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.39334 -0.10937  0.04655  0.06161  0.19791  1.43284      166 

p27.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.60171 -0.13513  0.01237  0.05836  0.22163  0.80199      166 

p27_pT157.R.C: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.25430 -0.10233 -0.02294 -0.00181  0.08912  0.48142      166 

p27_pT198.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.37698 -0.03844  0.05126  0.06777  0.16298  0.63016      166 

p53.R.E: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.60717 -0.11863  0.06964  0.16998  0.28937  1.42934      166 

PI3K.p110.alpha.R.C: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.71770 -0.10516 -0.02244 -0.03103  0.08442  0.47357      166 

PI3K.p85.R.V: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1.4047 -0.4065 -0.0840 -0.1256  0.1578  1.4917     166 

PTEN.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.36098 -0.26268 -0.03146 -0.07138  0.21487  0.96325      166 

Rb.M.QC: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.35582 -0.07866  0.02666  0.05330  0.13117  0.79715      166 

Rb_pS807_S811.R.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.83576 -0.32531 -0.02415  0.01706  0.22410  3.26631      166 

Smad4.M.V: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.58803 -0.08165  0.06643  0.09977  0.26038  0.84951      166 

TSC1.R.C: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.39109 -0.31343 -0.04545 -0.09643  0.22909  0.74405      166 

VHL.M.C: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.90047 -0.88448 -0.05037 -0.12740  0.44991  2.10064      166 

NF2.R.C: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-2.43770 -0.31225  0.00000 -0.02831  0.28318  1.58542      166 

ATM: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1.12200 -0.01100 -0.00300 -0.04086  0.00000  0.80900      130 

BRAF: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.5490  0.0000  0.4720  0.5404  0.8430  2.5410     130 

BAP1.2: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.8220 -0.0050  0.0030  0.1209  0.2200  1.0020     130 

CTNNB1: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.8540 -0.0060  0.0030  0.1209  0.1220  1.0020     130 

KIT: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.84500 -0.00700  0.00000 -0.03361  0.00500  0.94600      130 

MET.1: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.5490  0.0000  0.4720  0.5623  0.8470  3.0820     130 

CCND1: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.87000 -0.01200 -0.00300 -0.02417  0.00000  0.56500      130 

CDH1: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.5770  0.0020  0.3310  0.3629  0.6800  1.7040     130 

EGFR: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.5490  0.0000  0.5160  0.5313  0.8260  2.0690     130 

GATA3: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.90900 -0.01700 -0.00900 -0.03148 -0.00300  0.83200      130 

ERBB2: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.0480  0.0410  0.5980  0.5111  0.7760  3.6570     130 

ERBB3: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.0350 -0.0020  0.0070  0.2728  0.6190  1.4900     130 

STK11: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.80700 -0.06500 -0.03000 -0.05067 -0.00100  0.46000      130 

MAP2K1: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.84200 -0.02100 -0.00600 -0.04755  0.00200  0.47600      130 

NRAS: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.90200 -0.01300 -0.00400 -0.08484  0.00000  0.14600      130 

NOTCH1: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.9030 -0.0240 -0.0040 -0.0592  0.0010  0.1590     130 

CDKN1B: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.5460 -0.0010  0.0080  0.2765  0.6530  1.4900     130 

TP53.1: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.8410  0.0150  0.5300  0.4038  0.7450  1.8980     130 

PIK3CA: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.5640  0.0000  0.0090  0.2128  0.5770  0.9960     130 

PTEN.1: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.8140 -0.0140 -0.0080 -0.0316 -0.0030  0.8280     130 

RB1: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.83400 -0.00500  0.00100  0.01904  0.00800  1.02700      130 

SMAD4: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1.1120 -0.0120  0.0010 -0.0728  0.0090  0.8880     130 

TSC1: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-0.90300 -0.02100 -0.00300 -0.06219  0.00100  0.15900      130 

VHL: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-0.8540 -0.0050  0.0020  0.1235  0.2200  1.0020     130 

NF2.2: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1.0600 -0.1010 -0.0400 -0.1507 -0.0140  0.1400     130 

}}
\keyword{datasets}
