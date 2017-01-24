\name{ THCA }
\alias{ THCA }
\docType{data}
\title{ THCA }

\details{
\preformatted{
> THCA
A MultiAssayExperiment object of 5 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 5: 
 [1] miRNASeqGene: ExpressionSet with 1046 rows and 569 columns 
 [2] Methylation: SummarizedExperiment with 485577 rows and 567 columns 
 [3] RPPAArray: ExpressionSet with 175 rows and 224 columns 
 [4] gistica: SummarizedExperiment with 24776 rows and 499 columns 
 [5] gistict: SummarizedExperiment with 24776 rows and 499 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( THCA )
CharacterList of length 5
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_epsilon 4E-BP1 ... p70S6K_pT389 p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( THCA )
CharacterList of length 5
[["miRNASeqGene"]] TCGA-4C-A93U-01A-11R-A39B-13 ...
[["Methylation"]] TCGA-4C-A93U-01A-11D-A398-05 ... TCGA-QD-A8IV-01A-11D-A398-05
[["RPPAArray"]] TCGA-BJ-A0YZ-01A-21-A21L-20 ... TCGA-J8-A3O1-01A-21-A21M-20
[["gistica"]] TCGA-4C-A93U-01A-11D-A396-01 ... TCGA-QD-A8IV-01A-11D-A396-01
[["gistict"]] TCGA-4C-A93U-01A-11D-A396-01 ... TCGA-QD-A8IV-01A-11D-A396-01

Sizes of each ExperimentList element:

         assay size.Mb
1 miRNASeqGene  0.2 Mb
2  Methylation 67.5 Mb
3    RPPAArray  0.1 Mb
4      gistica  4.4 Mb
5      gistict  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   487 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  16.00   16.00    2.80    2.23    4.80 


--------------------------- 
Available sample meta-data: 
--------------------------- 


years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.00   35.00   46.00   47.26   58.00   89.00 

vital_status: 
  0   1 
487  16 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    174     743    1021    1176    1631    2973     487 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     519     931    1207    1512    5423      14 

tumor_tissue_site: 
thyroid 
    503 



pathology_N_stage: 
 n0  n1 n1a n1b  nx 
227  58  93  75  50 

pathology_M_stage: 
  m0   m1   mx NA's 
 280    9  213    1 

gender.x: 
female   male 
   368    135 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1993    2008    2010    2009    2011    2013 

radiation_therapy: 
  no  yes NA's 
 181  306   16 

histological_type.x: 
                                                        other  specify 
                                                                     7 
                         thyroid papillary carcinoma - classical/usual 
                                                                   358 
thyroid papillary carcinoma - follicular (>= 99% follicular patterned) 
                                                                   102 
   thyroid papillary carcinoma - tall cell (>= 50% tall cell features) 
                                                                    36 

radiation_exposure: 
  no  yes NA's 
 423   17   63 

extrathyroidal_extension: 
           minimal (t3) moderate/advanced (t4a)                    none 
                    133                      18                     333 
    very advanced (t4b)                    NA's 
                      1                      18 

residual_tumor: 
  r0   r1   r2   rx NA's 
 385   52    4   30   32 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   3.658   5.000  41.000     114 

multifocality: 
multifocal   unifocal       NA's 
       227        266         10 

tumor_size: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.700   2.600   2.975   4.000   8.200     100 

race.x: 
american indian or alaska native                            asian 
                               1                               52 
       black or african american                            white 
                              27                              331 
                            NA's 
                              92 

ethnicity.x: 
    hispanic or latino not hispanic or latino                   NA's 
                    38                    362                    103 

admin.bcr: 
nationwide children's hospital 
                           503 

admin.day_of_dcc_upload: 
  1 
503 

admin.disease_code: 
thca 
 503 


admin.month_of_dcc_upload: 
 10 
503 

admin.patient_withdrawal.withdrawn: 
false 
  503 

admin.project_code: 
tcga 
 503 

admin.year_of_dcc_upload: 
2015 
 503 

patient.age_at_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.00   35.00   46.00   47.27   58.00   89.00 



patient.clinical_cqcf.anatomic_neoplasm_subdivision: 
 left right  NA's 
  198   263    42 

patient.clinical_cqcf.consent_or_death_status: 
consented  deceased 
      490        13 

patient.clinical_cqcf.country: 
       canada        russia united states       vietnam 
           80             4           399            20 

patient.clinical_cqcf.days_to_consent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-256.00  -13.00    4.00   51.41   42.75 3999.00      13 

patient.clinical_cqcf.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    174     813    1385    1272    1734    2973     490 

patient.clinical_cqcf.histological_type: 
                                       classical/usual (papillary nos) 
                                                                   357 
                                                        other  specify 
                                                                     7 
thyroid papillary carcinoma - follicular (>= 99% follicular patterned) 
                                                                   103 
   thyroid papillary carcinoma - tall cell (>= 50% tall cell features) 
                                                                    36 

patient.clinical_cqcf.history_of_neoadjuvant_treatment: 
                               no yes, radiation prior to resection 
                              499                                 4 

patient.clinical_cqcf.normal_tissue_anatomic_site: 
        other thyroid gland          NA's 
            9           104           390 

patient.clinical_cqcf.normal_tissue_proximity: 
distal (>2cm)          NA's 
          105           398 


patient.clinical_cqcf.tumor_focality: 
multifocal   unifocal       NA's 
       230        272          1 

patient.clinical_cqcf.tumor_type: 
primary 
    503 

patient.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0    10.0    14.0    15.7    21.0    31.0 

patient.days_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -32682  -21272  -17115  -17446  -12918   -5612 

patient.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  174.0   850.5  1204.0  1253.9  1699.8  2973.0     489 

patient.days_to_initial_pathologic_diagnosis: 
  0 
503 

patient.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -210.0   130.0   262.0   586.8   578.0  4484.0      14 



patient.drugs.drug.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     5.0    17.5    13.0    18.0    28.0     491 

patient.drugs.drug.2.days_to_drug_therapy_end: 
  56 NA's 
   1  502 

patient.drugs.drug.2.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   47.75   75.00   92.75   97.25  267.00     491 

patient.drugs.drug.2.drug_name: 
      cytomel levothyroxine       levoxyl     synthroid          NA's 
            1             4             2             5           491 

patient.drugs.drug.2.measure_of_response: 
complete response              NA's 
                1               502 

patient.drugs.drug.2.month_of_form_completion: 
   1    2   10   12 NA's 
   1    6    4    1  491 

patient.drugs.drug.2.therapy_ongoing: 
  no  yes NA's 
   1   10  492 

patient.drugs.drug.2.therapy_types.therapy_type: 
hormone therapy            NA's 
             12             491 

patient.drugs.drug.2.tx_on_clinical_trial: 
  no NA's 
  12  491 

patient.drugs.drug.2.year_of_form_completion: 
2012 2013 2014 NA's 
   4    7    1  491 

patient.drugs.drug.3.bcr_drug_barcode: 
tcga-e3-a3dz-d35668                NA's 
                  1                 502 

patient.drugs.drug.3.bcr_drug_uuid: 
c3a97780-4ecf-4cb9-a66a-847cf6fc9265                                 NA's 
                                   1                                  502 

patient.drugs.drug.3.day_of_form_completion: 
   5 NA's 
   1  502 

patient.drugs.drug.3.days_to_drug_therapy_start: 
  59 NA's 
   1  502 

patient.drugs.drug.3.drug_name: 
synthroid      NA's 
        1       502 

patient.drugs.drug.3.month_of_form_completion: 
  10 NA's 
   1  502 

patient.drugs.drug.3.therapy_ongoing: 
 yes NA's 
   1  502 

patient.drugs.drug.3.therapy_types.therapy_type: 
hormone therapy            NA's 
              1             502 

patient.drugs.drug.3.tx_on_clinical_trial: 
  no NA's 
   1  502 

patient.drugs.drug.3.year_of_form_completion: 
2012 NA's 
   1  502 



patient.drugs.drug.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    12.5    18.0    19.0    28.0    31.0     460 

patient.drugs.drug.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0    47.0    71.0   104.9   116.0   322.0     490 

patient.drugs.drug.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    31.0    70.0   113.3   155.5   761.0     460 


patient.drugs.drug.measure_of_response: 
complete response    stable disease              NA's 
               10                 1               492 

patient.drugs.drug.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.279   5.500  10.000     460 

patient.drugs.drug.prescribed_dose: 
 112  150 NA's 
   1    1  501 

patient.drugs.drug.prescribed_dose_units: 
  ug NA's 
   2  501 

patient.drugs.drug.regimen_indication: 
adjuvant     NA's 
       2      501 

patient.drugs.drug.regimen_number: 
   1 NA's 
   2  501 

patient.drugs.drug.route_of_administrations.route_of_administration: 
oral NA's 
   2  501 

patient.drugs.drug.therapy_ongoing: 
  no  yes NA's 
  15   27  461 

patient.drugs.drug.therapy_types.therapy_type: 
   chemotherapy hormone therapy            NA's 
              1              41             461 

patient.drugs.drug.tx_on_clinical_trial: 
  no NA's 
  41  462 

patient.drugs.drug.year_of_form_completion: 
2012 2013 2014 2015 NA's 
  10   17   15    1  460 

patient.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    38                    362                    103 

patient.extrathyroid_carcinoma_present_extension_status: 
           minimal (t3) moderate/advanced (t4a)                    none 
                    133                      18                     333 
    very advanced (t4b)                    NA's 
                      1                      18 

patient.first_degree_relative_history_thyroid_gland_carcinoma_diagnosis_relationship_types.first_degree_relative_history_thyroid_gland_carcinoma_diagnosis_relationship_type: 
  child  parent sibling    NA's 
      1      14      10     478 

patient.first_degree_relative_history_thyroid_gland_carcinoma_diagnosis_relationship_types.first_degree_relative_history_thyroid_gland_carcinoma_diagnosis_relationship_type.2: 
child  NA's 
    1   502 



patient.follow_ups.follow_up.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    12.0    24.0    20.9    28.0    31.0     329 

patient.follow_ups.follow_up.2.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     15    1007    1251    1538    1972    5150     333 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason: 
    additional new tumor event scheduled follow-up submission 
                             1                              1 
                          NA's 
                           501 


patient.follow_ups.follow_up.2.i_131_subsequent_administered_dose: 
     100 29.9 mci       no     NA's 
       1        1        1      500 


patient.follow_ups.follow_up.2.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                   rhtsh 
                                     52                                      42 
                   thyroxine withdrawal                                    NA's 
                                     31                                     378 

patient.follow_ups.follow_up.2.lost_follow_up: 
  no  yes NA's 
 162   10  331 

patient.follow_ups.follow_up.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   5.414   9.000  12.000     329 

patient.follow_ups.follow_up.2.new_tumor.additional_pharmaceutical_therapy: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor.additional_radiation_therapy: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor.additional_surgery_locoregional_procedure: 
 yes NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor.days_to_additional_surgery_metastatic_procedure: 
 693 NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor.days_to_new_tumor_event_after_initial_treatment: 
 693 NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor.histologic_disease_progression_present_indicator: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                    NA's 
                                      1                                     502 

patient.follow_ups.follow_up.2.new_tumor.new_neoplasm_confirmed_diagnosis_method_name: 
pathology      NA's 
        1       502 

patient.follow_ups.follow_up.2.new_tumor.new_neoplasm_event_occurrence_anatomic_site: 
lymph node only            NA's 
              1             502 

patient.follow_ups.follow_up.2.new_tumor.new_neoplasm_event_type: 
locoregional recurrence                    NA's 
                      1                     502 

patient.follow_ups.follow_up.2.new_tumor.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
   1    1  501 

patient.follow_ups.follow_up.2.new_tumor.radiation_therapy_administered_preparation_technique_text: 
patient did not receive radiation therapy 
                                        1 
                                     NA's 
                                      502 

patient.follow_ups.follow_up.2.new_tumor.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
surgery (distant metastasis) or surgery (loco-regional) 
                                                      1 
                                                   NA's 
                                                    502 

patient.follow_ups.follow_up.2.new_tumor.thyroid_gland_carcinoma_involvement_regional_lymph_node_type: 
lateral (levels 2-5)                 NA's 
                   1                  502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy: 
  no NA's 
   2  501 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy: 
  no NA's 
   2  501 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure: 
1249 NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
1135 2072 NA's 
   1    1  501 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.histologic_disease_progression_present_indicator: 
  no NA's 
   2  501 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_confirmed_diagnosis_method_name: 
  imaging pathology      NA's 
        1         1       501 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site: 
           lung lymph node only            NA's 
              1               1             501 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence                    NA's 
                      1                       1                     501 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
surgery (distant metastasis) or surgery (loco-regional) 
                                                      1 
                                                   NA's 
                                                    502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.thyroid_gland_carcinoma_involvement_regional_lymph_node_type: 
lateral (levels 2-5)                 NA's 
                   1                  502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no  yes NA's 
  10    2  491 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no  yes NA's 
  10    2  491 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  359.0   498.5   759.5  1026.6  1234.0  2382.0     493 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0   385.0   720.0   783.8  1062.0  2093.0     490 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.histologic_disease_progression_present_indicator: 
  no  yes NA's 
   9    1  493 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.histologic_disease_progression_present_type: 
poorly differentiated                  NA's 
                    1                   502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                    NA's 
                                      4                                     499 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name: 
  imaging pathology      NA's 
        3         9       491 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site: 
           lung lymph node only  other, specify     soft tissue            NA's 
              3               6               1               1             492 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
biochemical evidence of disease              distant metastasis 
                              1                               3 
        locoregional recurrence               new primary tumor 
                              7                               2 
                           NA's 
                            490 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text: 
cecum  NA's 
    1   502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
   3    9  491 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.radiation_therapy_administered_dose_text: 
  50 NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.radiation_therapy_administered_preparation_technique_text: 
                                              imrt 
                                                 1 
patient did not receive external radiation therapy 
                                                 3 
                                              NA's 
                                               499 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.radiosensitizing_agent_administered_indicator: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
surgery (distant metastasis) or surgery (loco-regional) 
                                                     10 
                                                   NA's 
                                                    493 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.therapeutic_procedure_new_neoplasm_required_additional_therapy_type.2: 
                  ebrt pharmaceutical therapy            rai therapy 
                     1                      1                      1 
                  NA's 
                   500 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.therapeutic_procedure_new_neoplasm_required_additional_therapy_type.3: 
pharmaceutical therapy                   NA's 
                     1                    502 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.thyroid_gland_carcinoma_involvement_regional_lymph_node_type: 
central (levels 6-7) lateral (levels 2-5)                 NA's 
                   3                    3                  497 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 146   13  344 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       144          9        350 

patient.follow_ups.follow_up.2.post_surgical_procedure_assessment_thyroid_gland_carcinoma_status: 
 no imaging evidence of disease persistent locoregional disease 
                            115                               7 
                           NA's 
                            381 

patient.follow_ups.follow_up.2.postoperative_rx_tx: 
  no  yes NA's 
 160    7  336 

patient.follow_ups.follow_up.2.radiation_therapy: 
  no  yes NA's 
  71   99  333 

patient.follow_ups.follow_up.2.radiation_therapy_administered_dose_text: 
60 gy    66  66gy  NA's 
    1     1     1   500 

patient.follow_ups.follow_up.2.radiation_therapy_administered_preparation_technique_text: 
                                              imrt 
                                                 3 
patient did not receive external radiation therapy 
                                               128 
         patient did not receive radiation therapy 
                                                 1 
                                              NA's 
                                               371 

patient.follow_ups.follow_up.2.radiosensitizing_agent_administered_indicator: 
  no NA's 
   3  500 

patient.follow_ups.follow_up.2.targeted_molecular_therapy: 
  no NA's 
   2  501 

patient.follow_ups.follow_up.2.vital_status: 
alive  NA's 
  170   333 

patient.follow_ups.follow_up.2.year_of_form_completion: 
2012 2013 2014 2015 NA's 
   5   19   71   79  329 



patient.follow_ups.follow_up.3.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   24.00   24.00   22.69   25.00   25.00     490 

patient.follow_ups.follow_up.3.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    911    1653    2581    2609    3431    5068     490 


patient.follow_ups.follow_up.3.i_131_subsequent_administered_dose: 
100 mci    NA's 
      1     502 


patient.follow_ups.follow_up.3.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                   rhtsh 
                                      4                                       4 
                   thyroxine withdrawal                                    NA's 
                                      5                                     490 

patient.follow_ups.follow_up.3.lost_follow_up: 
  no NA's 
  13  490 

patient.follow_ups.follow_up.3.month_of_form_completion: 
   3    8   11 NA's 
  11    1    1  490 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.additional_radiation_therapy: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure: 
1249 NA's 
   1  502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
1249 NA's 
   1  502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.histologic_disease_progression_present_indicator: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_neoplasm_confirmed_diagnosis_method_name: 
pathology      NA's 
        1       502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site: 
lymph node only            NA's 
              1             502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
locoregional recurrence                    NA's 
                      1                     502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure: 
 yes NA's 
   1  502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure: 
 659  786 NA's 
   1    1  501 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
 659  786 NA's 
   1    1  501 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.histologic_disease_progression_present_indicator: 
  no NA's 
   2  501 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name: 
pathology      NA's 
        2       501 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site: 
lymph node only            NA's 
              2             501 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
locoregional recurrence                    NA's 
                      2                     501 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure: 
 yes NA's 
   2  501 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
surgery (distant metastasis) or surgery (loco-regional) 
                                                      2 
                                                   NA's 
                                                    501 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.thyroid_gland_carcinoma_involvement_regional_lymph_node_type: 
central (levels 6-7) lateral (levels 2-5)                 NA's 
                   1                    1                  501 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
  11    2  490 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status: 
tumor free       NA's 
        13        490 

patient.follow_ups.follow_up.3.post_surgical_procedure_assessment_thyroid_gland_carcinoma_status: 
 no imaging evidence of disease persistent locoregional disease 
                             11                               2 
                           NA's 
                            490 

patient.follow_ups.follow_up.3.postoperative_rx_tx: 
  no NA's 
  13  490 

patient.follow_ups.follow_up.3.radiation_therapy: 
  no  yes NA's 
   4    9  490 

patient.follow_ups.follow_up.3.radiation_therapy_administered_preparation_technique_text: 
patient did not receive external radiation therapy 
                                                12 
                                              NA's 
                                               491 

patient.follow_ups.follow_up.3.vital_status: 
alive  NA's 
   13   490 

patient.follow_ups.follow_up.3.year_of_form_completion: 
2014 2015 NA's 
   2   11  490 

patient.follow_ups.follow_up.4.bcr_followup_barcode: 
tcga-et-a3dt-f71400                NA's 
                  1                 502 

patient.follow_ups.follow_up.4.bcr_followup_uuid: 
6bbe07f2-0512-4927-8c6f-2d37c0aa582d                                 NA's 
                                   1                                  502 

patient.follow_ups.follow_up.4.day_of_form_completion: 
  25 NA's 
   1  502 

patient.follow_ups.follow_up.4.days_to_last_followup: 
1330 NA's 
   1  502 

patient.follow_ups.follow_up.4.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                    NA's 
                                      1                                     502 

patient.follow_ups.follow_up.4.lost_follow_up: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.4.month_of_form_completion: 
   3 NA's 
   1  502 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status: 
tumor free       NA's 
         1        502 

patient.follow_ups.follow_up.4.post_surgical_procedure_assessment_thyroid_gland_carcinoma_status: 
no imaging evidence of disease                           NA's 
                             1                            502 

patient.follow_ups.follow_up.4.postoperative_rx_tx: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.4.radiation_therapy: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.4.radiation_therapy_administered_preparation_technique_text: 
patient did not receive external radiation therapy 
                                                 1 
                                              NA's 
                                               502 

patient.follow_ups.follow_up.4.vital_status: 
alive  NA's 
    1   502 

patient.follow_ups.follow_up.4.year_of_form_completion: 
2015 NA's 
   1  502 



patient.follow_ups.follow_up.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   18.00   17.64   25.00   31.00      27 

patient.follow_ups.follow_up.days_to_death: 
 243 1011 1023 NA's 
   1    1    1  500 

patient.follow_ups.follow_up.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   422.5   621.5   961.7  1106.0  5423.0      39 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason: 
scheduled follow-up submission                           NA's 
                           100                            403 




patient.follow_ups.follow_up.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                   rhtsh 
                                    149                                      88 
                   thyroxine withdrawal                                    NA's 
                                     95                                     171 

patient.follow_ups.follow_up.lost_follow_up: 
  no  yes NA's 
 347   29  127 

patient.follow_ups.follow_up.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.063   9.000  12.000      27 

patient.follow_ups.follow_up.new_tumor.additional_pharmaceutical_therapy: 
  no  yes NA's 
   7    1  495 

patient.follow_ups.follow_up.new_tumor.additional_radiation_therapy: 
  no  yes NA's 
   6    1  496 

patient.follow_ups.follow_up.new_tumor.additional_surgery_locoregional_procedure: 
  no  yes NA's 
   1    7  495 

patient.follow_ups.follow_up.new_tumor.days_to_additional_surgery_metastatic_procedure: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  232.0   487.5   526.0   584.6   572.0  1215.0     496 

patient.follow_ups.follow_up.new_tumor.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  158.0   444.0   509.0   558.6   574.5  1215.0     493 

patient.follow_ups.follow_up.new_tumor.histologic_disease_progression_present_indicator: 
  no  yes NA's 
   6    1  496 

patient.follow_ups.follow_up.new_tumor.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                    NA's 
                                      7                                     496 

patient.follow_ups.follow_up.new_tumor.new_neoplasm_confirmed_diagnosis_method_name: 
  imaging pathology      NA's 
        3         6       494 

patient.follow_ups.follow_up.new_tumor.new_neoplasm_event_occurrence_anatomic_site: 
           bone            lung lymph node only            NA's 
              1               1               7             494 

patient.follow_ups.follow_up.new_tumor.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence                    NA's 
                      2                       7                     494 

patient.follow_ups.follow_up.new_tumor.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
  90   10  403 

patient.follow_ups.follow_up.new_tumor.radiation_therapy_administered_dose_text: 
6400 cgy     NA's 
       1      502 

patient.follow_ups.follow_up.new_tumor.radiation_therapy_administered_preparation_technique_text: 
                                     imrt 
                                        1 
patient did not receive radiation therapy 
                                        6 
                                     NA's 
                                      496 

patient.follow_ups.follow_up.new_tumor.radiosensitizing_agent_administered_indicator: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
surgery (distant metastasis) or surgery (loco-regional) 
                                                      6 
                                                   NA's 
                                                    497 

patient.follow_ups.follow_up.new_tumor.thyroid_gland_carcinoma_involvement_regional_lymph_node_type: 
central (levels 6-7) lateral (levels 2-5)                 NA's 
                   2                    5                  496 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy: 
 yes NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure: 
 356 1260 NA's 
   1    1  501 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
 330 1137 NA's 
   1    1  501 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.histologic_disease_progression_present_indicator: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.i_131_first_administered_dose: 
 171 NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.i_131_subsequent_administered_dose: 
4 mci  NA's 
    1   502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.i_131_total_administered_dose: 
 175 NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_confirmed_diagnosis_method_name: 
pathology      NA's 
        1       502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site: 
lymph node only            NA's 
              1             502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
locoregional recurrence                    NA's 
                      2                     501 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure: 
 yes NA's 
   2  501 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.radiation_therapy_administered_dose_text: 
6600 NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.radiation_therapy_administered_preparation_technique_text: 
imrt NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.radiosensitizing_agent_administered_indicator: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
surgery (distant metastasis) or surgery (loco-regional) 
                                                      1 
                                                   NA's 
                                                    502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.therapeutic_procedure_new_neoplasm_required_additional_therapy_type.2: 
rai therapy        NA's 
          1         502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.therapeutic_procedure_new_neoplasm_required_additional_therapy_type.3: 
ebrt NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.thyroid_gland_carcinoma_involvement_regional_lymph_node_type: 
other  NA's 
    1   502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.thyroid_gland_carcinoma_regional_lymph_node_involvement_anatomic_sites_text: 
r paratracheal           NA's 
             1            502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no NA's 
   9  494 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no  yes NA's 
   8    3  492 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   91.0   300.2   716.0   766.6   991.0  1855.0     495 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0   266.5   474.0   551.5   728.5  1785.0     483 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.histologic_disease_progression_present_indicator: 
  no  yes NA's 
   4    3  496 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.i_131_first_administered_dose: 
 125  154 NA's 
   1    1  501 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.i_131_total_administered_dose: 
 125  154 NA's 
   1    1  501 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                   rhtsh 
                                      3                                       1 
                   thyroxine withdrawal                                    NA's 
                                      1                                     498 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name: 
  imaging pathology      NA's 
        6         9       488 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site: 
           lung lymph node only  other, specify            NA's 
              5               9               2             487 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
biochemical evidence of disease              distant metastasis 
                              1                               6 
        locoregional recurrence               new primary tumor 
                             11                               2 
                           NA's 
                            483 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text: 
               abdomen            lt. thyroid pretracheal lymph node 
                     1                      1                      1 
              prostate                   NA's 
                     1                    499 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
   3    8  492 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.radiation_therapy_administered_preparation_technique_text: 
patient did not receive external radiation therapy 
                                                 5 
                                              NA's 
                                               498 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.radiosensitizing_agent_administered_indicator: 
  no NA's 
   1  502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
                                            rai therapy 
                                                      3 
surgery (distant metastasis) or surgery (loco-regional) 
                                                      7 
                                                   NA's 
                                                    493 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.therapeutic_procedure_new_neoplasm_required_additional_therapy_type.2: 
rai therapy        NA's 
          1         502 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.thyroid_gland_carcinoma_involvement_regional_lymph_node_type: 
lateral (levels 2-5)                 NA's 
                   6                  497 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 334   22  147 

patient.follow_ups.follow_up.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       410         31         62 

patient.follow_ups.follow_up.post_surgical_procedure_assessment_thyroid_gland_carcinoma_status: 
 no imaging evidence of disease   persistent distant metastases 
                            275                               9 
persistent locoregional disease                            NA's 
                             21                             198 

patient.follow_ups.follow_up.postoperative_rx_tx: 
  no  yes NA's 
 311   41  151 

patient.follow_ups.follow_up.radiation_therapy: 
  no  yes NA's 
 167  277   59 


patient.follow_ups.follow_up.radiation_therapy_administered_preparation_technique_text: 
                                              imrt 
                                                 4 
patient did not receive external radiation therapy 
                                               219 
         patient did not receive radiation therapy 
                                                41 
                                              NA's 
                                               239 

patient.follow_ups.follow_up.radiosensitizing_agent_administered_indicator: 
  no NA's 
  17  486 

patient.follow_ups.follow_up.targeted_molecular_therapy: 
  no  yes NA's 
  95    5  403 

patient.follow_ups.follow_up.vital_status: 
alive  dead  NA's 
  464     3    36 

patient.follow_ups.follow_up.year_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2013    2013    2014    2015      27 

patient.gender: 
female   male 
   368    135 

patient.genotype_analysis_performed_indicator: 
  no  yes NA's 
 286   28  189 

patient.genotyping_results_gene_mutation_not_reported_reasons.genotyping_results_gene_mutation_not_reported_reason: 
no braf mutation detected                      NA's 
                       10                       493 

patient.genotyping_results_gene_mutation_not_reported_reasons.genotyping_results_gene_mutation_not_reported_reason.2: 
no ras mutation detected                     NA's 
                       9                      494 

patient.genotyping_results_gene_mutation_not_reported_reasons.genotyping_results_gene_mutation_not_reported_reason.3: 
no ret/ptc rearrangement detected                              NA's 
                               11                               492 

patient.histological_type: 
                                                        other  specify 
                                                                     7 
                         thyroid papillary carcinoma - classical/usual 
                                                                   358 
thyroid papillary carcinoma - follicular (>= 99% follicular patterned) 
                                                                   102 
   thyroid papillary carcinoma - tall cell (>= 50% tall cell features) 
                                                                    36 


patient.history_of_neoadjuvant_treatment: 
 no yes 
499   4 




patient.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                   rhtsh 
                                     71                                      43 
                   thyroxine withdrawal                                    NA's 
                                     59                                     330 

patient.icd_10: 
c73 
503 


patient.icd_o_3_site: 
c73.9 
  503 

patient.informed_consent_verified: 
yes 
503 

patient.lymph_node_examined_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    5.00   12.66   16.00  106.00     112 


patient.lymph_node_preoperative_assessment_diagnostic_imaging_types.lymph_node_preoperative_assessment_diagnostic_imaging_type.2: 
    ct with contrast  ct without contrast    mri with contrast 
                  16                    6                    2 
mri without contrast                 NA's 
                   3                  476 

patient.lymph_node_preoperative_assessment_diagnostic_imaging_types.lymph_node_preoperative_assessment_diagnostic_imaging_type.3: 
mri with contrast              NA's 
                2               501 

patient.lymph_node_preoperative_scan_indicator: 
  no  yes NA's 
 104  371   28 

patient.malignant_neoplasm_metastatic_involvement_site: 
 bone  lung other  NA's 
    2    10     1   490 

patient.metastatic_neoplasm_confirmed_diagnosis_method_names.metastatic_neoplasm_confirmed_diagnosis_method_name: 
    biopsy proven imaging suspected          rai-avid              NA's 
                4                 4                 5               490 

patient.metastatic_neoplasm_confirmed_diagnosis_method_names.metastatic_neoplasm_confirmed_diagnosis_method_name.2: 
imaging suspected              NA's 
                3               500 

patient.metastatic_neoplasm_confirmed_diagnosis_method_text: 
papillary thyroid carcinoma                        NA's 
                          1                         502 

patient.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   5.618   8.000  12.000 

patient.neoplasm_dimension.neoplasm_depth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.500   1.733   2.050   5.500     100 

patient.neoplasm_dimension.neoplasm_length: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.100   1.625   2.500   2.897   4.000  12.000      29 

patient.neoplasm_dimension.neoplasm_width: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.200   1.500   2.000   2.255   3.000   9.000      78 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy: 
  no  yes NA's 
   1    1  501 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure: 
1708 NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
 221 1708 NA's 
   1    1  501 

patient.new_tumor_events.new_tumor_event.2.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                    NA's 
                                      1                                     502 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_confirmed_diagnosis_method_name: 
pathology      NA's 
        1       502 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site: 
          bone other, specify           NA's 
             1              1            501 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
distant metastasis               NA's 
                 2                501 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text: 
femur  NA's 
    1   502 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
   1    1  501 

patient.new_tumor_events.new_tumor_event.2.radiation_therapy_administered_preparation_technique_text: 
imrt NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.2.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
surgery (distant metastasis) or surgery (loco-regional) 
                                                      1 
                                                   NA's 
                                                    502 

patient.new_tumor_events.new_tumor_event.2.therapeutic_procedure_new_neoplasm_required_additional_therapy_type.2: 
rai therapy        NA's 
          1         502 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy: 
 yes NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.3.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                    NA's 
                                      1                                     502 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_confirmed_diagnosis_method_name: 
imaging    NA's 
      1     502 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site: 
other, specify           NA's 
             1            502 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type: 
distant metastasis               NA's 
                 1                502 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text: 
neck NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.3.radiosensitizing_agent_administered_indicator: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.3.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
pharmaceutical therapy                   NA's 
                     1                    502 

patient.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy: 
 yes NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.4.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                    NA's 
                                      1                                     502 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_confirmed_diagnosis_method_name: 
imaging    NA's 
      1     502 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site: 
other, specify           NA's 
             1            502 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type: 
distant metastasis               NA's 
                 1                502 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_occurrence_anatomic_site_text: 
pleura   NA's 
     1    502 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.4.radiosensitizing_agent_administered_indicator: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.4.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
pharmaceutical therapy                   NA's 
                     1                    502 

patient.new_tumor_events.new_tumor_event.5.additional_pharmaceutical_therapy: 
 yes NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.5.additional_radiation_therapy: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.5.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                                    NA's 
                                      1                                     502 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_confirmed_diagnosis_method_name: 
imaging    NA's 
      1     502 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site: 
other, specify           NA's 
             1            502 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type: 
distant metastasis               NA's 
                 1                502 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_occurrence_anatomic_site_text: 
liver  NA's 
    1   502 

patient.new_tumor_events.new_tumor_event.5.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.5.radiosensitizing_agent_administered_indicator: 
  no NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.5.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
pharmaceutical therapy                   NA's 
                     1                    502 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no  yes NA's 
   7    1  495 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no  yes NA's 
   6    4  493 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  271.0   455.0   462.0   707.4   494.0  1855.0     498 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  110.0   242.5   411.0   526.4   519.0  1855.0     492 

patient.new_tumor_events.new_tumor_event.histologic_disease_progression_present_indicator: 
  no  yes NA's 
   1    3  499 

patient.new_tumor_events.new_tumor_event.histologic_disease_progression_present_text: 
metastatic papillary thyroid ca             papillary carcinoma 
                              1                               1 
    papillary thyroid carcinoma                            NA's 
                              1                             500 

patient.new_tumor_events.new_tumor_event.histologic_disease_progression_present_type: 
other, specify           NA's 
             3            500 

patient.new_tumor_events.new_tumor_event.i_131_first_administered_dose: 
 125  152  200 NA's 
   1    1    1  500 

patient.new_tumor_events.new_tumor_event.i_131_total_administered_dose: 
 229  350 NA's 
   1    1  501 

patient.new_tumor_events.new_tumor_event.i_131_total_administered_preparation_technique: 
patient did not receive i-131 treatment                    thyroxine withdrawal 
                                      4                                       3 
                                   NA's 
                                    496 

patient.new_tumor_events.new_tumor_event.new_neoplasm_confirmed_diagnosis_method_name: 
  imaging pathology      NA's 
        6         3       494 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site: 
           lung lymph node only  other, specify     soft tissue            NA's 
              4               3               2               1             493 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence       new primary tumor 
                      4                       5                       1 
                   NA's 
                    493 

patient.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text: 
           bowel left thyroid bed             NA's 
               1                1              501 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
   5    5  493 

patient.new_tumor_events.new_tumor_event.radiation_therapy_administered_dose_text: 
  60 NA's 
   1  502 

patient.new_tumor_events.new_tumor_event.radiation_therapy_administered_preparation_technique_text: 
                                              imrt 
                                                 1 
patient did not receive external radiation therapy 
                                                 2 
                                              NA's 
                                               500 

patient.new_tumor_events.new_tumor_event.radiosensitizing_agent_administered_indicator: 
  no  yes NA's 
   3    1  499 

patient.new_tumor_events.new_tumor_event.therapeutic_procedure_new_neoplasm_required_additional_therapy_type: 
                                 pharmaceutical therapy 
                                                      1 
                                            rai therapy 
                                                      3 
surgery (distant metastasis) or surgery (loco-regional) 
                                                      3 
                                                   NA's 
                                                    496 

patient.new_tumor_events.new_tumor_event.therapeutic_procedure_new_neoplasm_required_additional_therapy_type.2: 
rai therapy        NA's 
          1         502 

patient.new_tumor_events.new_tumor_event.thyroid_gland_carcinoma_involvement_regional_lymph_node_type: 
lateral (levels 2-5)                other                 NA's 
                   2                    2                  499 

patient.new_tumor_events.new_tumor_event.thyroid_gland_carcinoma_regional_lymph_node_involvement_anatomic_sites_text: 
left level 3   right neck         NA's 
           1            1          501 

patient.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 250   11  242 

patient.number_of_lymphnodes_positive_by_he: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   3.658   5.000  41.000     114 

patient.other_genotyping_outcome_lab_results_text: 
not identified           NA's 
             1            502 

patient.other_metastatic_involvement_anatomic_site: 
lymph node       NA's 
         1        502 


patient.patient_personal_medical_history_thyroid_gland_disorder_names.patient_personal_medical_history_thyroid_gland_disorder_name: 
lymphocytic thyroiditis     nodular hyperplasia                  normal 
                     71                      69                     280 
         other, specify                    NA's 
                     25                      58 

patient.patient_personal_medical_history_thyroid_gland_disorder_names.patient_personal_medical_history_thyroid_gland_disorder_name.2: 
nodular hyperplasia      other, specify                NA's 
                 23                   6                 474 

patient.patient_personal_medical_history_thyroid_gland_disorder_names.patient_personal_medical_history_thyroid_gland_disorder_name.3: 
other, specify           NA's 
             1            502 


patient.person_lifetime_risk_radiation_exposure_indicator: 
  no  yes NA's 
 423   17   63 

patient.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       412         49         42 

patient.post_surgical_procedure_assessment_thyroid_gland_carcinoma_status: 
 no imaging evidence of disease   persistent distant metastases 
                             93                               3 
persistent locoregional disease                            NA's 
                             11                             396 

patient.postoperative_rx_tx: 
  no  yes NA's 
 217   17  269 

patient.primary_lymph_node_presentation_assessment: 
  no  yes NA's 
 106  388    9 

patient.primary_neoplasm_focus_type: 
multifocal   unifocal       NA's 
       227        266         10 

patient.primary_thyroid_gland_neoplasm_location_anatomic_site: 
 bilateral    isthmus  left lobe right lobe       NA's 
        86         22        175        214          6 

patient.prior_dx: 
 no yes 
471  32 

patient.race: 
american indian or alaska native                            asian 
                               1                               52 
       black or african american                            white 
                              27                              331 
                            NA's 
                              92 

patient.radiation_therapy: 
  no  yes NA's 
  77  156  270 

patient.radiation_therapy_administered_dose_text: 
   33 60 gy  NA's 
    2     1   500 

patient.radiation_therapy_administered_preparation_technique_text: 
                                              imrt 
                                                 3 
patient did not receive external radiation therapy 
                                               106 
                                              NA's 
                                               394 




patient.radiations.radiation.2.course_number: 
   2 NA's 
   2  501 

patient.radiations.radiation.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.50   15.00   16.63   25.00   28.00     484 

patient.radiations.radiation.2.days_to_radiation_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   52.0    97.0   323.0   428.9   540.0  1716.0     484 

patient.radiations.radiation.2.days_to_radiation_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   49.0    97.0   323.0   421.7   540.0  1716.0     484 

patient.radiations.radiation.2.measure_of_response: 
complete response  partial response              NA's 
                8                 3               492 

patient.radiations.radiation.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   7.000   6.684  10.000  12.000     484 

patient.radiations.radiation.2.numfractions: 
   1   30   33   35 NA's 
  10    1    1    1  490 

patient.radiations.radiation.2.radiation_dosage: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0   129.9   150.0   510.1   200.0  6400.0     484 

patient.radiations.radiation.2.radiation_treatment_ongoing: 
  no NA's 
  19  484 

patient.radiations.radiation.2.radiation_type: 
     external external beam radioisotopes      systemic          NA's 
            2             1             1            15           484 

patient.radiations.radiation.2.regimen_indication: 
  adjuvant recurrence       NA's 
         1          1        501 

patient.radiations.radiation.2.units: 
 cgy   gy  mci NA's 
   2    2   14  485 

patient.radiations.radiation.2.year_of_form_completion: 
2012 2013 2014 NA's 
   7    5    7  484 

patient.radiations.radiation.anatomic_treatment_site: 
 distant recurrence    local recurrence primary tumor field       regional site 
                  1                   1                 200                  38 
               NA's 
                263 



patient.radiations.radiation.course_number: 
   1 NA's 
  13  490 

patient.radiations.radiation.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   14.00   15.46   23.00   31.00     232 

patient.radiations.radiation.days_to_radiation_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    60.0    99.0   135.5   141.0  3490.0     241 

patient.radiations.radiation.days_to_radiation_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    56.0    96.0   128.2   139.5  3470.0     236 

patient.radiations.radiation.measure_of_response: 
               complete response                 partial response 
                             157                               18 
radiographic progressive disease                   stable disease 
                               7                                6 
                            NA's 
                             315 

patient.radiations.radiation.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.985  10.000  12.000     232 


patient.radiations.radiation.radiation_dosage: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.90   76.08  100.80  134.70  150.00 6600.00     251 

patient.radiations.radiation.radiation_treatment_ongoing: 
  no NA's 
 269  234 


patient.radiations.radiation.regimen_indication: 
adjuvant     NA's 
      14      489 

patient.radiations.radiation.units: 
 cgy   gy  mci NA's 
   1   11  234  257 

patient.radiations.radiation.year_of_form_completion: 
2012 2013 2014 2015 NA's 
  97  113   53    8  232 

patient.radiosensitizing_agent_administered_indicator: 
  no NA's 
  14  489 


patient.residual_tumor: 
  r0   r1   r2   rx NA's 
 385   52    4   30   32 



patient.stage_event.system_version: 
4th 5th 6th 7th 
  3  27 126 347 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m: 
  m0   m1   mx NA's 
 280    9  213    1 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n: 
 n0  n1 n1a n1b  nx 
227  58  93  75  50 


patient.tissue_prospective_collection_indicator: 
 no yes 
402 101 

patient.tissue_retrospective_collection_indicator: 
 no yes 
101 402 


patient.tumor_tissue_site: 
thyroid 
    503 

patient.vital_status: 
alive  dead 
  489    14 

patient.year_of_form_completion: 
2010 2011 2012 2013 2014 
   4  133  216  145    5 

patient.year_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1993    2008    2010    2009    2011    2013 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason: 
      case matched expression control      case matched methylation control 
                                   59                                     6 
                  germline comparison primary tumor and metastatic tumor(s) 
                                    9                                     8 
                                 NA's 
                                  421 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2: 
case matched methylation control              germline comparison 
                              50                                1 
                            NA's 
                             452 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3: 
germline comparison                NA's 
                 21                 482 

patient.bcr_canonical_check.bcr_patient_canonical_status: 
       canonical canonical - plus 
             421               82 

patient.biospecimen_cqcf.consent_or_death_status: 
consented  deceased 
      490        13 

patient.biospecimen_cqcf.country: 
       canada        russia united states       vietnam 
           80             4           399            20 

patient.biospecimen_cqcf.days_to_pathology_review: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -166.0    92.0   215.0   760.1  1057.0  6853.0 

patient.biospecimen_cqcf.days_to_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-211.00    0.00    0.00   19.35   37.50  214.00 

patient.biospecimen_cqcf.digital_image_submitted: 
 no yes 
361 142 

patient.biospecimen_cqcf.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     1                     24                    478 

patient.biospecimen_cqcf.ffpe_tumor_slide_submitted: 
digital image         slide          NA's 
          103           381            19 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site: 
   thyroid gland thyroid; isthmus             NA's 
             471               25                7 

patient.biospecimen_cqcf.histological_type: 
                                       classical/usual (papillary nos) 
                                                                   357 
                                                        other  specify 
                                                                     7 
thyroid papillary carcinoma - follicular (>= 99% follicular patterned) 
                                                                   103 
   thyroid papillary carcinoma - tall cell (>= 50% tall cell features) 
                                                                    36 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment: 
                               no yes, radiation prior to resection 
                              499                                 4 

patient.biospecimen_cqcf.laterality: 
bilateral      left     right      NA's 
        3       198       263        39 

patient.biospecimen_cqcf.method_of_sample_procurement: 
surgical resection 
               503 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-211.00    0.00   14.50   18.65   48.25  138.00     463 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement: 
        blood draw surgical resection               NA's 
                27                 13                463 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site: 
thyroid gland          NA's 
           12           491 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
           12           491 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number: 
   1    2 NA's 
  25   16  462 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -731.0     0.0     8.0    56.6    49.0  4032.0       2 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0086   0.0715   0.2252  45.5717   0.9054 532.3000      284 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.92   15.00   20.00   17.69   20.00   22.31     284 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.3    42.5    93.0   222.6   269.0  2326.0     284 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement: 
                   blood draw        other (please specify) 
                          407                             1 
other method (please specify)            surgical resection 
                            2                            91 
                         NA's 
                            2 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site: 
        other thyroid gland          NA's 
            9            93           401 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
           94           409 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number: 
   1    2    4 NA's 
 493    8    1    1 


patient.biospecimen_cqcf.path_confirm_diagnosis_matching: 
 no yes 
  4 499 

patient.biospecimen_cqcf.path_confirm_report_attached: 
  no  yes NA's 
  86  414    3 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics: 
yes 
503 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics: 
 no yes 
  1 502 

patient.biospecimen_cqcf.race: 
american indian or alaska native                            asian 
                               1                                4 
       black or african american                            white 
                               8                               14 
                            NA's 
                             476 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching: 
  pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                  3 
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  1 
                                                                                                                               NA's 
                                                                                                                                499 

patient.biospecimen_cqcf.sample_prescreened: 
yes 
503 

patient.biospecimen_cqcf.site_of_disease: 
thyroid gland          NA's 
          496             7 

patient.biospecimen_cqcf.site_of_disease_description: 
isthmus    NA's 
     25     478 

patient.biospecimen_cqcf.submitted_for_lce: 
  no  yes NA's 
 266    4  233 

patient.biospecimen_cqcf.top_slide_submitted: 
 no yes 
142 361 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent: 
   0    1    5   10 NA's 
  12    1    2    1  487 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   70.00   82.50   81.56   91.25  100.00     487 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   85.25   97.00  120.38  129.50  259.00     487 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number: 
   1    2 NA's 
  13    3  487 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.bcr_sample_uuid: 
c25aed9b-a1b7-48a9-8587-8d218f6a9562                                 NA's 
                                   1                                  502 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_necrosis_percent: 
   0 NA's 
   1  502 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_nuclei_percent: 
  90 NA's 
   1  502 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_weight: 
  50 NA's 
   1  502 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.vial_number: 
   1 NA's 
   1  502 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.bcr_sample_uuid: 
fc3b3071-4069-4919-83c1-0bb2fbe718a8                                 NA's 
                                   1                                  502 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_necrosis_percent: 
   0 NA's 
   1  502 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_nuclei_percent: 
  90 NA's 
   1  502 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_weight: 
  50 NA's 
   1  502 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.vial_number: 
   1 NA's 
   1  502 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   6.215   5.000  30.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  25.00   70.00   80.00   80.62   90.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    7.0   100.0   200.0   221.5   250.0  1719.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number: 
  1   2 
502   1 

patient.biospecimen_cqcf.vessel_used: 
biospecimen storage bag                cassette                cryomold 
                     28                      31                     149 
               cryovial  other (please specify) 
                    254                      41 

patient.days_to_index: 
  0 
503 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
primary tumor and metastatic tumor(s)                                  NA's 
                                    8                                   495 



patient.samples.sample.2.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -138.0   138.5   257.0   807.0  1098.5  6870.0 

patient.samples.sample.2.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0   100.0   215.0   184.7   240.0   430.0     429 

patient.samples.sample.2.is_ffpe: 
 no 
503 

patient.samples.sample.2.oct_embedded: 
false  true 
  481    22 



patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.340   1.780   1.820   1.821   1.860   2.070     457 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97491237 118955598 122967854 124698590 132180991 132257061       457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
   7   13 NA's 
  35   11  457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.15 0.16 0.17 0.18 NA's 
  14   18    9    5  457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   17.00   29.00   24.17   29.00   31.00     457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.250   7.000   6.043   8.000  12.000     457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.022   6.000  10.000     457 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.080   2.080   2.416   2.835   4.270     457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  23 NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
  13   20 26.7 NA's 
  34   11    1  457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2011 2012 NA's 
   5   41  457 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97489969 113419855 122848642 124551951 132272605 175156215         3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   7    8   13 NA's 
  11  454   35    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.5000  0.5000  0.4688  0.5000  0.5000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    15.0    20.0    18.2    25.0    31.0       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 500    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.296   8.000  12.000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.534   9.000  12.000       3 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.95   40.00   40.00   36.59   40.00   40.00       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
 500    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  13   20   80 NA's 
  11   35  454    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2011 2012 2013 2014 NA's 
 132  277   84    7    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.40   10.75   13.87   17.59   18.05   55.44     457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna                 NA's 
                 454                   46                    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    w NA's 
  46  454    3 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration: 
0.15 0.16 0.17 0.18 NA's 
  14   18    9    5  457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
 454   49 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
 454   49 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             500 
                                            NA's 
                                               3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
 500    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                   repli-g                      NA's 
                       46                       454                         3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.900   1.275   1.390   1.392   1.450   2.600     457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.525   7.850   7.917   8.175   9.400     457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec    NA's 
     46     457 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2    3 NA's 
  65    8    2  428 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113388311 122878406 132272604 127565558 132272702 145630498       457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8 NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   16.00   17.00   16.87   20.00   25.00     457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   7.065  10.000  12.000     457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   6.565   8.000  11.000     457 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2011 2012 2013 NA's 
   3   41    2  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  46                  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
  46  457 



patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              46 
                                            NA's 
                                             457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
  46  457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
     46     457 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2    3 NA's 
  37    7    2  457 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.180   1.870   1.930   1.911   2.000   2.430     414 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97484832 113420436 118965699 121192842 132181368 167788187 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  1   2   8 
 81  26 396 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07000 0.08000 0.08000 0.09624 0.09000 0.18000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   13.00   19.00   20.17   29.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   8.000   7.513  11.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   7.000   8.000   7.682   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.980   2.080   3.200   2.668   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 23 
503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   26.00   40.00   32.43   40.00   40.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2011 2012 2014 
 228  268    7 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97486990 108474541 113417144 113002618 118965637 153889058       313 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   1    5    8   26 NA's 
  24   33  108   25  313 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0900  0.1133  0.1500  0.1700     313 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   15.00   27.00   21.99   30.00   31.00     313 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
 190  313 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.311   8.000  12.000     313 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   9.000   8.968  10.000  12.000     313 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.700   3.200   2.552   3.200   3.600     313 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  23 NA's 
 190  313 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   10.00   40.00   27.01   40.00   40.00     313 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2011 2012 2013 NA's 
 133   56    1  313 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99039597 113397052 113417220 118169305 117572104 167811810       485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   5.000   7.167   8.000  26.000     485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.0825  0.1550  0.1317  0.1600  0.1800     485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.25   24.00   21.28   31.00   31.00     485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
  18  485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.250   8.000   7.111   8.750  11.000     485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000  10.000   9.111  12.000  12.000     485 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.070   1.950   2.275   2.617   3.200   4.540     485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  23 NA's 
  18  485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.00   13.40   22.31   36.67   40.00     485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2011 2012 2013 2015 NA's 
  14    2    1    1  485 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode: 
tcga-e8-a2jq-10a-01d-a19m-08 tcga-em-a2cs-06a-11d-a19i-01 
                           1                            1 
tcga-fe-a22z-10a-01d-a33c-08                         NA's 
                           1                          500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid: 
1e799591-6697-4c1c-8289-af911b46df3a 4d9b9597-a773-4777-93e0-2e5a3eb178ab 
                                   1                                    1 
cbc0921a-26ee-4492-af40-ba86d2868ef0                                 NA's 
                                   1                                  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
118953091 118957833 153889060      NA's 
        1         1         1       500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
   1    8 NA's 
   1    2  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
0.08 0.09 0.16 NA's 
   1    1    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
   8   21 NA's 
   2    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
   3  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
  10   11 NA's 
   1    2  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
   1   12 NA's 
   1    2  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id: 
a19i a19m a33c NA's 
   1    1    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row: 
   a    d    f NA's 
   1    1    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
1.07  3.6 NA's 
   2    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  23 NA's 
   3  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
 6.7 13.4   40 NA's 
   1    1    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2011 2013 NA's 
   2    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode: 
tcga-e8-a2jq-10a-01d-a19p-02 tcga-em-a2cs-06a-11d-a19j-08 
                           1                            1 
tcga-fe-a22z-10a-01d-a35r-08                         NA's 
                           1                          500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid: 
46efe84b-e856-4b4d-a1fa-578578741495 60b04e1e-2889-44e1-94af-2eed7e43095e 
                                   1                                    1 
e9058fde-c9df-49ec-94f3-b918a84c4c2e                                 NA's 
                                   1                                  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom: 
118955649 118957911 160356588      NA's 
        1         1         1       500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id: 
   2    8 NA's 
   1    2  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration: 
0.08 0.17 NA's 
   2    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment: 
   6    8 NA's 
   1    2  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe: 
  no NA's 
   3  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment: 
   1   11 NA's 
   1    2  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column: 
   1   12 NA's 
   2    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id: 
a19j a19p a35r NA's 
   1    1    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row: 
   a    b    d NA's 
   1    1    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity: 
1.07  1.7  3.2 NA's 
   1    1    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center: 
  23 NA's 
   3  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume: 
  10 13.4   40 NA's 
   1    1    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment: 
2011 2014 NA's 
   2    1  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode: 
tcga-em-a2cs-06a-11d-a19k-05                         NA's 
                           1                          502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid: 
4d1a9ca1-d441-430f-b9cd-b20d632428f0                                 NA's 
                                   1                                  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom: 
118955632      NA's 
        1       502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id: 
   5 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration: 
0.16 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment: 
   8 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe: 
  no NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment: 
  11 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column: 
  12 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id: 
a19k NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row: 
   d NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity: 
2.08 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center: 
  23 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume: 
  13 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment: 
2011 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode: 
tcga-em-a2cs-06a-11d-a19p-02                         NA's 
                           1                          502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid: 
5f6ded47-5c5f-4154-b030-e08ecf2ed005                                 NA's 
                                   1                                  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom: 
118957856      NA's 
        1       502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id: 
   2 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration: 
0.16 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment: 
   8 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe: 
  no NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment: 
  11 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column: 
   1 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id: 
a19p NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row: 
   f NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity: 
 1.6 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center: 
  23 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.volume: 
  10 NA's 
   1  502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment: 
2011 NA's 
   1  502 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486798 113420434 118966770 121196951 132180816 167823255 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  1   2 
398 105 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1574  0.1600  0.1800 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   12.50   19.00   18.85   29.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   8.000   7.557  11.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   7.000   8.000   7.682   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.010   1.070   1.222   1.140   2.340 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 23 
503 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.7  10  13 
398  46  59 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2011 2012 2014 
 228  268    7 

patient.samples.sample.2.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.91   13.91   24.23   32.98   42.02  227.77 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type: 
dna 
503 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id: 
  d 
503 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1574  0.1600  0.1800 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
503 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
yes 
503 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             503 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
            adna preparation type pre-extracted dna received by tss 
                              276                               226 
                             NA's 
                                1 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen   uv spec 
      414        89 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence: 
   1    2    3 NA's 
  65    8    2  428 



patient.samples.sample.2.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   10.00   16.00   16.52   23.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe: 
 no 
503 

patient.samples.sample.2.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   8.000   7.185  10.000  12.000 

patient.samples.sample.2.portions.portion.portion_number: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   2.598   1.000  22.000 

patient.samples.sample.2.portions.portion.portion_sequence: 
   1    2 NA's 
 495    6    2 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
  74  429 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   3.014   2.750  50.000     429 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.108   2.000  53.000     429 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis: 
   0 NA's 
  74  429 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0    1    2 NA's 
  72    1    1  429 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00  100.00  100.00   85.32  100.00  100.00     429 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   6.568   0.000 100.000     429 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   8.108   0.000  90.000     429 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   8.514   0.000 100.000     429 

patient.samples.sample.2.portions.portion.slides.slide.section_location: 
 top NA's 
  74  429 

patient.samples.sample.2.portions.portion.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    2.00   46.36   30.00 2000.00     230 

patient.samples.sample.2.portions.portion.year_of_creation: 
2010 2011 2012 2013 2014 
  26  222  248    6    1 

patient.samples.sample.2.portions.shipment_portion.bcr_shipment_portion_uuid: 
c73b5e9e-3e72-439d-bc78-ff7464ab61f2 f66c2890-ee95-41d8-b061-929ff212e0b3 
                                   1                                    1 
                                NA's 
                                 501 

patient.samples.sample.2.portions.shipment_portion.center_id: 
  20 NA's 
   2  501 

patient.samples.sample.2.portions.shipment_portion.is_ffpe: 
  no NA's 
   2  501 

patient.samples.sample.2.portions.shipment_portion.plate_id: 
a21m NA's 
   2  501 

patient.samples.sample.2.portions.shipment_portion.portion_number: 
  21 NA's 
   2  501 

patient.samples.sample.2.portions.shipment_portion.portion_sequence: 
   2 NA's 
   2  501 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode: 
tcga-em-a2cs-06a-21-a21m-20 tcga-em-a2p1-06a-21-a21m-20 
                          1                           1 
                       NA's 
                        501 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment: 
   7 NA's 
   2  501 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment: 
   3 NA's 
   2  501 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment: 
2012 NA's 
   2  501 

patient.samples.sample.2.sample_type: 
blood derived normal           metastatic  solid tissue normal 
                 426                    8                   69 

patient.samples.sample.2.sample_type_id: 
  6  10  11 
  8 426  69 

patient.samples.sample.2.vial_number: 
  a   b   c 
497   5   1 



patient.samples.sample.3.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0    99.5   136.0   191.2   204.5  1386.0     464 

patient.samples.sample.3.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0   120.0   160.0   188.4   240.0   500.0     472 

patient.samples.sample.3.is_ffpe: 
  no NA's 
  39  464 

patient.samples.sample.3.oct_embedded: 
false  true  NA's 
   18    21   464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.720   1.790   1.830   1.821   1.840   1.910     482 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108474817 108474907 113416599 114104393 117832637 122971823       482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
   7   13 NA's 
  17    4  482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.15 0.16 0.17 0.18 NA's 
   6    6    3    6  482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   27.00   30.00   27.24   30.00   31.00     482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   6.000   5.762   8.000  11.000     482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   4.000   4.619   5.000   9.000     482 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.080   2.340   3.002   4.010   4.810     482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  23 NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
  13   20 26.7 NA's 
  11    4    6  482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2011 2012 NA's 
  16    5  482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode: 
tcga-bj-a28w-11a-11r-a32y-07                         NA's 
                           1                          502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid: 
a378a703-e62d-4860-aec0-caa90f5af30f                                 NA's 
                                   1                                  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom: 
150756965      NA's 
        1       502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id: 
   7 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration: 
0.18 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment: 
   2 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment: 
  10 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column: 
   3 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id: 
a32y NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row: 
   b NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity: 
2.34 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center: 
  23 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume: 
  13 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment: 
2013 NA's 
   1  502 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99013079 113391034 118956640 120453153 132159296 145633395       464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   7    8   13 NA's 
   4   18   17  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.1600  0.1800  0.3192  0.5000  0.5000     464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   22.00   30.00   25.67   30.00   31.00     464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.974   8.000  12.000     464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.667   9.000  12.000     464 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.95    3.20    3.60   20.12   40.00   40.00     464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  13   20   80 NA's 
   4   17   18  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2011 2012 2013 NA's 
  18   10   11  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.17   10.04   13.09   14.96   16.62   33.90     482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna                 NA's 
                  18                   21                  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    w NA's 
  21   18  464 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration: 
0.15 0.16 0.17 0.18 NA's 
   6    6    3    6  482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
  18  485 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
  18  485 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              39 
                                            NA's 
                                             464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                   repli-g                      NA's 
                       21                        18                       464 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.200   1.300   1.300   1.371   1.400   1.900     482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.20    7.40    7.70    7.79    8.10    8.80     482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec    NA's 
     21     482 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2 NA's 
  29    2  472 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97490924  97490978 113416446 112366120 118956658 132272634       482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8 NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     3.0    16.0    13.9    20.0    25.0     482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   7.476  11.000  12.000     482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   6    9   11   12 NA's 
   9    1    6    5  482 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2011 2012 NA's 
  15    6  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  21                  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
  21  482 



patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              21 
                                            NA's 
                                             482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
  21  482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
     21     482 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2 NA's 
  19    2  482 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.820   1.880   1.890   1.928   1.950   2.160     492 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108474245 113417365 117833147 120193881 132177282 132273012       464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
   1    2    8 NA's 
  13    4   22  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0800  0.1136  0.1600  0.1800     464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   18.00   27.00   23.85   30.00   31.00     464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.500   8.000   7.359  10.000  12.000     464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   6.000  10.000   9.026  11.000  12.000     464 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.120   2.800   2.288   3.200   3.200     464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  23 NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    7.00   40.00   25.85   40.00   40.00     464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2011 2012 NA's 
  19   20  464 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108474533 112159056 113417209 114799963 118113955 132173120       479 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   5    8   26 NA's 
   3   17    4  479 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
0.08 0.09 0.15 0.16 NA's 
  14    3    4    3  479 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0    25.5    30.0    26.5    30.0    31.0     479 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
  24  479 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.75    6.00    6.00    8.00   11.00     479 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.00   10.00    8.75   11.00   12.00     479 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   2.047   3.200   2.824   3.200   3.600     479 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  23 NA's 
  24  479 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
  10   13   40 NA's 
   4    3   17  479 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2011 2012 NA's 
  18    6  479 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100491482 108432288 113415902 112892385 113421354 122971498       483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   1    5 NA's 
   1   19  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
0.15 0.16 0.17 0.18 NA's 
   8    9    1    2  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0    25.5    30.0    26.8    30.0    31.0     483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
  20  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     3.0     6.0     5.8     8.0    11.0     483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   6    7   11   12 NA's 
   8    1    7    4  483 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   2.047   2.080   2.665   4.010   4.810     483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  23 NA's 
  20  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
 6.7   13 26.7 NA's 
   1   13    6  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2011 2012 NA's 
  17    3  483 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode: 
tcga-bj-a28t-11a-11d-a17v-08 tcga-e8-a2jq-11a-11d-a19i-01 
                           1                            1 
tcga-em-a2cs-10a-01d-a19m-08                         NA's 
                           1                          500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid: 
2e6f101a-4c11-4f60-8549-6f4eec0c4121 6a6fd1e9-9604-4d58-a7a7-3ce60b565972 
                                   1                                    1 
f5a3d4dc-ea96-4225-bafe-2042ccb44ae1                                 NA's 
                                   1                                  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
113417147 118955630 118957834      NA's 
        1         1         1       500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
   1    8 NA's 
   1    2  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
0.08 0.16 NA's 
   2    1  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
   8   31 NA's 
   2    1  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
   3  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
   8   11 NA's 
   1    2  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
  12 NA's 
   3  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id: 
a17v a19i a19m NA's 
   1    1    1  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row: 
   b    d NA's 
   2    1  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
1.07  3.2 NA's 
   1    2  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  23 NA's 
   3  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
 6.7   40 NA's 
   1    2  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2011 NA's 
   3  500 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode: 
tcga-e8-a2jq-11a-11d-a19j-08 tcga-em-a2cs-10a-01d-a19p-02 
                           1                            1 
                        NA's 
                         501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid: 
1e2e5415-3eb0-4279-a6cd-aac232c67b8a ec6a8027-e14a-4592-bf7d-8f7e1232fe69 
                                   1                                    1 
                                NA's 
                                 501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom: 
118955654 118957740      NA's 
        1         1       501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id: 
   2    8 NA's 
   1    1  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration: 
0.08 0.15 NA's 
   1    1  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment: 
   8 NA's 
   2  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe: 
  no NA's 
   2  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment: 
  11 NA's 
   2  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column: 
   1   12 NA's 
   1    1  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id: 
a19j a19p NA's 
   1    1  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row: 
   b NA's 
   2  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity: 
 1.5  3.2 NA's 
   1    1  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center: 
  23 NA's 
   2  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume: 
  10   40 NA's 
   1    1  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment: 
2011 NA's 
   2  501 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode: 
tcga-e8-a2jq-11a-11d-a19k-05                         NA's 
                           1                          502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid: 
a01e6e2a-1051-45d0-b5d1-17b9b9e687d0                                 NA's 
                                   1                                  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom: 
118957831      NA's 
        1       502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id: 
   5 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration: 
0.16 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment: 
   8 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe: 
  no NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment: 
  11 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column: 
  12 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id: 
a19k NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row: 
   b NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity: 
2.08 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center: 
  23 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.volume: 
  13 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment: 
2011 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode: 
tcga-e8-a2jq-11a-11d-a19p-02                         NA's 
                           1                          502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid: 
362a65b7-3be1-4b5e-95b0-95895c6e683a                                 NA's 
                                   1                                  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom: 
118957880      NA's 
        1       502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id: 
   2 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration: 
0.16 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment: 
   8 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe: 
  no NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment: 
  11 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column: 
   1 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id: 
a19p NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row: 
   d NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity: 
 1.6 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center: 
  23 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.volume: 
  10 NA's 
   1  502 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment: 
2011 NA's 
   1  502 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
108474437 113417273 117833051 120198781 132255780 132273941       464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   1    2 NA's 
  26   13  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1579  0.1600  0.1800     464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   17.00   21.00   20.77   29.50   30.00     464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.500   9.000   7.462  10.000  12.000     464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   6.000  10.000   9.026  11.000  12.000     464 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.070   1.070   1.307   1.600   2.340     464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
 6.7   10   13 NA's 
  26    7    6  464 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2011 2012 NA's 
  19   20  464 

patient.samples.sample.3.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.74   22.22   31.44   40.34   41.67  153.28     464 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type: 
 dna NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id: 
   d NA's 
  39  464 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1579  0.1600  0.1800     464 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              39 
                                            NA's 
                                             464 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  39  464 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
            adna preparation type pre-extracted dna received by tss 
                               38                                 1 
                             NA's 
                              464 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen   uv spec      NA's 
       28        11       464 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence: 
   1    2 NA's 
  29    2  472 



patient.samples.sample.3.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   16.00   15.21   22.50   29.00     464 

patient.samples.sample.3.portions.portion.is_ffpe: 
  no NA's 
  39  464 

patient.samples.sample.3.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   6.000   6.128   9.000  11.000     464 

patient.samples.sample.3.portions.portion.portion_number: 
   1   11   12   21 NA's 
   8   27    2    2  464 

patient.samples.sample.3.portions.portion.portion_sequence: 
   1    2 NA's 
  37    2  464 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
  31  472 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   6.129   7.500  40.000     472 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.7742  1.5000  4.0000     472 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis: 
   0 NA's 
  31  472 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0    1 NA's 
  28    3  472 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   90.00  100.00   91.45  100.00  100.00     472 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   8.548  10.000  40.000     472 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells: 
   0 NA's 
  31  472 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei: 
   0 NA's 
  31  472 

patient.samples.sample.3.portions.portion.slides.slide.section_location: 
 top NA's 
  31  472 

patient.samples.sample.3.portions.portion.weight: 
   2   20   30  500 NA's 
   5    2   29    1  466 

patient.samples.sample.3.portions.portion.year_of_creation: 
2011 2012 NA's 
  19   20  464 

patient.samples.sample.3.sample_type: 
blood derived normal  solid tissue normal                 NA's 
                   8                   31                  464 

patient.samples.sample.3.sample_type_id: 
  10   11 NA's 
   8   31  464 

patient.samples.sample.3.vial_number: 
   a NA's 
  39  464 



patient.samples.sample.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -138.0   126.0   241.0   797.5  1098.5  6870.0 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3: 
0628a966-c9de-4bec-989c-3ad13a407843                                 NA's 
                                   1                                  502 

patient.samples.sample.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    7.0    80.0   150.0   188.9   240.0  1120.0 

patient.samples.sample.is_ffpe: 
 no 
503 

patient.samples.sample.oct_embedded: 
false  true 
  200   303 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.850   1.790   1.830   2.199   1.860 187.500 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97490570 113421376 118956576 121167596 132181010 167787830 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
  7  13 
325 178 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1600  0.1600  0.1642  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   13.00   19.00   20.12   29.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   8.000   7.509  11.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.316   6.000  10.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   2.080   2.340   2.696   3.200   4.810 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
 23 
503 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
  13   20 26.7 
 290  178   35 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2011 2012 2014 
 228  268    7 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132268528 150757013 160356782 155877152 160357090 160357144       486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id: 
   2    7 NA's 
  12    5  486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration: 
0.15 0.16 0.17 0.18 NA's 
   3    5    6    3  486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment: 
   2   18   30 NA's 
   4    3   10  486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
  17  486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment: 
   7   10   12 NA's 
   1   13    3  486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   2.235   4.000   6.000     486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id: 
a239 a32y a345 a35p NA's 
   1    4    9    3  486 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.3200  0.3400  0.8676  1.9500  2.3400     486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center: 
  23 NA's 
  17  486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume: 
   2   13 NA's 
  12    5  486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment: 
2012 2013 NA's 
   1   16  486 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode: 
tcga-em-a2ck-01a-11r-a32y-07 tcga-et-a2n1-01a-11r-a466-07 
                           1                            1 
                        NA's 
                         501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid: 
651085af-0c4a-4351-be94-2867815b4810 aadcd317-8fb2-4968-9682-ac4625f3a302 
                                   1                                    1 
                                NA's 
                                 501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom: 
150756987 167812990      NA's 
        1         1       501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id: 
   7 NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration: 
0.16 NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment: 
   2    9 NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment: 
  10   12 NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column: 
   2    4 NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id: 
a32y a466 NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row: 
   a    f NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity: 
1.06 2.08 NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center: 
  23 NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume: 
 6.6   13 NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment: 
2013 2014 NA's 
   1    1  501 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97490550 113423112 118968824 121570509 132180366 167787734 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
  7  13 
178 325 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1600  0.1600  0.1642  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   13.00   19.00   20.12   29.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   8.000   7.509  11.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.316   6.000  10.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   2.210   3.200   2.981   3.400   4.540 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
 23 
503 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  13   20 26.7 
 154  325   24 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2011 2012 2014 
 228  268    7 

patient.samples.sample.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.43   24.04   35.72   37.42   49.37   86.98 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type: 
rna 
503 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id: 
  r 
503 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1600  0.1600  0.1642  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             503 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna 
                      503 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.300   1.470   1.523   1.700  13.000 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00    7.60    8.10    8.07    8.50    9.40 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec 
    503 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence: 
  1   2 
469  34 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97490922 113421106 122848698 124858940 132272948 175154487         1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8 NA's 
 502    1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
 502    1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   15.00   20.00   17.59   25.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 502    1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.369   9.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    3.97    5.00   11.00       1 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
 502    1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
 502    1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
 502    1 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2011 2012 2013 2014 NA's 
 130  277   88    7    1 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                 502                    1 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
 502    1 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
 502    1 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
 502    1 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             502 
                                            NA's 
                                               1 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
 502    1 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
    502       1 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2 NA's 
 468   34    1 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.610   1.910   1.950   1.958   2.000   2.270     405 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486216 113421444 118966468 121165147 132179920 167823349 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  1   2   8 
 81  26 396 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07000 0.08000 0.08000 0.09648 0.09000 0.18000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   13.00   19.00   20.16   29.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   8.000   7.509  11.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.966   5.000  11.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.080   3.200   2.673   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 23 
503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   26.00   40.00   32.43   40.00   40.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2011 2012 2014 
 228  268    7 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486984 113421484 118968427 121171050 132180776 167823351 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
  1   5   8  26 
 24 347 107  25 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1400  0.1500  0.1404  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   13.00   19.00   20.12   29.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   8.000   7.509  11.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.217   6.000  11.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.950   2.080   2.235   2.210   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
 23 
503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
6.67   10   13   40 
  24   25  347  107 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2011 2012 2014 
 228  268    7 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99039430 113390714 113421404 131348446 164054166 164054555       246 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   5    8   26 NA's 
 156  100    1  246 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1500  0.1284  0.1600  0.1800     246 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   21.00   26.00   23.38   27.00   31.00     246 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
 257  246 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   4.389   6.000   9.000     246 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.424   6.000  12.000     246 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.94    1.07    1.95    2.19    2.21    4.81     246 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  23 NA's 
 257  246 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
  10   13 13.4 26.7 NA's 
   1   97  100   59  246 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2011 2012 2014 NA's 
 131   26  100  246 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113417130 164054183 164054520 161234920 164054559 164054591       374 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
   1    8 NA's 
   2  127  374 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0800  0.0822  0.0800  0.1700     374 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
   8   21   26   31 NA's 
   2    2  120    5  374 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
 129  374 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
   3    8   10   11 NA's 
 120    5    2    2  374 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    5.00    5.38    8.00   12.00     374 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.070   1.070   1.171   1.070   3.600     374 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  23 NA's 
 129  374 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
 6.7 13.4   40 NA's 
   2  122    5  374 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2011 2013 2014 NA's 
   7    2  120  374 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118955648 127689906 158971737 147953706 164054431 167811821       497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id: 
   8    9 NA's 
   5    1  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration: 
0.08 0.09 NA's 
   4    2  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment: 
   8   21   26 NA's 
   2    2    2  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe: 
  no NA's 
   6  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment: 
   1    3   10   11 NA's 
   1    2    1    2  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column: 
   1    6   12 NA's 
   3    1    2  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id: 
a19j a33c a37v a46a NA's 
   2    1    2    1  497 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity: 
1.07 1.21  3.2 NA's 
   2    2    2  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center: 
  23 NA's 
   6  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume: 
13.4   40 NA's 
   4    2  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment: 
2011 2013 2014 2015 NA's 
   2    1    2    1  497 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode: 
tcga-e8-a2jq-01a-11d-a19k-05 tcga-em-a2cs-01a-11d-a19k-05 
                           1                            1 
tcga-fe-a22z-01a-11d-a35r-08                         NA's 
                           1                          500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid: 
06471ec4-e769-48fc-bb41-963c7b9a50d3 2e5cc596-533a-43a1-95df-343289ded648 
                                   1                                    1 
8845fce6-e2b4-4929-942b-57096b0bbafc                                 NA's 
                                   1                                  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom: 
118955647 118956954 160355813      NA's 
        1         1         1       500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id: 
   5    8 NA's 
   2    1  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration: 
0.08 0.15 0.17 NA's 
   1    1    1  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment: 
   6    8 NA's 
   1    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe: 
  no NA's 
   3  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment: 
   1   11 NA's 
   1    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column: 
   1   12 NA's 
   1    2  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id: 
a19k a35r NA's 
   2    1  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row: 
   a    c NA's 
   2    1  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity: 
1.07 1.95 2.21 NA's 
   1    1    1  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center: 
  23 NA's 
   3  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume: 
  13 13.4 NA's 
   2    1  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment: 
2011 2014 NA's 
   2    1  500 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode: 
tcga-e8-a2jq-01a-11d-a19p-02 tcga-em-a2cs-01a-11d-a19p-02 
                           1                            1 
                        NA's 
                         501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid: 
9abc3c93-71e6-463a-aab3-da03676907e1 eaffe7a0-3b58-4cce-a745-8ff8e05dcfd3 
                                   1                                    1 
                                NA's 
                                 501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom: 
118957863 118957887      NA's 
        1         1       501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id: 
   2 NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration: 
0.15 0.17 NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment: 
   8 NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe: 
  no NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment: 
  11 NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column: 
   1 NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id: 
a19p NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row: 
   c    e NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity: 
 1.5  1.7 NA's 
   1    1  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center: 
  23 NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume: 
  10 NA's 
   2  501 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment: 
2011 NA's 
   2  501 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97483774 113420436 118966761 121168775 132181298 167823447 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  1   2 
398 105 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1577  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   12.50   19.00   18.85   29.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   8.000   7.553  11.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   3.966   5.000  11.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.010   1.070   1.226   1.140   2.340 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 23 
503 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.7  10  13 
398  46  59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2011 2012 2014 
 228  268    7 

patient.samples.sample.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.25   28.34   38.84   42.75   53.12  110.16 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type: 
dna 
503 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id: 
  d 
503 



patient.samples.sample.portions.portion.analytes.analyte.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1577  0.1600  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
503 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
yes 
503 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             503 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
503 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
adna preparation type 
                  503 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen   uv spec 
      405        98 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence: 
  1   2 
469  34 



patient.samples.sample.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    9.00   17.00   16.39   24.00   31.00 

patient.samples.sample.portions.portion.is_ffpe: 
 no 
503 

patient.samples.sample.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   8.000   7.207  10.000  12.000 

patient.samples.sample.portions.portion.portion_number: 
 11  12  21  22  31 
438  31  29   3   2 

patient.samples.sample.portions.portion.portion_sequence: 
   1    2    3 NA's 
 468   32    2    1 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe: 
  no NA's 
  27  476 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration: 
   0    1    2   10 NA's 
  19    4    2    2  476 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration: 
   0    1 NA's 
  24    3  476 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis: 
   0 NA's 
  27  476 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration: 
   0 NA's 
  27  476 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells: 
   0   80 NA's 
  26    1  476 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   9.704  17.500  35.000     476 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   82.50   95.00   87.33  100.00  100.00     476 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   85.00   95.00   86.59   96.00   99.00     476 

patient.samples.sample.portions.portion.slides.slide.2.section_location: 
 top NA's 
  27  476 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
 502    1 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.568   2.000  40.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5219  1.0000 20.0000       1 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
 0.00000  0.00000  0.00000  0.05378  0.00000 10.00000        1 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0    1    2    6 NA's 
 473   24    4    1    1 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.102   0.000  65.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   15.09   20.00   90.00       1 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   75.00   85.00   82.76   95.00  100.00       1 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   80.00   90.00   86.89   95.00  100.00       1 

patient.samples.sample.portions.portion.slides.slide.section_location: 
bottom    top   NA's 
    27    475      1 

patient.samples.sample.portions.portion.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   30.00   30.00   28.95   30.00   30.00 

patient.samples.sample.portions.portion.year_of_creation: 
2010 2011 2012 2013 
  25  229  242    7 

patient.samples.sample.portions.shipment_portion.2.bcr_shipment_portion_uuid: 
3c8418c9-533a-4b89-b9c9-f7c25367feee c7308eff-0b82-4336-a809-9fdc08c4e06f 
                                   1                                    1 
                                NA's 
                                 501 

patient.samples.sample.portions.shipment_portion.2.center_id: 
  20 NA's 
   2  501 

patient.samples.sample.portions.shipment_portion.2.is_ffpe: 
  no NA's 
   2  501 

patient.samples.sample.portions.shipment_portion.2.plate_id: 
a301 NA's 
   2  501 

patient.samples.sample.portions.shipment_portion.2.portion_number: 
  22 NA's 
   2  501 

patient.samples.sample.portions.shipment_portion.2.portion_sequence: 
   2 NA's 
   2  501 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_bcr_aliquot_barcode: 
tcga-ce-a3md-01a-22-a301-20 tcga-fy-a3on-01a-22-a301-20 
                          1                           1 
                       NA's 
                        501 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_day_of_shipment: 
  21 NA's 
   2  501 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_month_of_shipment: 
   5 NA's 
   2  501 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_year_of_shipment: 
2013 NA's 
   2  501 


patient.samples.sample.portions.shipment_portion.center_id: 
  20 NA's 
 373  130 

patient.samples.sample.portions.shipment_portion.is_ffpe: 
  no NA's 
 373  130 


patient.samples.sample.portions.shipment_portion.portion_number: 
  11   13   21   41 NA's 
  18    1  343   11  130 

patient.samples.sample.portions.shipment_portion.portion_sequence: 
   1    2    4 NA's 
  19  342   11  131 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment: 
   4    7   21 NA's 
   5  222  146  130 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment: 
   3    5    6 NA's 
 222  146    5  130 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment: 
2012 2013 2014 NA's 
 222  146    5  130 

patient.samples.sample.sample_type: 
primary tumor 
          503 

patient.samples.sample.sample_type_id: 
  1 
503 

patient.samples.sample.vial_number: 
  a   b 
498   5 

}}
\keyword{datasets}
