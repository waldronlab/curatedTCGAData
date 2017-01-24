\name{ ESCA }
\alias{ ESCA }
\docType{data}
\title{ ESCA }

\details{
\preformatted{
> ESCA
A MultiAssayExperiment object of 10 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 10: 
 [1] RNASeqGene: ExpressionSet with 26120 rows and 198 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 196 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 198 columns 
 [4] CNASNP: RangedRaggedAssay with 203186 rows and 373 columns 
 [5] CNVSNP: RangedRaggedAssay with 60803 rows and 373 columns 
 [6] CNAseq: RangedRaggedAssay with 17059 rows and 104 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 202 columns 
 [8] RPPAArray: ExpressionSet with 192 rows and 126 columns 
 [9] gistica: SummarizedExperiment with 24776 rows and 184 columns 
 [10] gistict: SummarizedExperiment with 24776 rows and 184 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( ESCA )
CharacterList of length 10
[["RNASeqGene"]] AADACL3 AADACL4 AB007962 AB074166 AB1 ... VCY1B VCY XKRY ZFY
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-15024591 1:15027535-15489638 ... 24:28457337-59018259
[["CNVSNP"]] 1:3218610-15024591 1:15027535-15500102 ... 23:3157107-154905589
[["CNAseq"]] 1:10208-224538737 1:224538738-249240606 ... 23:2699504-154930285
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( ESCA )
CharacterList of length 10
[["RNASeqGene"]] TCGA-2H-A9GF-01A-11R-A37I-31 ... TCGA-ZR-A9CJ-01B-11R-A38D-31
[["RNASeq2GeneNorm"]] TCGA-2H-A9GF-01A-11R-A37I-31 ...
[["miRNASeqGene"]] TCGA-2H-A9GF-01A-11R-A37J-13 ...
[["CNASNP"]] TCGA-2H-A9GF-01A-11D-A37B-01 ... TCGA-ZR-A9CJ-10A-01D-A389-01
[["CNVSNP"]] TCGA-2H-A9GF-01A-11D-A37B-01 ... TCGA-ZR-A9CJ-10A-01D-A389-01
[["CNAseq"]] TCGA-IG-A3I8-01A-11D-A248-26 ... TCGA-LN-A5U5-10A-01D-A28P-26
[["Methylation"]] TCGA-2H-A9GF-01A-11D-A37D-05 ... TCGA-ZR-A9CJ-01B-11D-A388-05
[["RPPAArray"]] TCGA-2H-A9GF-01A-21-A41Y-20 ... TCGA-Z6-AAPN-01A-21-A41Y-20
[["gistica"]] TCGA-2H-A9GF-01A-11D-A37B-01 ... TCGA-ZR-A9CJ-01B-11D-A386-01
[["gistict"]] TCGA-2H-A9GF-01A-11D-A37B-01 ... TCGA-ZR-A9CJ-01B-11D-A386-01

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.5 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP   16 Mb
5           CNVSNP  5.7 Mb
6           CNAseq  1.1 Mb
7      Methylation 67.4 Mb
8        RPPAArray    0 Mb
9          gistica  4.3 Mb
10         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   108 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 77.000  77.000   0.962   0.677   1.353 


--------------------------- 
Available sample meta-data: 
--------------------------- 


years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  27.00   54.00   61.00   62.46   72.00   90.00 

vital_status: 
  0   1 
108  77 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   180.0   351.0   495.2   650.0  2532.0     108 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   223.2   391.0   513.3   633.8  3714.0      57 

tumor_tissue_site: 
esophagus 
      185 




pathology_M_stage: 
  m0   m1  m1a   mx NA's 
 136    4    5   18   22 

gender: 
female   male 
    27    158 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1998    2007    2011    2009    2012    2013       7 

radiation_therapy: 
  no  yes NA's 
 124   43   18 

karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   60.00   80.00   73.82   90.00  100.00     117 

histological_type: 
    esophagus adenocarcinoma  nos esophagus squamous cell carcinoma 
                               89                                96 

number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   17.62   30.00   34.48   47.25  102.00      87 

residual_tumor: 
  r0   r1   r2   rx NA's 
 137   13    2    7   26 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.526   4.000  21.000      50 

race: 
                    asian black or african american                     white 
                       46                         5                       114 
                     NA's 
                       20 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     6                     88                     91 

admin.bcr: 
nationwide children's hospital 
                           185 

admin.day_of_dcc_upload: 
  1 
185 

admin.disease_code: 
esca 
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

patient.age_began_smoking_in_years: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   16.50   21.00   22.33   26.00   57.00     106 

patient.alcohol_history_documented: 
  no  yes NA's 
  53  129    3 

patient.amount_of_alcohol_consumption_per_day: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    1.00    1.00    1.83    2.00   14.00      91 

patient.antireflux_treatment_types.antireflux_treatment_type: 
medically treated      no treatment              NA's 
               39                 8               138 

patient.antireflux_treatment_types.antireflux_treatment_type.2: 
surgically treated               NA's 
                 2                183 

patient.barretts_esophagus: 
     no  yes-uk yes-usa    NA's 
    118      10      18      39 



patient.clinical_cqcf.consent_or_death_status: 
consented  deceased 
      163        22 


patient.clinical_cqcf.days_to_consent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -42.00    0.00    3.00   30.69   31.75 1529.00      29 

patient.clinical_cqcf.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   81.0   245.0   464.5   707.2   909.2  2532.0     163 

patient.clinical_cqcf.histological_type: 
    esophagus adenocarcinoma  nos esophagus squamous cell carcinoma 
                               89                                96 

patient.clinical_cqcf.history_of_neoadjuvant_treatment: 
 no 
185 

patient.clinical_cqcf.normal_tissue_anatomic_site: 
esophageal; distal              other               NA's 
                 3                 63                119 

patient.clinical_cqcf.normal_tissue_proximity: 
distal (>2cm)          NA's 
           64           121 

patient.clinical_cqcf.tumor_type: 
primary 
    185 



patient.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   11.00   16.00   16.55   25.00   30.00 

patient.days_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -32872  -26348  -22313  -22992  -19840  -10222 

patient.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   161.0   283.0   488.7   610.0  2532.0     128 

patient.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -4.0     2.0    94.0   288.9   403.5  3714.0      57 



patient.drugs.drug.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0    15.0    17.0    19.5    27.0    31.0     155 

patient.drugs.drug.2.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     53      82     159     204     270     693     160 

patient.drugs.drug.2.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   25.0    49.0   103.5   154.3   174.5   609.0     157 


patient.drugs.drug.2.measure_of_response: 
clinical progressive disease            complete response 
                           6                           18 
            partial response               stable disease 
                           2                            1 
                        NA's 
                         158 

patient.drugs.drug.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     1.0     1.5     4.2     7.0    12.0     155 

patient.drugs.drug.2.therapy_ongoing: 
  no  yes NA's 
  27    2  156 

patient.drugs.drug.2.therapy_types.therapy_type: 
   ancillary chemotherapy         NA's 
           1           29          155 

patient.drugs.drug.2.tx_on_clinical_trial: 
  no NA's 
  30  155 

patient.drugs.drug.2.year_of_form_completion: 
2012 2013 2014 2015 NA's 
   1    2   12   15  155 



patient.drugs.drug.3.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   12.25   25.50   20.38   29.25   31.00     177 

patient.drugs.drug.3.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  225.0   251.5   325.5   374.3   419.0   693.0     179 

patient.drugs.drug.3.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   97.0   137.5   160.5   222.9   283.0   537.0     177 


patient.drugs.drug.3.measure_of_response: 
clinical progressive disease            complete response 
                           1                            3 
            partial response                         NA's 
                           2                          179 

patient.drugs.drug.3.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    3.50    4.75    6.50   12.00     177 

patient.drugs.drug.3.therapy_ongoing: 
  no  yes NA's 
   6    1  178 

patient.drugs.drug.3.therapy_types.therapy_type: 
chemotherapy         NA's 
           8          177 

patient.drugs.drug.3.tx_on_clinical_trial: 
  no NA's 
   8  177 

patient.drugs.drug.3.year_of_form_completion: 
2013 2014 2015 NA's 
   1    5    2  177 

patient.drugs.drug.4.bcr_drug_barcode: 
tcga-ln-a4a8-d70200                NA's 
                  1                 184 

patient.drugs.drug.4.bcr_drug_uuid: 
f40942c3-c5b9-4fe6-b163-5cb4ff67bc24                                 NA's 
                                   1                                  184 

patient.drugs.drug.4.day_of_form_completion: 
  30 NA's 
   1  184 

patient.drugs.drug.4.days_to_drug_therapy_end: 
 432 NA's 
   1  184 

patient.drugs.drug.4.days_to_drug_therapy_start: 
 276 NA's 
   1  184 

patient.drugs.drug.4.drug_name: 
cisplatin      NA's 
        1       184 

patient.drugs.drug.4.measure_of_response: 
complete response              NA's 
                1               184 

patient.drugs.drug.4.month_of_form_completion: 
   1 NA's 
   1  184 

patient.drugs.drug.4.therapy_ongoing: 
  no NA's 
   1  184 

patient.drugs.drug.4.therapy_types.therapy_type: 
chemotherapy         NA's 
           1          184 

patient.drugs.drug.4.tx_on_clinical_trial: 
  no NA's 
   1  184 

patient.drugs.drug.4.year_of_form_completion: 
2015 NA's 
   1  184 



patient.drugs.drug.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   15.00   19.00   19.23   26.00   31.00     142 

patient.drugs.drug.days_to_drug_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  53.00   83.75  147.50  196.53  269.50  693.00     147 

patient.drugs.drug.days_to_drug_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   23.0    49.0    97.0   132.7   151.0   556.0     144 


patient.drugs.drug.measure_of_response: 
clinical progressive disease            complete response 
                          10                           28 
            partial response               stable disease 
                           2                            1 
                        NA's 
                         144 

patient.drugs.drug.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   4.512  10.000  12.000     142 

patient.drugs.drug.therapy_ongoing: 
  no  yes NA's 
  41    1  143 

patient.drugs.drug.therapy_types.therapy_type: 
chemotherapy         NA's 
          43          142 

patient.drugs.drug.tx_on_clinical_trial: 
  no NA's 
  43  142 

patient.drugs.drug.year_of_form_completion: 
2012 2013 2014 2015 NA's 
   1    5   14   23  142 

patient.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     6                     88                     91 

patient.follow_ups.follow_up.2.additional_treatment_completion_success_outcome: 
  complete response progressive disease      stable disease                NA's 
                  9                   1                   1                 174 



patient.follow_ups.follow_up.2.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   12.75   14.50   14.75   17.25   23.00     173 

patient.follow_ups.follow_up.2.days_to_death: 
 318 1405 NA's 
   1    1  183 

patient.follow_ups.follow_up.2.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  191.0   536.5   824.0   831.3  1041.5  1590.0     174 

patient.follow_ups.follow_up.2.lost_follow_up: 
  no NA's 
  12  173 

patient.follow_ups.follow_up.2.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.750  10.500   8.583  11.000  12.000     173 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no NA's 
  11  174 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
         8          1        176 

patient.follow_ups.follow_up.2.postoperative_rx_tx: 
  no  yes NA's 
   8    4  173 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success: 
complete remission/response         progressive disease 
                          9                           3 
                       NA's 
                        173 

patient.follow_ups.follow_up.2.radiation_therapy: 
  no  yes NA's 
   8    4  173 

patient.follow_ups.follow_up.2.vital_status: 
alive  dead  NA's 
   10     2   173 

patient.follow_ups.follow_up.2.year_of_form_completion: 
2014 2015 NA's 
  10    2  173 

patient.follow_ups.follow_up.3.additional_treatment_completion_success_outcome: 
complete response              NA's 
                3               182 

patient.follow_ups.follow_up.3.bcr_followup_barcode: 
tcga-vr-a8eo-f72384 tcga-vr-a8ey-f70874 tcga-vr-aa4g-f68749                NA's 
                  1                   1                   1                 182 

patient.follow_ups.follow_up.3.bcr_followup_uuid: 
3c306ef8-9cfa-4da9-a86d-f0c302e196d2 6ce0ba7e-2efc-470e-95b6-0a1ed25cce62 
                                   1                                    1 
e10208db-9912-4dcf-9549-32a1c4d3a548                                 NA's 
                                   1                                  182 

patient.follow_ups.follow_up.3.day_of_form_completion: 
  10   11   15 NA's 
   1    1    1  182 

patient.follow_ups.follow_up.3.days_to_last_followup: 
 549  785 1025 NA's 
   1    1    1  182 

patient.follow_ups.follow_up.3.lost_follow_up: 
  no NA's 
   3  182 

patient.follow_ups.follow_up.3.month_of_form_completion: 
   3    6   12 NA's 
   1    1    1  182 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no NA's 
   3  182 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status: 
tumor free       NA's 
         3        182 

patient.follow_ups.follow_up.3.postoperative_rx_tx: 
  no NA's 
   3  182 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success: 
complete remission/response                        NA's 
                          3                         182 

patient.follow_ups.follow_up.3.radiation_therapy: 
  no  yes NA's 
   2    1  182 

patient.follow_ups.follow_up.3.vital_status: 
alive  NA's 
    3   182 

patient.follow_ups.follow_up.3.year_of_form_completion: 
2014 2015 NA's 
   1    2  182 

patient.follow_ups.follow_up.4.additional_treatment_completion_success_outcome: 
complete response              NA's 
                1               184 

patient.follow_ups.follow_up.4.bcr_followup_barcode: 
tcga-vr-a8ey-f72386                NA's 
                  1                 184 

patient.follow_ups.follow_up.4.bcr_followup_uuid: 
1f5d8381-d3b8-4dd0-b6a9-b2a93553b623                                 NA's 
                                   1                                  184 

patient.follow_ups.follow_up.4.day_of_form_completion: 
  11 NA's 
   1  184 

patient.follow_ups.follow_up.4.days_to_last_followup: 
1025 NA's 
   1  184 

patient.follow_ups.follow_up.4.lost_follow_up: 
  no NA's 
   1  184 

patient.follow_ups.follow_up.4.month_of_form_completion: 
   6 NA's 
   1  184 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no NA's 
   1  184 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status: 
tumor free       NA's 
         1        184 

patient.follow_ups.follow_up.4.postoperative_rx_tx: 
  no NA's 
   1  184 

patient.follow_ups.follow_up.4.primary_therapy_outcome_success: 
complete remission/response                        NA's 
                          1                         184 

patient.follow_ups.follow_up.4.radiation_therapy: 
  no NA's 
   1  184 

patient.follow_ups.follow_up.4.vital_status: 
alive  NA's 
    1   184 

patient.follow_ups.follow_up.4.year_of_form_completion: 
2015 NA's 
   1  184 

patient.follow_ups.follow_up.additional_treatment_completion_success_outcome: 
  complete response    partial response progressive disease      stable disease 
                 78                   1                  30                   5 
               NA's 
                 71 



patient.follow_ups.follow_up.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   15.00   19.00   17.92   22.00   31.00      46 

patient.follow_ups.follow_up.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   143.8   293.0   392.2   564.8  1458.0     155 

patient.follow_ups.follow_up.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   342.5   403.0   564.2   635.5  3714.0      78 

patient.follow_ups.follow_up.lost_follow_up: 
  no  yes NA's 
 113   26   46 

patient.follow_ups.follow_up.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    5.00    6.46   12.00   12.00      46 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy: 
  no  yes NA's 
   2    1  182 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy: 
  no NA's 
   3  182 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure: 
 735 NA's 
   1  184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
 282  548  666  824 NA's 
   1    1    1    1  181 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site: 
          lung other, specify           NA's 
             1              2            182 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence                    NA's 
                      3                       1                     181 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text: 
brain - right occipital lobe and cerebellum 
                                          1 
                                      colon 
                                          1 
                                      liver 
                                          1 
                                       NA's 
                                        182 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
   3    1  181 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.progression_determined_by: 
biopsy with histologic confirmation                  convincing imaging 
                                  1                                   3 
                               NA's 
                                181 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy: 
  no NA's 
   2  183 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy: 
 yes NA's 
   2  183 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment: 
 282  850 1127 NA's 
   1    1    1  182 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site: 
         liver           lung other, specify           NA's 
             1              1              1            182 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type: 
distant metastasis               NA's 
                 3                182 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text: 
adrenal gland          NA's 
            1           184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   3  182 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.progression_determined_by: 
convincing imaging               NA's 
                 3                182 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy: 
  no NA's 
   1  184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_radiation_therapy: 
 yes NA's 
   1  184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment: 
1011 NA's 
   1  184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site: 
other, specify           NA's 
             1            184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type: 
distant metastasis               NA's 
                 1                184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_occurrence_anatomic_site_text: 
thigh  NA's 
    1   184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   1  184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.progression_determined_by: 
biopsy with histologic confirmation                                NA's 
                                  1                                 184 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no  yes NA's 
  17   14  154 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no  yes NA's 
  17   15  153 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure: 
 542  587  735  882 NA's 
   1    1    1    1  181 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   25.0   158.0   267.0   291.9   320.0   885.0     148 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site: 
         liver           lung other, specify           NA's 
             4              5              9            167 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence       new primary tumor 
                     13                      22                       2 
                   NA's 
                    148 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
  29    6  150 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.progression_determined_by: 
biopsy with histologic confirmation                  convincing imaging 
                                  6                                  12 
                               NA's 
                                167 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
  77   37   71 

patient.follow_ups.follow_up.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
        75         35         75 

patient.follow_ups.follow_up.postoperative_rx_tx: 
  no  yes NA's 
  80   31   74 

patient.follow_ups.follow_up.primary_therapy_outcome_success: 
complete remission/response  partial remission/response 
                         90                           3 
        progressive disease              stable disease 
                          9                           7 
                       NA's 
                         76 

patient.follow_ups.follow_up.radiation_therapy: 
  no  yes NA's 
  73   36   76 

patient.follow_ups.follow_up.vital_status: 
alive  dead  NA's 
  107    30    48 

patient.follow_ups.follow_up.year_of_form_completion: 
2012 2013 2014 2015 NA's 
   1   10   70   58   46 

patient.frequency_of_alcohol_consumption: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   3.673   7.000   7.000      72 

patient.gender: 
female   male 
    27    158 

patient.goblet_cells_present: 
  no  yes NA's 
  16    4  165 

patient.h_pylori_infection: 
current   never    NA's 
      1      74     110 

patient.height: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  150.0   167.0   173.0   172.1   178.0   197.0      10 

patient.history_of_esophageal_cancer: 
  no  yes NA's 
 134   12   39 

patient.history_of_neoadjuvant_treatment: 
 no 
185 

patient.icd_10: 
c15.3 c15.4 c15.5 c15.9 c16.0 
    5    42   124    12     2 



patient.informed_consent_verified: 
yes 
185 

patient.initial_diagnosis_by: 
   screening surveillance  symptomatic         NA's 
          10            3          137           35 

patient.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   4.865   8.000  12.000 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy: 
  no  yes NA's 
  10    1  174 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy: 
  no  yes NA's 
  10    1  174 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure: 
 735 NA's 
   1  184 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  112.0   242.0   666.0   608.4   824.0  1344.0     172 


patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type: 
distant metastasis               NA's 
                13                172 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text: 
     colon lymph node    stomach       NA's 
         1          2          1        181 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
  12    1  172 

patient.new_tumor_events.new_tumor_event.2.progression_determined_by: 
biopsy with histologic confirmation                  convincing imaging 
                                  4                                   9 
                               NA's 
                                172 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy: 
  no  yes NA's 
   5    1  179 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy: 
  no NA's 
   6  179 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  172.0   302.0   400.0   518.9   812.0   832.0     178 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site: 
         liver other, specify           NA's 
             5              1            179 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence                    NA's 
                      6                       1                     178 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text: 
adrenal    NA's 
      1     184 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   6  179 

patient.new_tumor_events.new_tumor_event.3.progression_determined_by: 
biopsy with histologic confirmation                  convincing imaging 
                                  2                                   5 
                               NA's 
                                178 

patient.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy: 
  no NA's 
   1  184 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy: 
  no NA's 
   1  184 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment: 
 362 NA's 
   1  184 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site: 
other, specify           NA's 
             1            184 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type: 
locoregional recurrence                    NA's 
                      1                     184 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_occurrence_anatomic_site_text: 
lymph node       NA's 
         1        184 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure: 
  no NA's 
   1  184 

patient.new_tumor_events.new_tumor_event.4.progression_determined_by: 
biopsy with histologic confirmation                                NA's 
                                  1                                 184 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy: 
  no  yes NA's 
  22   11  152 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy: 
  no  yes NA's 
  26    7  152 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure: 
 587  735 NA's 
   1    1  183 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   29.0   161.5   325.0   397.9   498.5  1261.0     142 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type: 
     distant metastasis locoregional recurrence                    NA's 
                     29                      14                     142 


patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure: 
  no  yes NA's 
  38    2  145 

patient.new_tumor_events.new_tumor_event.progression_determined_by: 
biopsy with histologic confirmation                  convincing imaging 
                                 12                                  26 
                               NA's 
                                147 

patient.new_tumor_events.new_tumor_event_after_initial_treatment: 
  no  yes NA's 
 127   43   15 

patient.number_of_relatives_diagnosed: 
   0    1    2 NA's 
  30    9    2  144 

patient.number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   17.62   30.00   34.48   47.25  102.00      87 


patient.person_neoplasm_cancer_status: 
tumor free with tumor       NA's 
       107         66         12 

patient.primary_pathology.age_at_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  27.00   54.00   61.00   62.45   72.00   90.00 

patient.primary_pathology.columnar_metaplasia_present: 
  no  yes NA's 
  78   32   75 

patient.primary_pathology.columnar_mucosa_dysplasia: 
 high grade dysplasia   low grade dysplasia negative/no dysplasia 
                   30                     6                    42 
                 NA's 
                  107 

patient.primary_pathology.columnar_mucosa_goblet_cell_present: 
  no  yes NA's 
  73    9  103 

patient.primary_pathology.days_to_initial_pathologic_diagnosis: 
  0 
185 

patient.primary_pathology.eastern_cancer_oncology_group: 
   0    1    2    3 NA's 
  13   40    9    3  120 

patient.primary_pathology.esophageal_tumor_cental_location: 
  distal      mid proximal     NA's 
     128       50        6        1 

patient.primary_pathology.esophageal_tumor_involvement_sites.esophageal_tumor_involvement_site: 
  distal      mid proximal     NA's 
     125       51        8        1 

patient.primary_pathology.esophageal_tumor_involvement_sites.esophageal_tumor_involvement_site.2: 
distal    mid   NA's 
     8      2    175 

patient.primary_pathology.histological_type: 
    esophagus adenocarcinoma  nos esophagus squamous cell carcinoma 
                               89                                96 


patient.primary_pathology.initial_pathologic_diagnosis_method: 
     endoscopic biopsy other method, specify:     surgical resection 
                   107                     42                     31 
                  NA's 
                     5 

patient.primary_pathology.karnofsky_performance_score: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   60.00   80.00   73.82   90.00  100.00     117 

patient.primary_pathology.lymph_node_examined_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00   12.00   14.47   19.00   87.00      50 

patient.primary_pathology.lymph_node_metastasis_radiographic_evidence: 
  no  yes NA's 
  95   51   39 

patient.primary_pathology.neoplasm_histologic_grade: 
g1 g2 g3 gx 
19 77 49 40 

patient.primary_pathology.number_of_lymphnodes_positive_by_he: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.526   4.000  21.000      50 

patient.primary_pathology.number_of_lymphnodes_positive_by_ihc: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3231  0.0000  9.0000     120 

patient.primary_pathology.planned_surgery_status: 
                       no surgery already performed                       yes 
                       15                        52                         5 
                     NA's 
                      113 

patient.primary_pathology.postoperative_rx_tx: 
  no  yes NA's 
 138   16   31 

patient.primary_pathology.primary_lymph_node_presentation_assessment: 
  no  yes NA's 
  33  136   16 

patient.primary_pathology.radiation_therapy: 
  no  yes NA's 
 137   18   30 

patient.primary_pathology.residual_tumor: 
  r0   r1   r2   rx NA's 
 137   13    2    7   26 

patient.primary_pathology.treatment_prior_to_surgery: 
              no treatment radiation and chemotherapy 
                        53                          2 
                      NA's 
                       130 

patient.primary_pathology.tumor_tissue_site: 
esophagus 
      185 

patient.primary_pathology.year_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1998    2007    2011    2009    2012    2013       7 

patient.prior_dx: 
 no yes 
171  14 

patient.race: 
                    asian black or african american                     white 
                       46                         5                       114 
                     NA's 
                       20 

patient.radiations.radiation.anatomic_treatment_site: 
   local recurrence primary tumor field       regional site                NA's 
                  3                  24                  21                 137 



patient.radiations.radiation.day_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   15.00   19.00   18.54   22.00   31.00     135 

patient.radiations.radiation.days_to_radiation_therapy_end: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  56.00   74.75  122.50  135.21  166.75  342.00     137 

patient.radiations.radiation.days_to_radiation_therapy_start: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   40.00   92.00   99.63  126.00  342.00     136 

patient.radiations.radiation.measure_of_response: 
               complete response                 partial response 
                              33                                2 
radiographic progressive disease                   stable disease 
                              13                                1 
                            NA's 
                             136 

patient.radiations.radiation.month_of_form_completion: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    1.00    3.84    8.50   12.00     135 

patient.radiations.radiation.numfractions: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   20.00   25.00   22.49   25.00   28.00     140 

patient.radiations.radiation.radiation_dosage: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     30      45      50    1220    2982    5400     139 

patient.radiations.radiation.radiation_treatment_ongoing: 
  no  yes NA's 
  49    1  135 

patient.radiations.radiation.radiation_type: 
external     NA's 
      47      138 

patient.radiations.radiation.units: 
 cgy   gy NA's 
  12   34  139 

patient.radiations.radiation.year_of_form_completion: 
2013 2014 2015 NA's 
   5   15   30  135 

patient.reflux_history: 
  no  yes NA's 
  97   57   31 



patient.stage_event.system_version: 
5th 6th 7th 
 24  68  93 


patient.stage_event.tnm_categories.clinical_categories.clinical_n: 
  n0   n1   n2   nx NA's 
  23   26    3   13  120 


patient.stage_event.tnm_categories.pathologic_categories.pathologic_m: 
  m0   m1  m1a   mx NA's 
 136    4    5   18   22 




patient.stopped_smoking_year: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1949    1974    1992    1990    2010    2013     116 

patient.tissue_prospective_collection_indicator: 
  no  yes NA's 
 107   76    2 

patient.tissue_retrospective_collection_indicator: 
  no  yes NA's 
  76  107    2 


patient.tobacco_smoking_history: 
current reformed smoker for < or = 15 years 
                                         37 
     current reformed smoker for > 15 years 
                                         36 
                             current smoker 
                                         37 
                        lifelong non-smoker 
                                         56 
                                       NA's 
                                         19 

patient.vital_status: 
alive  dead 
  128    57 

patient.weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  46.00   62.00   72.00   75.04   84.50  193.00       2 

patient.year_of_form_completion: 
2012 2013 2014 2015 
   6   75   99    5 


patient.microsatellite_instability_test_results.microsatellite_instability_test_result.mononucleotide_and_dinucleotide_marker_panel_analysis_status: 
msi-h msi-l   mss  NA's 
    2    18    69    96 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason: 
      case matched expression control      case matched methylation control 
                                   12                                     5 
                  germline comparison primary tumor and metastatic tumor(s) 
                                    2                                     1 
                                 NA's 
                                  165 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2: 
case matched methylation control                             NA's 
                              10                              175 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3: 
germline comparison                NA's 
                  4                 181 

patient.bcr_canonical_check.bcr_patient_canonical_status: 
       canonical canonical - plus 
             165               20 

patient.biospecimen_cqcf.consent_or_death_status: 
consented  deceased 
      163        22 


patient.biospecimen_cqcf.days_to_pathology_review: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    73.0   390.5  1257.8  2027.8  5461.0       7 

patient.biospecimen_cqcf.days_to_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -13.00    0.00    5.50   25.29   42.75  336.00       7 

patient.biospecimen_cqcf.digital_image_submitted: 
  no NA's 
   6  179 

patient.biospecimen_cqcf.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     4                     61                    120 

patient.biospecimen_cqcf.histological_type: 
    esophagus adenocarcinoma  nos esophagus squamous cell carcinoma 
                               89                                96 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment: 
 no 
185 

patient.biospecimen_cqcf.method_of_sample_procurement: 
           endoscopic biopsy endoscopic mucosal resection 
                          23                            1 
          surgical resection 
                         161 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    7.75   33.00   34.83   53.00   84.00     179 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement: 
                   blood draw other method (please specify) 
                            4                             1 
           surgical resection                          NA's 
                            3                           177 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site: 
esophageal; distal              other               NA's 
                 1                  3                181 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
            3           182 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number: 
   1    2 NA's 
  10    2  173 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid: 
6c56860f-48ff-47a9-a2c2-583ec966d2f6                                 NA's 
                                   1                                  184 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_conc: 
0.12795    NA's 
      1     184 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qm: 
nanodrop     NA's 
       1      184 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qty: 
19.1925    NA's 
      1     184 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_vol: 
 150 NA's 
   1  184 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number: 
   2 NA's 
   1  184 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -18.00    0.00    4.00   53.42   43.00 1529.00       7 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc: 
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.1250   0.2092   0.3372  29.8080   0.7875 216.0000      155 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm: 
  nano drop    nanodrop salting-out        NA's 
         13          15           2         155 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   15.00   15.00   23.87   15.10  147.00     155 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.30   36.83   66.50   69.39   93.75  150.00     155 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement: 
                   blood draw other method (please specify) 
                          123                             1 
           surgical resection 
                           61 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site: 
esophageal; distal              other               NA's 
                 2                 60                123 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity: 
distal (>2cm)          NA's 
           61           124 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number: 
  1 
185 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching: 
yes 
185 

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
                       25                         5                        74 
                     NA's 
                       81 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching: 
pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                1 
                                                                                                                             NA's 
                                                                                                                              184 

patient.biospecimen_cqcf.sample_prescreened: 
 no yes 
 13 172 

patient.biospecimen_cqcf.site_of_disease: 
esophagus      NA's 
      175        10 

patient.biospecimen_cqcf.site_of_disease_description: 
  distal   middle proximal     NA's 
     126       43        6       10 

patient.biospecimen_cqcf.submitted_for_lce: 
 no yes 
176   9 

patient.biospecimen_cqcf.top_slide_submitted: 
 yes NA's 
   6  179 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   6.071  10.000  20.000     171 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   62.50   70.00   72.14   80.00   90.00     171 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight: 
   5   50  100  400 NA's 
  11    1    1    1  171 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number: 
   1    2 NA's 
   1   13  171 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.bcr_sample_uuid: 
bb3b0ddf-9896-4a64-b59a-5a545383cbf0 e4fd9fd5-f75f-4b63-9c84-13887bb13682 
                                   1                                    1 
                                NA's 
                                 183 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_necrosis_percent: 
   0   10 NA's 
   1    1  183 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_nuclei_percent: 
  60 NA's 
   2  183 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_weight: 
 100  500 NA's 
   1    1  183 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.vial_number: 
   1 NA's 
   2  183 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.908   0.000  20.000 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  40.00   60.00   70.00   72.68   80.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      5      97     150     273     350    3344 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number: 
  1 
185 

patient.biospecimen_cqcf.vessel_used: 
biospecimen storage bag                cassette                cryomold 
                     40                      19                      13 
               cryovial 
                    113 

patient.days_to_index: 
   0 NA's 
 178    7 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
primary tumor and metastatic tumor(s)                                  NA's 
                                    1                                   184 



patient.samples.sample.2.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   62.0   181.8   551.0  1356.9  2063.2  5481.0       7 

patient.samples.sample.2.diagnostic_slides.ffpe_slide_uuid: 
044c13b6-a5d9-4dbb-bd0c-b392bb8a46d5                                 NA's 
                                   1                                  184 

patient.samples.sample.2.diagnostic_slides.ffpe_slide_uuid.2: 
54636905-0758-440e-843e-f8c3af64f7e1                                 NA's 
                                   1                                  184 

patient.samples.sample.2.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   100.0   120.0   137.7   162.5   320.0     125 

patient.samples.sample.2.is_ffpe: 
 no 
185 

patient.samples.sample.2.oct_embedded: 
false  true 
  155    30 

patient.samples.sample.2.pathology_report_file_name: 
tcga-v5-a7rc.8d80b81b-b363-4971-ae00-e357fbc6aef9.pdf 
                                                    1 
                                                 NA's 
                                                  184 

patient.samples.sample.2.pathology_report_uuid: 
8d80b81b-b363-4971-ae00-e357fbc6aef9                                 NA's 
                                   1                                  184 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.780   1.830   1.830   1.854   1.870   1.980     176 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
  13 NA's 
   9  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.15 0.16 0.17 0.18 NA's 
   2    2    4    1  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
  12   14   30 NA's 
   1    1    7  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   9  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   1    5   11 NA's 
   7    1    1  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   1    2    3 NA's 
   2    4    3  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
a24l a261 a407 NA's 
   1    7    1  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row: 
   b    c    f    h NA's 
   2    1    3    3  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   0 NA's 
   9  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  23 NA's 
   9  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
   0 NA's 
   9  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2012 2013 2014 NA's 
   1    7    1  176 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132158045 132269707 164054266 156474147 167802632 174166142         8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   9   31 NA's 
 168    9    8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.5000  0.5000  0.4829  0.5000  0.5000       8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00    8.00    8.00   14.07   26.00   30.00       8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 177    8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   7.000   5.401   7.000  11.000       8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.469   7.000  10.000       8 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   40.00   40.00   38.13   40.00   40.00       8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
 177    8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  20   80 NA's 
   9  168    8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2012 2013 2014 NA's 
   1   72  104    8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.50   19.85   32.04   37.03   50.24   81.56     176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna                 NA's 
                 168                    9                    8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    w NA's 
   9  168    8 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration: 
0.15 0.16 0.17 0.18 NA's 
   2    2    4    1  176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
 168   17 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
 168   17 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             177 
                                            NA's 
                                               8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
 177    8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                   repli-g                      NA's 
                        9                       168                         8 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.980   1.100   1.130   1.229   1.360   1.620     176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.000   7.100   7.411   7.600   8.900     176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec    NA's 
      9     176 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2    3 NA's 
  53    7    1  124 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132178930 132269545 132269705 132258296 132269732 132269742       177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   9 NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   8   30 NA's 
   7    1  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   1    5 NA's 
   1    7  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   4    7    8 NA's 
   1    4    3  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id: 
a25i a295 NA's 
   1    7  177 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2013 NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                   8                  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
   8  177 



patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               8 
                                            NA's 
                                             177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
   8  177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
      8     177 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2 NA's 
   3    5  177 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
128923807 132270931 153889148 151139902 164053928 167780533 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  1   5   9  26 
  7  10 153  15 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07000 0.08000 0.08000 0.09151 0.08000 0.17000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   16.00   26.00   21.99   29.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   3.000   4.762  10.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    4.00    5.00    5.67    7.00   12.00 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   2.800   3.200   2.857   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 23 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
6.7  10  13  40 
  7  15  10 153 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2012 2013 2014 
  20   83   82 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
128923797 132271282 145676591 146727310 164053277 167780454        63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   5    9   26 NA's 
   1   23   98   63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1402  0.1600  0.1700      63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   16.00   24.00   20.57   26.00   30.00      63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
 122   63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   5.607  10.000  12.000      63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   6.000   6.426   9.000  12.000      63 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   1.500   1.600   1.835   1.700   3.600      63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  23 NA's 
 122   63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
 6.7   10   40 NA's 
   1   98   23   63 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2012 2013 2014 NA's 
  20   69   33   63 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
128923781 128924843 128925012 140498484 158908578 167788091       171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   1    5    9 NA's 
   4    1    9  171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0850  0.1057  0.1475  0.1700     171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
  12   14   25 NA's 
   1    4    9  171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
  14  171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   3    5   11 NA's 
   9    4    1  171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.500   5.143   9.000  12.000     171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id: 
a249 a27g a40c NA's 
   1    9    4  171 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.343   3.000   2.489   3.200   3.600     171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  23 NA's 
  14  171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
 6.7   13   40 NA's 
   4    1    9  171 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2012 2013 2014 NA's 
   1    9    4  171 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
128924393 132272360 153889141 151156137 164054012 167780538 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  1  26 
178   7 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.14 0.15 0.16 0.17 
  29   53   64   39 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   17.00   26.00   22.02   29.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   4.114   7.000  11.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   5.000   5.459   7.000  10.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.010   1.070   1.066   1.070   1.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 23 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
6.7  10 
178   7 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2012 2013 2014 
  20   73   92 

patient.samples.sample.2.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.02   13.43   21.97   31.48   41.87  227.42 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type: 
dna 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id: 
  d 
185 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration: 
0.14 0.15 0.16 0.17 
  29   53   64   39 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
yes 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             185 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
185 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type: 
            adna preparation type pre-extracted dna received by tss 
                              155                                30 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method: 
picogreen 
      185 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence: 
   1    2    3 NA's 
  53    7    1  124 



patient.samples.sample.2.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    6.00   13.00   13.59   21.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe: 
 no 
185 

patient.samples.sample.2.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   9.000   8.059  12.000  12.000 

patient.samples.sample.2.portions.portion.portion_number: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   4.622  11.000  21.000 

patient.samples.sample.2.portions.portion.portion_sequence: 
   1    2 NA's 
 117    6   62 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
  60  125 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   5.767   5.000  40.000     125 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.517   0.000  20.000     125 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis: 
   0    3 NA's 
  59    1  125 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0    1    3   20 NA's 
  51    4    1    4  125 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    95.0   100.0    88.5   100.0   100.0     125 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   11.08    5.00   85.00     125 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells: 
   0   22 NA's 
  59    1  125 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei: 
   0   60 NA's 
  59    1  125 

patient.samples.sample.2.portions.portion.slides.slide.section_location: 
 top NA's 
  60  125 

patient.samples.sample.2.portions.portion.weight: 
   2   10   20   30 NA's 
  95    1    1   58   30 

patient.samples.sample.2.portions.portion.year_of_creation: 
2012 2013 2014 
  57   94   34 

patient.samples.sample.2.sample_type: 
blood derived normal           metastatic  solid tissue normal 
                 125                    1                   59 

patient.samples.sample.2.sample_type_id: 
  6  10  11 
  1 125  59 

patient.samples.sample.2.vial_number: 
  a   b 
181   4 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
germline comparison                NA's 
                  4                 181 



patient.samples.sample.3.days_to_collection: 
 104  113  241  447 NA's 
   1    1    1    1  181 

patient.samples.sample.3.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    27.5   105.0   124.2   160.0   350.0     179 

patient.samples.sample.3.is_ffpe: 
  no NA's 
   7  178 

patient.samples.sample.3.oct_embedded: 
false  true  NA's 
    3     4   178 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio: 
1.74 1.84 1.89 1.96 NA's 
   1    1    1    2  180 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched expression control                            NA's 
                              3                             182 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
160340562      NA's 
        1       184 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
  13   31 NA's 
   4    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.16 0.18 NA's 
   1    4  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
  12   16   18   26 NA's 
   1    2    1    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   3   10   11   12 NA's 
   1    2    1    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
   1    2    3 NA's 
   2    1    2  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
a24l a337 a354 a38e NA's 
   1    2    1    1  180 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
   0  3.2 NA's 
   4    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  23 NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
   0   20 NA's 
   4    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2012 2013 2014 NA's 
   1    3    1  180 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched expression control                            NA's 
                              3                             182 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132178882 132254349 153889448 147263633 153889450 164106036       180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   9   13   31 NA's 
   1    1    4  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
0.16 0.18  0.5 NA's 
   1    4    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   16.00   17.00   19.67   24.00   30.00     179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   6  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.750  10.000   7.833  10.750  12.000     179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
   1    2    3    6 NA's 
   2    1    2    1  179 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
   0  3.6   40 NA's 
   1    4    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
   6  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
   0   20   80 NA's 
   1    4    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2012 2013 2014 NA's 
   1    4    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.13    7.07    9.34   17.69   33.12   33.78     180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                  rna                 NA's 
                   1                    5                  179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    w NA's 
   5    1  179 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration: 
0.16 0.18 NA's 
   1    4  180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
   1  184 

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
                        5                         1                       179 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.250   1.300   1.620   1.556   1.800   1.810     180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.50    8.20    8.30    8.34    8.70    9.00     180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec    NA's 
      5     180 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2 NA's 
   5    1  179 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132178938 164054046 164054057 159178727 167802666 167803926       180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   9 NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   8   26   30 NA's 
   2    2    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   1    2    7 NA's 
   1    2    2  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   3    6    7 NA's 
   1    1    3  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id: 
a25i a36t a43y a44r NA's 
   1    2    1    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row: 
   d    e    f    g NA's 
   1    2    1    1  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2013 2014 NA's 
   1    4  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                   5                  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
   5  180 



patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
   5  180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
      5     180 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2 NA's 
   4    1  180 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132270979 143076068 153881033 152068731 162198231 167780506       178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
   9 NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
0.07 0.08 NA's 
   3    4  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   10.50   16.00   14.86   17.00   26.00     178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   7.500  10.000   8.857  11.000  12.000     178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   3    5    6    7 NA's 
   1    1    2    3  178 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row: 
   d    e    f    g NA's 
   1    3    1    2  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
 1.4  2.8  3.2 NA's 
   1    2    4  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  23 NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
  20   40 NA's 
   1    6  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2012 2013 2014 NA's 
   2    3    2  178 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason: 
case matched methylation control                             NA's 
                               3                              182 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132270095 139288156 160348040 151608124 160356230 164055414       179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
   5   26 NA's 
   4    2  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
0.14 0.15 0.16 NA's 
   1    2    3  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   7   16   18   26 NA's 
   2    2    1    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
   6  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   3   10   11   12 NA's 
   1    2    2    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   3    6    7 NA's 
   1    2    3  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id: 
a248 a33f a357 a388 NA's 
   2    2    1    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row: 
   d    e    f    g NA's 
   1    2    1    2  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
0.98  1.6 1.95 2.08 NA's 
   1    2    2    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  23 NA's 
   6  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
   7   10   13 NA's 
   1    2    3  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2012 2013 2014 NA's 
   2    3    1  179 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode: 
tcga-ig-a3i8-11a-11d-a249-05                         NA's 
                           1                          184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid: 
e5f100b9-7eb1-453b-bd57-dba050748659                                 NA's 
                                   1                                  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
132270992      NA's 
        1       184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   5 NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
0.16 NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
  12 NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
  11 NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   3 NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id: 
a249 NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row: 
   g NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
2.08 NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  23 NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
  13 NA's 
   1  184 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2012 NA's 
   1  184 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132272359 143080868 153889385 152071315 162197616 167780491       178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   1 NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.14 0.15 0.16 NA's 
   1    3    3  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    7.00   14.00   14.57   21.00   26.00     178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000  10.000   7.286  10.500  11.000     178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   3    5    6    7 NA's 
   1    1    2    3  178 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_row: 
   d    e    f    g NA's 
   1    3    1    2  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
0.94 1.01 1.07 NA's 
   1    3    3  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
 6.7 NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2012 2013 2014 NA's 
   2    2    3  178 

patient.samples.sample.3.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.640   8.115  14.980  15.161  16.005  38.270     178 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type: 
 dna NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id: 
   d NA's 
   7  178 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration: 
0.14 0.15 0.16 NA's 
   1    3    3  178 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
   7  178 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
   7  178 

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
   5    1  179 



patient.samples.sample.3.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   16.00   24.00   22.29   28.50   30.00     178 

patient.samples.sample.3.portions.portion.is_ffpe: 
  no NA's 
   7  178 

patient.samples.sample.3.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   7.000   8.000   7.714   9.000  12.000     178 

patient.samples.sample.3.portions.portion.portion_number: 
   1   11   12   21 NA's 
   1    4    1    1  178 

patient.samples.sample.3.portions.portion.portion_sequence: 
   1    2 NA's 
   5    1  179 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
   6  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   1    3    5   15 NA's 
   1    1    3    1  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration: 
   0    1    4 NA's 
   3    2    1  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis: 
   0 NA's 
   6  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   0    1    3 NA's 
   3    2    1  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells: 
   6   15   70  100 NA's 
   1    1    1    3  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells: 
   0   30   85   94 NA's 
   3    1    1    1  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells: 
   0 NA's 
   6  179 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei: 
   0 NA's 
   6  179 

patient.samples.sample.3.portions.portion.slides.slide.section_location: 
 top NA's 
   6  179 

patient.samples.sample.3.portions.portion.weight: 
   2    5   10   30 NA's 
   1    1    1    4  178 

patient.samples.sample.3.portions.portion.year_of_creation: 
2011 2012 2013 2014 NA's 
   1    1    4    1  178 

patient.samples.sample.3.sample_type: 
blood derived normal  solid tissue normal                 NA's 
                   1                    6                  178 

patient.samples.sample.3.sample_type_id: 
  10   11 NA's 
   1    6  178 

patient.samples.sample.3.vial_number: 
   a NA's 
   7  178 



patient.samples.sample.days_to_collection: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   62.0   181.8   551.0  1352.0  2044.0  5481.0       7 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.2: 
718fd821-e782-4388-83cb-a5e666af66b1                                 NA's 
                                   1                                  184 

patient.samples.sample.initial_weight: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    80.0   140.0   223.9   290.0  1890.0       3 

patient.samples.sample.is_ffpe: 
 no 
185 

patient.samples.sample.oct_embedded: 
false  true 
   77   108 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.380   1.850   1.880   1.879   1.910   2.120 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
160338175 160340550 160340610 160340323 160340706 160341029       175 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
 13  31 
175  10 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration: 
0.15 0.16 0.17 0.18 
  13   60   59   53 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  12.00   16.00   26.00   22.67   29.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
185 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   3.000   4.708  10.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column: 
 1  2  3  4  5 
68 53 36 16 12 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity: 
  0   3 3.2 3.4 3.6 
175   2   3   2   3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
 23 
185 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume: 
  0  20 
175  10 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment: 
2012 2013 2014 
  20   83   82 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode: 
tcga-ig-a3yb-01a-11r-a360-13 tcga-ig-a51d-01a-11r-a360-13 
                           1                            1 
tcga-l5-a4oi-01a-11r-a360-13                         NA's 
                           1                          182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid: 
02fac5fc-b838-4ed0-96fd-218c84e9373c 1107e842-e7d1-4625-a678-f8295fc40849 
                                   1                                    1 
60abcbec-1c27-4188-b37c-67b98ad2bee4                                 NA's 
                                   1                                  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id: 
  13 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration: 
0.16 0.18 NA's 
   1    2  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment: 
  29 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment: 
   1 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column: 
   1 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id: 
a360 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row: 
   b    c    d NA's 
   1    1    1  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity: 
   0 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center: 
  23 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume: 
   0 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment: 
2014 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode: 
tcga-ig-a3yb-01a-11r-a36d-31 tcga-ig-a51d-01a-11r-a36d-31 
                           1                            1 
tcga-l5-a4oi-01a-11r-a36d-31                         NA's 
                           1                          182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid: 
2235d68c-75ea-47f2-a3c5-9e9cff416d22 70092809-ec16-413c-85b4-d9f58359fe32 
                                   1                                    1 
df2c969f-190e-4697-aa69-4c5b2b0f7f2b                                 NA's 
                                   1                                  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom: 
164082192 164082215 164082216      NA's 
        1         1         1       182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id: 
  31 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration: 
0.16 0.18 NA's 
   1    2  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment: 
  29 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment: 
   1 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column: 
   7 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id: 
a36d NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row: 
   b    c    d NA's 
   1    1    1  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity: 
3.73 4.19 NA's 
   1    2  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center: 
  23 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume: 
23.3 NA's 
   3  182 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment: 
2014 NA's 
   3  182 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
128923778 132254376 153889500 151817942 164105004 167812260        10 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
 13  31 
 10 175 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration: 
0.15 0.16 0.17 0.18 
  13   60   59   53 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  12.00   16.00   26.00   22.67   29.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
 no 
185 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   3.000   4.708  10.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column: 
 1  2  3  4  5 
68 53 36 16 12 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity: 
  0   3 3.2 3.4 3.6 
 10  11  57  57  50 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
 23 
185 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume: 
  0  20 
 10 175 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment: 
2012 2013 2014 
  20   83   82 

patient.samples.sample.portions.portion.analytes.analyte.2.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.83   30.93   47.83   50.28   67.28  115.88 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type: 
rna 
185 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id: 
  r 
185 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration: 
0.15 0.16 0.17 0.18 
  13   60   59   53 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             185 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
 no 
185 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type: 
mirvana (allprep dna) rna                      NA's 
                      184                         1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.480   1.400   1.580   1.572   1.710   2.630 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.400   7.800   8.053   8.700   9.900 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method: 
uv spec 
    185 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence: 
   1    2    3 NA's 
 159   12    1   13 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132177718 132269737 164054088 155586049 167802888 167804727         9 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   9 NA's 
 176    9 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration: 
 0.5 NA's 
 176    9 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0     8.0     8.0    13.3    16.0    30.0       9 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 176    9 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   5.506   7.000  10.000       9 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.750   4.000   3.722   5.000   8.000       9 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity: 
  40 NA's 
 176    9 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
 176    9 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume: 
  80 NA's 
 176    9 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment: 
2013 2014 NA's 
  73  103    9 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                 176                    9 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
 176    9 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
 176    9 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
 176    9 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             176 
                                            NA's 
                                               9 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
 176    9 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type: 
repli-g    NA's 
    176       9 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence: 
   1    2    3 NA's 
 152   12    1   20 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
128924399 132270943 153889435 151474298 164055879 167822339 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
 1  5  9 26 
90 18 74  3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.0800  0.1500  0.1256  0.1600  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   17.00   26.00   22.13   29.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
 no 
185 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   4.411   7.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   3.562   5.000   8.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.070   1.600   1.988   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
 23 
185 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume: 
6.7  10  13  40 
 90   3  18  74 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment: 
2012 2013 2014 
  20   78   87 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
128924352 132270966 160341050 151756790 164056084 167780504 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
 1  5  9 26 
 3 70 80 32 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.0800  0.1400  0.1231  0.1600  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.00   16.00   26.00   22.08   29.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
 no 
185 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    1.00    3.00    4.73   10.00   12.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   3.654   5.000  10.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.820   2.210   2.417   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
 23 
185 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume: 
6.7  10  13  40 
  3  32  70  80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment: 
2012 2013 2014 
  20   83   82 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
128923951 132270969 160354989 150866764 164053851 167780343        20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id: 
   5    9   26 NA's 
  90   23   52   20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1500  0.1600  0.1465  0.1600  0.1700      20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    7.0    17.0    25.0    22.1    26.0    30.0      20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe: 
  no NA's 
 165   20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    5.03   10.00   12.00      20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.527   4.000  12.000      20 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.700   1.950   2.056   2.080   3.600      20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center: 
  23 NA's 
 165   20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume: 
  10   13   40 NA's 
  52   90   23   20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment: 
2012 2013 2014 NA's 
  20   79   66   20 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
128924356 132270990 145676580 148258091 160355810 164103632       141 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id: 
   5    9   26 NA's 
   7    8   29  141 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1400  0.1500  0.1409  0.1600  0.1700     141 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   16.00   18.00   20.68   25.00   29.00     141 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe: 
  no NA's 
  44  141 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   6.068  10.000  12.000     141 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.568   3.250   9.000     141 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.500   1.600   1.925   1.982   3.600     141 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center: 
  23 NA's 
  44  141 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume: 
  10   13   40 NA's 
  29    7    8  141 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment: 
2012 2013 2014 NA's 
   4   30   10  141 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom: 
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
128924376 132159638 153889392 149062172 164081868 167780542        89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
 1 23 26 
92 89  4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration: 
0.14 0.15 0.16 0.17 NA's 
  14   25   38   19   89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   14.00   26.00   21.07   29.00   30.00      89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  96   89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   4.385   7.750  11.000      89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.688   5.000   8.000      89 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.94    1.01    1.07    1.07    1.07    1.60      89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 23 
185 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume: 
 6.7   10 NA's 
  92    4   89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment: 
2012 2013 2014 NA's 
  16   37   43   89 

patient.samples.sample.portions.portion.analytes.analyte.amount: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.59   21.13   29.63   30.82   40.40   66.44 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type: 
dna 
185 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id: 
  d 
185 



patient.samples.sample.portions.portion.analytes.analyte.concentration: 
0.14 0.15 0.16 0.17 
  26   47   71   41 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
185 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
yes 
185 

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
picogreen 
      185 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence: 
   1    2    3 NA's 
 159   12    1   13 



patient.samples.sample.portions.portion.day_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    6.00   14.00   13.36   18.00   31.00 

patient.samples.sample.portions.portion.is_ffpe: 
 no 
185 

patient.samples.sample.portions.portion.month_of_creation: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   6.000   9.000   8.049  11.000  12.000 

patient.samples.sample.portions.portion.portion_number: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  11.00   11.00   11.00   12.97   11.00   32.00 

patient.samples.sample.portions.portion.portion_sequence: 
   1    2    3 NA's 
 120   11    7   47 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe: 
  no NA's 
 183    2 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    4.00   10.35   10.00   80.00       2 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   3.623   2.000  20.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.557   5.000  18.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   3.355   2.000  20.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   3.661   0.000  50.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   19.00   18.61   30.00   75.00       2 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   62.50   75.00   75.17   93.50  100.00       2 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   65.00   75.00   75.77   85.00  100.00       2 

patient.samples.sample.portions.portion.slides.slide.section_location: 
 top NA's 
 183    2 

patient.samples.sample.portions.portion.weight: 
  5  10  20  30 
 16   3   9 157 

patient.samples.sample.portions.portion.year_of_creation: 
2011 2012 2013 2014 
   1   58   93   33 


patient.samples.sample.portions.shipment_portion.center_id: 
  20 NA's 
 126   59 

patient.samples.sample.portions.shipment_portion.is_ffpe: 
  no NA's 
 126   59 

patient.samples.sample.portions.shipment_portion.plate_id: 
a41x a41y NA's 
  81   45   59 

patient.samples.sample.portions.shipment_portion.portion_number: 
  11   21   41   51 NA's 
  10  108    7    1   59 

patient.samples.sample.portions.shipment_portion.portion_sequence: 
   1    2    5 NA's 
   8   88    1   88 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment: 
   4 NA's 
 126   59 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment: 
   6 NA's 
 126   59 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment: 
2014 NA's 
 126   59 

patient.samples.sample.sample_type: 
primary tumor 
          185 

patient.samples.sample.sample_type_id: 
  1 
185 

patient.samples.sample.vial_number: 
  a   b 
174  11 

}}
\keyword{datasets}
