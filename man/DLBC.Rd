\name{ DLBC }
\alias{ DLBC }
\docType{data}
\title{ Lymphoid Neoplasm Diffuse Large B-cell Lymphoma }

\details{
\preformatted{
> experiments( DLBC )
ExperimentList class object of length 9: 
 [1] CNASNP-20160128: RaggedExperiment with 44725 rows and 94 columns 
 [2] CNVSNP-20160128: RaggedExperiment with 9343 rows and 94 columns 
 [3] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 48 columns 
 [4] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 48 columns 
 [5] Methylation-20160128: SummarizedExperiment with 485577 rows and 48 columns 
 [6] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 47 columns 
 [7] Mutation-20160128: RaggedExperiment with 16918 rows and 48 columns 
 [8] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 48 columns 
 [9] RPPAArray-20160128: SummarizedExperiment with 192 rows and 33 columns 

> rownames( DLBC )
CharacterList of length 9
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363

> colnames( DLBC )
CharacterList of length 9
[["CNASNP-20160128"]] TCGA-FA-8693-01A-11D-2389-01 ...
[["CNVSNP-20160128"]] TCGA-FA-8693-01A-11D-2389-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-FA-8693-01A-11D-2389-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-FA-8693-01A-11D-2389-01 ...
[["Methylation-20160128"]] TCGA-FA-8693-01A-11D-2399-05 ...
[["miRNASeqGene-20160128"]] TCGA-FA-8693-01A-11R-2401-13 ...
[["Mutation-20160128"]] TCGA-FA-8693-01A-11D-2397-10 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-FA-8693-01A-11R-2404-07 ...
[["RPPAArray-20160128"]] TCGA-FA-8693-01A-21-A45K-20 ...

Sizes of each ExperimentList element:

                              assay size.Mb
1                   CNASNP-20160128  1.2 Mb
2                   CNVSNP-20160128  0.3 Mb
3         GISTIC_AllByGene-20160128  4.3 Mb
4 GISTIC_ThresholdedByGene-20160128  4.3 Mb
5              Methylation-20160128 67.4 Mb
6             miRNASeqGene-20160128  0.1 Mb
7                 Mutation-20160128 11.9 Mb
8          RNASeq2GeneNorm-20160128  1.1 Mb
9                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   39 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  9.000   9.000   1.630   0.858      NA 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.00   46.00   57.50   56.27   67.00   82.00 

vital_status:
 0  1 
39  9 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     58     313     595    1505    1252    6425      39 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     616     946    1328    1581    5980       9 


gender:
female   male 
    26     22 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1987    2009    2011    2009    2012    2013 

radiation_therapy:
  no  yes NA's 
  40    7    1 

histological_type:
diffuse large b-cell lymphoma (dlbcl) nos (any anatomic site nodal or extranodal) 
                                                                               41 
                                                         primary dlbcl of the cns 
                                                                                3 
                                               primary mediastinal (thymic) dlbcl 
                                                                                4 

race:
                    asian black or african american                     white 
                       18                         1                        29 

ethnicity:
    hispanic or latino not hispanic or latino 
                    12                     36 

admin.bcr:
nationwide children's hospital 
                            48 

admin.day_of_dcc_upload:
 1 
48 

admin.disease_code:
dlbc 
  48 


admin.month_of_dcc_upload:
10 
48 

admin.patient_withdrawal.withdrawn:
false 
   48 

admin.project_code:
tcga 
  48 

admin.year_of_dcc_upload:
2015 
  48 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.00   46.00   57.50   56.27   67.00   82.00 

patient.b_lymphocyte_genotyping_method:
          igh pcr igh southern blot              NA's 
                5                 2                41 


patient.bone_marrow_biopsy_done:
  no  yes NA's 
  10   19   19 

patient.bone_marrow_involvement:
  no  yes NA's 
  32    3   13 

patient.clinical_cqcf.consent_or_death_status:
consented 
       48 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  -12.0     0.0    19.0   204.6   101.5  2527.0 


patient.clinical_cqcf.histological_type:
diffuse large b-cell lymphoma (dlbcl) nos (any anatomic site nodal or extranodal) 
                                                                               42 
                                                         primary dlbcl of the cns 
                                                                                3 
                                               primary mediastinal (thymic) dlbcl 
                                                                                3 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                              47 
yes, pharmaceutical treatment prior to resection 
                                               1 

patient.clinical_cqcf.history_of_prior_malignancy:
no 
48 

patient.clinical_cqcf.other_anatomic_site:
left perithyroidal soft tissue                         spleen 
                             1                              1 
         subgaleal, skull bone                           NA's 
                             1                             45 

patient.clinical_cqcf.tumor_type:
primary 
     48 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    3.00   12.50   12.15   17.00   30.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -30256  -24608  -21146  -20731  -16998   -8493 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    313     391     595    1795    1252    6425      43 

patient.days_to_initial_pathologic_diagnosis:
 0 
48 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -10.0    33.5   415.0   907.6  1036.5  5616.0       5 

patient.eastern_cancer_oncology_group:
   0    1    2    3 NA's 
  16   12    2    1   17 

patient.ebv_antibody_status:
negative positive     NA's 
       6        1       41 

patient.ebv_diagnostic_methodology:
eber in situ hybridization                       NA's 
                         7                         41 

patient.ethnicity:
    hispanic or latino not hispanic or latino 
                    12                     36 



patient.extranodal_disease_involvement_sites.extranodal_disease_involvement_site.3:
                                  bone marrow 
                                            1 
                                        brain 
                                            1 
                      pleura/pleural effusion 
                                            1 
soft tissue (muscle, ligaments, subcutaneous) 
                                            2 
                                         NA's 
                                           43 

patient.extranodal_disease_involvement_sites.extranodal_disease_involvement_site.4:
  liver thyroid    NA's 
      1       1      46 

patient.extranodal_disease_involvement_sites.extranodal_disease_involvement_site.5:
                   lung pleura/pleural effusion                    NA's 
                      1                       1                      46 

patient.extranodal_disease_involvement_sites.extranodal_disease_involvement_site.6:
other extranodal site                  NA's 
                    1                    47 

patient.extranodal_disease_involvement_sites.extranodal_disease_involvement_site_other:
bladder omentum  spleen    NA's 
      1       1       2      44 

patient.extranodal_disease_involvement_sites.number_of_involved_extranodal_sites:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   1.243   2.000   6.000      11 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    10.0    11.0    14.5    17.5    30.0      34 

patient.follow_ups.follow_up.2.days_to_death:
 708 NA's 
   1   47 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    719     761    1012    1533    1746    3897      36 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
scheduled follow-up submission                           NA's 
                            14                             34 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  10    4   34 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.000   2.500   4.929   8.000  12.000      34 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 739 NA's 
   1   47 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.first_recurrence_biopsy_confirmed:
  no NA's 
   1   47 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.followup_treatment_success:
complete remission/response         progressive disease 
                         10                           2 
                       NA's 
                         36 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
paraaortic lymph nodes                   NA's 
                     1                     47 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.pet_scan_results:
indeterminate      negative      not done          NA's 
            2             8             3            35 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  10    1   37 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        10          1         37 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         11                           1 
        progressive disease                        NA's 
                          1                          35 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
   8    5   35 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
   1   12   35 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   12     1    35 

patient.follow_ups.follow_up.2.year_of_form_completion:
2014 2015 NA's 
   5    9   34 

patient.follow_ups.follow_up.3.bcr_followup_barcode:
tcga-gs-a9tq-f72118 tcga-gs-a9tx-f72119                NA's 
                  1                   1                  46 

patient.follow_ups.follow_up.3.bcr_followup_uuid:
109ad70a-707b-4db1-8b46-3ae6a20a7463 da46dbc6-0abb-450a-9322-b6206193a4ff 
                                   1                                    1 
                                NA's 
                                  46 

patient.follow_ups.follow_up.3.day_of_form_completion:
  16 NA's 
   2   46 

patient.follow_ups.follow_up.3.days_to_last_followup:
 982 1373 NA's 
   1    1   46 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
scheduled follow-up submission                           NA's 
                             2                             46 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
   2   46 

patient.follow_ups.follow_up.3.month_of_form_completion:
   4 NA's 
   2   46 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.followup_treatment_success:
complete remission/response                        NA's 
                          2                          46 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.pet_scan_results:
negative     NA's 
       2       46 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   2   46 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free       NA's 
         1         47 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          2                          46 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
   1    1   46 

patient.follow_ups.follow_up.3.targeted_molecular_therapy:
 yes NA's 
   2   46 

patient.follow_ups.follow_up.3.vital_status:
alive  NA's 
    2    46 

patient.follow_ups.follow_up.3.year_of_form_completion:
2015 NA's 
   2   46 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   10.00   12.23   17.00   30.00       1 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   58.0   280.5   391.0  1748.6  2402.5  6425.0      41 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   418.5   832.0  1241.0  1387.5  5980.0       9 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             3                             44 
                          NA's 
                             1 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
  31    8    9 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000  10.000   8.191  11.000  12.000       1 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   24.0   176.2   314.0  1568.8  2745.8  6383.0      38 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.first_progression_histology_type:
diffuse large b-cell lymphoma       other histological type 
                            2                             1 
                         NA's 
                           45 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.first_progression_histology_type_other:
composite lymphoma: b-fl-3a (60%) & b-dlcl-nc (40%) 
                                                  1 
                                               NA's 
                                                 47 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.first_recurrence_biopsy_confirmed:
  no  yes NA's 
   5    3   40 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.followup_treatment_success:
complete remission/response  partial remission/response 
                         33                           1 
        progressive disease              stable disease 
                          7                           2 
                       NA's 
                          5 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
inguinal, mediastinal, submandibular, hilar 
                                          1 
                 skin (calf and chest wall) 
                                          1 
                                       NA's 
                                         46 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.pet_scan_results:
indeterminate      negative      not done      positive          NA's 
            2            19            21             2             4 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  32   10    6 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        35          7          6 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         35                           2 
        progressive disease              stable disease 
                          4                           2 
                       NA's 
                          5 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
  39    6    3 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
   2   43    3 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
   39     7     2 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2014    2013    2014    2015       1 

patient.gender:
female   male 
    26     22 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  146.0   157.0   164.8   165.2   171.0   187.0 

patient.histological_type:
diffuse large b-cell lymphoma (dlbcl) nos (any anatomic site nodal or extranodal) 
                                                                               41 
                                                         primary dlbcl of the cns 
                                                                                3 
                                               primary mediastinal (thymic) dlbcl 
                                                                                4 

patient.histology_of_bone_marrow_sample:
concordant histology discordant histology                 NA's 
                   2                    2                   44 

patient.history_of_neoadjuvant_treatment:
 no yes 
 47   1 

patient.hiv_positive_status:
negative     NA's 
      42        6 


patient.icd_o_3_histology:
9680/3 
    48 


patient.igh_genotype_results:
    clonal non-clonal not tested       NA's 
         4          3         16         25 

patient.igk_genotype_results:
not tested       NA's 
        22         26 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.10.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   4                    8                   36 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.10.result_of_immunophenotypic_analysis:
indeterminate      negative      positive          NA's 
            1             5             6            36 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.11.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   2                    8                   38 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.11.result_of_immunophenotypic_analysis:
indeterminate      negative      positive          NA's 
            1             8             1            38 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.12.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   1                    6                   41 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.12.result_of_immunophenotypic_analysis:
indeterminate      negative      positive          NA's 
            1             3             3            41 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.13.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   1                    3                   44 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.13.immunophenotypic_analysis_test:
          cd30 cytoplasmic ig           eber     surface ig           NA's 
             1              1              1              1             44 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.13.result_of_immunophenotypic_analysis:
indeterminate      negative      positive          NA's 
            1             1             2            44 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.14.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   1                    1                   46 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.14.immunophenotypic_analysis_test:
cytoplasmic ig     surface ig           NA's 
             1              1             46 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.14.result_of_immunophenotypic_analysis:
indeterminate      positive          NA's 
            1             1            46 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.15.immunophenotypic_analysis_methodology:
flow cytometry           NA's 
             1             47 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.15.immunophenotypic_analysis_test:
surface ig       NA's 
         1         47 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.15.result_of_immunophenotypic_analysis:
indeterminate          NA's 
            1            47 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.2.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   3                   38                    7 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.2.result_of_immunophenotypic_analysis:
negative positive     NA's 
      12       29        7 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.3.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   2                   36                   10 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.3.result_of_immunophenotypic_analysis:
negative positive     NA's 
       3       35       10 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.4.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   1                   33                   14 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.4.result_of_immunophenotypic_analysis:
negative positive     NA's 
      11       23       14 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.5.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   2                   31                   15 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.5.result_of_immunophenotypic_analysis:
negative positive     NA's 
      12       21       15 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.6.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   2                   29                   17 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.6.result_of_immunophenotypic_analysis:
negative positive     NA's 
      14       17       17 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.7.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   3                   21                   24 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.7.result_of_immunophenotypic_analysis:
negative positive     NA's 
      11       13       24 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.8.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   2                   19                   27 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.8.result_of_immunophenotypic_analysis:
indeterminate      negative      positive          NA's 
            2            11             8            27 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.9.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   4                   14                   30 


patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.9.result_of_immunophenotypic_analysis:
indeterminate      negative      positive          NA's 
            1            12             5            30 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.immunophenotypic_analysis_methodology:
      flow cytometry immunohistochemistry                 NA's 
                   7                   39                    2 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.immunophenotypic_analysis_test:
cd 10> 30%       cd19       cd20       NA's 
        30          8          8          2 

patient.immunophenotypic_analysis_results.immunophenotypic_analysis_result.result_of_immunophenotypic_analysis:
negative positive     NA's 
      20       26        2 

patient.informed_consent_verified:
yes 
 48 

patient.ldh_lab_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  120.0   328.0   436.0   480.3   626.8   978.0      18 

patient.ldh_upper_limit:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  185.0   380.0   450.0   426.8   450.0   618.0      15 


patient.maximum_tumor_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   2.000   3.000   4.601   5.300  17.000      10 

patient.mib1_positive_percentage_range:
  0 - 25%  26 - 50%  51 - 75% 76 - 100%      NA's 
        1         3        12        11        21 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.750   6.000   5.917  10.000  12.000 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  106.0   255.0   279.0   429.6   290.0  1218.0      43 

patient.new_tumor_events.new_tumor_event.first_progression_histology_type:
diffuse large b-cell lymphoma       other histological type 
                            1                             1 
                         NA's 
                           46 

patient.new_tumor_events.new_tumor_event.first_progression_histology_type_other:
composite lymphoma: b-fl-3a (60%), b-dlcl-nc (40%) 
                                                 1 
                                              NA's 
                                                47 

patient.new_tumor_events.new_tumor_event.first_recurrence_biopsy_confirmed:
  no  yes NA's 
   3    2   43 

patient.new_tumor_events.new_tumor_event.followup_treatment_success:
complete remission/response              stable disease 
                          2                           1 
                       NA's 
                         45 


patient.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
bone marrow, skin (calf and chest wall)                                    NA's 
                                      1                                      47 

patient.new_tumor_events.new_tumor_event.pet_scan_results:
indeterminate      negative      not done      positive          NA's 
            1            10            10             3            24 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  27    5   16 


patient.nodal_anatomic_sites.nodal_anatomic_site.10:
splenic    NA's 
      1      47 




patient.nodal_anatomic_sites.nodal_anatomic_site.5:
 iliac-external         splenic supraclavicular            NA's 
              2               1               1              44 

patient.nodal_anatomic_sites.nodal_anatomic_site.6:
   inguinal mediastinal        NA's 
          1           1          46 

patient.nodal_anatomic_sites.nodal_anatomic_site.7:
mediastinal  mesenteric        NA's 
          1           1          46 

patient.nodal_anatomic_sites.nodal_anatomic_site.8:
    para-aortic retroperitoneal            NA's 
              1               1              46 

patient.nodal_anatomic_sites.nodal_anatomic_site.9:
retroperitoneal supraclavicular            NA's 
              1               1              46 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.2.abnormality_tested_methodology:
fish NA's 
  11   37 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.2.abnormality_tested_results:
       normal translocation          NA's 
            7             4            37 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.2.other_genetic_abnormality_tested:
 igh pax5 NA's 
  10    1   37 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.3.abnormality_tested_methodology:
fish NA's 
   4   44 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.3.abnormality_tested_results:
normal   NA's 
     4     44 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.3.other_genetic_abnormality_tested:
bcl10 foxp1   igk  NA's 
    1     1     2    44 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.4.abnormality_tested_methodology:
fish NA's 
   3   45 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.4.abnormality_tested_results:
normal   NA's 
     3     45 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.4.other_genetic_abnormality_tested:
bcl10   igl  NA's 
    1     2    45 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.abnormality_tested_methodology:
cytogenetics         fish         NA's 
           1           12           35 

patient.other_tested_genetic_abnormalities.other_tested_genetic_abnormality.abnormality_tested_results:
  loss normal   NA's 
     1     12     35 



patient.percent_ebv_positive_malignant_cells:
  80 NA's 
   1   47 

patient.percentage_of_follicular_component:
< or = 10%       NA's 
        40          8 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        37          7          4 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         18                           3 
        progressive disease              stable disease 
                          2                           2 
                       NA's 
                         23 

patient.prior_dx:
no 
48 

patient.prior_immunologic_disease_types.prior_immunologic_disease_other:
                       behget's disease        common variable immunodeficiency 
                                      1                                       1 
     diverticulitis requiring colectomy non-insulin dependent diabetes mellitus 
                                      1                                       1 
                                   NA's 
                                     44 

patient.prior_immunologic_disease_types.prior_immunologic_disease_type:
hashimoto's thyroiditis                   other    rheumatoid arthritis 
                      1                       3                       1 
                   NA's 
                     43 

patient.prior_immunologic_disease_types.prior_immunologic_disease_type.2:
other  NA's 
    1    47 

patient.prior_immunosuppressive_therapy_types.prior_immunosuppressive_therapy_other:
immunoglobulin     prednisone           NA's 
             1              1             46 

patient.prior_immunosuppressive_therapy_types.prior_immunosuppressive_therapy_type:
azathioprine methotrexate         none        other         NA's 
           1            1           30            1           15 

patient.prior_immunosuppressive_therapy_types.prior_immunosuppressive_therapy_type.2:
other  NA's 
    1    47 

patient.prior_infectious_disease:
hepatitis b virus       hepatitis c              NA's 
                4                 1                43 

patient.race:
                    asian black or african american                     white 
                       18                         1                        29 

patient.radiation_therapy:
  no  yes NA's 
  28    1   19 

patient.radiations.radiation.anatomic_treatment_site:
       distant site primary tumor field                NA's 
                  1                   7                  40 



patient.radiations.radiation.course_number:
   1 NA's 
   2   46 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    6.00    6.50    9.60   12.75   17.00      38 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   96.0   172.2   221.5   493.5   358.8  2648.0      38 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   76.0   150.8   201.0   473.1   348.2  2646.0      38 

patient.radiations.radiation.measure_of_response:
complete response    stable disease              NA's 
                6                 2                40 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     5.5     9.5     8.3    11.0    11.0      38 


patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   27.0    31.5    39.8   296.1   292.5  1200.0      38 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  10   38 

patient.radiations.radiation.radiation_type:
     external external beam          NA's 
            8             2            38 

patient.radiations.radiation.regimen_indication:
   adjuvant progression        NA's 
          1           1          46 

patient.radiations.radiation.units:
 cgy   gy NA's 
   2    8   38 

patient.radiations.radiation.year_of_form_completion:
2012 2014 2015 NA's 
   4    5    1   38 

patient.stage_event.ann_arbor.b_symptoms:
  no  yes NA's 
  35   11    2 

patient.stage_event.ann_arbor.extranodal_involvement:
  no  yes NA's 
  25   21    2 

patient.stage_event.clinical_stage:
  stage i  stage ii stage iii  stage iv      NA's 
        8        17         5        12         6 

patient.stage_event.system_version:
 4th  5th  6th  7th NA's 
   1    2    6   32    7 

patient.targeted_molecular_therapy:
  no  yes NA's 
   3   22   23 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.2.abnormality_tested_methodology:
fish NA's 
  13   35 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.2.abnormality_tested_results:
amplification          gain        normal translocation          NA's 
            1             1            10             1            35 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.2.genetic_abnormality_tested:
bcl2 bcl6 NA's 
  11    2   35 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.3.abnormality_tested_methodology:
fish NA's 
  12   36 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.3.abnormality_tested_results:
       normal translocation          NA's 
           11             1            36 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.3.genetic_abnormality_tested:
 bcl6 ccnd1  NA's 
   11     1    36 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.4.abnormality_tested_methodology:
fish NA's 
  11   37 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.4.abnormality_tested_results:
normal   NA's 
    11     37 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.4.genetic_abnormality_tested:
ccnd1 malt1  NA's 
    9     2    37 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.5.abnormality_tested_methodology:
fish NA's 
   9   39 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.5.abnormality_tested_results:
normal   NA's 
     9     39 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.5.genetic_abnormality_tested:
malt1  NA's 
    9    39 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.abnormality_tested_methodology:
fish  pcr NA's 
  15    4   29 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.abnormality_tested_results:
         gain          loss        normal translocation          NA's 
            1             1            14             3            29 

patient.tested_genetic_abnormalities.tested_genetic_abnormality.genetic_abnormality_tested:
 bcl2 c-myc  NA's 
    6    13    29 

patient.tissue_prospective_collection_indicator:
 no yes 
 33  15 

patient.tissue_retrospective_collection_indicator:
 no yes 
 15  33 


patient.vital_status:
alive  dead 
   43     5 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  41.50   58.67   68.00   71.04   81.92  127.70 

patient.year_of_form_completion:
2011 2012 2013 2014 2015 
   4   14   14   15    1 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1987    2009    2011    2009    2012    2013 

patient.bcr_canonical_check.bcr_patient_canonical_status:
canonical 
       48 

patient.biospecimen_cqcf.b_cell_tumor_slide_submitted:
  no  yes NA's 
  42    2    4 

patient.biospecimen_cqcf.consent_or_death_status:
consented 
       48 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  -5.00   51.25  343.50 1042.04  955.50 8798.00 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -16.00    0.00    0.00   55.58    0.00 2527.00 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
 42   6 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    12                     13                     23 


patient.biospecimen_cqcf.histological_type:
diffuse large b-cell lymphoma (dlbcl) nos (any anatomic site nodal or extranodal) 
                                                                               42 
                                                         primary dlbcl of the cns 
                                                                                3 
                                               primary mediastinal (thymic) dlbcl 
                                                                                3 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                              47 
yes, pharmaceutical treatment prior to resection 
                                               1 

patient.biospecimen_cqcf.hiv_positive_status:
     negative            no not evaluated 
           26            21             1 

patient.biospecimen_cqcf.method_of_sample_procurement:
     excisional biopsy      incisional biopsy other (please specify) 
                    29                      9                      9 
          other method 
                     1 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -29.0     0.0    19.0   418.8   387.5  3481.0       4 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0941   0.3255  87.0000 104.7816 207.5000 334.0000       29 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   20.00   20.00   21.21   24.00   25.00      29 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   26.0    79.5    90.0   116.9   156.5   270.0      29 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
blood draw       NA's 
        44          4 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1 NA's 
  44    4 


patient.biospecimen_cqcf.other_vessel_used:
tube NA's 
   6   42 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
yes 
 48 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
 48 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 no yes 
  2  46 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 no yes 
  1  47 

patient.biospecimen_cqcf.race:
asian white  NA's 
   10    15    23 

patient.biospecimen_cqcf.sample_prescreened:
yes 
 48 


patient.biospecimen_cqcf.site_of_disease_text:
left perithyroidal soft tissue                         spleen 
                             1                              1 
         subgaleal, skull bone                           NA's 
                             1                             45 

patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
  31    2   15 

patient.biospecimen_cqcf.t_cell_tumor_slide_submitted:
  no  yes NA's 
  41    3    4 

patient.biospecimen_cqcf.top_slide_submitted:
 no yes 
  6  42 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid:
19ed41c5-85c4-4ec5-8cb0-4ce11fbde80d 2ae6e89c-5729-4dd6-8592-87dc5df758ea 
                                   1                                    1 
5efe5e8a-8463-4eec-9634-10b3b2acaa74 a6fe4865-b64f-4e32-a63e-18112068b3f3 
                                   1                                    1 
                                NA's 
                                  44 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
  10   50 NA's 
   3    1   44 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  80 NA's 
   4   44 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
 200 NA's 
   4   44 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   2 NA's 
   4   44 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
 0  5 10 20 50 
41  1  4  1  1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     60      80      90      87      95     100 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  100.0   100.0   186.0   325.1   200.0  2079.0       2 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
 1 
48 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                      1                       9                       7 
               cryovial  other (please specify) 
                     25                       6 

patient.days_to_index:
 0 
48 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   85.0   165.0   542.5   796.1   896.5  3444.0      18 

patient.samples.sample.2.is_ffpe:
no 
48 

patient.samples.sample.2.oct_embedded:
false  NA's 
   30    18 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
125245075 125245124 145674291 147485074 167803719 167803768 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
10 
48 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.5 
 48 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
 8 11 16 
21 17 10 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
no 
48 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
 4  5  7 10 
17  1 20 10 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   5.000   7.000   6.562   8.000   9.000 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
2233 a29f a334 a44q a44w 
  17    1   10   15    5 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
40 
48 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
22 23 
17 31 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
80 
48 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 2014 
  17   11   20 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  152.0   160.0   166.0   164.6   167.0   175.0      31 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna 
                  48 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
 w 
48 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
 0.5 NA's 
  17   31 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
yes 
 48 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 no yes 
 17  31 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              48 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
no 
48 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x 
       47         1 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen      NA's 
       17        31 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9 NA's 
   1    6   41 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
124428098 124428169 150705898 146957510 164054598 167822291 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
 1 10 
26 22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0500  0.0800  0.1350  0.1196  0.1600  0.1800 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
10 14 24 26 30 
 1 17 10 15  5 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
48 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 3  4  7  9 
32  5 10  1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.271   8.000   9.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.008   1.135   2.027   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
22 23 
18 30 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7   40   60 
  17    9   21    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 
  18   10   20 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
124428002 124428069 150705936 145718152 164105324 167823254         4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  10   26 NA's 
  26   18    4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.05000 0.05000 0.08000 0.09977 0.15000 0.17000       4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   14.00   24.00   21.05   26.00   30.00       4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  44    4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   3    4    7    9 NA's 
  29    5    9    1    4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   5.500   5.273   7.000   8.000       4 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.600   3.000   2.455   3.000   3.600       4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
  18   26    4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10   40   60 NA's 
  18    9   17    4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2012 2013 2014 NA's 
  18    9   17    4 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.244e+08 1.244e+08 1.539e+08 1.672e+08 1.641e+08 1.076e+09 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 26 
22 26 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1554  0.1600  0.1800 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
10 14 24 26 30 
 1 17 10 15  5 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
48 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 3  4  7  9 
32  5 10  1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   4.000   4.500   5.854   8.000  11.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.070   1.350   1.317   1.600   1.800 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
22 23 
18 30 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7   10 
   1   21   26 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 2013 2014 
  18   10   20 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.27   14.83   23.51   87.95  113.25  597.00 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
 48 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
 d 
48 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1554  0.1600  0.1800 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 48 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
 18  30 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              48 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
48 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                               16                                 9 
pre-extracted dna received by tss 
                               23 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
       41         7 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00   12.75   17.00   17.98   27.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
no 
48 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   6.000   5.354   9.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
 1 
48 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
  18   30 

patient.samples.sample.2.portions.portion.weight:
   2 NA's 
  16   32 

patient.samples.sample.2.portions.portion.year_of_creation:
2011 2012 2013 2014 
   9   14   14   11 

patient.samples.sample.2.sample_type:
blood derived normal   bone marrow normal 
                  44                    4 

patient.samples.sample.2.sample_type_id:
10 14 
44  4 

patient.samples.sample.2.vial_number:
 a 
48 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   85.0   165.0   542.5   794.0   896.5  3444.0      18 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.2:
23c4445e-5ffb-4722-b913-01f952059e7d ac079933-2c30-42b0-ad5d-140be3da7302 
                                   1                                    1 
                                NA's 
                                  46 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
1c5215a5-4e0c-4c2e-a83b-1bad6f6f67b3 3578770b-6298-4740-9609-413f93e9c082 
                                   1                                    1 
                                NA's 
                                  46 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    75.0   150.0   312.8   360.0  1430.0      18 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.3000  0.5000  0.5615  0.8000  1.5000      35 

patient.samples.sample.is_ffpe:
no 
48 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.5000  0.9000  0.9923  1.1000  2.8000      35 

patient.samples.sample.oct_embedded:
false  true  NA's 
   10    20    18 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.770   1.820   1.860   1.861   1.900   1.980      11 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
124425823 124427399 150756720 146980224 164105516 167788101 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
 7 13 
18 30 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1596  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
10 14 24 26 30 
 1 17 10 15  5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
 3  4  7  9 
32  5 10  1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   4.604   7.000   9.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
2213 2404 a31s a38n a39j 
  17    1   10   15    5 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.730   2.098   3.200   2.829   3.400   3.600 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
22 23 
18 30 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3   20 
  18   30 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 
  18   10   20 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.244e+08 1.244e+08 1.508e+08 1.667e+08 1.641e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
 7 13 
30 18 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1596  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
10 14 24 26 30 
 1 17 10 15  5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
 3  4  7  9 
32  5 10  1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   4.729   7.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
2212 2401 a31o a38c a39d 
  17    1   10   15    5 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   2.210   2.340   2.454   2.600   3.200 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
22 23 
18 30 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
13 20 
30 18 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 2014 
  18   10   20 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  21.62   52.57   85.98  155.74  268.45  572.20 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
 48 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
 r 
48 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1596  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              48 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                       48 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.020   1.620   1.745   1.715   1.855   2.080 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.100   8.000   9.150   8.781   9.500   9.800 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
     48 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
 1  2 
47  1 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
125244139 125245126 148189214 147993633 167788759 167803192 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
10 
48 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
0.5 
 48 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
 8 11 16 
21 17 10 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
 4  5  7 10 
17  1 20 10 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   4.479   8.000   9.000 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
2233 a29f a334 a44p a44w 
  17    1   10   15    5 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
40 
48 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
22 23 
17 31 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
80 
48 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2013 2014 
  17   11   20 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  160.0   166.0   169.0   170.1   172.0   185.0      31 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna 
                  48 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
 w 
48 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
  17   31 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
yes 
 48 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 no yes 
 17  31 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              48 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x 
       47         1 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen      NA's 
       17        31 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
 1  2 
47  1 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
 1.8  1.9    2 NA's 
   1    5    1   41 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
124428096 124428168 150705472 146959641 164054983 167822098 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
 1 10 
26 22 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.050   0.080   0.140   0.115   0.150   0.170 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
10 14 24 26 30 
 1 17 10 15  5 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 3  4  7  9 
32  5 10  1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   4.271   8.000   9.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.000   1.070   1.941   3.200   3.200 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
22 23 
18 30 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67  6.7   40   60 
  17    9   21    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 
  18   10   20 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
124425836 124428065 150705962 146954232 164055031 167823249 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
 5 10 26 
17 26  5 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0500  0.0500  0.0800  0.1021  0.1500  0.1600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
10 14 24 26 30 
 1 17 10 15  5 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
 3  4  7  9 
32  5 10  1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   3.000   4.167   8.000   9.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.400   1.950   2.800   2.501   3.000   3.200 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
22 23 
18 30 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10   13 13.3   40   60 
   5   16    1    9   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2012 2013 2014 
  18   10   20 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
124428480 124428551 150705950 145716945 164105326 167788220         4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5   26 NA's 
  31   13    4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1502  0.1600  0.1700       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   14.00   24.00   21.05   26.00   30.00       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  44    4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   3    4    7    9 NA's 
  29    5    9    1    4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   2.614   3.000   7.000       4 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.600   1.860   1.835   2.000   2.260       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
  18   26    4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10   13 13.3 NA's 
  13   14   17    4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2012 2013 2014 NA's 
  18    9   17    4 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
1.244e+08 1.244e+08 1.539e+08 1.673e+08 1.641e+08 1.076e+09 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 26 
22 26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.13 0.14 0.15 0.16 0.17 
   1   11   22   13    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
10 14 24 26 30 
 1 17 10 15  5 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 3  4  7  9 
32  5 10  1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   2.000   4.229   8.000  11.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.010   1.400   1.274   1.500   1.700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
22 23 
18 30 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7   10 
   1   21   26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 2013 2014 
  18   10   20 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   8.69   41.04   63.80  177.06  330.95  612.70 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
 48 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
 d 
48 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 0.17 
   1   11   22   13    1 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 48 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
 18  30 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              48 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                   48 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
       41         7 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
 1  2 
47  1 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      3      15      21      19      24      30 

patient.samples.sample.portions.portion.is_ffpe:
no 
48 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   5.500   5.312   8.250  12.000 

patient.samples.sample.portions.portion.portion_number:
11 12 
47  1 

patient.samples.sample.portions.portion.portion_sequence:
   1 NA's 
  18   30 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  13   35 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       9       9      35      90      95      39 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   8   40 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   0    5 NA's 
  12    1   35 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0    2    5 NA's 
   6    1    1   40 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   0    5   10 NA's 
  11    1    1   35 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   5.769  10.000  25.000      35 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  70.00   90.00   95.00   92.69  100.00  100.00      35 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  75.00   80.00   90.00   86.77   90.00   95.00      35 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
 top NA's 
  13   35 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
no 
48 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   15.50   80.00   59.65   87.50  100.00       5 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.000   5.000   5.568  10.000  20.000      11 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.915   5.000  15.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    2    5   10 NA's 
  21    1   11    5   10 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   4.106   5.000  40.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    5.00    8.34   10.00   35.00       1 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   80.00   90.00   85.64   95.00  100.00       1 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0    80.0    90.0    86.3    90.5    95.0       1 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
    13     35 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   30.00   30.00   27.94   31.00   79.00 

patient.samples.sample.portions.portion.year_of_creation:
2011 2012 2013 2014 
   9   14   14   11 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
  33   15 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
  33   15 

patient.samples.sample.portions.shipment_portion.plate_id:
a45k NA's 
  33   15 

patient.samples.sample.portions.shipment_portion.portion_number:
  21 NA's 
  33   15 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   2 NA's 
  26   22 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  30 NA's 
  33   15 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   7 NA's 
  33   15 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
  33   15 

patient.samples.sample.sample_type:
primary tumor 
           48 

patient.samples.sample.sample_type_id:
 1 
48 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.2     0.4     0.3     0.4     0.5      35 

patient.samples.sample.vial_number:
 a  b 
47  1 

}}
\keyword{datasets}
