\name{ LAML }
\alias{ LAML }
\docType{data}
\title{ LAML }

\details{
\preformatted{
> LAML
A MultiAssayExperiment object of 3 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 3: 
 [1] RNASeqGene: ExpressionSet with 19990 rows and 179 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 173 columns 
 [3] Methylation: SummarizedExperiment with 27578 rows and 194 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( LAML )
CharacterList of length 3
[["RNASeqGene"]] AADACL3 AADACL4 ABCA4 ABCB10 ABCD3 ... UTY VCY XGPY2 XKRY2 ZFY
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["Methylation"]] cg00000292 cg00002426 cg00003994 ... cg27662877 cg27665659

> colnames( LAML )
CharacterList of length 3
[["RNASeqGene"]] TCGA-AB-2803-03A-01T-0734-13 ... TCGA-AB-3012-03A-01T-0736-13
[["RNASeq2GeneNorm"]] TCGA-AB-2803-03A-01T-0734-13 ...
[["Methylation"]] TCGA-AB-2802-03A-01D-0741-05 ... TCGA-AB-3012-03A-01D-0741-05

Sizes of each ExperimentList element:

            assay size.Mb
1      RNASeqGene  1.1 Mb
2 RNASeq2GeneNorm  1.2 Mb
3     Methylation  4.4 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   79 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
118.000 118.000   0.748   0.586   0.918 


--------------------------- 
Available sample meta-data: 
--------------------------- 


years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  18.00   44.00   57.00   54.98   67.00   88.00 

vital_status: 
  0   1 
 66 131 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    99.5   273.0   356.3   488.2  1706.0      79 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   288.5   699.0   903.0  1461.0  2861.0     130 

tumor_tissue_site: 
bone marrow        NA's 
        196           1 

gender: 
female   male 
    89    108 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2001    2004    2006    2006    2008    2010 

race: 
                    asian black or african american                     white 
                        2                        14                       179 
                     NA's 
                        2 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     1                    193                      3 

admin.bcr: 
washington university 
                  197 

admin.day_of_dcc_upload: 
  7 
197 

admin.disease_code: 
laml 
 197 


admin.month_of_dcc_upload: 
  1 
197 

admin.patient_withdrawal.withdrawn: 
false 
  197 

admin.project_code: 
tcga 
 197 

admin.year_of_dcc_upload: 
2015 
 197 

patient.acute_myeloid_leukemia_calgb_cytogenetics_risk_category: 
          favorable intermediate/normal                poor                NA's 
                 37                 115                  42                   3 

patient.age_at_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  18.00   44.00   58.00   54.98   67.00   88.00 

patient.atra_exposure: 
  no  yes NA's 
 189    4    4 


patient.cumulative_agent_total_dose: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0       0    2000    3547    4875   32000     123 



patient.cytogenetic_abnormalities.cytogenetic_abnormality.3: 
del (7q) / 7q-       t (9;11)      trisomy 8           NA's 
            10              1              5            181 

patient.cytogenetic_abnormalities.cytogenetic_abnormality.4: 
t (15;17) trisomy 8      NA's 
        1         3       193 


patient.cytogenetic_analysis_performed_ind: 
  no  yes NA's 
   1  193    3 

patient.day_of_form_completion: 
 14 
197 

patient.days_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -32326  -24684  -21184  -20243  -16285   -6636 

patient.days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    99.5   273.0   356.3   488.2  1706.0      79 

patient.days_to_initial_pathologic_diagnosis: 
  0 
197 

patient.days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   288.5   699.0   903.0  1461.0  2861.0     130 

patient.disease_detection_molecular_analysis_method_type: 
rt-pcr   NA's 
    86    111 

patient.ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     1                    193                      3 

patient.fish_evaluation_performed_ind: 
  no  yes NA's 
  41  153    3 


patient.fish_test_component_results.fish_test_component_result.2.fish_test_component_percentage_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     0.0    14.1     1.0   100.0      91 


patient.fish_test_component_results.fish_test_component_result.3.fish_test_component_percentage_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   14.69    1.00  100.00     110 


patient.fish_test_component_results.fish_test_component_result.4.fish_test_component_percentage_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   10.86    0.00  100.00     139 


patient.fish_test_component_results.fish_test_component_result.5.fish_test_component_percentage_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   9.087   0.000  99.000     151 

patient.fish_test_component_results.fish_test_component_result.6.fish_test_component: 
-5 or del(5q) -7 or del(7q)            +8     tel-aml 1          NA's 
            3            24            12             1           157 

patient.fish_test_component_results.fish_test_component_result.6.fish_test_component_percentage_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   6.838   0.000  99.000     157 

patient.fish_test_component_results.fish_test_component_result.7.fish_test_component: 
-5 or del(5q) -7 or del(7q)            +8         other          NA's 
           19            11             1             2           164 

patient.fish_test_component_results.fish_test_component_result.7.fish_test_component_percentage_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   5.061   0.000  86.000     164 

patient.fish_test_component_results.fish_test_component_result.8.fish_test_component: 
-5 or del(5q) -7 or del(7q)     del (20q)         other          NA's 
            4             1             1             3           188 

patient.fish_test_component_results.fish_test_component_result.8.fish_test_component_percentage_value: 
   0    4   97 NA's 
   7    1    1  188 

patient.fish_test_component_results.fish_test_component_result.9.fish_test_component: 
other  NA's 
    2   195 

patient.fish_test_component_results.fish_test_component_result.9.fish_test_component_percentage_value: 
   0   63 NA's 
   1    1  195 


patient.fish_test_component_results.fish_test_component_result.fish_test_component_percentage_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   18.66    4.00  100.00      45 

patient.fluorescence_in_situ_hybrid_cytogenetics_metaphase_nucleus_result_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   20.00   20.00   20.02   20.00   32.00       5 

patient.fluorescence_in_situ_hybridization_abnormal_result_indicator: 
  no  yes NA's 
  78   76   43 

patient.gender: 
female   male 
    89    108 

patient.history_of_neoadjuvant_treatment: 
 no yes 
148  49 

patient.hydroxyurea_administration_prior_registration_clinical_study_indicator: 
 no yes 
148  49 

patient.hydroxyurea_agent_administered_day_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   1.527   3.000   4.000     123 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.10.immunophenotype_cytochemistry_testing_result: 
cd15 not tested   cd19 negative cd19 not tested   cd19 positive            NA's 
              4              49             134               9               1 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.11.immunophenotype_cytochemistry_testing_result: 
  cd19 negative cd19 not tested   cd20 negative cd20 not tested            NA's 
              1               3               2             190               1 


patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.13.immunophenotype_cytochemistry_testing_result: 
cd23 not tested   cd25 negative cd25 not tested            NA's 
              4               1             191               1 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.14.immunophenotype_cytochemistry_testing_result: 
cd25 not tested   cd33 negative cd33 not tested   cd33 positive            NA's 
              4              14              25             153               1 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.15.immunophenotype_cytochemistry_testing_result: 
  cd33 positive   cd34 negative cd34 not tested   cd34 positive            NA's 
              4              45              31             116               1 



patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.18.immunophenotype_cytochemistry_testing_result: 
cd 117 not tested    cd117 positive     cd56 negative   cd56 not tested 
               22               166                 5                 3 
             NA's 
                1 


patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.2.immunophenotype_cytochemistry_testing_result: 
mpx not tested   mpx positive   nse negative nse not tested   nse positive 
             3              1            112             22             59 


patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.21.immunophenotype_cytochemistry_testing_result: 
other cd not tested                NA's 
                  4                 193 


patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.4.immunophenotype_cytochemistry_testing_result: 
  cd3 negative cd3 not tested  cd34 positive tdt not tested 
             7            185              1              4 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.5.immunophenotype_cytochemistry_testing_result: 
cd3 not tested   cd5 negative cd5 not tested   cd5 positive  cd56 positive 
             4             35            154              3              1 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.6.immunophenotype_cytochemistry_testing_result: 
cd117 positive cd5 not tested   cd7 negative cd7 not tested   cd7 positive 
             1              4              9            167             16 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.7.immunophenotype_cytochemistry_testing_result: 
  cd10 negative cd10 not tested    cd7 negative  cd7 not tested            NA's 
              2             190               1               3               1 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.8.immunophenotype_cytochemistry_testing_result: 
cd10 not tested   cd14 negative cd14 not tested   cd14 positive            NA's 
              4              63             118              11               1 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.9.immunophenotype_cytochemistry_testing_result: 
cd14 not tested   cd15 negative cd15 not tested   cd15 positive            NA's 
              4               6             183               3               1 

patient.immunophenotype_cytochemistry_testing_results.immunophenotype_cytochemistry_testing_result_values.immunophenotype_cytochemistry_testing_result: 
  mpx negative mpx not tested   mpx positive  na not tested 
            31             18            144              4 

patient.informed_consent_verified: 
yes 
197 

patient.lab_procedure_abnormal_lymphocyte_result_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.301   2.000  48.000       1 

patient.lab_procedure_blast_cell_outcome_percentage_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   51.00   72.00   66.48   85.00  100.00 

patient.lab_procedure_bone_marrow_band_cell_result_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.741   2.000  22.000 

patient.lab_procedure_bone_marrow_basophil_result_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.3472  0.0000 10.0000 

patient.lab_procedure_bone_marrow_blast_cell_outcome_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    6.00   32.00   36.62   63.00   98.00 

patient.lab_procedure_bone_marrow_cellularity_outcome_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   75.00   90.00   79.79   90.00  100.00      10 

patient.lab_procedure_bone_marrow_lab_eosinophil_result_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.7223  1.0000 12.0000 

patient.lab_procedure_bone_marrow_lymphocyte_outcome_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    9.00   19.00   27.04   42.00   94.00 

patient.lab_procedure_bone_marrow_metamyelocyte_result_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.4569  0.0000  6.0000 

patient.lab_procedure_bone_marrow_myelocyte_result_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   1.066   1.000  22.000 

patient.lab_procedure_bone_marrow_neutrophil_result_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    2.00    6.00   12.19   18.00   94.00 

patient.lab_procedure_bone_marrow_prolymphocyte_result_percent_value: 
  0   1   2   4 
191   3   2   1 

patient.lab_procedure_bone_marrow_promonocyte_count_result_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    0.00    1.67    0.00   41.00 

patient.lab_procedure_bone_marrow_promyelocyte_result_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   2.076   0.000  71.000 

patient.lab_procedure_hematocrit_outcome_percent_value: 
   9 NA's 
   1  196 

patient.lab_procedure_hemoglobin_result_specified_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.000   9.000   9.000   9.546  10.000  14.000       1 

patient.lab_procedure_leukocyte_result_unspecified_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00   17.00   37.79   52.25  298.00       1 

patient.lab_procedure_monocyte_result_percent_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.0     1.0     5.0    12.5    19.0    76.0 

patient.lab_procedure_platelet_result_specified_value: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   8.00   31.00   52.00   66.11   86.00  351.00 


patient.leukemia_specimen_cell_source_type: 
bone marrow aspirate 
                 197 

patient.molecular_analysis_abnormal_result_indicator: 
  no  yes NA's 
  49   38  110 






patient.molecular_analysis_abnormality_testing_results.molecular_analysis_abnormality_testing_result_values.7.molecular_analysis_abnormality_testing_result: 
activating ras negative           npmc negative           npmc positive 
                      5                      19                       5 
                   NA's 
                    168 

patient.molecular_analysis_abnormality_testing_results.molecular_analysis_abnormality_testing_result_values.8.molecular_analysis_abnormality_testing_result: 
npmc negative          NA's 
            5           192 


patient.molecular_analysis_performed_indicator: 
  no  yes NA's 
 107   86    4 

patient.month_of_form_completion: 
 12 
197 

patient.patient_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2802    2853    2904    2905    2956    3012 


patient.prior_dx: 
 no yes 
183  14 

patient.prior_hematologic_disorder_diagnosis_indicator: 
  no  yes NA's 
 182    3   12 

patient.race: 
                    asian black or african american                     white 
                        2                        14                       179 
                     NA's 
                        2 

patient.steroid_therapy_administered: 
  no NA's 
   1  196 

patient.tissue_source_site: 
 ab 
197 

patient.total_dose_units: 
  mg NA's 
  53  144 

patient.tumor_tissue_site: 
bone marrow        NA's 
        196           1 

patient.vital_status: 
alive  dead 
   66   131 

patient.year_of_form_completion: 
2010 
 197 

patient.year_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2001    2004    2006    2006    2008    2010 

patient.bcr_canonical_check.bcr_patient_canonical_status: 
canonical 
      197 

patient.days_to_index: 
  0 
197 



patient.samples.sample.2.is_ffpe: 
 no 
197 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id: 
   8    9 NA's 
   1    8  188 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   9  188 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id: 
 732  755  761 NA's 
   1    3    5  188 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center: 
  23 NA's 
   9  188 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   8    9   13 NA's 
  76    4   10  107 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  90  107 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  728.0   732.0   732.0   737.6   745.0   755.0     107 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
  90  107 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna            total rna                 NA's 
                  80                   10                  107 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id: 
   t    w NA's 
  10   80  107 



patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
  80  117 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
  80  117 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              90 
                                            NA's 
                                             107 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
  90  107 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode: 
tcga-ab-2845-03b-01w-0728-08                         NA's 
                           1                          196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid: 
0555cf75-c027-43d4-8035-98be08b0f043                                 NA's 
                                   1                                  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8 NA's 
   1  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
   1  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id: 
 728 NA's 
   1  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
   1  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna                 NA's 
                   1                  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id: 
   w NA's 
   1  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_barcode: 
tcga-ab-2845-03b-01w                 NA's 
                   1                  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_uuid: 
b5c92584-71aa-46a1-bc4e-8698b744c229                                 NA's 
                                   1                                  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
   1  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
   1  196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             196 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
   1  196 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  21 NA's 
  72  125 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
  72  125 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
 756 NA's 
  72  125 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  23 NA's 
  72  125 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
 8  9 13 21 
92 41  6 58 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
197 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  728.0   728.0   739.0   739.9   756.0   760.0 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 23 
197 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type: 
                 dna repli-g (qiagen) dna                  rna 
                 120                   61                   10 
           total rna 
                   6 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id: 
  d   r   t   w 
120  10   6  61 



patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
 181   16 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
 181   16 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             197 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
197 



patient.samples.sample.2.portions.portion.is_ffpe: 
 no 
197 

patient.samples.sample.2.portions.portion.portion_number: 
  1 
197 

patient.samples.sample.2.sample_type: 
primary blood derived cancer - peripheral blood 
                                             77 
                            solid tissue normal 
                                            120 

patient.samples.sample.2.sample_type_id: 
  3  11 
 77 120 

patient.samples.sample.2.vial_number: 
  a   b 
120  77 



patient.samples.sample.3.is_ffpe: 
  no NA's 
  77  120 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   8 NA's 
  11  186 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  11  186 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
 726  727  732 NA's 
   2    4    5  186 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
  11  186 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  11                  186 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id: 
   w NA's 
  11  186 



patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
  11  186 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
  11  186 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              11 
                                            NA's 
                                             186 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
  11  186 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
   9   21 NA's 
   1    8  188 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   9  188 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
 756  761 NA's 
   8    1  188 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  23 NA's 
   9  188 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   8    9   13   21 NA's 
  60    6    2    9  120 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  77  120 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  730.0   737.0   737.0   739.6   737.0   760.0     120 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
  77  120 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type: 
                 dna repli-g (qiagen) dna            total rna 
                  17                   58                    2 
                NA's 
                 120 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id: 
   d    t    w NA's 
  17    2   58  120 



patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  75  122 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
  75  122 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              77 
                                            NA's 
                                             120 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  77  120 



patient.samples.sample.3.portions.portion.is_ffpe: 
  no NA's 
  77  120 

patient.samples.sample.3.portions.portion.portion_number: 
   1 NA's 
  77  120 

patient.samples.sample.3.sample_type: 
primary blood derived cancer - peripheral blood 
                                             60 
                            solid tissue normal 
                                             17 
                                           NA's 
                                            120 

patient.samples.sample.3.sample_type_id: 
   3   11 NA's 
  60   17  120 

patient.samples.sample.3.vial_number: 
   a    c NA's 
  17   60  120 



patient.samples.sample.4.is_ffpe: 
  no NA's 
  66  131 



patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   8 NA's 
  44  153 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  44  153 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
 726  727 NA's 
  20   24  153 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
  44  153 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  44                  153 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id: 
   w NA's 
  44  153 



patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
  44  153 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
  44  153 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              44 
                                            NA's 
                                             153 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
  44  153 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode: 
tcga-ab-2833-11a-01d-0756-21 tcga-ab-2981-11a-01d-0756-21 
                           1                            1 
tcga-ab-2988-11a-01d-0756-21                         NA's 
                           1                          194 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid: 
06f9c659-c4d4-4dd8-804c-651a8477a5fc 9cd79e81-9796-499a-a9ce-7a8fe51c36cd 
                                   1                                    1 
b4d80502-0794-4da0-9fca-633c870d8a1a                                 NA's 
                                   1                                  194 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
  21 NA's 
   3  194 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   3  194 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
 756 NA's 
   3  194 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  23 NA's 
   3  194 



patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   8    9   21 NA's 
  10   13   43  131 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  66  131 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  728.0   755.0   756.0   751.6   756.0   756.0     131 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
  66  131 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type: 
                 dna repli-g (qiagen) dna                 NA's 
                  46                   20                  131 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id: 
   d    w NA's 
  46   20  131 



patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  66  131 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
  66  131 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              66 
                                            NA's 
                                             131 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  66  131 



patient.samples.sample.4.portions.portion.is_ffpe: 
  no NA's 
  66  131 

patient.samples.sample.4.portions.portion.portion_number: 
   1 NA's 
  66  131 

patient.samples.sample.4.sample_type: 
primary blood derived cancer - peripheral blood 
                                             14 
                            solid tissue normal 
                                             52 
                                           NA's 
                                            131 

patient.samples.sample.4.sample_type_id: 
   3   11 NA's 
  14   52  131 

patient.samples.sample.4.vial_number: 
   a    b    d NA's 
  46    6   14  131 



patient.samples.sample.5.is_ffpe: 
  no NA's 
  63  134 



patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
   8 NA's 
  14  183 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  14  183 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
 726  727 NA's 
   7    7  183 

patient.samples.sample.5.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
  14  183 

patient.samples.sample.5.portions.portion.analytes.analyte.2.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  14                  183 

patient.samples.sample.5.portions.portion.analytes.analyte.2.analyte_type_id: 
   w NA's 
  14  183 



patient.samples.sample.5.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match: 
 yes NA's 
  14  183 

patient.samples.sample.5.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful: 
 yes NA's 
  14  183 

patient.samples.sample.5.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              14 
                                            NA's 
                                             183 

patient.samples.sample.5.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
  14  183 



patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   8   21 NA's 
  49   14  134 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  63  134 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  728.0   728.0   729.0   735.4   738.0   756.0     134 

patient.samples.sample.5.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
  63  134 

patient.samples.sample.5.portions.portion.analytes.analyte.analyte_type: 
                 dna repli-g (qiagen) dna                 NA's 
                  14                   49                  134 

patient.samples.sample.5.portions.portion.analytes.analyte.analyte_type_id: 
   d    w NA's 
  14   49  134 



patient.samples.sample.5.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  63  134 

patient.samples.sample.5.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
  63  134 

patient.samples.sample.5.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              63 
                                            NA's 
                                             134 

patient.samples.sample.5.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  63  134 



patient.samples.sample.5.portions.portion.is_ffpe: 
  no NA's 
  63  134 

patient.samples.sample.5.portions.portion.portion_number: 
   1 NA's 
  63  134 

patient.samples.sample.5.sample_type: 
solid tissue normal                NA's 
                 63                 134 

patient.samples.sample.5.sample_type_id: 
  11 NA's 
  63  134 

patient.samples.sample.5.vial_number: 
   a    b    c NA's 
  14   44    5  134 



patient.samples.sample.6.is_ffpe: 
  no NA's 
  57  140 



patient.samples.sample.6.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   8 NA's 
  57  140 

patient.samples.sample.6.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  57  140 

patient.samples.sample.6.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
 728  729  737  738 NA's 
   7    7   20   23  140 

patient.samples.sample.6.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
  57  140 

patient.samples.sample.6.portions.portion.analytes.analyte.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  57                  140 

patient.samples.sample.6.portions.portion.analytes.analyte.analyte_type_id: 
   w NA's 
  57  140 



patient.samples.sample.6.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  57  140 

patient.samples.sample.6.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
  57  140 

patient.samples.sample.6.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              57 
                                            NA's 
                                             140 

patient.samples.sample.6.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  57  140 



patient.samples.sample.6.portions.portion.is_ffpe: 
  no NA's 
  57  140 

patient.samples.sample.6.portions.portion.portion_number: 
   1 NA's 
  57  140 

patient.samples.sample.6.sample_type: 
solid tissue normal                NA's 
                 57                 140 

patient.samples.sample.6.sample_type_id: 
  11 NA's 
  57  140 

patient.samples.sample.6.vial_number: 
   b    c    d NA's 
  14   42    1  140 



patient.samples.sample.7.is_ffpe: 
  no NA's 
  14  183 



patient.samples.sample.7.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   8    9 NA's 
  10    4  183 

patient.samples.sample.7.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  14  183 

patient.samples.sample.7.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
 731  737  738  755 NA's 
   3    5    2    4  183 

patient.samples.sample.7.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
  14  183 

patient.samples.sample.7.portions.portion.analytes.analyte.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  14                  183 

patient.samples.sample.7.portions.portion.analytes.analyte.analyte_type_id: 
   w NA's 
  14  183 



patient.samples.sample.7.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  14  183 

patient.samples.sample.7.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
  14  183 

patient.samples.sample.7.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              14 
                                            NA's 
                                             183 

patient.samples.sample.7.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  14  183 



patient.samples.sample.7.portions.portion.is_ffpe: 
  no NA's 
  14  183 

patient.samples.sample.7.portions.portion.portion_number: 
   1 NA's 
  14  183 

patient.samples.sample.7.sample_type: 
solid tissue normal                NA's 
                 14                 183 

patient.samples.sample.7.sample_type_id: 
  11 NA's 
  14  183 

patient.samples.sample.7.vial_number: 
   c    d NA's 
  10    4  183 



patient.samples.sample.8.is_ffpe: 
  no NA's 
  10  187 



patient.samples.sample.8.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
   8    9 NA's 
   3    7  187 

patient.samples.sample.8.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
  10  187 

patient.samples.sample.8.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
 737  738  755 NA's 
   1    2    7  187 

patient.samples.sample.8.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
  23 NA's 
  10  187 

patient.samples.sample.8.portions.portion.analytes.analyte.analyte_type: 
repli-g (qiagen) dna                 NA's 
                  10                  187 

patient.samples.sample.8.portions.portion.analytes.analyte.analyte_type_id: 
   w NA's 
  10  187 



patient.samples.sample.8.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
 yes NA's 
  10  187 

patient.samples.sample.8.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
 yes NA's 
  10  187 

patient.samples.sample.8.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              10 
                                            NA's 
                                             187 

patient.samples.sample.8.portions.portion.analytes.analyte.is_derived_from_ffpe: 
  no NA's 
  10  187 



patient.samples.sample.8.portions.portion.is_ffpe: 
  no NA's 
  10  187 

patient.samples.sample.8.portions.portion.portion_number: 
   1 NA's 
  10  187 

patient.samples.sample.8.sample_type: 
 buccal cell normal solid tissue normal                NA's 
                  1                   9                 187 

patient.samples.sample.8.sample_type_id: 
  11   12 NA's 
   9    1  187 

patient.samples.sample.8.vial_number: 
   a    d NA's 
   1    9  187 



patient.samples.sample.is_ffpe: 
 no 
197 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id: 
  13   21 NA's 
  11  184    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 195    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id: 
 734  735  757 NA's 
   7    4  184    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center: 
  23 NA's 
 195    2 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type: 
      rna total rna      NA's 
      184        11         2 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id: 
   r    t NA's 
 184   11    2 



patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             195 
                                            NA's 
                                               2 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe: 
  no NA's 
 195    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode: 
tcga-ab-2909-03a-01t-0744-13 tcga-ab-2992-03a-01t-0740-13 
                           1                            1 
                        NA's 
                         195 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid: 
647d1ea6-bb66-48d1-ba66-32b57b24851b 77952b61-6b89-4285-9443-a41246a7d8f2 
                                   1                                    1 
                                NA's 
                                 195 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id: 
  13 NA's 
   2  195 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   2  195 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id: 
 740  744 NA's 
   1    1  195 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center: 
  23 NA's 
   2  195 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id: 
   8   13 NA's 
   9  180    8 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 189    8 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  726.0   734.0   735.0   735.3   736.0   740.0       8 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center: 
  23 NA's 
 189    8 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type: 
repli-g (qiagen) dna            total rna                 NA's 
                   9                  180                    8 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id: 
   t    w NA's 
 180    9    8 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match: 
 yes NA's 
   9  188 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful: 
 yes NA's 
   9  188 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             189 
                                            NA's 
                                               8 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe: 
  no NA's 
 189    8 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id: 
   8    9 NA's 
   1    6  190 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
   7  190 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id: 
 733  755  761 NA's 
   1    3    3  190 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center: 
  23 NA's 
   7  190 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode: 
tcga-ab-2903-03a-01w-0761-09                         NA's 
                           1                          196 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid: 
dfa1b9d8-2350-47c4-8e24-999a0fdc367a                                 NA's 
                                   1                                  196 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id: 
   9 NA's 
   1  196 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
   1  196 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id: 
 761 NA's 
   1  196 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center: 
  23 NA's 
   1  196 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id: 
   8    9 NA's 
 134    4   59 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe: 
  no NA's 
 138   59 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  726.0   726.0   732.0   732.1   733.0   755.0      59 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center: 
  23 NA's 
 138   59 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type: 
repli-g (qiagen) dna                 NA's 
                 138                   59 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id: 
   w NA's 
 138   59 



patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match: 
 yes NA's 
 138   59 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful: 
 yes NA's 
 138   59 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             138 
                                            NA's 
                                              59 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe: 
  no NA's 
 138   59 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id: 
   5   21 NA's 
  82  113    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe: 
  no NA's 
 195    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id: 
 741  742  756 NA's 
  24   58  113    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center: 
  23 NA's 
 195    2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id: 
  21 NA's 
  82  115 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe: 
  no NA's 
  82  115 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id: 
 756 NA's 
  82  115 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center: 
  23 NA's 
  82  115 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id: 
  5   8   9  21 
112  34  49   2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe: 
 no 
197 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  730.0   739.0   741.0   739.1   741.0   756.0 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center: 
 23 
197 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type: 
dna 
197 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id: 
  d 
197 



patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match: 
yes 
197 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful: 
yes 
197 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file: 
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             197 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe: 
 no 
197 



patient.samples.sample.portions.portion.is_ffpe: 
 no 
197 

patient.samples.sample.portions.portion.portion_number: 
  1 
197 

patient.samples.sample.sample_type: 
primary blood derived cancer - peripheral blood 
                                            197 

patient.samples.sample.sample_type_id: 
  3 
197 

patient.samples.sample.vial_number: 
  a 
197 

cluster.x: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.324   6.000   7.000      18 

cluster.y: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.176   4.000   5.000      10 

}}
\keyword{datasets}
