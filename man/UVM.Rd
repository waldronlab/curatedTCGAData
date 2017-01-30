\name{ UVM }
\alias{ UVM }
\docType{data}
\title{ UVM }

\details{
\preformatted{
> UVM
A MultiAssayExperiment object of 10 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 10: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 80 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 80 columns 
 [3] CNASNP: RangedRaggedAssay with 69487 rows and 160 columns 
 [4] CNVSNP: RangedRaggedAssay with 12973 rows and 160 columns 
 [5] CNAseq: RangedRaggedAssay with 6496 rows and 102 columns 
 [6] Methylation: SummarizedExperiment with 485577 rows and 80 columns 
 [7] RPPAArray: ExpressionSet with 192 rows and 12 columns 
 [8] Mutations: RangedRaggedAssay with 2174 rows and 80 columns 
 [9] gistica: SummarizedExperiment with 24776 rows and 80 columns 
 [10] gistict: SummarizedExperiment with 24776 rows and 80 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( UVM )
CharacterList of length 10
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-235670 1:235728-17048405 ... 24:28438116-59018259
[["CNVSNP"]] 1:3218610-120527361 1:149879545-247813706 ... 23:3157107-154905589
[["CNAseq"]] 1:10209-72766537 1:72766538-72811656 ... 23:2699504-154930285
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] PPP4R1 POU4F1 DNAH8 LILRA2 MTOR ... AGAP2 UBR2 SF3A2 SCAF1 BAP1
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( UVM )
CharacterList of length 10
[["RNASeq2GeneNorm"]] TCGA-RZ-AB0B-01A-11R-A405-07 ...
[["miRNASeqGene"]] TCGA-RZ-AB0B-01A-11R-A40B-13 ...
[["CNASNP"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ... TCGA-YZ-A985-10A-01D-A39Y-01
[["CNVSNP"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ... TCGA-YZ-A985-10A-01D-A39Y-01
[["CNAseq"]] TCGA-RZ-AB0B-01A-11D-A40D-26 ... TCGA-VD-AA8P-10A-01D-A40E-26
[["Methylation"]] TCGA-RZ-AB0B-01A-11D-A39X-05 ... TCGA-YZ-A985-01A-11D-A39X-05
[["RPPAArray"]] TCGA-V3-A9ZX-01A-21-A41Z-20 ... TCGA-WC-AA9E-01A-21-A41Z-20
[["Mutations"]] TCGA-RZ-AB0B-01A-11D-A39W-08 ... TCGA-YZ-A985-01A-11D-A39W-08
[["gistica"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ... TCGA-YZ-A985-01A-11D-A39V-01
[["gistict"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ... TCGA-YZ-A985-01A-11D-A39V-01

Sizes of each ExperimentList element:

             assay size.Mb
1  RNASeq2GeneNorm  1.1 Mb
2     miRNASeqGene  0.1 Mb
3           CNASNP  5.4 Mb
4           CNVSNP  1.1 Mb
5           CNAseq  0.4 Mb
6      Methylation 67.4 Mb
7        RPPAArray    0 Mb
8        Mutations 11.3 Mb
9          gistica  4.3 Mb
10         gistict  4.3 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   66 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  14.00   14.00    1.11    0.66    3.82 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  22.00   51.00   61.50   61.65   74.25   86.00 

vital_status:
 0  1 
66 14 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   172.0   406.5   517.9   661.2  1581.0      66 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   299.5   618.0   636.1   889.0  2600.0      13 

tumor_tissue_site:
choroid 
     80 



pathology_N_stage:
  n0   nx NA's 
  52   27    1 

pathology_M_stage:
  m0   m1  m1b   mx NA's 
  51    2    2   23    2 

gender:
female   male 
    35     45 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2007    2011    2012    2012    2013    2013 

radiation_therapy:
  no  yes NA's 
  76    3    1 

race:
white  NA's 
   55    25 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     1                     52                     27 

admin.bcr:
nationwide children's hospital 
                            80 

admin.day_of_dcc_upload:
 1 
80 

admin.disease_code:
uvm 
 80 


admin.month_of_dcc_upload:
10 
80 

admin.patient_withdrawal.withdrawn:
false 
   80 

admin.project_code:
tcga 
  80 

admin.year_of_dcc_upload:
2015 
  80 


patient.clinical_cqcf.consent_or_death_status:
consented 
       80 

patient.clinical_cqcf.country:
        france united kingdom  united states 
            33             25             22 

patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-248.00  -21.00  -12.50   63.46    0.00 1283.00 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
no 
80 

patient.clinical_cqcf.tumor_type:
primary 
     80 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   8.00    9.00   11.00   12.04   12.00   25.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -31475  -27328  -22738  -22707  -18922   -8271 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   149.0   398.0   450.4   455.0  1581.0      67 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    46.5   382.0   457.2   714.0  2149.0      13 

patient.drugs.drug.bcr_drug_barcode:
tcga-v4-a9em-d66786 tcga-v4-a9et-d66793 tcga-v4-a9f1-d66811 tcga-v4-a9f3-d66816 
                  1                   1                   1                   1 
               NA's 
                 76 

patient.drugs.drug.bcr_drug_uuid:
03e230c9-56c9-46ea-a977-54c65fb0964a 6af33bb6-d0ec-444f-bf8e-781b6a753326 
                                   1                                    1 
92f45447-981b-440b-aa4b-22e237defafa c6e8afaa-cd54-43d5-ada6-ea3312148433 
                                   1                                    1 
                                NA's 
                                  76 

patient.drugs.drug.day_of_form_completion:
  23 NA's 
   4   76 

patient.drugs.drug.days_to_drug_therapy_end:
 147  174  175  187 NA's 
   1    1    1    1   76 

patient.drugs.drug.days_to_drug_therapy_start:
  21   22   24   27 NA's 
   1    1    1    1   76 

patient.drugs.drug.drug_name:
fotemustine        NA's 
          4          76 

patient.drugs.drug.month_of_form_completion:
  10 NA's 
   4   76 

patient.drugs.drug.therapy_ongoing:
  no NA's 
   4   76 

patient.drugs.drug.therapy_types.therapy_type:
chemotherapy         NA's 
           4           76 

patient.drugs.drug.tx_on_clinical_trial:
 yes NA's 
   4   76 

patient.drugs.drug.year_of_form_completion:
2014 NA's 
   4   76 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     1                     52                     27 

patient.eye_color:
 blue brown green  NA's 
    9    15     6    50 



patient.follow_ups.follow_up.2.day_of_form_completion:
  17   27   28 NA's 
   1    1    5   73 

patient.follow_ups.follow_up.2.days_to_death:
1396 NA's 
   1   79 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   500.8   743.0   943.3  1042.2  2600.0      74 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
   6    1   73 

patient.follow_ups.follow_up.2.month_of_form_completion:
   1    2    7 NA's 
   1    1    5   73 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   7   73 

patient.follow_ups.follow_up.2.patient_death_reason:
metastatic uveal melanoma                      NA's 
                        1                        79 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         6          1         73 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no NA's 
   7   73 

patient.follow_ups.follow_up.2.radiation_therapy:
  no NA's 
   7   73 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
    6     1    73 

patient.follow_ups.follow_up.2.year_of_form_completion:
2015 NA's 
   7   73 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   16.00   23.00   19.55   26.00   30.00      15 

patient.follow_ups.follow_up.days_to_death:
 389 NA's 
   1   79 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   376.5   661.0   649.6   850.0  2267.0      17 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
  63    2   15 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   9.000   6.831  10.000  10.000      15 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no NA's 
   1   79 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   1   79 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
1008 NA's 
   1   79 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    1    79 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                 79 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1   79 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   7    4   69 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   7    3   70 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   78.0   281.0   413.0   587.4   828.5  1556.0      69 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
         liver other, specify           NA's 
             7              4             69 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      7                       2                       2 
                   NA's 
                     69 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
               choroid left nasofacial sulcus                  scalp 
                     2                      1                      1 
                  NA's 
                    76 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   6    5   69 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  53   11   16 

patient.follow_ups.follow_up.patient_death_reason:
metastatic uveal melanoma                      NA's 
                        1                        79 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        55          8         17 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  59    5   16 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
  61    3   16 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
   63     1    16 

patient.follow_ups.follow_up.year_of_form_completion:
2014 2015 NA's 
  40   25   15 

patient.gender:
female   male 
    35     45 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  147.0   158.0   168.0   166.5   175.0   193.0      27 

patient.history_of_neoadjuvant_treatment:
no 
80 

patient.icd_10:
c69.30 c69.40 c69.80 c69.90 
    67      5      7      1 


patient.icd_o_3_site:
c69.3 c69.4 c69.8 c69.9 
   67     5     7     1 

patient.informed_consent_verified:
yes 
 80 

patient.month_of_form_completion:
 3  5  6  7 12 
12 25 30  8  5 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no NA's 
   2   78 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   1   79 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 595 NA's 
   1   79 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 597  916 NA's 
   1    1   78 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
liver  skin  NA's 
    1     1    78 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                 78 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_histology_other:
melanoma of epitheloid type            no tissue biopsy 
                          1                           1 
                       NA's 
                         78 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    1   78 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   4    8   68 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   6   74 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   56.0   154.0   371.0   645.2   625.0  2020.0      75 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -4.0   186.2   303.5   397.8   432.0  2020.0      64 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
         liver other, specify           skin           NA's 
            11              2              3             64 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
distant metastasis  new primary tumor               NA's 
                13                  4                 63 


patient.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
biliary thyroid    NA's 
      1       1      78 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    5   73 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  62   17    1 

patient.patient_death_reason:
metastatic uveal melanoma            other, specify                      NA's 
                       11                         1                        68 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        61         18          1 

patient.primary_pathology.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  22.00   51.00   61.50   61.65   74.25   86.00 

patient.primary_pathology.cytogenetic_abnormality_list.cytogenetic_abnormality:
 chromosome 1 loss  chromosome 3 loss chromosome 6p gain chromosome 8q gain 
                 9                 26                 16                  1 
              NA's 
                28 

patient.primary_pathology.cytogenetic_abnormality_list.cytogenetic_abnormality.2:
 chromosome 3 loss chromosome 6p gain chromosome 8q gain               NA's 
                 5                  7                 23                 45 

patient.primary_pathology.cytogenetic_abnormality_list.cytogenetic_abnormality.3:
chromosome 8q gain               NA's 
                 9                 71 

patient.primary_pathology.days_to_initial_pathologic_diagnosis:
 0 
80 

patient.primary_pathology.extranocular_nodule_size:
<=5mm  >5mm  NA's 
    5     1    74 

patient.primary_pathology.extrascleral_extension:
  no  yes NA's 
  68    7    5 

patient.primary_pathology.extravascular_matrix_patterns:
                 loops other complex patterns                   NA's 
                    15                      1                     64 

patient.primary_pathology.gene_expression_profile_list.gene_expression_profile:
 class 1 class 1a class 1b  class 2     NA's 
       1        1        5        4       69 

patient.primary_pathology.initial_pathologic_diagnosis_method:
                                   enucleation 
                                            78 
local resection (exoresection, wall resection) 
                                             2 

patient.primary_pathology.metastatic_site_list.metastatic_site:
liver  none other  NA's 
    3    44     1    32 

patient.primary_pathology.metastatic_site_list.metastatic_site.2:
cutaneous      NA's 
        1        79 

patient.primary_pathology.mitotic_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.00    7.00   11.21   16.00   40.00      27 

patient.primary_pathology.other_metastatic_site:
metastatic pancreatic cancer to the liver 
                                        1 
                                     NA's 
                                       79 

patient.primary_pathology.postoperative_rx_tx:
  no  yes NA's 
  72    6    2 

patient.primary_pathology.radiation_therapy:
  no  yes NA's 
  73    4    3 

patient.primary_pathology.tumor_basal_diameter:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.79   14.80   17.00   16.93   19.35   25.00       1 

patient.primary_pathology.tumor_basal_diameter_mx:
echographic measurement  pathologic measurement                    NA's 
                     26                      53                       1 

patient.primary_pathology.tumor_infiltrating_lymphocytes:
             few             many moderate numbers             NA's 
              24                2                2               52 

patient.primary_pathology.tumor_infiltrating_macrophages:
 few NA's 
  17   63 

patient.primary_pathology.tumor_morphology_list.tumor_morphology.2.histological_type:
epithelioid cell     spindle cell             NA's 
              16               21               43 

patient.primary_pathology.tumor_morphology_list.tumor_morphology.2.tumor_morphology_percentage:
    0%  1-30% 31-60% 61-90%   NA's 
     1     24      7      5     43 

patient.primary_pathology.tumor_morphology_list.tumor_morphology.histological_type:
epithelioid cell     spindle cell 
              34               46 

patient.primary_pathology.tumor_morphology_list.tumor_morphology.tumor_morphology_percentage:
 > 90%  1-30% 31-60% 61-90% 
    40      6      8     26 

patient.primary_pathology.tumor_shape_pathologic_clinical:
 diffuse     dome mushroom     NA's 
       2       36       13       29 

patient.primary_pathology.tumor_thickness:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  4.000   8.475  10.500  10.421  12.045  16.000 

patient.primary_pathology.tumor_thickness_measurement:
echographic measurement  pathologic measurement 
                     59                      21 

patient.primary_pathology.tumor_tissue_site_list.tumor_tissue_site:
choroid 
     80 

patient.primary_pathology.tumor_tissue_site_list.tumor_tissue_site.2:
ciliary body         NA's 
          24           56 

patient.primary_pathology.tumor_tissue_site_list.tumor_tissue_site.3:
iris NA's 
   2   78 

patient.primary_pathology.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2007    2011    2012    2012    2013    2013 

patient.prior_dx:
 no yes 
 76   4 

patient.race:
white  NA's 
   55    25 

patient.radiations.radiation.anatomic_treatment_site:
primary tumor field                NA's 
                  2                  78 

patient.radiations.radiation.bcr_radiation_barcode:
tcga-wc-a87y-r64835 tcga-wc-aa9e-r67772                NA's 
                  1                   1                  78 

patient.radiations.radiation.bcr_radiation_uuid:
942ced73-38cf-425e-9603-2c675c5ae541 d177098d-ac7e-44e5-8b8a-133b2cb76a1d 
                                   1                                    1 
                                NA's 
                                  78 

patient.radiations.radiation.day_of_form_completion:
   3   25 NA's 
   1    1   78 

patient.radiations.radiation.days_to_radiation_therapy_end:
  64  296 NA's 
   1    1   78 

patient.radiations.radiation.days_to_radiation_therapy_start:
  22  253 NA's 
   1    1   78 

patient.radiations.radiation.measure_of_response:
complete response    stable disease              NA's 
                1                 1                78 

patient.radiations.radiation.month_of_form_completion:
   9   12 NA's 
   1    1   78 

patient.radiations.radiation.numfractions:
  30 NA's 
   2   78 

patient.radiations.radiation.radiation_dosage:
  60 NA's 
   2   78 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
   2   78 

patient.radiations.radiation.radiation_type:
external     NA's 
       2       78 

patient.radiations.radiation.units:
  gy NA's 
   2   78 

patient.radiations.radiation.year_of_form_completion:
2014 NA's 
   2   78 



patient.stage_event.system_version:
7th 
 80 

patient.stage_event.tnm_categories.clinical_categories.clinical_m:
 m0  m1 m1b  mx 
 73   1   2   4 

patient.stage_event.tnm_categories.clinical_categories.clinical_n:
n0 nx 
76  4 


patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
  m0   m1  m1b   mx NA's 
  51    2    2   23    2 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
  n0   nx NA's 
  52   27    1 


patient.tissue_prospective_collection_indicator:
 no yes 
 77   3 

patient.tissue_retrospective_collection_indicator:
 no yes 
  3  77 


patient.vital_status:
alive  dead 
   67    13 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  41.00   65.00   80.00   79.39   90.20  186.00      27 

patient.year_of_form_completion:
2013 2014 
   5   75 

patient.bcr_canonical_check.bcr_patient_canonical_status:
canonical 
       80 

patient.biospecimen_cqcf.consent_or_death_status:
consented 
       80 

patient.biospecimen_cqcf.country:
        france united kingdom  united states 
            33             25             22 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-248.00  -15.00   -7.00   -8.85    0.00  155.00 

patient.biospecimen_cqcf.ethnicity:
not hispanic or latino                   NA's 
                    21                     59 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
         eye eye; choroid 
          16           64 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
no 
80 

patient.biospecimen_cqcf.laterality:
 left right  NA's 
   25    39    16 

patient.biospecimen_cqcf.method_of_sample_procurement:
                                   enucleation 
                                            77 
local resection (exoresection; wall resection) 
                                             2 
                 other method (please specify) 
                                             1 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   0 NA's 
   1   79 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
0.123  NA's 
    1    79 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
picogreen quantification (qubit)                             NA's 
                               1                               79 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
   6 NA's 
   1   79 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
48.8 NA's 
   1   79 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
blood draw       NA's 
         1         79 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
   2    4   74 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
24e71e64-5d55-4bd0-b291-5fae378b9e8a 992d0e43-d833-4a7e-834d-bd8ff0c486a9 
                                   1                                    1 
                                NA's 
                                  78 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   3 NA's 
   2   78 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-248.00  -21.00  -12.00   26.04    0.00  980.00       2 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0290  0.1400  0.1795  0.2570  0.2343  2.0199      42 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm:
                         nanodrop picogreen  quantification (qubit) 
                                5                                 1 
         picogreen quantification  picogreen quantification (qubit) 
                                1                                31 
                             NA's 
                               42 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.40   15.00   15.00   15.33   15.00   20.00      42 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.90   64.03   83.55   97.02  107.10  230.60      42 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
blood draw       NA's 
        78          2 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
 1  2 
78  2 

patient.biospecimen_cqcf.other_method_of_sample_procurement:
orbitotomy       NA's 
         1         79 

patient.biospecimen_cqcf.other_vessel_used:
bullet   NA's 
     1     79 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
  no  yes NA's 
  15   64    1 

patient.biospecimen_cqcf.path_confirm_report_attached:
yes 
 80 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 yes NA's 
  77    3 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 yes NA's 
  77    3 

patient.biospecimen_cqcf.race:
white  NA's 
   22    58 

patient.biospecimen_cqcf.sample_prescreened:
  no  yes NA's 
   5   74    1 

patient.biospecimen_cqcf.site_of_disease:
 eye NA's 
  64   16 

patient.biospecimen_cqcf.site_of_disease_description:
choroid    NA's 
     64      16 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid:
1b626599-ec44-409d-8123-ed69d8442c19 7b9619b2-1fb6-4076-8a47-fcd392e8b27e 
                                   1                                    1 
f028a8c2-1ad9-4ccd-9ef4-076f90d73806                                 NA's 
                                   1                                   77 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0 NA's 
   3   77 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
 100 NA's 
   3   77 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
  20   50   90 NA's 
   1    1    1   77 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   2 NA's 
   3   77 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   0    5   10 NA's 
  68    3    3    6 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  80.00   90.00   90.00   91.96   95.00  100.00       6 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.03   30.00   50.00   53.03   80.00  200.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
 1 
80 

patient.biospecimen_cqcf.vessel_used:
              cryovial other (please specify) 
                    79                      1 

patient.days_to_index:
 0 
80 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    4.0   272.8   478.5   542.4   715.2  2199.0 

patient.samples.sample.2.is_ffpe:
no 
80 

patient.samples.sample.2.oct_embedded:
false 
   80 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167822317 167822337 167822358 167822358 167822379 167822399 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
 8 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
  42   30    8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
14 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 5 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.225   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a39z 
  80 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
 42  30   8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
23 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
40 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 
  80 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167789376 167789401 167789424 167789425 167789448 167789471        10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  26 NA's 
  70   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 NA's 
  25   17   15   13   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  14 NA's 
  70   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  70   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   5 NA's 
  70   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.886   7.000   9.000      10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a40e NA's 
  70   10 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
 1.4  1.5  1.6  1.7 NA's 
  25   17   15   13   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
  70   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  10 NA's 
  70   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 NA's 
  70   10 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167780173 167780193 167780214 167780214 167780235 167780255 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
  29   21   16   14 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
14 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 5 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.225   9.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a39y 
  80 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
  29   21   16   14 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
23 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
 80 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 
  80 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.04    9.07   12.21   13.09   15.35   44.08 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
 80 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
 d 
80 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
  29   21   16   14 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 80 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
 80 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              80 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
80 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                               42                                38 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       80 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   13.00   17.00   16.82   20.25   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
no 
80 

patient.samples.sample.2.portions.portion.month_of_creation:
 1  2  3 11 12 
23 17 14 12 14 

patient.samples.sample.2.portions.portion.portion_number:
 1 
80 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
  12   68 

patient.samples.sample.2.portions.portion.weight:
   2  300 NA's 
  40    2   38 

patient.samples.sample.2.portions.portion.year_of_creation:
2013 2014 
  26   54 

patient.samples.sample.2.sample_type:
blood derived normal 
                  80 

patient.samples.sample.2.sample_type_id:
10 
80 

patient.samples.sample.2.vial_number:
 a  b  c 
76  2  2 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    4.0   272.0   478.5   541.4   715.2  2199.0 


patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.00   40.00   48.64   60.00  150.00       3 

patient.samples.sample.is_ffpe:
no 
80 

patient.samples.sample.oct_embedded:
false  true 
   66    14 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.710   1.830   1.890   1.866   1.910   1.960 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167780272 167789206 167789234 167788674 167789254 167789274 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
13 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.16 0.17 0.18 
  19   15   22   24 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
14 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
no 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
 5 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0     3.0     5.5     5.5     8.0    10.0 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a40b 
  80 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
  3 3.2 3.4 3.6 
 19  15  22  24 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
23 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
20 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2014 
  80 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167809407 167812247 167812272 167812236 167812295 167812319 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
 7 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.16 0.17 0.18 
  19   15   22   24 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
14 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
no 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
 5 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0     3.0     5.5     5.5     8.0    10.0 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a405 
  80 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
1.95 2.08 2.21 2.34 
  19   15   22   24 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
23 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
13 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2014 
  80 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.39   10.97   16.84   18.53   25.28   39.41 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
 80 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
 r 
80 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
0.15 0.16 0.17 0.18 
  19   15   22   24 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              80 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
no 
80 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
nrna - melanoma protocol 
                      80 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.820   1.220   1.350   1.379   1.552   1.990 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.100   7.700   8.300   8.328   9.025   9.700 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
     80 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  70    3    7 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167822413 167822433 167822454 167822454 167822475 167822495 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
 8 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 0.09 
  10   58   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
14 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
no 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
 5 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.225   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a39w 
  80 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
2.8 3.2 3.6 
 10  58  12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
23 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
40 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2014 
  80 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167822125 167822145 167822166 167822166 167822187 167822207 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
 5 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 
   3   20   31   26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
14 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
no 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
 5 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.225   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a39x 
  80 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.82 1.95 2.08 2.21 
   3   20   31   26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
23 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
13 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2014 
  80 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
167788992 167789017 167789040 167789041 167789064 167789087        10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
  26 NA's 
  70   10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.14 0.15 0.16 0.17 NA's 
   2   18   27   23   10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  14 NA's 
  70   10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  70   10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   5 NA's 
  70   10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   4.886   7.000   9.000      10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a40d NA's 
  70   10 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 1.4  1.5  1.6  1.7 NA's 
   2   18   27   23   10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
  70   10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10 NA's 
  70   10 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2014 NA's 
  70   10 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
167820925 167821997 167822112 167822128 167822310 167822436 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 
   3   20   31   26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
14 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
no 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
 5 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.225   9.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a39v 
  80 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
0.94 1.01 1.07 1.14 
   3   20   31   26 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
23 
80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7 
 80 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 
  80 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  10.87   32.16   39.53   39.92   48.82   65.26 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
 80 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
 d 
80 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 
   3   20   31   26 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
 80 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
 80 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              80 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
no 
80 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                   80 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen 
       80 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  70    3    7 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   5.00    8.00   18.00   16.44   24.00   29.00 

patient.samples.sample.portions.portion.is_ffpe:
no 
80 

patient.samples.sample.portions.portion.month_of_creation:
 1  2  3  4 11 
22 18 12  1 27 

patient.samples.sample.portions.portion.portion_number:
11 12 21 
76  3  1 

patient.samples.sample.portions.portion.portion_sequence:
   1 NA's 
  27   53 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  78    2 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.250   1.000   1.654   2.000  30.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1    2 NA's 
  74    2    2    2 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4872  0.0000 10.0000       2 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    3 NA's 
  76    1    1    2 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   4.295   0.000  50.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.128   0.000  20.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   90.00  100.00   93.09  100.00  100.00       2 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   80.00   80.00   81.81   85.00   97.00       2 

patient.samples.sample.portions.portion.slides.slide.section_location:
 top NA's 
  78    2 

patient.samples.sample.portions.portion.weight:
10 15 20 25 30 
 7  1 17  1 54 

patient.samples.sample.portions.portion.year_of_creation:
2013 2014 
  27   53 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
  12   68 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
  12   68 

patient.samples.sample.portions.shipment_portion.plate_id:
a41z NA's 
  12   68 

patient.samples.sample.portions.shipment_portion.portion_number:
  21 NA's 
  12   68 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   2 NA's 
   3   77 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   4 NA's 
  12   68 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   6 NA's 
  12   68 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
  12   68 

patient.samples.sample.sample_type:
primary tumor 
           80 

patient.samples.sample.sample_type_id:
 1 
80 

patient.samples.sample.vial_number:
 a  b 
77  3 

}}
\keyword{datasets}
