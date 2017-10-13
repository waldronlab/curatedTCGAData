\name{ CESC }
\alias{ CESC }
\docType{data}
\title{ Cervical squamous cell carcinoma and endocervical adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( CESC )
ExperimentList class object of length 10: 
 [1] CNASeq-20160128: RaggedExperiment with 7834 rows and 100 columns 
 [2] CNASNP-20160128: RaggedExperiment with 267855 rows and 586 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 59450 rows and 586 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 295 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 295 columns 
 [6] Methylation-20160128: SummarizedExperiment with 485577 rows and 312 columns 
 [7] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 312 columns 
 [8] Mutation-20160128: RaggedExperiment with 46547 rows and 194 columns 
 [9] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 309 columns 
 [10] RPPAArray-20160128: SummarizedExperiment with 192 rows and 173 columns 

> rownames( CESC )
CharacterList of length 10
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["Mutation-20160128"]] character(0)
[["RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 A2LD1 ... ZZZ3 psiTPTE22 tAKR
[["RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363

> colnames( CESC )
CharacterList of length 10
[["CNASeq-20160128"]] TCGA-C5-A1BQ-01C-11D-A20X-26 ...
[["CNASNP-20160128"]] TCGA-2W-A8YY-01A-11D-A37M-01 ...
[["CNVSNP-20160128"]] TCGA-2W-A8YY-01A-11D-A37M-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-2W-A8YY-01A-11D-A37M-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-2W-A8YY-01A-11D-A37M-01 ...
[["Methylation-20160128"]] TCGA-2W-A8YY-01A-11D-A37Q-05 ...
[["miRNASeqGene-20160128"]] TCGA-2W-A8YY-01A-11R-A37P-13 ...
[["Mutation-20160128"]] TCGA-BI-A0VR-01A-11D-A10S-08 ...
[["RNASeq2GeneNorm-20160128"]] TCGA-2W-A8YY-01A-11R-A37O-07 ...
[["RPPAArray-20160128"]] TCGA-2W-A8YY-01A-21-A40H-20 ...

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128  0.3 Mb
2                    CNASNP-20160128  7.3 Mb
3                    CNVSNP-20160128  1.8 Mb
4          GISTIC_AllByGene-20160128  4.3 Mb
5  GISTIC_ThresholdedByGene-20160128  4.3 Mb
6               Methylation-20160128 67.4 Mb
7              miRNASeqGene-20160128  0.1 Mb
8                  Mutation-20160128 28.4 Mb
9           RNASeq2GeneNorm-20160128  1.1 Mb
10                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   235 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  72.00   72.00    1.70    1.39    2.29 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0    38.0    46.0    48.2    56.0    88.0       2 

vital_status:
  0   1 
235  72 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   349.5   620.0   881.3  1069.5  4086.0     235 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     374     688    1071    1362    6408      72 

tumor_tissue_site:
cervical 
     307 


pathology_N_stage:
  n0   n1   nx NA's 
 135   60   67   45 

pathology_M_stage:
  m0   m1   mx NA's 
 116   10  131   50 

gender:
female 
   307 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1994    2006    2010    2008    2012    2013       2 

radiation_therapy:
  no  yes NA's 
  55  129  123 


number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.20    6.50   15.00   17.38   25.00   57.00     214 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.044   1.000  16.000     148 


ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    24                    171                    112 

weight_kg_at_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   58.25   70.50   73.04   83.75  210.00      29 

tumor_status:
tumor free with tumor       NA's 
       201         80         26 


tobacco_smoking_year_stopped:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1949    1995    2003    2000    2010    2013     264 

tobacco_smoking_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.20    6.50   15.00   17.38   25.00   57.00     214 

tobacco_smoking_history:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.829   2.000   5.000      44 

agebegansmokinginyears:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   16.00   18.00   21.14   24.00   44.00     222 

radiation_therapy_status:
   completed as planned treatment not completed                    NA's 
                     29                       3                     275 

pregnancies_count_total:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    3.00    3.61    5.00   15.00      40 

pregnancies_count_stillbirth:
   0    1    3 NA's 
 106    5    1  195 

pregnancy_spontaneous_abortion_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5442  1.0000  6.0000     160 

pregnancies_count_live_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.250   2.000   2.844   4.000  11.000      45 

pregnancy_therapeutic_abortion_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.8525  1.0000 13.0000     185 

pregnancies_count_ectopic:
   0    1    2 NA's 
 104   11    1  191 


lymph_node_location:
lymph_node_location_positive_pathology_name|lymph_node_location_positive_pathology_text 
                                                                                    307 


menopause_status:
                                               indeterminate (neither pre or postmenopausal) 
                                                                                           3 
                                              peri (6-12 months since last menstrual period) 
                                                                                          25 
           post (prior bilateral ovariectomy or >12 mo since lmp with no prior hysterectomy) 
                                                                                          84 
pre (<6 months since lmp and no prior bilateral ovariectomy and not on estrogen replacement) 
                                                                                         125 
                                                                                        NA's 
                                                                                          70 

lymphovascular_involvement:
 absent present    NA's 
     72      80     155 

lymph_nodes_examined_he_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.044   1.000  16.000     148 

lymph_nodes_examined:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.00   21.00   22.28   29.00   63.00     127 

keratinization_squamous_cell:
    keratinizing squamous cell carcinoma 
                                      55 
non-keratinizing squamous cell carcinoma 
                                     120 
                                    NA's 
                                     132 

initial_pathologic_dx_year:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1994    2006    2010    2008    2012    2013       2 

hysterectomy_type:
hysterectomy_performed_type|hysterectomy_performed_text 
                                                    307 

history_hormonal_contraceptives_use:
current user  former user   never used         NA's 
          15           54           90          148 

height_cm_at_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    132     157     160     161     165     183      43 

corpus_involvement:
 absent present    NA's 
     99      19     189 

chemo_concurrent_type:
carboplatin   cisplatin       other        NA's 
          7         104           2         194 

cervix_suv_results:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    8.70   12.00   13.22   16.92   28.80     290 

cause_of_death:
cardiopulmonary arrest            lung cancer          renal failure 
                     1                      1                      1 
                  NA's 
                   304 

age_at_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  20.00   38.00   46.00   48.27   56.50   88.00 


admin.bcr:
nationwide children's hospital 
                           307 

admin.day_of_dcc_upload:
  1 
307 

admin.disease_code:
cesc 
 307 


admin.month_of_dcc_upload:
 10 
307 

admin.patient_withdrawal.withdrawn:
false 
  307 

admin.project_code:
tcga 
 307 

admin.year_of_dcc_upload:
2015 
 307 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  20.00   38.00   46.00   48.27   56.50   88.00 

patient.age_began_smoking_in_years:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   16.00   18.00   21.14   24.00   44.00     222 


patient.birth_control_pill_history_usage_category:
current user  former user   never used         NA's 
          15           54           90          148 

patient.cervical_carcinoma_corpus_uteri_involvement_indicator:
 absent present    NA's 
     99      19     189 




patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      275        32 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-334.00    0.00    6.00  140.61   30.75 5560.00      37 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   403.0   678.5  1165.4  1558.5  4086.0     275 


patient.clinical_cqcf.history_of_neoadjuvant_treatment:
 no 
307 

patient.clinical_cqcf.normal_tissue_anatomic_site:
fallopian tube     myometrium          other         uterus           NA's 
             1              2              2              6            296 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           13           294 

patient.clinical_cqcf.tumor_type:
primary 
    307 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    6.00   16.00   14.76   22.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32500  -20714  -17020  -17779  -14162   -7649       2 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   353.8   593.0   899.5  1069.5  4086.0     247 

patient.days_to_diagnostic_computed_tomography_performed:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -54.00    1.00    9.00   17.41   20.00  316.00     238 

patient.days_to_diagnostic_mri_performed:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -2.00    1.75   12.00   65.10   19.25  519.00     297 

patient.days_to_fdg_or_ct_pet_performed:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -11.00    5.50   18.00   37.79   39.50  521.00     264 

patient.days_to_initial_pathologic_diagnosis:
  0 
307 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    36.0   186.0   676.1   919.5  5957.0      60 

patient.days_to_performance_status_assessment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-334.00    0.00    6.00   42.32   36.50  875.00     144 




patient.diagnostic_ct_result_outcomes.diagnostic_ct_result_outcome.4:
paraaortic nodes absent     pelvic nodes absent    pelvic nodes present 
                     43                       5                       2 
 supraclavicular absent                    NA's 
                      1                     256 

patient.diagnostic_ct_result_outcomes.diagnostic_ct_result_outcome.5:
   pelvic nodes absent   pelvic nodes present supraclavicular absent 
                    40                      6                      2 
                  NA's 
                   259 

patient.diagnostic_ct_result_outcomes.diagnostic_ct_result_outcome.6:
supraclavicular absent                   NA's 
                    40                    267 

patient.diagnostic_mri_result_outcomes.diagnostic_mri_result_outcome:
                         bladder absent  extra-pelvic metastatic disease absent 
                                     10                                       1 
extra-pelvic metastatic disease present                    pelvic nodes present 
                                      1                                       1 
                                   NA's 
                                    294 

patient.diagnostic_mri_result_outcomes.diagnostic_mri_result_outcome.2:
extra-pelvic metastatic disease absent                     parametrium absent 
                                     9                                      1 
                                  NA's 
                                   297 

patient.diagnostic_mri_result_outcomes.diagnostic_mri_result_outcome.3:
parametrium absent               NA's 
                11                296 

patient.diagnostic_mri_result_outcomes.diagnostic_mri_result_outcome.4:
paraaortic nodes absent                    NA's 
                     10                     297 

patient.diagnostic_mri_result_outcomes.diagnostic_mri_result_outcome.5:
 pelvic nodes absent pelvic nodes present                 NA's 
                   9                    2                  296 

patient.diagnostic_mri_result_outcomes.diagnostic_mri_result_outcome.6:
supraclavicular absent                   NA's 
                     9                    298 



patient.drugs.drug.2.clinical_trail_drug_classification:
5-flurouracil          NA's 
            1           306 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   13.00   19.00   18.45   24.00   31.00     276 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   26.0   107.0   155.0   255.8   314.5   800.0     280 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   62.75   91.50  212.37  231.25 1036.00     277 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                           5                           12 
            partial response               stable disease 
                           1                            1 
                        NA's 
                         288 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   6.161   8.500  12.000     276 

patient.drugs.drug.2.number_cycles:
   1    2    4    6 NA's 
   1    1    1    1  303 

patient.drugs.drug.2.prescribed_dose:
   40    60  1000 14000  NA's 
    1     1     1     1   303 

patient.drugs.drug.2.prescribed_dose_units:
      mg    mg/m2 mg/m2/wk     NA's 
       1        2        1      303 

patient.drugs.drug.2.regimen_indication:
               adjuvant other: specify in notes             progression 
                      3                       3                       1 
                   NA's 
                    300 

patient.drugs.drug.2.regimen_indication_notes:
       metastatic stable/persistent              NA's 
                2                 1               304 

patient.drugs.drug.2.regimen_number:
   1    2    4 NA's 
   3    2    2  300 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               6                1              300 

patient.drugs.drug.2.therapy_ongoing:
  no NA's 
  31  276 

patient.drugs.drug.2.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           1           30          276 

patient.drugs.drug.2.total_dose:
  200  7080 14000  NA's 
    1     1     1   304 

patient.drugs.drug.2.total_dose_units:
  mg NA's 
   3  304 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  23    1  283 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2013    2013    2014    2015     276 



patient.drugs.drug.3.clinical_trail_drug_classification:
amifostine       NA's 
         1        306 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   14.00   19.00   18.86   24.00   28.00     293 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0   134.0   211.0   317.3   489.0   800.0     294 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    46.0   122.0   215.4   330.5   784.0     293 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                           3                            4 
              stable disease                         NA's 
                           2                          298 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.500   6.714   9.750  12.000     293 

patient.drugs.drug.3.number_cycles:
   2    5 NA's 
   1    1  305 

patient.drugs.drug.3.prescribed_dose:
 1.7  150 NA's 
   1    1  305 

patient.drugs.drug.3.prescribed_dose_units:
 auc   mg NA's 
   1    1  305 

patient.drugs.drug.3.regimen_indication:
   adjuvant progression        NA's 
          1           1         305 

patient.drugs.drug.3.regimen_number:
   1    4 NA's 
   1    1  305 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              306 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  13    1  293 

patient.drugs.drug.3.therapy_types.therapy_type:
chemotherapy         NA's 
          14          293 

patient.drugs.drug.3.total_dose:
 400  750 NA's 
   1    1  305 

patient.drugs.drug.3.total_dose_units:
  mg NA's 
   2  305 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  10    2  295 

patient.drugs.drug.3.year_of_form_completion:
2011 2012 2013 2014 NA's 
   1    4    4    5  293 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   14.00   16.14   23.00   24.00     300 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  211.0   282.8   429.0   403.2   497.2   596.0     301 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  135.0   249.5   320.0   299.0   357.5   423.0     301 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                           1                            2 
              stable disease                         NA's 
                           2                          302 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   8.000   6.857  10.000  12.000     300 

patient.drugs.drug.4.number_cycles:
   3   14 NA's 
   1    1  305 

patient.drugs.drug.4.prescribed_dose:
1000 NA's 
   1  306 

patient.drugs.drug.4.prescribed_dose_units:
  mg NA's 
   1  306 

patient.drugs.drug.4.regimen_indication:
  adjuvant recurrence       NA's 
         1          1        305 

patient.drugs.drug.4.regimen_number:
   1    2 NA's 
   1    1  305 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              306 

patient.drugs.drug.4.therapy_ongoing:
  no NA's 
   7  300 

patient.drugs.drug.4.therapy_types.therapy_type:
chemotherapy         NA's 
           7          300 

patient.drugs.drug.4.total_dose:
14600  NA's 
    1   306 

patient.drugs.drug.4.total_dose_units:
  mg NA's 
   1  306 

patient.drugs.drug.4.tx_on_clinical_trial:
  no NA's 
   5  302 

patient.drugs.drug.4.year_of_form_completion:
2011 2012 2013 2014 NA's 
   1    3    2    1  300 

patient.drugs.drug.5.bcr_drug_barcode:
tcga-c5-a1m6-d33211 tcga-ds-a1oa-d15318 tcga-ds-a1ob-d20935 tcga-ea-a44s-d49907 
                  1                   1                   1                   1 
               NA's 
                303 

patient.drugs.drug.5.bcr_drug_uuid:
1c316e84-6ca7-434f-be32-6a8aee5b89a9 37fc5233-bcca-45aa-bcc1-7312562622ac 
                                   1                                    1 
7a4f7393-7b23-4a22-a621-ac7a98c0ab8f c564de36-51a0-4f7b-ae3d-900803489e96 
                                   1                                    1 
                                NA's 
                                 303 

patient.drugs.drug.5.day_of_form_completion:
  12   14   22   24 NA's 
   1    1    1    1  303 

patient.drugs.drug.5.days_to_drug_therapy_end:
 369  489  710  786 NA's 
   1    1    1    1  303 

patient.drugs.drug.5.days_to_drug_therapy_start:
 299  341  675  758 NA's 
   1    1    1    1  303 

patient.drugs.drug.5.drug_name:
     bleomycin    carboplatin         gemzar hydrocortisone           NA's 
             1              1              1              1            303 

patient.drugs.drug.5.measure_of_response:
clinical progressive disease               stable disease 
                           1                            1 
                        NA's 
                         305 

patient.drugs.drug.5.month_of_form_completion:
   1    6    8   10 NA's 
   1    1    1    1  303 

patient.drugs.drug.5.number_cycles:
   3    6 NA's 
   1    1  305 

patient.drugs.drug.5.prescribed_dose:
 1.5 1200 NA's 
   1    1  305 

patient.drugs.drug.5.prescribed_dose_units:
 auc   mg NA's 
   1    1  305 

patient.drugs.drug.5.regimen_indication:
progression  recurrence        NA's 
          1           1         305 

patient.drugs.drug.5.regimen_number:
   2    5 NA's 
   1    1  305 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              306 

patient.drugs.drug.5.therapy_ongoing:
  no NA's 
   4  303 

patient.drugs.drug.5.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
              3               1             303 

patient.drugs.drug.5.total_dose:
1008 3600 NA's 
   1    1  305 

patient.drugs.drug.5.total_dose_units:
  mg NA's 
   2  305 

patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
   2  305 

patient.drugs.drug.5.year_of_form_completion:
2011 2012 2013 NA's 
   1    2    1  303 

patient.drugs.drug.6.bcr_drug_barcode:
tcga-ds-a1ob-d20936                NA's 
                  1                 306 

patient.drugs.drug.6.bcr_drug_uuid:
4a846040-97b3-454e-a090-73d8365e21cf                                 NA's 
                                   1                                  306 

patient.drugs.drug.6.day_of_form_completion:
  24 NA's 
   1  306 

patient.drugs.drug.6.days_to_drug_therapy_end:
 710 NA's 
   1  306 

patient.drugs.drug.6.days_to_drug_therapy_start:
 675 NA's 
   1  306 

patient.drugs.drug.6.drug_name:
taxol  NA's 
    1   306 

patient.drugs.drug.6.month_of_form_completion:
   1 NA's 
   1  306 

patient.drugs.drug.6.number_cycles:
   6 NA's 
   1  306 

patient.drugs.drug.6.prescribed_dose:
  60 NA's 
   1  306 

patient.drugs.drug.6.prescribed_dose_units:
mg/m2  NA's 
    1   306 

patient.drugs.drug.6.regimen_indication:
recurrence       NA's 
         1        306 

patient.drugs.drug.6.regimen_number:
   2 NA's 
   1  306 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              306 

patient.drugs.drug.6.therapy_ongoing:
  no NA's 
   1  306 

patient.drugs.drug.6.therapy_types.therapy_type:
chemotherapy         NA's 
           1          306 

patient.drugs.drug.6.total_dose:
 660 NA's 
   1  306 

patient.drugs.drug.6.total_dose_units:
  mg NA's 
   1  306 

patient.drugs.drug.6.year_of_form_completion:
2012 NA's 
   1  306 

patient.drugs.drug.7.bcr_drug_barcode:
tcga-ds-a1ob-d20937                NA's 
                  1                 306 

patient.drugs.drug.7.bcr_drug_uuid:
e42b0f05-014e-43da-b1f9-063b4e5e0c28                                 NA's 
                                   1                                  306 

patient.drugs.drug.7.day_of_form_completion:
  24 NA's 
   1  306 

patient.drugs.drug.7.days_to_drug_therapy_end:
 765 NA's 
   1  306 

patient.drugs.drug.7.days_to_drug_therapy_start:
 745 NA's 
   1  306 

patient.drugs.drug.7.drug_name:
gemzar   NA's 
     1    306 

patient.drugs.drug.7.month_of_form_completion:
   1 NA's 
   1  306 

patient.drugs.drug.7.number_cycles:
   4 NA's 
   1  306 

patient.drugs.drug.7.prescribed_dose:
 300 NA's 
   1  306 

patient.drugs.drug.7.prescribed_dose_units:
  mg NA's 
   1  306 

patient.drugs.drug.7.regimen_indication:
progression        NA's 
          1         306 

patient.drugs.drug.7.regimen_number:
   3 NA's 
   1  306 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              306 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   1  306 

patient.drugs.drug.7.therapy_types.therapy_type:
chemotherapy         NA's 
           1          306 

patient.drugs.drug.7.total_dose:
1500 NA's 
   1  306 

patient.drugs.drug.7.total_dose_units:
  mg NA's 
   1  306 

patient.drugs.drug.7.year_of_form_completion:
2012 NA's 
   1  306 



patient.drugs.drug.clinical_trail_drug_classification:
cisplatinum         n/a        NA's 
          1           2         304 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   14.00   15.96   24.00   31.00     166 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   26.0    78.5   106.0   143.5   148.2   968.0     187 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    42.5    67.0   111.2    94.5  1666.0     172 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                          12                           57 
            partial response               stable disease 
                           7                            3 
                        NA's 
                         228 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.312   9.000  12.000     166 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.75    6.00    5.20    6.00    8.00     287 

patient.drugs.drug.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    40.0    50.0   111.5    70.0   588.0     293 

patient.drugs.drug.prescribed_dose_units:
       mg     mg/m2 mg/m2/day  mg/m2/wk      NA's 
        7         4         1         2       293 

patient.drugs.drug.regimen_indication:
               adjuvant other: specify in notes             progression 
                     21                       1                       1 
             recurrence                    NA's 
                      5                     279 

patient.drugs.drug.regimen_indication_notes:
stable/persistent              NA's 
                1               306 

patient.drugs.drug.regimen_number:
   1    2    3 NA's 
  25    2    1  279 

patient.drugs.drug.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
              26              281 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 137    1  169 

patient.drugs.drug.therapy_types.therapy_type:
chemotherapy         NA's 
         139          168 

patient.drugs.drug.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   70.0   277.5   350.0   612.7   393.5  3528.0     296 

patient.drugs.drug.total_dose_units:
  mg NA's 
  11  296 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  95   12  200 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2014    2013    2014    2015     166 

patient.eastern_cancer_oncology_group:
   0    1    2    4 NA's 
  80   84   10    1  132 

patient.ectopic_pregnancy_count:
   0    1    2 NA's 
 104   11    1  191 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    24                    171                    112 


patient.fdg_or_ct_pet_performed_outcomes.fdg_or_ct_pet_performed_outcome.2:
 extra-pelvic metastatic disease absent extra-pelvic metastatic disease present 
                                     20                                       4 
                     parametrium absent                    pelvic nodes present 
                                      2                                       2 
                                   NA's 
                                    279 

patient.fdg_or_ct_pet_performed_outcomes.fdg_or_ct_pet_performed_outcome.3:
 paraaortic nodes absent paraaortic nodes present       parametrium absent 
                       2                        1                       22 
     parametrium present                     NA's 
                       1                      281 

patient.fdg_or_ct_pet_performed_outcomes.fdg_or_ct_pet_performed_outcome.4:
 paraaortic nodes absent paraaortic nodes present      pelvic nodes absent 
                      23                        1                        2 
    pelvic nodes present                     NA's 
                       1                      280 

patient.fdg_or_ct_pet_performed_outcomes.fdg_or_ct_pet_performed_outcome.5:
   pelvic nodes absent   pelvic nodes present supraclavicular absent 
                    18                      7                      3 
                  NA's 
                   279 

patient.fdg_or_ct_pet_performed_outcomes.fdg_or_ct_pet_performed_outcome.6:
supraclavicular absent                   NA's 
                    23                    284 

patient.female_breast_feeding_or_pregnancy_status_indicator:
  no NA's 
  72  235 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.2.additional_radiation_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.2.additional_treatment_completion_success_outcome:
  complete response    partial response progressive disease                NA's 
                 58                   1                   3                 245 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   10.00   17.00   16.93   26.00   31.00     226 

patient.follow_ups.follow_up.2.days_to_additional_surgery_metastatic_procedure:
1152 NA's 
   1  306 

patient.follow_ups.follow_up.2.days_to_death:
1011 1372 2052 NA's 
   1    1    1  304 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     81     730    1106    1416    1762    6375     242 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
1124 NA's 
   1  306 

patient.follow_ups.follow_up.2.days_to_performance_status_assessment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   81.0   430.5   644.0  1346.0  1259.0  6369.0     297 

patient.follow_ups.follow_up.2.death_cause_text:
cardiopulmonary arrest                   NA's 
                     1                    306 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   0    1    3 NA's 
  11    1    1  294 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
additional new tumor event                       NA's 
                         1                        306 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  61   16  230 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   5.593   9.000  12.000     226 

patient.follow_ups.follow_up.2.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            306 

patient.follow_ups.follow_up.2.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
tumor resection            NA's 
              1             306 

patient.follow_ups.follow_up.2.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                306 

patient.follow_ups.follow_up.2.new_neoplasm_occurrence_anatomic_site_text:
vagina   NA's 
     1    306 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
1707 NA's 
   1  306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
1638 NA's 
   1  306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
tumor resection            NA's 
              1             306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
locoregional recurrence                    NA's 
                      1                     306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   1  306 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    2  303 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   3    1  303 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 799  907 NA's 
   1    1  305 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 456  675  778  868 NA's 
   1    1    1    1  303 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             1              3            303 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
       cytology tumor resection            NA's 
              1               1             305 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      3                       1                     303 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
mediastinal lymph nodes         retroperitoneal                    NA's 
                      1                       1                     305 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   2    2  303 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   2  305 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  56    4  247 

patient.follow_ups.follow_up.2.patient_death_reason:
other, specify           NA's 
             1            306 

patient.follow_ups.follow_up.2.performance_status_assessment_timepoint_category_other_text:
                               at hospiace 
                                         1 
endocrinology and metabolism clinical note 
                                         1 
                                 follow-up 
                                         2 
                           follow-up visit 
                                         2 
                                      NA's 
                                       301 

patient.follow_ups.follow_up.2.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                    7                     4                     1 
         preoperative                  NA's 
                    1                   294 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        53          7        247 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  26   37  244 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         52                           1 
        progressive disease              stable disease 
                          1                           1 
                       NA's 
                        252 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  11   50  246 

patient.follow_ups.follow_up.2.residual_disease_post_new_tumor_event_margin_status:
  rx NA's 
   1  306 

patient.follow_ups.follow_up.2.treatment.concurrent_rx_tx_info.chemotherapy_negation_radiation_therapy_concurrent_not_administered_reason:
not done per treating physicians discretion 
                                          1 
                                       NA's 
                                        306 

patient.follow_ups.follow_up.2.treatment.concurrent_rx_tx_info.concurrent_rx_tx.agent_total_dose_count:
   3    4    5 NA's 
   1    1    4  301 

patient.follow_ups.follow_up.2.treatment.concurrent_rx_tx_info.concurrent_rx_tx.chemotherapy_regimen_type:
carboplatin   cisplatin        NA's 
          1           6         300 

patient.follow_ups.follow_up.2.treatment.concurrent_rx_tx_info.concurrent_rx_tx.concurrent_chemotherapy_dose:
30mg/dose  40 mg/m2       570        68      NA's 
        1         1         1         1       303 

patient.follow_ups.follow_up.2.treatment.concurrent_rx_tx_info.concurrent_rx_tx.dose_frequency_text:
once weekly        NA's 
          6         301 

patient.follow_ups.follow_up.2.treatment.concurrent_rx_tx_info.targeted_molecular_therapy:
  no  yes NA's 
   7    7  293 

patient.follow_ups.follow_up.2.treatment.postoperative_rx_tx:
  no  yes NA's 
  26   37  244 

patient.follow_ups.follow_up.2.treatment.radiation_therapy:
  no  yes NA's 
  11   50  246 

patient.follow_ups.follow_up.2.treatment.radiation_therapy_info.brachytherapy_first_reference_point_administered_total_dose:
  40 4000 NA's 
   1    1  305 

patient.follow_ups.follow_up.2.treatment.radiation_therapy_info.brachytherapy_method_type:
 hdr  ldr NA's 
   1    1  305 

patient.follow_ups.follow_up.2.treatment.radiation_therapy_info.external_beam_radiation_therapy_administered_paraaortic_region_lymph_node_dose:
4500 NA's 
   1  306 

patient.follow_ups.follow_up.2.treatment.radiation_therapy_info.rt_administered_type:
external beam radiation                    NA's 
                      6                     301 

patient.follow_ups.follow_up.2.treatment.radiation_therapy_info.rt_pelvis_administered_total_dose:
3740 3770 4500 5500 NA's 
   1    1    2    1  302 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   65     3   239 

patient.follow_ups.follow_up.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   1    7   38   35  226 

patient.follow_ups.follow_up.3.additional_pharmaceutical_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.3.additional_radiation_therapy:
 yes NA's 
   1  306 

patient.follow_ups.follow_up.3.additional_treatment_completion_success_outcome:
  complete response    partial response progressive disease      stable disease 
                  5                   1                   1                   1 
               NA's 
                299 

patient.follow_ups.follow_up.3.adjuvant_rad_therapy_prior_admin:
  no NA's 
   1  306 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    9.75   10.50   16.50   26.50   31.00     299 

patient.follow_ups.follow_up.3.days_to_death:
2052 NA's 
   1  306 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  376.0   680.5  1122.0  1043.9  1324.0  1800.0     300 

patient.follow_ups.follow_up.3.days_to_new_tumor_event_after_initial_treatment:
 693 NA's 
   1  306 

patient.follow_ups.follow_up.3.days_to_performance_status_follow_up_score:
  14 NA's 
   1  306 

patient.follow_ups.follow_up.3.eastern_cancer_oncology_group:
   1 NA's 
   1  306 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
scheduled follow-up submission                           NA's 
                             1                            306 

patient.follow_ups.follow_up.3.lost_follow_up:
  no  yes NA's 
   6    1  300 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.00    3.50    5.25    6.50   12.00     299 

patient.follow_ups.follow_up.3.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            306 

patient.follow_ups.follow_up.3.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
tumor resection            NA's 
              1             306 

patient.follow_ups.follow_up.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                306 

patient.follow_ups.follow_up.3.new_neoplasm_occurrence_anatomic_site_text:
regional recurrence nos                    NA's 
                      1                     306 

patient.follow_ups.follow_up.3.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  306 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 601 1360 NA's 
   1    1  305 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             2            305 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                305 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
                      brain supraclavicular lymph nodes 
                          1                           1 
                       NA's 
                        305 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    1  305 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   4    2  301 

patient.follow_ups.follow_up.3.patient_death_reason:
cervical cancer            NA's 
              1             306 

patient.follow_ups.follow_up.3.performance_status_scale_timing:
preoperative         NA's 
           1          306 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         5          3        299 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no  yes NA's 
   2    5  300 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          8                         299 

patient.follow_ups.follow_up.3.radiation_supplemental.chemotherapy_negation_radiation_therapy_concurrent_not_administered_reason:
not done per treating physicians discretion 
                                          1 
                                       NA's 
                                        306 

patient.follow_ups.follow_up.3.radiation_supplemental.targeted_molecular_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
   1    7  299 

patient.follow_ups.follow_up.3.radiation_therapy_not_administered_reason:
not done per treating physician discretion 
                                         1 
                                      NA's 
                                       306 

patient.follow_ups.follow_up.3.targeted_molecular_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.3.treatment.concurrent_rx_tx_info.targeted_molecular_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.3.treatment.postoperative_rx_tx:
  no  yes NA's 
   2    5  300 

patient.follow_ups.follow_up.3.treatment.radiation_therapy:
 yes NA's 
   7  300 

patient.follow_ups.follow_up.3.treatment.radiation_therapy_info.rt_administered_type:
external beam radiation                    NA's 
                      1                     306 

patient.follow_ups.follow_up.3.treatment.radiation_therapy_info.rt_pelvis_administered_total_dose:
3740 NA's 
   1  306 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
    7     1   299 

patient.follow_ups.follow_up.3.year_of_form_completion:
2012 2014 2015 NA's 
   1    2    5  299 

patient.follow_ups.follow_up.4.additional_treatment_completion_success_outcome:
complete response              NA's 
                2               305 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-vs-a9ua-f70278 tcga-vs-a9uq-f72391                NA's 
                  1                   1                 305 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
3e210d28-e4be-41af-9d53-51888b9c7751 8358ce9c-a187-49f4-9da0-3a88ce73a887 
                                   1                                    1 
                                NA's 
                                 305 

patient.follow_ups.follow_up.4.day_of_form_completion:
   3   11 NA's 
   1    1  305 

patient.follow_ups.follow_up.4.days_to_last_followup:
 867 1263 NA's 
   1    1  305 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   2  305 

patient.follow_ups.follow_up.4.month_of_form_completion:
   2    6 NA's 
   1    1  305 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   2  305 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
tumor free       NA's 
         2        305 

patient.follow_ups.follow_up.4.postoperative_rx_tx:
  no  yes NA's 
   1    1  305 

patient.follow_ups.follow_up.4.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          2                         305 

patient.follow_ups.follow_up.4.radiation_therapy:
 yes NA's 
   2  305 

patient.follow_ups.follow_up.4.treatment.postoperative_rx_tx:
  no  yes NA's 
   1    1  305 

patient.follow_ups.follow_up.4.treatment.radiation_therapy:
 yes NA's 
   2  305 

patient.follow_ups.follow_up.4.vital_status:
alive  NA's 
    2   305 

patient.follow_ups.follow_up.4.year_of_form_completion:
2015 NA's 
   2  305 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
   3    9  295 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
   9    3  295 

patient.follow_ups.follow_up.additional_treatment_completion_success_outcome:
  complete response    partial response progressive disease      stable disease 
                160                   5                  27                   4 
               NA's 
                111 

patient.follow_ups.follow_up.adjuvant_rad_therapy_prior_admin:
  no  yes NA's 
  12   34  261 

patient.follow_ups.follow_up.agent_total_dose_count:
   3    4    5    6 NA's 
   1    2    3   10  291 



patient.follow_ups.follow_up.brachytherapy_administered_status:
   completed as planned treatment not completed                    NA's 
                     19                       1                     287 

patient.follow_ups.follow_up.brachytherapy_first_reference_point_administered_total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     60    1000    1200    2705    2400    8867     302 

patient.follow_ups.follow_up.brachytherapy_method_type:
 hdr  ldr NA's 
  19    1  287 

patient.follow_ups.follow_up.chemotherapy_regimen_types.chemotherapy_regimen_type:
carboplatin   cisplatin       other        NA's 
          3          18           1         285 

patient.follow_ups.follow_up.chemotherapy_regimen_types.chemotherapy_regimen_type.2:
other  NA's 
    2   305 


patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.75   13.00   14.42   23.00   31.00      59 

patient.follow_ups.follow_up.days_to_brachytherapy_begin_occurrence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   35.25   66.50   63.25   78.25  171.00     287 

patient.follow_ups.follow_up.days_to_brachytherapy_end_occurrence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  35.00   74.25   95.00   99.50  118.75  186.00     289 

patient.follow_ups.follow_up.days_to_chemotherapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  65.00   85.25  101.00  118.31  135.00  332.00     291 

patient.follow_ups.follow_up.days_to_chemotherapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   38.75   66.50   70.25   81.75  252.00     287 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   270.5   638.0   855.2  1100.5  3046.0     272 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   411.5   619.0  1067.7  1275.0  6408.0     104 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   62.0   281.5   417.5   612.8   690.2  1804.0     295 

patient.follow_ups.follow_up.days_to_performance_status_assessment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -71.0    84.5   444.0   523.2   736.0  5957.0     216 

patient.follow_ups.follow_up.days_to_performance_status_follow_up_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -273.0    26.5    79.5   243.8   212.2  1479.0     281 

patient.follow_ups.follow_up.death_cause_text:
  lung cancer renal failure          NA's 
            1             1           305 

patient.follow_ups.follow_up.dose_frequency_text:
once weekly        NA's 
         14         293 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3852  1.0000  4.0000     185 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                             49 
                          NA's 
                           257 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 156   41  110 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.891  10.000  12.000      59 

patient.follow_ups.follow_up.new_neoplasm_event_occurrence_anatomic_site:
 head and neck           lung other, specify           NA's 
             1              3              5            298 

patient.follow_ups.follow_up.new_neoplasm_event_post_initial_therapy_diagnosis_method_text:
                           ct                ct/core biopsy 
                            4                             1 
fine needle aspiration biopsy            pet, needle biopsy 
                            1                             1 
                         NA's 
                          300 

patient.follow_ups.follow_up.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
 other, specify tumor resection            NA's 
              8               1             298 

patient.follow_ups.follow_up.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                     10                       1                     296 


patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  36   12  259 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    3  302 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   3    1  303 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
1707 NA's 
   1  306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    321     396     574     848    1311    1638     302 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             5            302 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_post_initial_therapy_diagnosis_method_text:
ct scan    NA's 
      1     306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
 other, specify tumor resection            NA's 
              1               1             305 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      3                       2                     302 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
    abdominopelvis              liver peritoneum, pelvis               NA's 
                 1                  2                  1                303 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   4    1  302 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   1  306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no NA's 
   1  306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
 yes NA's 
   1  306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 321 NA's 
   1  306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_post_initial_therapy_diagnosis_method_text:
ct scan    NA's 
      1     306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
other, specify           NA's 
             1            306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
brain  NA's 
    1   306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  306 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   3   10  294 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   8    5  294 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  209.0   347.0   432.5   476.0   606.5   799.0     301 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   94.0   238.0   425.0   628.6   704.0  2893.0     288 



patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
       cytology  other, specify tumor resection            NA's 
              1               7               4             295 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     12                       8                       1 
                   NA's 
                    286 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   9    5  293 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   rx NA's 
   4    1  302 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 137   22  148 

patient.follow_ups.follow_up.other_chemotherapy_agent_administration_specify:
cetuximab    gemzar     taxol      NA's 
        1         1         1       304 

patient.follow_ups.follow_up.patient_death_reason:
cervical cancer  other, specify            NA's 
             24               5             278 


patient.follow_ups.follow_up.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                   37                    44                    11 
         preoperative                  NA's 
                   31                   184 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       161         48         98 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  69   99  139 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        158                           5 
        progressive disease              stable disease 
                          8                           5 
                       NA's 
                        131 

patient.follow_ups.follow_up.radiation_supplemental.chemotherapy_negation_radiation_therapy_concurrent_administered_text:
                   not part of recommended treatment plan 
                                                        1 
on randomized study and received arm without chemotherapy 
                                                        1 
                                                     NA's 
                                                      305 

patient.follow_ups.follow_up.radiation_supplemental.chemotherapy_negation_radiation_therapy_concurrent_not_administered_reason:
not done per treating physicians discretion 
                                         11 
                                      other 
                                          4 
                        participant refusal 
                                          1 
                                       NA's 
                                        291 

patient.follow_ups.follow_up.radiation_supplemental.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  57.00   74.75  103.50  105.39  126.25  174.00     279 

patient.follow_ups.follow_up.radiation_supplemental.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   37.50   60.50   62.81   76.00  154.00     275 

patient.follow_ups.follow_up.radiation_supplemental.external_beam_radiation_therapy_administered_paraaortic_region_lymph_node_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0       0    2200    2344    4500    5040     296 

patient.follow_ups.follow_up.radiation_supplemental.external_beam_radiation_therapy_administered_status:
   completed as planned treatment not completed                    NA's 
                     29                       3                     275 

patient.follow_ups.follow_up.radiation_supplemental.rt_pelvis_administered_total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1440    4500    5040    4632    5040    5840     282 

patient.follow_ups.follow_up.radiation_supplemental.targeted_molecular_therapy:
  no  yes NA's 
  17   22  268 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
  60  153   94 

patient.follow_ups.follow_up.radiation_therapy_not_administered_reason:
not done per treating physician discretion 
                                         6 
                                     other 
                                         2 
                       participant refusal 
                                         1 
                                      NA's 
                                       298 

patient.follow_ups.follow_up.rt_administered_type:
external beam radiation                    imrt                    NA's 
                     27                       2                     278 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
  25   22  260 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.chemotherapy_negation_radiation_therapy_concurrent_not_administered_reason:
               adverse event/ complications 
                                          1 
not done per treating physicians discretion 
                                          9 
                        participant refusal 
                                          2 
                                       NA's 
                                        295 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.2.agent_total_dose_count:
   4 NA's 
   1  306 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.2.chemotherapy_regimen_type:
cisplatin     other      NA's 
        1         6       300 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.2.concurrent_chemotherapy_dose:
  40mg/m2 750 mg/m2      NA's 
        1         1       305 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.2.dose_frequency_text:
once weekly        NA's 
          1         306 


patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.3.agent_total_dose_count:
   4 NA's 
   1  306 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.3.chemotherapy_regimen_type:
other  NA's 
    3   304 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.3.concurrent_chemotherapy_dose:
0.75mg/m2      NA's 
        1       306 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.3.other_chemotherapy_agent_administration_specify:
  doxorubicin   gemcitabine topotecan hcl          NA's 
            1             1             1           304 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.4.agent_total_dose_count:
   4 NA's 
   1  306 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.4.chemotherapy_regimen_type:
cisplatin     other      NA's 
        1         1       305 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.4.concurrent_chemotherapy_dose:
50mg/m2    NA's 
      1     306 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.4.other_chemotherapy_agent_administration_specify:
doxorubicin        NA's 
          1         306 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.agent_total_dose_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   5.000   5.167   6.000   7.000     271 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.chemotherapy_regimen_type:
carboplatin   cisplatin       other        NA's 
          5          46           1         255 


patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.dose_frequency_text:
once weekly        NA's 
         33         274 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.concurrent_rx_tx.other_chemotherapy_agent_administration_specify:
external radiation therapy                       NA's 
                         1                        306 

patient.follow_ups.follow_up.treatment.concurrent_rx_tx_info.targeted_molecular_therapy:
  no  yes NA's 
  24   50  233 

patient.follow_ups.follow_up.treatment.postoperative_rx_tx:
  no  yes NA's 
  69   99  139 

patient.follow_ups.follow_up.treatment.radiation_therapy:
  no  yes NA's 
  47  119  141 

patient.follow_ups.follow_up.treatment.radiation_therapy_info.brachytherapy_first_reference_point_administered_total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     21    1500    2100    2613    4000    6545     282 

patient.follow_ups.follow_up.treatment.radiation_therapy_info.brachytherapy_method_other_specify_text:
vaginal cuff         NA's 
           1          306 

patient.follow_ups.follow_up.treatment.radiation_therapy_info.brachytherapy_method_type:
  hdr   ldr other  NA's 
   14    11     3   279 

patient.follow_ups.follow_up.treatment.radiation_therapy_info.external_beam_radiation_therapy_administered_paraaortic_region_lymph_node_dose:
   0   15  540 4500 NA's 
   6    1    1    2  297 

patient.follow_ups.follow_up.treatment.radiation_therapy_info.radiation_therapy_not_administered_reason:
not done per treating physician discretion 
                                         9 
                                     other 
                                         2 
                       participant refusal 
                                         4 
                                      NA's 
                                       292 

patient.follow_ups.follow_up.treatment.radiation_therapy_info.radiation_therapy_not_administered_specify:
                                                                                                           deceased 
                                                                                                                  1 
pt only received external beam tx - she moved during tx & brachytherapy was not scheduled due to travel constraints 
                                                                                                                  1 
                                                                                                               NA's 
                                                                                                                305 

patient.follow_ups.follow_up.treatment.radiation_therapy_info.rt_administered_type:
           3d conformal external beam radiation                    imrt 
                      3                      38                       7 
                   NA's 
                    259 

patient.follow_ups.follow_up.treatment.radiation_therapy_info.rt_pelvis_administered_total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     45    4500    4500    4521    5040    8200     262 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  202    35    70 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2003    2013    2014    2013    2014    2015      59 

patient.gender:
female 
   307 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    132     157     160     161     165     183      43 


patient.history_of_neoadjuvant_treatment:
 no 
307 

patient.human_papillomavirus_laboratory_procedure_performed_name:
other, specify            pcr           NA's 
             3              9            295 

patient.human_papillomavirus_laboratory_procedure_performed_text:
hpv hybrid capture ii      hybrid capture 2           immunostain 
                    1                     1                     1 
                 NA's 
                  304 


patient.human_papillomavirus_types.human_papillomavirus_type:
           hpv 16            hpv 18    not applicable other hpv type(s) 
               12                 3                39                 7 
             NA's 
              246 

patient.human_papillomavirus_types.human_papillomavirus_type.2:
           hpv 18 other hpv type(s)              NA's 
                1                 2               304 

patient.human_papillomavirus_types.human_papillomavirus_type.3:
other hpv type(s)              NA's 
                1               306 


patient.hysterectomy_performed_type:
               other radical hysterectomy  simple hysterectomy 
                   8                  155                    6 
                NA's 
                 138 

patient.icd_10:
c53.0 c53.1 c53.9 c56.9 
    4     1   301     1 


patient.icd_o_3_site:
c53.0 c53.1 c53.9 c56.9 
    4     1   301     1 

patient.informed_consent_verified:
yes 
307 



patient.keratinizing_squamous_cell_carcinoma_present_indicator:
    keratinizing squamous cell carcinoma 
                                      55 
non-keratinizing squamous cell carcinoma 
                                     120 
                                    NA's 
                                     132 

patient.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.00   21.00   22.28   29.00   63.00     127 

patient.lymph_node_location_positive_pathology_names.lymph_node_location_positive_pathology_name:
                                      common iliac 
                                                 1 
                           other location, specify 
                                                 4 
pelvic (external iliac, internal iliac, obturator) 
                                                52 
                                              NA's 
                                               250 

patient.lymph_node_location_positive_pathology_names.lymph_node_location_positive_pathology_name.2:
           common iliac other location, specify             para-aortic 
                      5                       2                       2 
                   NA's 
                    298 

patient.lymph_node_location_positive_pathology_names.lymph_node_location_positive_pathology_name.3:
    para-aortic supraclavicular            NA's 
              2               1             304 


patient.lymphovascular_invasion_indicator:
 absent present    NA's 
     72      80     155 

patient.menopause_status:
                                               indeterminate (neither pre or postmenopausal) 
                                                                                           3 
                                              peri (6-12 months since last menstrual period) 
                                                                                          25 
           post (prior bilateral ovariectomy or >12 mo since lmp with no prior hysterectomy) 
                                                                                          84 
pre (<6 months since lmp and no prior bilateral ovariectomy and not on estrogen replacement) 
                                                                                         125 
                                                                                        NA's 
                                                                                          70 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.489  10.000  12.000 


patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    2  303 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   1    1  305 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 508 2072 NA's 
   1    1  305 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 321  491  508 1638 NA's 
   1    1    1    1  303 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            306 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_post_initial_therapy_diagnosis_method_text:
  ct NA's 
   1  306 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
       cytology  other, specify tumor resection            NA's 
              1               1               2             303 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      2                       2                     303 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
                liver paracaval lymph nodes                pelvis 
                    1                     1                     1 
    posterior bladder                  NA's 
                    1                   303 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   1    2  304 

patient.new_tumor_events.new_tumor_event.2.residual_disease_post_new_tumor_event_margin_status:
  r0   r1 NA's 
   1    1  305 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no NA's 
   1  306 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
 yes NA's 
   1  306 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 321  617 NA's 
   1    1  305 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            306 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_post_initial_therapy_diagnosis_method_text:
  ct NA's 
   1  306 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
other, specify           NA's 
             1            306 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                305 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
     brain l shoulder       NA's 
         1          1        305 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  306 

patient.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
 yes NA's 
   1  306 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 617 NA's 
   1  306 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            306 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 1                306 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_occurrence_anatomic_site_text:
l chest wall         NA's 
           1          306 

patient.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  306 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   9    5  293 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   9    6  292 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  346.0   462.2   546.5   575.8   723.8   799.0     301 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   58.0   204.0   317.0   443.7   481.5  2893.0     284 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
        cervix  head and neck           lung other, specify           NA's 
             1              2              3              5            296 


patient.new_tumor_events.new_tumor_event.new_neoplasm_event_post_initial_therapy_diagnosis_method_type:
       cytology  other, specify tumor resection            NA's 
              1              12               3             291 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     14                       6                       3 
                   NA's 
                    284 


patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  13    6  288 

patient.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r2   rx NA's 
   1    1    1  304 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 159   23  125 

patient.number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.044   1.000  16.000     148 

patient.number_of_lymphnodes_positive_by_ihc:
   0    6 NA's 
  85    1  221 

patient.number_of_successful_pregnancies_which_resulted_in_at_least_1_live_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.250   2.000   2.844   4.000  11.000      45 

patient.number_pack_years_smoked:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.20    6.50   15.00   17.38   25.00   57.00     214 

patient.oligonucleotide_primer_pair_laboratory_procedure_performed_name:
gp5+/gp6+      NA's 
        8       299 

patient.patient_death_reason:
cervical cancer            NA's 
             28             279 

patient.patient_history_immune_system_and_related_disorders_names.patient_history_immune_system_and_related_disorders_name:
           hiv other, specify           NA's 
             1              2            304 

patient.patient_history_immune_system_and_related_disorders_text:
+ for hepatitis a            herpes              NA's 
                1                 1               305 


patient.patient_pregnancy_spontaneous_abortion_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5442  1.0000  6.0000     160 

patient.patient_pregnancy_therapeutic_abortion_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.8525  1.0000 13.0000     185 


patient.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                    9                    15                    52 
         preoperative                  NA's 
                   96                   135 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       191         75         41 

patient.pregnancy_stillbirth_count:
   0    1    3 NA's 
 106    5    1  195 

patient.pregnant_at_diagnosis:
  no  yes NA's 
 156    3  148 

patient.primary_lymph_node_presentation_assessment:
  no  yes NA's 
  41  180   86 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         93                           7 
        progressive disease              stable disease 
                         15                           3 
                       NA's 
                        189 

patient.prior_dx:
 no yes 
299   8 


patient.radiations.radiation.2.anatomic_treatment_site:
 distant recurrence primary tumor field       regional site                NA's 
                  1                  45                  34                 227 



patient.radiations.radiation.2.course_number:
   1    2 NA's 
   3    1  303 

patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   14.00   15.12   24.00   30.00     226 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   28.0    92.0   122.5   139.0   154.2   612.0     233 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    61.0    92.5   106.5   124.8   605.0     229 

patient.radiations.radiation.2.measure_of_response:
               complete response                 partial response 
                              46                                5 
radiographic progressive disease                   stable disease 
                               6                                1 
                            NA's 
                             249 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.309   8.000  12.000     226 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    4.00    5.00   11.33   25.00   32.00     241 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     15     900    2438    2632    4500    6545     237 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
  79  228 

patient.radiations.radiation.2.radiation_type:
     external external beam      internal          NA's 
           39             4            37           227 

patient.radiations.radiation.2.regimen_indication:
  adjuvant recurrence       NA's 
         3          1        303 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
  60    7  240 

patient.radiations.radiation.2.year_of_form_completion:
2012 2013 2014 2015 NA's 
   8    5   67    1  226 

patient.radiations.radiation.3.anatomic_treatment_site:
primary tumor field       regional site                NA's 
                 10                   8                 289 



patient.radiations.radiation.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.25   11.00   14.17   22.25   24.00     289 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   42.0   107.0   124.5   138.8   150.2   482.0     289 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   70.75  111.50  115.22  134.00  469.00     289 

patient.radiations.radiation.3.measure_of_response:
               complete response                 partial response 
                              11                                2 
radiographic progressive disease                   stable disease 
                               4                                1 
                            NA's 
                             289 

patient.radiations.radiation.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.000   8.000   7.944   8.000  11.000     289 

patient.radiations.radiation.3.numfractions:
   4    5   25 NA's 
   9    4    5  289 

patient.radiations.radiation.3.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    900    2450    2800    2956    4475    4500     289 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
  18  289 

patient.radiations.radiation.3.radiation_type:
external internal     NA's 
       8       10      289 

patient.radiations.radiation.3.units:
 cgy NA's 
  18  289 

patient.radiations.radiation.3.year_of_form_completion:
2014 NA's 
  18  289 

patient.radiations.radiation.anatomic_treatment_site:
 distant recurrence    local recurrence primary tumor field       regional site 
                  1                   4                 105                  59 
               NA's 
                138 



patient.radiations.radiation.course_number:
   1    2 NA's 
  34    3  270 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   14.00   15.12   23.00   31.00     130 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   38.0    87.0   113.0   145.9   148.0  1124.0     146 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   48.00   71.00   99.82  107.00  798.00     136 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              79                                8 
radiographic progressive disease                   stable disease 
                              14                                3 
                            NA's 
                             203 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.124   8.000  12.000     130 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.00   25.00   19.27   25.00   33.00     176 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     21    2800    4500    3520    4500    6150     150 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
 174  133 


patient.radiations.radiation.radiation_type_notes:
cyberknife       NA's 
         1        306 

patient.radiations.radiation.regimen_indication:
   adjuvant  palliative progression  recurrence        NA's 
         32           2           1           2         270 

patient.radiations.radiation.units:
 cgy   gy NA's 
 137   20  150 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2014    2013    2014    2015     130 



patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
  m0   m1   mx NA's 
 116   10  131   50 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_n:
  n0   n1   nx NA's 
 135   60   67   45 


patient.standardized_uptake_value_cervix_uteri_assessment_measurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    8.70   12.00   13.22   16.92   28.80     290 

patient.stopped_smoking_year:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1949    1995    2003    2000    2010    2013     264 

patient.tissue_prospective_collection_indicator:
 no yes 
213  94 

patient.tissue_retrospective_collection_indicator:
 no yes 
 94 213 



patient.total_number_of_pregnancies:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    3.00    3.61    5.00   15.00      40 

patient.treatment.concurrent_rx_tx_info.chemotherapy_negation_radiation_therapy_concurrent_administered_text:
concurent chemo rt ongoing-treatment delayed due to postoperative complications. 
                                                                               1 
                                                                            NA's 
                                                                             306 


patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.2.agent_total_dose_count:
   3    4    6 NA's 
   2    2    1  302 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.2.chemotherapy_regimen_type:
carboplatin       other        NA's 
          1           7         299 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.2.concurrent_chemotherapy_dose:
1000mg/m2 750 mg/m2     auc/5      NA's 
        1         2         1       303 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.2.dose_frequency_text:
once weekly        NA's 
          3         304 


patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.3.agent_total_dose_count:
   6 NA's 
   1  306 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.3.chemotherapy_regimen_type:
other  NA's 
    1   306 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.3.concurrent_chemotherapy_dose:
175mg/m2; 135mg/m2               NA's 
                 1                306 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.3.dose_frequency_text:
once weekly        NA's 
          1         306 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.3.other_chemotherapy_agent_administration_specify:
paclitaxel       NA's 
         1        306 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.4.agent_total_dose_count:
   5 NA's 
   1  306 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.4.chemotherapy_regimen_type:
other  NA's 
    1   306 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.4.concurrent_chemotherapy_dose:
15mg/kg    NA's 
      1     306 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.4.other_chemotherapy_agent_administration_specify:
bevacizumab        NA's 
          1         306 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.agent_total_dose_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   4.817   6.000   7.000     236 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.chemotherapy_regimen_type:
carboplatin   cisplatin       other        NA's 
          3          81           1         222 


patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.dose_frequency_text:
once weekly        NA's 
         69         238 

patient.treatment.concurrent_rx_tx_info.concurrent_rx_tx.other_chemotherapy_agent_administration_specify:
cisplatin      NA's 
        1       306 

patient.treatment.concurrent_rx_tx_info.targeted_molecular_therapy:
  no  yes NA's 
  34   86  187 

patient.treatment.postoperative_rx_tx:
  no  yes NA's 
  69   89  149 

patient.treatment.radiation_therapy:
  no  yes NA's 
  48  110  149 

patient.treatment.radiation_therapy_info.brachytherapy_first_reference_point_administered_total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     40    2700    2800    3067    3050    8239     248 

patient.treatment.radiation_therapy_info.brachytherapy_method_other_specify_text:
implants-tandem and ovoids            pulse dose rate 
                         1                          2 
           vaginal implant                       NA's 
                         1                        303 

patient.treatment.radiation_therapy_info.brachytherapy_method_type:
  hdr   ldr other  NA's 
   56     9     5   237 

patient.treatment.radiation_therapy_info.external_beam_radiation_therapy_administered_paraaortic_region_lymph_node_dose:
   0  540 4500 NA's 
   8    1    2  296 

patient.treatment.radiation_therapy_info.radiation_therapy_not_administered_reason:
               adverse event/complications 
                                         2 
not done per treating physician discretion 
                                         5 
                                     other 
                                         2 
                       participant refusal 
                                         3 
                                      NA's 
                                       295 

patient.treatment.radiation_therapy_info.radiation_therapy_not_administered_specify:
                                 brain and pulmonary metastases leading to death 
                                                                               1 
radiation currently ongoing- treatment delayed due to postoperative complication 
                                                                               1 
                                                                            NA's 
                                                                             305 

patient.treatment.radiation_therapy_info.radiation_type_notes:
external beam and bilateral parametrial boost 
                                            1 
                     whole pelvis split field 
                                            1 
                                         NA's 
                                          305 

patient.treatment.radiation_therapy_info.rt_administered_type:
           3d conformal external beam radiation                    imrt 
                     54                      27                       5 
                  other                    NA's 
                      2                     219 

patient.treatment.radiation_therapy_info.rt_pelvis_administered_total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     50    4500    4500    4735    5400   11400     224 

patient.tumor_response_cdus_type:
complete response  partial response       progression            stable 
               42                 2                 4                 1 
             NA's 
              258 

patient.tumor_tissue_site:
cervical 
     307 

patient.vital_status:
alive  dead 
  247    60 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   58.25   70.50   73.04   83.75  210.00      29 

patient.year_of_form_completion:
2011 2012 2013 2014 2015 
  66   53   55  132    1 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1994    2006    2010    2008    2012    2013       2 



patient.hpv_test_results.hpv_test_result.hpv_call_3:
hpv58  NA's 
    1   306 

patient.hpv_test_results.hpv_test_result.hpv_status:
indeterminate      negative      positive 
            1            22           284 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
      case matched expression control                   germline comparison 
                                    3                                     1 
primary tumor and metastatic tumor(s)                                  NA's 
                                    2                                   301 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control                             NA's 
                               3                              304 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                  2                 305 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             301                6 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      275        32 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   118.0   748.5  1389.0  2128.8  7081.0       5 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -29.0     0.0     3.5    15.4    27.0   122.0       5 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
282  25 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    19                     65                    223 

patient.biospecimen_cqcf.ffpe_tumor_slide_submitted:
digital image         slide          NA's 
            8            85           214 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
cervix 
   307 


patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
 no 
307 

patient.biospecimen_cqcf.method_of_sample_procurement:
       other (please specify) other method (please specify) 
                           17                            94 
              tumor resection 
                          196 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -1.00    0.00   34.50   49.36   54.25  331.00     293 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
0.099   0.4   1.5   110  NA's 
    1     1     1     1   303 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
pico-green         uv    uv spec       NA's 
         1          2          1        303 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  10   11 20.3   21 NA's 
   1    1    1    1  303 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
0.101  13.5    55   100  NA's 
    1     1     1     1   303 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
                 8                  6                293 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
fallopian tube           skin         uterus           NA's 
             1              1              5            300 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            7           300 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
  13   14  280 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
8b52ee44-e43b-45fa-a978-ea4fe9068a67 de5453af-e271-44fa-9e5d-2bebd11f68b3 
                                   1                                    1 
                                NA's 
                                 305 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
  41 NA's 
   1  306 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
surgical resection               NA's 
                 1                306 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
skin NA's 
   1  306 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            2           305 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    2 NA's 
   1    1  305 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -69.00    0.00   11.00   27.58   30.00  904.00      11 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0530   0.2112   0.4345  69.0474   0.7518 915.6200      189 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.50   17.00   20.00   20.32   20.70   68.90     178 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.101  31.000  48.250  73.923  83.800 700.000     189 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
               291                  6                 10 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
myometrium      other     uterus       NA's 
         2          3          1        301 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            7           300 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2 NA's 
 302    1    4 


patient.biospecimen_cqcf.other_vessel_used:
biospecimen storage bag               cryosette    specimen storage bag 
                      1                       1                       2 
                   NA's 
                    303 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 yes NA's 
 303    4 

patient.biospecimen_cqcf.path_confirm_report_attached:
 no yes 
 14 293 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
yes 
307 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 no yes 
  1 306 

patient.biospecimen_cqcf.race:
american indian or alaska native                            asian 
                               6                               12 
       black or african american                            white 
                              17                              105 
                            NA's 
                             167 

patient.biospecimen_cqcf.sample_prescreened:
yes 
307 

patient.biospecimen_cqcf.site_of_disease:
cervix 
   307 

patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
 161   23  123 

patient.biospecimen_cqcf.top_slide_submitted:
 no yes 
 27 280 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0    5   30 NA's 
   1    1    7  298 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  60   70   95 NA's 
   1    7    1  298 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
  50   80  100 1312 NA's 
   1    1    6    1  298 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   8    2  297 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.bcr_sample_uuid:
6ef38e19-07ac-4032-8fb4-59129a126d3f                                 NA's 
                                   1                                  306 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_necrosis_percent:
  30 NA's 
   1  306 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_nuclei_percent:
  70 NA's 
   1  306 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_weight:
 100 NA's 
   1  306 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.vial_number:
   1 NA's 
   1  306 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   7.618  10.000  35.000       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   65.00   70.00   73.65   80.00  100.00       1 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   67.75  120.00  263.77  210.50 3133.00      15 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1 NA's 
 306    1 

patient.biospecimen_cqcf.vessel_used:
biospecimen storage bag                cassette                cryomold 
                     24                      51                      20 
               cryovial  other (please specify) 
                    208                       4 

patient.days_to_index:
   0 NA's 
 302    5 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and metastatic tumor(s)                                  NA's 
                                    2                                   305 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   197.2   778.5  1445.2  2171.0  7129.0       5 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0    82.5   100.0   330.0   267.5  1340.0     301 

patient.samples.sample.2.is_ffpe:
 no 
307 

patient.samples.sample.2.oct_embedded:
false  true 
  304     3 

patient.samples.sample.2.pathology_report_file_name:
tcga-hm-a6w2.fd13c302-78f0-4fee-b90a-95a10c236ce9.pdf 
                                                    1 
tcga-uc-a7pg.3b9b2cf0-2bfb-45d2-bab8-7dc83277acce.pdf 
                                                    1 
                                                 NA's 
                                                  305 

patient.samples.sample.2.pathology_report_uuid:
3b9b2cf0-2bfb-45d2-bab8-7dc83277acce fd13c302-78f0-4fee-b90a-95a10c236ce9 
                                   1                                    1 
                                NA's 
                                 305 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.48 1.87 1.91 NA's 
   1    1    1  304 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
118955149 118955936 122967819 132998060 141664282 167823013       300 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   9   13 NA's 
   4    3  300 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.17 0.18  0.5 NA's 
   1    2    4  300 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   12.50   24.00   20.29   28.00   30.00     300 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   7  300 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   1    6   10   12 NA's 
   1    1    1    4  300 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   6.000   6.571  11.000  12.000     300 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
 3.4  3.6   33 NA's 
   1    2    4  300 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   7  300 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  20   66 NA's 
   3    4  300 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 NA's 
   4    1    1    1  300 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97490628 113419970 132178904 138559371 167788814 174166172        51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8    9 NA's 
   3   42  211   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.17 0.18  0.5 NA's 
   1    2  253   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    8.00   15.00   15.93   22.00   30.00      51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 256   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.863   7.000  11.000      51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.422   9.000  12.000      51 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
2.21 2.34   40 NA's 
   1    2  253   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
 256   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   80 NA's 
   3  253   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
  75   47   42   92   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
  6.8 20.83 51.12  NA's 
    1     1     1   304 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 253                    3                   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   3  253   51 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
0.17 0.18 NA's 
   1    2  304 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 253   54 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
 253   54 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             256 
                                            NA's 
                                              51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 256   51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                        3                       253                        51 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
0.98  1.3 1.69 NA's 
   1    1    1  304 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
 7.5  7.6  9.1 NA's 
   1    1    1  304 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      3     304 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
   3    2    1  301 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-fu-a3eo-11a-13w-a21i-09 tcga-hm-a6w2-06a-22w-a37w-09 
                           1                            1 
                        NA's 
                         305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
6833bc09-f071-4d82-90f5-3e87481b3d14 a4620479-da52-4fce-a82f-a5e944a9bcb7 
                                   1                                    1 
                                NA's 
                                 305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
122969410 164054440      NA's 
        1         1       305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   9 NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  26   29 NA's 
   1    1  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   2    3 NA's 
   1    1  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   2    7 NA's 
   1    1  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a21i a37w NA's 
   1    1  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   b    d NA's 
   1    1  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2014 NA's 
   1    1  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-fu-a3eo-11a-13w tcga-hm-a6w2-06a-22w                 NA's 
                   1                    1                  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
2deae69c-d2a8-44bc-8f5a-916a73ee52e4 b3324bfc-336e-41ae-b865-7bd74231de2d 
                                   1                                    1 
                                NA's 
                                 305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     305 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   2    3 NA's 
   1    1  305 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.170   1.790   1.920   1.898   1.970   2.500     262 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97484789 117833484 145672995 140807271 164103672 174165207 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   2   8   9 
 40  27   8 232 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07000 0.07000 0.08000 0.09635 0.09000 0.18000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   17.00   24.00   20.11   26.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.277   8.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   6.873   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.800   2.800   2.678   3.200   3.600 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
307 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   40.00   40.00   32.66   40.00   40.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
  75   59   66  107 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97486992 108474569 113390580 115260795 122969396 174165177       183 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00    9.00   14.25   26.00   26.00     183 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0900  0.1157  0.1500  0.1700     183 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00    7.00   12.96   27.00   31.00     183 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 124  183 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.008  10.000  11.000     183 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   8.000   8.435  10.250  12.000     183 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.600   3.200   2.487   3.200   3.600     183 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 124  183 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   10.00   40.00   26.15   40.00   40.00     183 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 NA's 
  75   45    1    3  183 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113422276 118955166 118955192 121678757 118955217 160356080       262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    8    9 NA's 
   1    1   43  262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.07 0.08 0.09 0.16 NA's 
   3   24   17    1  262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   5    7   24   25 NA's 
   1   40    1    3  262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  45  262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1    7   11   12 NA's 
   1    1    3   40  262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   9.000   8.467  10.000  12.000     262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a16y a200 a212 a34h NA's 
   1   40    1    3  262 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   2.080   2.080   2.133   2.340   2.700     262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
  45  262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  13   20   26   30 NA's 
   1    3   40    1  262 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 2013 NA's 
  41    1    3  262 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
122970851 122970875 122970876 131939249 122971895 167811750       302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   9 NA's 
   5  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 0.08 0.09 NA's 
   1    1    3  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  14   21   28 NA's 
   3    1    1  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   5  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1    3   12 NA's 
   1    3    1  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    2    7 NA's 
   1    3    1  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a208 a21o a46a NA's 
   1    3    1  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   a    b    d    h NA's 
   2    1    1    1  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
1.21 1.82  3.2  3.6 NA's 
   1    1    1    2  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   5  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
13.4   26   40 NA's 
   1    1    3  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 2012 2015 NA's 
   1    3    1  302 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-ds-a0vk-10a-01d-a21o-09                         NA's 
                           1                          306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
93844bfb-3675-4251-b1c0-aca221d0c97e                                 NA's 
                                   1                                  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
122970852      NA's 
        1       306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   9 NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  14 NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   3 NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   2 NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a21o NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   c NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
 2.8 NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  40 NA's 
   1  306 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2012 NA's 
   1  306 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97483781 117833478 132273057 140262143 164103664 174165215 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2 
259  48 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 0.18 
  51   90   83   72   11 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   12.00   25.00   18.98   26.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.909   6.000  11.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   7.000   6.873   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.94    1.01    1.07    1.20    1.14    2.34 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
307 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7  10  13 
259   8  40 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
  75   59   55  118 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.88   13.47   21.55   30.43   39.56  197.10 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
307 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
307 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 0.18 
  51   90   83   72   11 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
307 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
307 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             307 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                              176                               131 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      262        45 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
   3    2    1  301 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    5.00   13.00   13.55   21.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no 
307 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   6.033   9.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   1.306   1.000  31.000 

patient.samples.sample.2.portions.portion.portion_sequence:
   1    2    3 NA's 
 204    1    1  101 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   6  301 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    5    8   40 NA's 
   2    1    1    1  302 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0    1    2   20 NA's 
   2    1    1    1  302 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0    5 NA's 
   4    1  302 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1    2   20 NA's 
   2    1    1    1  302 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0   15   20  100 NA's 
   1    1    1    3  301 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   0   20   80 NA's 
   2    2    1  302 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0   60   80 NA's 
   3    1    1  302 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0   60   80 NA's 
   3    1    1  302 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
   6  301 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    2.00   16.35    2.00  500.00     131 

patient.samples.sample.2.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
   8   98   34   93   74 

patient.samples.sample.2.sample_type:
blood derived normal           metastatic  solid tissue normal 
                 301                    2                    4 

patient.samples.sample.2.sample_type_id:
  6  10  11 
  2 301   4 

patient.samples.sample.2.vial_number:
  a   b 
290  17 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   51.0    97.0   117.0   951.2   629.0  3862.0     302 

patient.samples.sample.3.initial_weight:
   1  220  430  950 NA's 
   1    1    1    1  303 

patient.samples.sample.3.is_ffpe:
  no NA's 
   5  302 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
    3     2   302 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.74 1.85 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-hm-a3jj-11a-12r-a21u-13 tcga-my-a5bf-11a-11r-a26x-13 
                           1                            1 
                        NA's 
                         305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
870c8c76-1283-425f-ac64-12216e1ef8fe d403aaf5-26af-448f-9f71-e9abc40a57ac 
                                   1                                    1 
                                NA's 
                                 305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
122971608 145675911      NA's 
        1         1       305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  13 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.17 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  27   28 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   2    3 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   3    4 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a21u a26x NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   e NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   3  3.4 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  20 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2012 2013 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-hm-a3jj-11a-12r-a21t-07 tcga-hm-a4s6-11a-11w-a30l-09 
                           1                            1 
tcga-hm-a6w2-10a-01w-a37w-09 tcga-my-a5bf-11a-11r-a26t-07 
                           1                            1 
                        NA's 
                         303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
0a78e78c-f31a-488a-bf37-c47b108954fd 5a5ce32f-5f84-4e96-b655-fa98d0101fc5 
                                   1                                    1 
8c68f443-2093-4feb-afed-2a67e558129c ea52408f-b5e1-4a15-80bc-945cc74135e4 
                                   1                                    1 
                                NA's 
                                 303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
122970471 145676103 150705556 164054418      NA's 
        1         1         1         1       303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    9 NA's 
   2    2  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.17  0.5 NA's 
   1    1    2  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  12   26   27   28 NA's 
   1    1    1    1  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   2    3    6 NA's 
   1    2    1  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   3    4    9 NA's 
   2    1    1  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a21t a26t a30l a37w NA's 
   1    1    1    1  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   d    e    f NA's 
   1    2    1  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
1.95 2.21   40 NA's 
   1    1    2  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   4  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   80 NA's 
   2    2  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
   1    2    1  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
7.58 9.01 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   2                    2                  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   2    2  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-hm-a3jj-11a-12r tcga-hm-a4s6-11a-11w tcga-hm-a6w2-10a-01w 
                   1                    1                    1 
tcga-my-a5bf-11a-11r                 NA's 
                   1                  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
0d4dcbc0-b81e-4ddb-bc45-94726417cf83 2c686b3a-37d1-470d-9570-d001fd6ee489 
                                   1                                    1 
61635452-bed9-4baf-a3c1-40362cf8ce11 eecb0432-3d8d-4adf-812a-4c61a6d2ef6a 
                                   1                                    1 
                                NA's 
                                 303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.15 0.17 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   4  303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                        2                         2                       303 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   1  1.2 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   7  7.4 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      2     305 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
   2    1  304 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-hm-a3jj-11a-12w-a225-09 tcga-my-a5bf-11a-11w-a30l-09 
                           1                            1 
                        NA's 
                         305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
b9b6f8ec-562a-42a4-bc08-e0072df2bcc8 ec19c33c-cd0f-45a4-afdf-dc30275ea6d9 
                                   1                                    1 
                                NA's 
                                 305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
122972370 150705555      NA's 
        1         1       305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   9 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  12   16 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5    6 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   4    6 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a225 a30l NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   d    g NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2012 2013 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-hm-a3jj-11a-12w tcga-my-a5bf-11a-11w                 NA's 
                   1                    1                  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
54896a86-e2cd-4849-a671-1748d966be26 6493e683-49af-4c94-9729-056296e1b90b 
                                   1                                    1 
                                NA's 
                                 305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     305 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
   1    1  305 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
122971218 145673039 145673052 149767247 160355550 174163377       302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   9 NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.07 0.08 NA's 
   1    4  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  18   27   28   30 NA's 
   1    2    1    1  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   2    3    6   10 NA's 
   2    1    1    1  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     4.0     6.0     5.8     7.0     9.0     302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a21q a26g a33o a42r NA's 
   1    2    1    1  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   d    f    g NA's 
   2    1    2  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 2.8  3.2 NA's 
   1    4  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  40 NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2012 2013 2014 NA's 
   1    3    1  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-hm-a3jj-11a-12d-a21r-05 tcga-my-a5bf-11a-11d-a26s-05 
                           1                            1 
                        NA's 
                         305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
caf87bfa-9620-490f-a284-7917eabbb221 f2db94bc-a410-459a-b8a7-845f0eaf0575 
                                   1                                    1 
                                NA's 
                                 305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
122970503 132159814      NA's 
        1         1       305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.15 0.17 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  27   28 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   2    3 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   4    6 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a21r a26s NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   d    g NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
1.95 2.21 NA's 
   1    1  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  13 NA's 
   2  305 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2012 2013 NA's 
   1    1  305 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
122970738 128925374 145673000 146417957 160355534 174165137       302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.15 0.17 NA's 
   2    3  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  18   27   28   30 NA's 
   1    2    1    1  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   2    3    6   10 NA's 
   2    1    1    1  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     4.0     6.0     5.8     7.0     9.0     302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a21p a26f a33n a42q NA's 
   1    2    1    1  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   d    f    g NA's 
   2    1    2  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
1.01 1.14 NA's 
   2    3  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2012 2013 2014 NA's 
   1    3    1  302 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.12    7.52   12.38   16.04   14.33   39.84     302 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   5  302 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
0.15 0.17 NA's 
   2    3  302 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             302 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   5  302 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    5                   302 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        5       302 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
   3    1  303 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0     9.0    19.0    16.2    21.0    27.0     302 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
   5  302 

patient.samples.sample.3.portions.portion.month_of_creation:
   5    8   11   12 NA's 
   1    1    1    2  302 

patient.samples.sample.3.portions.portion.portion_number:
   1   11   12 NA's 
   1    3    1  302 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
   4  303 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_barcode:
tcga-hm-a3jj-11a-01-ts1 tcga-hm-a4s6-11a-01-ts1 tcga-my-a5bf-11a-01-tsa 
                      1                       1                       1 
tcga-uc-a7pg-11c-01-ts1                    NA's 
                      1                     303 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_uuid:
176defe3-92cd-4f70-8b77-362b45c19c2d 566b4c64-64c8-4849-a166-5b64637798d1 
                                   1                                    1 
cf3df233-0fc1-49b6-bada-bf52a9b11830 d33167ba-479f-4278-a4ef-a0b00cdd09c6 
                                   1                                    1 
                                NA's 
                                 303 

patient.samples.sample.3.portions.portion.slides.slide.image_file_name:
tcga-hm-a3jj-11a-01-ts1.176defe3-92cd-4f70-8b77-362b45c19c2d.svs 
                                                               1 
tcga-hm-a4s6-11a-01-ts1.cf3df233-0fc1-49b6-bada-bf52a9b11830.svs 
                                                               1 
tcga-my-a5bf-11a-01-tsa.566b4c64-64c8-4849-a166-5b64637798d1.svs 
                                                               1 
tcga-uc-a7pg-11c-01-ts1.d33167ba-479f-4278-a4ef-a0b00cdd09c6.svs 
                                                               1 
                                                            NA's 
                                                             303 

patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   4  303 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    1 NA's 
   1    3  303 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   4  303 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   4  303 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   4  303 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
 100 NA's 
   4  303 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0 NA's 
   4  303 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   4  303 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   4  303 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
   4  303 

patient.samples.sample.3.portions.portion.weight:
   2   30 NA's 
   1    4  302 

patient.samples.sample.3.portions.portion.year_of_creation:
2011 2012 2013 2014 NA's 
   1    2    1    1  302 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   1                    4                  302 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
   1    4  302 

patient.samples.sample.3.vial_number:
   a    c NA's 
   4    1  302 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   197.2   776.0  1446.4  2171.0  7129.0       5 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
19acbf59-9114-4980-a44c-5acf414a09cf 6da5ead4-bb7c-42ed-be81-cef2fae7a11c 
                                   1                                    1 
cf669de8-217e-47a6-839f-abe9657ba566                                 NA's 
                                   1                                  304 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.4:
48f35f8c-450b-4851-95d0-642b2a700671 feb6720a-6d9c-4ee7-9251-1be636ea5269 
                                   1                                    1 
                                NA's 
                                 305 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0    60.0   110.0   214.7   190.0  2320.0 

patient.samples.sample.is_ffpe:
 no 
307 

patient.samples.sample.oct_embedded:
false  true 
  145   162 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.330   1.820   1.860   1.858   1.890   2.160 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486220 117832620 145675881 140600317 164103800 167823167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  7  13 
 93 214 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1400  0.1600  0.1700  0.1674  0.1700  0.2000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   17.00   24.00   20.31   26.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.277   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.505   6.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   3.000   3.400   3.229   3.600   5.340 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
 23 
307 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 26.7 
  61  214   32 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
  75   59   66  107 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
150756966 150757028 150757039 153381017 150757051 167812939       294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
  13  294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 0.16 0.17 0.18 NA's 
   4    3    3    3  294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2    9 NA's 
  11    2  294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  13  294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  10   12 NA's 
  11    2  294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       3       4       4       5       6     294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a32y a466 NA's 
  11    2  294 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.060   1.950   2.080   1.968   2.210   2.340     294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
  13  294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6   13 NA's 
   2   11  294 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2013 2014 NA's 
  11    2  294 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486988 122969186 145676073 140117152 164103804 174163810 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  7  13 
214  93 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1400  0.1600  0.1700  0.1674  0.1700  0.2000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   17.00   24.00   20.11   26.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.277   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.505   6.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   2.210   2.340   2.587   3.200   4.540 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
 23 
307 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20 26.7 
 206   93    8 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
  75   59   66  107 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.62   41.05   53.71   52.77   64.02  110.71 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
307 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
  r 
307 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1400  0.1600  0.1700  0.1674  0.1700  0.2000 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             307 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                      307 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.200   1.400   1.590   1.571   1.800   2.680 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  4.600   7.500   8.200   8.163   8.800  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
    307 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
 278   20    9 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
1.19e+08 1.19e+08 1.19e+08 1.19e+08 1.19e+08 1.19e+08      302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   9 NA's 
   5  302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   5  302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   7 NA's 
   5  302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
  12 NA's 
   5  302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   5    6 NA's 
   3    2  302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
a200 NA's 
   5  302 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  33 NA's 
   5  302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  23 NA's 
   5  302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  66 NA's 
   5  302 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2011 NA's 
   5  302 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97490630 113421813 132178898 138285349 167788806 167804650        51 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8    9 NA's 
  42  214   51 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 256   51 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    8.00   15.00   15.97   22.00   30.00      51 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 256   51 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    6.00    5.82    7.00   11.00      51 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   2.734   4.000   6.000      51 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 256   51 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
 256   51 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 256   51 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 NA's 
  75   47   41   93   51 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                 256                   51 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
 256   51 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 256   51 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
 256   51 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             256 
                                            NA's 
                                              51 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 256   51 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
    256      51 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
 230   17   60 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   1.870   1.940   1.932   1.980   2.180     262 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486218 117833378 145672994 140807460 164105014 174162815 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   2   8   9 
 40  27   8 232 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07000 0.08000 0.08000 0.09668 0.09000 0.18000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   17.00   24.00   20.11   26.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   5.000   5.277   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   2.971   4.000   8.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.800   3.200   2.696   3.200   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
307 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67   40.00   40.00   32.66   40.00   40.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 2012 2013 2014 
  75   59   66  107 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486986 120903201 132273348 140914565 164103574 174165206 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
  1   5   8   9  26 
  8 209  34  33  23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1400  0.1500  0.1397  0.1600  0.1700 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00   12.00   18.00   19.03   26.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.427   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   2.984   4.000   8.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.930   1.950   2.080   2.241   2.210   3.600 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
 23 
307 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
6.67   10   13   40 
   8   23  209   67 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 2012 2013 2014 
  75   59   66  107 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99019064 108473711 113390570 113860261 122848420 132271195       187 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5   26 NA's 
  98   22  187 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.1592  0.1700  0.1800     187 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00   16.50   16.11   27.00   31.00     187 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 120  187 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   5.000   5.575  10.000  11.000     187 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   3.583   5.000   8.000     187 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.40    1.95    2.08    2.73    4.27    4.81     187 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 120  187 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10   13 26.7 NA's 
  22   58   40  187 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 2012 NA's 
  75   45  187 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113422275 118955152 118955180 121274924 118955209 160356040       263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    8    9 NA's 
   3    1   40  263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.07000 0.08000 0.08000 0.08841 0.09000 0.17000     263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   5    7   25 NA's 
   4   37    3  263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  44  263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   7   11   12 NA's 
   4    3   37  263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.75    2.50    2.75    4.00    7.00     263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a16y a200 a34h NA's 
   4   37    3  263 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.600   2.080   2.080   2.175   2.340   4.500     263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
  44  263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0    26.0    26.0    25.3    26.0    50.0     263 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 2013 NA's 
  41    3  263 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113422286 113422288 122970822 124063273 122970824 160356031       299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8    9 NA's 
   3    5  299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 0.08 0.09 NA's 
   1    5    2  299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   5   14   25 NA's 
   3    4    1  299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   8  299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   3    7   11 NA's 
   4    3    1  299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   1    2    7 NA's 
   4    1    3  299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a16y a21o a34h NA's 
   3    4    1  299 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.28    2.02    2.52    2.51    3.20    3.60     299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   8  299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     16      23      34      31      40      40     299 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 2012 2013 NA's 
   3    4    1  299 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 97486794 117833268 145673003 140809811 164103670 174164247 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2 
259  48 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16 0.17 0.18 
  25   97  124   53    8 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   12.00   25.00   18.98   26.00   30.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   4.000   4.909   6.000  11.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   2.971   4.000   8.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.940   1.010   1.070   1.204   1.140   2.340 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
307 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.7  10  13 
259   8  40 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 2012 2013 2014 
  75   59   55  118 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   7.21   33.00   46.29   48.83   61.19  124.76 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
307 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
307 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
0.14 0.15 0.16 0.17 0.18 
  25   97  124   53    8 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
307 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
307 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             307 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  307 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      262        45 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
 278   20    9 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   12.00   18.00   18.42   27.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
307 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.500   6.000   6.251  10.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  11.00   11.00   11.00   13.19   11.00   81.00 

patient.samples.sample.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.259   1.000   8.000      91 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   9  298 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   20.00   20.22   30.00   60.00     298 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0    1   10 NA's 
   6    1    2  298 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   0    5   10 NA's 
   6    2    1  298 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   5.111   3.000  20.000     298 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   0    3   35 NA's 
   7    1    1  298 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0       5      20      18      20      50     298 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   60.00   80.00   75.56   90.00  100.00     298 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  45.00   60.00   70.00   67.22   75.00   90.00     298 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
 top NA's 
   9  298 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
307 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    2.00    5.00   16.48   40.00   80.00 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   5.033  10.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   1.000   3.371   5.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   7.531  20.000  70.000 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   0.000   0.000   4.156   5.000  92.000 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    5.00   15.00   13.69   20.00   65.00 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   70.00   80.00   78.99   90.00  100.00       2 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.00   70.00   80.00   76.16   85.00   98.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
     9    298 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  10.00   30.00   30.00   28.44   30.00   30.00 

patient.samples.sample.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
   9   88   47   97   66 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 173  134 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 173  134 

patient.samples.sample.portions.shipment_portion.plate_id:
a40h a40i a40j NA's 
  81   81   11  134 

patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   20.54   21.00   71.00     134 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   1.964   2.000   7.000     167 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  10 NA's 
 173  134 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   6 NA's 
 173  134 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
 173  134 

patient.samples.sample.sample_type:
primary tumor 
          307 

patient.samples.sample.sample_type_id:
  1 
307 

patient.samples.sample.vial_number:
  a   b   c 
299   7   1 

}}
\keyword{datasets}
